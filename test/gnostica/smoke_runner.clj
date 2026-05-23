(ns gnostica.smoke-runner
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [gnostica.icon-layout :as icon-layout]
            [gnostica.server :as app-server]
            [ring.adapter.jetty :as jetty])
  (:import [java.io ByteArrayInputStream File]
           [java.net ServerSocket URI URLEncoder]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers WebSocket WebSocket$Listener]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.time Duration]
           [java.util Base64]
           [java.util.concurrent CompletableFuture TimeUnit]
           [java.util.concurrent.atomic AtomicLong]
           [java.lang ProcessBuilder ProcessBuilder$Redirect]
           [javax.imageio ImageIO]))

(def cdp-timeout-ms 10000)
(def wait-timeout-ms 20000)
(def expected-table-surface-color "#2a1020")
(def expected-table-clear-color "#12070f")
(def min-velvet-pixels 120)

(def viewports
  [{:name "desktop" :width 1280 :height 900 :mobile false}
   {:name "mobile" :width 390 :height 844 :mobile true}])

(defn- getenv [k]
  (not-empty (System/getenv k)))

(defn- executable-file? [path]
  (when (seq path)
    (let [file (io/file path)]
      (when (and (.isFile file) (.canExecute file))
        (.getAbsolutePath file)))))

(defn- path-command [command]
  (some executable-file?
        (for [dir (str/split (or (System/getenv "PATH") "") #":")]
          (str dir File/separator command))))

(defn- chrome-executable []
  (or (some executable-file?
            [(getenv "SMOKE_CHROME")
             (getenv "CHROME_PATH")
             "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
             "/Applications/Chromium.app/Contents/MacOS/Chromium"
             "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary"
             "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe"
             "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe"])
      (some path-command
            ["google-chrome" "google-chrome-stable" "chromium" "chromium-browser" "chrome"])))

(defn- free-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn- delete-recursive! [path]
  (let [file (.toFile path)]
    (when (.exists file)
      (doseq [child (reverse (file-seq file))]
        (io/delete-file child true)))))

(defn- launch-chrome! []
  (let [executable (or (chrome-executable)
                       (throw (ex-info "Chrome or Chromium executable not found. Set SMOKE_CHROME to the browser path."
                                       {:checked-env ["SMOKE_CHROME" "CHROME_PATH"]})))
        port (free-port)
        profile-dir (Files/createTempDirectory "gnostica-smoke-chrome-"
                                               (make-array FileAttribute 0))
        args [executable
              "--headless=new"
              "--remote-debugging-address=127.0.0.1"
              (str "--remote-debugging-port=" port)
              (str "--user-data-dir=" profile-dir)
              "--no-first-run"
              "--no-default-browser-check"
              "--disable-background-networking"
              "--disable-extensions"
              "--disable-sync"
              "--disable-dev-shm-usage"
              "--hide-scrollbars"
              "--window-size=1280,900"
              "about:blank"]
        process (-> (ProcessBuilder. args)
                    (.redirectOutput ProcessBuilder$Redirect/DISCARD)
                    (.redirectError ProcessBuilder$Redirect/DISCARD)
                    .start)]
    {:executable executable
     :port port
     :process process
     :profile-dir profile-dir}))

(defn- stop-chrome! [{:keys [process profile-dir]}]
  (when process
    (.destroy process)
    (when-not (.waitFor process 5 TimeUnit/SECONDS)
      (.destroyForcibly process)
      (.waitFor process 5 TimeUnit/SECONDS)))
  (when profile-dir
    (delete-recursive! profile-dir)))

(defn- request-builder [url]
  (-> (HttpRequest/newBuilder (URI/create url))
      (.timeout (Duration/ofSeconds 5))))

(defn- http-json
  ([client url]
   (http-json client :get url))
  ([client method url]
   (let [builder (request-builder url)
         request (case method
                   :put (-> builder
                            (.PUT (HttpRequest$BodyPublishers/noBody))
                            .build)
                   (-> builder .GET .build))
         response (.send client request (HttpResponse$BodyHandlers/ofString))]
     (when-not (<= 200 (.statusCode response) 299)
       (throw (ex-info "Chrome DevTools HTTP request failed."
                       {:url url
                        :status (.statusCode response)
                        :body (.body response)})))
     (json/read-str (.body response)))))

(defn- wait-for-cdp! [client {:keys [port process]}]
  (let [version-url (str "http://127.0.0.1:" port "/json/version")
        deadline (+ (System/currentTimeMillis) cdp-timeout-ms)]
    (loop [last-error nil]
      (cond
        (not (.isAlive process))
        (throw (ex-info "Chrome exited before the DevTools endpoint became available."
                        {:port port
                         :last-error (some-> last-error ex-message)}))

        (> (System/currentTimeMillis) deadline)
        (throw (ex-info "Timed out waiting for Chrome DevTools endpoint."
                        {:url version-url
                         :last-error (some-> last-error ex-message)}))

        :else
        (let [result (try
                       (http-json client version-url)
                       (catch Exception error
                         error))]
          (if (map? result)
            result
            (do
              (Thread/sleep 100)
              (recur result))))))))

(defn- new-page-websocket! [client port]
  (let [url (str "http://127.0.0.1:"
                 port
                 "/json/new?"
                 (URLEncoder/encode "about:blank" StandardCharsets/UTF_8))
        result (http-json client :put url)
        websocket-url (get result "webSocketDebuggerUrl")]
    (or websocket-url
        (throw (ex-info "Chrome did not return a page WebSocket URL."
                        {:response result})))))

(defn- handle-cdp-message! [pending events text]
  (let [message (json/read-str text)]
    (if-let [id (get message "id")]
      (when-let [response (get @pending id)]
        (swap! pending dissoc id)
        (deliver response message))
      (swap! events conj message))))

(defn- connect-cdp! [http-client websocket-url]
  (let [next-id (AtomicLong. 0)
        pending (atom {})
        events (atom [])
        partial (atom "")]
    (let [listener (reify WebSocket$Listener
                     (onOpen [_ websocket]
                       (.request websocket 1))
                     (onText [_ websocket data last?]
                       (if last?
                         (let [text (str @partial data)]
                           (reset! partial "")
                           (handle-cdp-message! pending events text))
                         (swap! partial str data))
                       (.request websocket 1)
                       (CompletableFuture/completedFuture nil))
                     (onError [_ _ error]
                       (swap! events conj {"method" "gnostica.smoke/websocket-error"
                                           "params" {"message" (.getMessage error)}}))
                     (onClose [_ _ status-code reason]
                       (swap! events conj {"method" "gnostica.smoke/websocket-closed"
                                           "params" {"status" status-code
                                                     "reason" reason}})
                       (CompletableFuture/completedFuture nil)))
          websocket (-> http-client
                        .newWebSocketBuilder
                        (.buildAsync (URI/create websocket-url) listener)
                        .join)]
      {:websocket websocket
       :next-id next-id
       :pending pending
       :events events})))

(defn- close-cdp! [{:keys [websocket]}]
  (when websocket
    (.join (.sendClose websocket WebSocket/NORMAL_CLOSURE "done"))))

(defn- cdp-command!
  ([client method]
   (cdp-command! client method nil))
  ([{:keys [websocket next-id pending]} method params]
   (let [id (.incrementAndGet next-id)
         response (promise)
         payload (cond-> {"id" id
                          "method" method}
                   params (assoc "params" params))]
     (swap! pending assoc id response)
     (.join (.sendText websocket (json/write-str payload) true))
     (let [message (deref response cdp-timeout-ms ::timeout)]
       (swap! pending dissoc id)
       (cond
         (= ::timeout message)
         (throw (ex-info "Timed out waiting for Chrome DevTools command response."
                         {:method method
                          :params params}))

         (get message "error")
         (throw (ex-info "Chrome DevTools command failed."
                         {:method method
                          :params params
                          :error (get message "error")}))

         :else
         (get message "result"))))))

(defn- evaluate! [client expression]
  (let [result (cdp-command! client
                             "Runtime.evaluate"
                             {"expression" expression
                              "returnByValue" true
                              "awaitPromise" true})
        exception (get result "exceptionDetails")]
    (when exception
      (throw (ex-info "Browser evaluation failed."
                      {:expression expression
                       :exception exception})))
    (get-in result ["result" "value"])))

(defn- wait-for! [client description expression ready?]
  (let [deadline (+ (System/currentTimeMillis) wait-timeout-ms)]
    (loop [last-value nil
           last-error nil]
      (if (> (System/currentTimeMillis) deadline)
        (throw (ex-info (str "Timed out waiting for " description ".")
                        {:last-value last-value
                         :last-error (some-> last-error ex-message)}))
        (let [[ok? value error] (try
                                  [true (evaluate! client expression) nil]
                                  (catch Exception error
                                    [false nil error]))]
          (cond
            (and ok? (ready? value))
            value

            ok?
            (do
              (Thread/sleep 250)
              (recur value nil))

            :else
            (do
              (Thread/sleep 250)
              (recur last-value error))))))))

(defn- prepare-page!
  ([client viewport blocked-urls]
   (prepare-page! client viewport blocked-urls nil))
  ([client {:keys [width height mobile]} blocked-urls init-script]
   (cdp-command! client "Page.enable")
   (cdp-command! client "Runtime.enable")
   (cdp-command! client "Log.enable")
   (cdp-command! client "Network.enable")
   (cdp-command! client "Network.setCacheDisabled" {"cacheDisabled" (boolean (seq blocked-urls))})
   (when (seq blocked-urls)
     (cdp-command! client "Network.setBlockedURLs" {"urls" blocked-urls}))
   (when init-script
     (cdp-command! client
                   "Page.addScriptToEvaluateOnNewDocument"
                   {"source" init-script}))
   (cdp-command! client
                 "Emulation.setDeviceMetricsOverride"
                 {"width" width
                  "height" height
                  "deviceScaleFactor" 1
                  "mobile" (boolean mobile)})))

(def app-ready-js
  "Boolean(document.querySelector('.app-shell') || document.querySelector('.setup-error'))")

(def happy-stats-js
  "(() => {
     const board = document.querySelector('.board-three');
     const canvas = document.querySelector('.board-three__canvas');
     const rect = canvas ? canvas.getBoundingClientRect() : null;
     const antialiasSupported = (() => {
       const probe = document.createElement('canvas');
       const context = probe.getContext('webgl2', {antialias: true})
         || probe.getContext('webgl', {antialias: true})
         || probe.getContext('experimental-webgl', {antialias: true});
       const attributes = context && context.getContextAttributes ? context.getContextAttributes() : null;
       return Boolean(attributes && attributes.antialias);
     })();
     const cardZones = document.querySelector('.card-zones');
     const cardZonesRect = cardZones ? cardZones.getBoundingClientRect() : null;
     const status = Array.from(document.querySelectorAll('.board-3d-status.is-error')).map((node) => node.textContent.trim());
     const imageResourceCount = performance.getEntriesByType('resource')
       .filter((entry) => /\\/images\\/.*\\.png(?:$|\\?)/.test(entry.name)).length;
     const iconMetrics = (selector) => {
       const icon = document.querySelector(selector);
       const stack = icon ? icon.closest('.gnostica-icon-stack') : null;
       const face = icon ? icon.closest('.card-face') : null;
       const iconRect = icon ? icon.getBoundingClientRect() : null;
       const faceRect = face ? face.getBoundingClientRect() : null;
       const stackStyle = stack ? getComputedStyle(stack) : null;
       const gap = stackStyle ? Number.parseFloat(stackStyle.rowGap || stackStyle.gap || '') : -1;
       return {
         scale: stack ? Number(stack.dataset.iconScale || -1) : -1,
         iconWidth: iconRect ? iconRect.width : 0,
         faceWidth: faceRect ? faceRect.width : 0,
         widthRatio: iconRect && faceRect && faceRect.width > 0 ? iconRect.width / faceRect.width : 0,
         gap: Number.isFinite(gap) ? gap : -1
       };
     };
     return {
       url: location.href,
       cardIconMode: (document.querySelector('.app-shell') || {}).dataset.cardIconMode || null,
       threeRevision: window.THREE ? window.THREE.REVISION : null,
       orbitControls: Boolean(window.THREE && window.THREE.OrbitControls),
       board: Boolean(board),
       boardCardIconMode: board ? board.dataset.cardIconMode : null,
       boardCardCount: board ? Number(board.dataset.boardCardCount || -1) : -1,
       majorIconCardCount: board ? Number(board.dataset.majorIconCardCount || -1) : -1,
       majorIconCount: board ? Number(board.dataset.majorIconCount || -1) : -1,
       cardIconScale: board ? Number(board.dataset.cardIconScale || -1) : -1,
       cardIconSize: board ? Number(board.dataset.cardIconSize || -1) : -1,
       wastelandCount: board ? Number(board.dataset.wastelandCount || -1) : -1,
       visiblePieceCount: board ? Number(board.dataset.visiblePieceCount || -1) : -1,
       pieceEdgeOutlineCount: board ? Number(board.dataset.pieceEdgeOutlineCount || -1) : -1,
       selectedIndex: board ? Number(board.dataset.selectedBoardIndex || -1) : -1,
       tableSurfaceColor: board ? board.dataset.tableSurfaceColor : null,
       tableClearColor: board ? board.dataset.tableClearColor : null,
       textureErrorCount: board ? Number(board.dataset.textureErrorCount || -1) : -1,
       fallback: Boolean(document.querySelector('.board-fallback')),
       canvas: Boolean(canvas),
       canvasWidth: canvas ? canvas.width : 0,
       canvasHeight: canvas ? canvas.height : 0,
       canvasClientWidth: rect ? Math.round(rect.width) : 0,
       canvasClientHeight: rect ? Math.round(rect.height) : 0,
       antialiasRequested: board ? board.dataset.antialiasRequested === 'true' : false,
       antialiasEnabled: board ? board.dataset.antialiasEnabled === 'true' : false,
       antialiasSupported,
       reset: Boolean(document.querySelector('.board-three__reset')),
       cardZones: Boolean(cardZones),
       cardZonesVisible: Boolean(cardZonesRect && cardZonesRect.width > 0 && cardZonesRect.height > 0),
       handCardCount: document.querySelectorAll('.hand-card').length,
       handMajorIconStackCount: document.querySelectorAll('.hand-card .gnostica-icon-stack').length,
       handMajorIconCount: document.querySelectorAll('.hand-card .gnostica-icon').length,
       handIconMetrics: iconMetrics('.hand-card .gnostica-icon'),
       drawCount: cardZones ? Number(cardZones.dataset.drawCount || -1) : -1,
       discardCount: cardZones ? Number(cardZones.dataset.discardCount || -1) : -1,
       status,
       imageResourceCount
     };
   })()")

(def canvas-rect-js
  "(() => {
     const canvas = document.querySelector('.board-three__canvas');
     if (!canvas) return null;
     const rect = canvas.getBoundingClientRect();
     return {
       x: rect.left,
       y: rect.top,
       width: rect.width,
       height: rect.height,
       centerX: rect.left + (rect.width / 2),
       centerY: rect.top + (rect.height / 2)
     };
   })()")

(def selection-js
  "(() => {
     const board = document.querySelector('.board-three');
     const panel = document.querySelector('.territory-panel');
     return {
       selectedIndex: board ? Number(board.dataset.selectedBoardIndex || -1) : -1,
       panelText: panel ? panel.innerText : ''
     };
   })()")

(def fallback-stats-js
  "(() => {
     const status = document.querySelector('.board-3d-status');
     const stage = document.querySelector('.board-fallback .board-stage');
     const fallbackFace = document.querySelector('.board-fallback .card-face');
     const cardZones = document.querySelector('.card-zones');
     const cardZonesRect = cardZones ? cardZones.getBoundingClientRect() : null;
     const iconMetrics = (selector) => {
       const icon = document.querySelector(selector);
       const stack = icon ? icon.closest('.gnostica-icon-stack') : null;
       const face = icon ? icon.closest('.card-face') : null;
       const iconRect = icon ? icon.getBoundingClientRect() : null;
       const faceRect = face ? face.getBoundingClientRect() : null;
       const stackStyle = stack ? getComputedStyle(stack) : null;
       const gap = stackStyle ? Number.parseFloat(stackStyle.rowGap || stackStyle.gap || '') : -1;
       return {
         scale: stack ? Number(stack.dataset.iconScale || -1) : -1,
         iconWidth: iconRect ? iconRect.width : 0,
         faceWidth: faceRect ? faceRect.width : 0,
         widthRatio: iconRect && faceRect && faceRect.width > 0 ? iconRect.width / faceRect.width : 0,
         gap: Number.isFinite(gap) ? gap : -1
       };
     };
     return {
       cardIconMode: (document.querySelector('.app-shell') || {}).dataset.cardIconMode || null,
       threeRevision: window.THREE ? window.THREE.REVISION : null,
       orbitControls: Boolean(window.THREE && window.THREE.OrbitControls),
       fallback: Boolean(document.querySelector('.board-fallback')),
       boardCardIconMode: fallbackFace ? fallbackFace.dataset.iconMode : null,
       cssCards: document.querySelectorAll('.board-fallback .board-card').length,
       cssMajorIconStackCount: document.querySelectorAll('.board-fallback .board-card .gnostica-icon-stack').length,
       cssMajorIconCount: document.querySelectorAll('.board-fallback .board-card .gnostica-icon').length,
       cssBoardIconMetrics: iconMetrics('.board-fallback .board-card.is-portrait .gnostica-icon'),
       cssWastelands: document.querySelectorAll('.board-fallback .board-wasteland').length,
       canvas: Boolean(document.querySelector('.board-three__canvas')),
       tableSurfaceColor: stage ? stage.dataset.tableSurfaceColor : null,
       tableClearColor: stage ? stage.dataset.tableClearColor : null,
       cardZones: Boolean(cardZones),
       cardZonesVisible: Boolean(cardZonesRect && cardZonesRect.width > 0 && cardZonesRect.height > 0),
       handCardCount: document.querySelectorAll('.hand-card').length,
       handMajorIconStackCount: document.querySelectorAll('.hand-card .gnostica-icon-stack').length,
       handMajorIconCount: document.querySelectorAll('.hand-card .gnostica-icon').length,
       handIconMetrics: iconMetrics('.hand-card .gnostica-icon'),
       drawCount: cardZones ? Number(cardZones.dataset.drawCount || -1) : -1,
       discardCount: cardZones ? Number(cardZones.dataset.discardCount || -1) : -1,
       statusText: status ? status.textContent.trim() : '',
       panelText: (document.querySelector('.territory-panel') || {}).innerText || ''
     };
   })()")

(def popup-mode-js
  "(() => {
     const visible = (node) => {
       if (!node) return false;
       const rect = node.getBoundingClientRect();
       const style = getComputedStyle(node);
       return rect.width > 0
         && rect.height > 0
         && style.visibility !== 'hidden'
         && Number(style.opacity || 0) > 0.8;
     };
     const text = (node) => node ? node.textContent.trim() : '';
     const focusStats = (target, popoverSelector) => {
       if (target && target.focus) target.focus();
       const popover = document.querySelector(popoverSelector);
       return {
         visible: visible(popover),
         iconCount: popover ? popover.querySelectorAll('.gnostica-icon').length : 0,
         itemCount: popover ? popover.querySelectorAll('.card-icon-popover__item').length : 0,
         text: text(popover)
       };
     };
     const shell = document.querySelector('.app-shell');
     const board = document.querySelector('.board-three');
     const fallback = document.querySelector('.board-fallback');
     const fallbackFace = document.querySelector('.board-fallback .card-face');
     const handFace = document.querySelector('.hand-card.has-gnostica-icons .card-face');
     const fallbackTwoIconCard = document.querySelector('.board-fallback .board-card .card-icon-popover[data-icon-count=\"2\"]');
     const fallbackThreeIconCard = document.querySelector('.board-fallback .board-card .card-icon-popover[data-icon-count=\"3\"]');
     const hand = focusStats(handFace, '.hand-card.has-gnostica-icons .card-icon-popover');
     const boardTwo = focusStats(
       board || (fallbackTwoIconCard ? fallbackTwoIconCard.closest('.board-card') : null),
       board ? '.board-three-icon-popover .card-icon-popover' : '.board-fallback .board-card .card-icon-popover[data-icon-count=\"2\"]'
     );
     const boardThree = focusStats(
       fallbackThreeIconCard ? fallbackThreeIconCard.closest('.board-card') : null,
       '.board-fallback .board-card .card-icon-popover[data-icon-count=\"3\"]'
     );
     return {
       appMode: shell ? shell.dataset.cardIconMode : null,
       boardMode: board ? board.dataset.cardIconMode : (fallbackFace ? fallbackFace.dataset.iconMode : null),
       fallback: Boolean(fallback),
       togglePressed: (document.querySelector('.card-icon-mode-toggle') || {}).getAttribute('aria-pressed'),
       handStackCount: document.querySelectorAll('.hand-card .gnostica-icon-stack').length,
       boardStackCount: document.querySelectorAll('.board-fallback .board-card .gnostica-icon-stack').length,
       hand,
       boardTwo,
       boardThree
     };
   })()")

(def mismatched-three-js
  "window.THREE = {REVISION: '999', OrbitControls: function OrbitControls() {}};")

(defn- velvet-pixel? [argb]
  (let [r (bit-and (bit-shift-right argb 16) 0xff)
        g (bit-and (bit-shift-right argb 8) 0xff)
        b (bit-and argb 0xff)]
    (and (>= r 24)
         (>= b 18)
         (<= g 80)
         (<= b 130)
         (> r g)
         (> b g)
         (>= (- r g) 12)
         (>= (- b g) 4))))

(defn- pixel-ok? [stats]
  (and (true? (get stats "ok"))
       (>= (long (or (get stats "sampledPixels") 0)) 100)
       (>= (long (or (get stats "distinctColors") 0)) 16)
       (>= (long (or (get stats "velvetPixels") 0)) min-velvet-pixels)))

(defn- antialias-ready? [stats]
  (and (true? (get stats "antialiasRequested"))
       (or (false? (get stats "antialiasSupported"))
           (true? (get stats "antialiasEnabled")))))

(defn- roughly= [expected actual tolerance]
  (<= (Math/abs (- (double expected) (double actual))) tolerance))

(defn- dom-icon-layout-ready? [metrics]
  (let [width-ratio (double (or (get metrics "widthRatio") 0))
        gap (double (or (get metrics "gap") -1))
        icon-width (double (or (get metrics "iconWidth") 0))]
    (and (map? metrics)
         (= icon-layout/card-icon-scale
            (long (or (get metrics "scale") -1)))
         (roughly= (/ icon-layout/dom-card-icon-width-percent 100.0)
                   width-ratio
                   0.03)
         (roughly= icon-layout/dom-card-icon-gap-px gap 1.0)
         (pos? icon-width))))

(defn- three-icon-layout-ready? [stats]
  (and (= icon-layout/card-icon-scale
          (long (or (get stats "cardIconScale") -1)))
       (= icon-layout/texture-card-icon-size
          (long (or (get stats "cardIconSize") -1)))))

(defn- happy-ready? [stats]
  (let [visible-piece-count (long (or (get stats "visiblePieceCount") -1))
        piece-edge-outline-count (long (or (get stats "pieceEdgeOutlineCount") -1))]
    (and (= "128" (get stats "threeRevision"))
         (= "always" (get stats "cardIconMode"))
         (true? (get stats "orbitControls"))
         (true? (get stats "board"))
         (= "always" (get stats "boardCardIconMode"))
         (= 2 (long (or (get stats "majorIconCardCount") -1)))
         (= 5 (long (or (get stats "majorIconCount") -1)))
         (three-icon-layout-ready? stats)
         (= 12 (get stats "wastelandCount"))
         (pos? visible-piece-count)
         (= visible-piece-count piece-edge-outline-count)
         (= expected-table-surface-color (get stats "tableSurfaceColor"))
         (= expected-table-clear-color (get stats "tableClearColor"))
         (false? (get stats "fallback"))
         (true? (get stats "canvas"))
         (pos? (long (or (get stats "canvasClientWidth") 0)))
         (pos? (long (or (get stats "canvasClientHeight") 0)))
         (antialias-ready? stats)
         (true? (get stats "reset"))
         (true? (get stats "cardZones"))
         (true? (get stats "cardZonesVisible"))
         (= 6 (long (or (get stats "handCardCount") -1)))
         (= 1 (long (or (get stats "handMajorIconStackCount") -1)))
         (= 1 (long (or (get stats "handMajorIconCount") -1)))
         (dom-icon-layout-ready? (get stats "handIconMetrics"))
         (pos? (long (or (get stats "drawCount") 0)))
         (zero? (long (or (get stats "discardCount") -1)))
         (empty? (get stats "status"))
         (>= (long (or (get stats "imageResourceCount") 0)) 9))))

(defn- fallback-ready? [stats]
  (and (nil? (get stats "threeRevision"))
       (= "always" (get stats "cardIconMode"))
       (false? (get stats "orbitControls"))
       (true? (get stats "fallback"))
       (= "always" (get stats "boardCardIconMode"))
       (false? (get stats "canvas"))
       (= expected-table-surface-color (get stats "tableSurfaceColor"))
       (= expected-table-clear-color (get stats "tableClearColor"))
       (= 9 (get stats "cssCards"))
       (= 2 (long (or (get stats "cssMajorIconStackCount") -1)))
       (= 5 (long (or (get stats "cssMajorIconCount") -1)))
       (dom-icon-layout-ready? (get stats "cssBoardIconMetrics"))
       (= 12 (get stats "cssWastelands"))
       (true? (get stats "cardZones"))
       (true? (get stats "cardZonesVisible"))
       (= 6 (long (or (get stats "handCardCount") -1)))
       (= 1 (long (or (get stats "handMajorIconStackCount") -1)))
       (= 1 (long (or (get stats "handMajorIconCount") -1)))
       (dom-icon-layout-ready? (get stats "handIconMetrics"))
       (pos? (long (or (get stats "drawCount") 0)))
       (zero? (long (or (get stats "discardCount") -1)))
       (str/includes? (or (get stats "statusText") "") "Three.js is unavailable")))

(defn- mismatch-ready? [stats]
  (and (= "999" (get stats "threeRevision"))
       (= "always" (get stats "cardIconMode"))
       (true? (get stats "orbitControls"))
       (true? (get stats "fallback"))
       (= "always" (get stats "boardCardIconMode"))
       (false? (get stats "canvas"))
       (= expected-table-surface-color (get stats "tableSurfaceColor"))
       (= expected-table-clear-color (get stats "tableClearColor"))
       (= 9 (get stats "cssCards"))
       (= 2 (long (or (get stats "cssMajorIconStackCount") -1)))
       (= 5 (long (or (get stats "cssMajorIconCount") -1)))
       (dom-icon-layout-ready? (get stats "cssBoardIconMetrics"))
       (= 12 (get stats "cssWastelands"))
       (true? (get stats "cardZones"))
       (true? (get stats "cardZonesVisible"))
       (= 6 (long (or (get stats "handCardCount") -1)))
       (= 1 (long (or (get stats "handMajorIconStackCount") -1)))
       (= 1 (long (or (get stats "handMajorIconCount") -1)))
       (dom-icon-layout-ready? (get stats "handIconMetrics"))
       (pos? (long (or (get stats "drawCount") 0)))
       (zero? (long (or (get stats "discardCount") -1)))
       (str/includes? (or (get stats "statusText") "") "revision 999 is incompatible")))

(defn- popup-mode-ready? [stats]
  (let [hand (get stats "hand")
        board-two (get stats "boardTwo")
        board-three (get stats "boardThree")
        fallback? (true? (get stats "fallback"))]
    (and (= "popup" (get stats "appMode"))
         (= "popup" (get stats "boardMode"))
         (= "false" (get stats "togglePressed"))
         (zero? (long (or (get stats "handStackCount") -1)))
         (true? (get hand "visible"))
         (= 1 (long (or (get hand "iconCount") -1)))
         (str/includes? (or (get hand "text") "") "Sword, rod, cup, or disc")
         (true? (get board-two "visible"))
         (= 2 (long (or (get board-two "iconCount") -1)))
         (str/includes? (or (get board-two "text") "") "Rod")
         (or (not fallback?)
             (and (true? (get board-three "visible"))
                  (= 3 (long (or (get board-three "iconCount") -1)))
                  (str/includes? (or (get board-three "text") "")
                                 "Orient a target piece"))))))

(defn- browser-diagnostics [client]
  (->> @(:events client)
       (filter (fn [event]
                 (#{"Log.entryAdded"
                    "Network.loadingFailed"
                    "Runtime.consoleAPICalled"
                    "Runtime.exceptionThrown"
                    "gnostica.smoke/websocket-error"}
                  (get event "method"))))
       (take-last 20)
       vec))

(defn- screenshot-pixel-stats! [client {:strs [x y width height]}]
  (let [result (cdp-command! client
                             "Page.captureScreenshot"
                             {"format" "png"
                              "clip" {"x" x
                                      "y" y
                                      "width" width
                                      "height" height
                                      "scale" 1}})
        bytes (.decode (Base64/getDecoder) ^String (get result "data"))
        image (ImageIO/read (ByteArrayInputStream. bytes))]
    (if-not image
      {"ok" false
       "error" "Chrome returned screenshot bytes that ImageIO could not decode."}
      (let [image-width (.getWidth image)
            image-height (.getHeight image)
            step-x (max 1 (quot image-width 80))
            step-y (max 1 (quot image-height 80))
            colors (volatile! (transient #{}))
            velvet-pixels (volatile! 0)
            sampled (volatile! 0)]
        (doseq [sample-x (range 0 image-width step-x)
                sample-y (range 0 image-height step-y)]
          (vswap! sampled inc)
          (let [argb (.getRGB image sample-x sample-y)]
            (when (velvet-pixel? argb)
              (vswap! velvet-pixels inc))
            (vswap! colors conj! argb)))
        {"ok" true
         "width" image-width
         "height" image-height
         "sampledPixels" @sampled
         "velvetPixels" @velvet-pixels
         "distinctColors" (count (persistent! @colors))}))))

(defn- open-page!
  ([http-client chrome viewport url blocked-urls]
   (open-page! http-client chrome viewport url blocked-urls nil))
  ([http-client chrome viewport url blocked-urls init-script]
   (let [websocket-url (new-page-websocket! http-client (:port chrome))
         client (connect-cdp! http-client websocket-url)]
     (prepare-page! client viewport blocked-urls init-script)
     (cdp-command! client "Page.navigate" {"url" url})
     (wait-for! client "the Gnostica app shell" app-ready-js true?)
     client)))

(defn- major-icons-smoke-url [url]
  (str url
       (if (str/includes? url "?") "&" "?")
       "gnostica-smoke=major-icons"))

(defn- dispatch-click! [client {:strs [x y]}]
  (doseq [event-type ["mouseMoved" "mousePressed" "mouseReleased"]]
    (cdp-command! client
                  "Input.dispatchMouseEvent"
                  (cond-> {"type" event-type
                           "x" x
                           "y" y}
                    (not= "mouseMoved" event-type)
                    (assoc "button" "left"
                           "clickCount" 1)))))

(defn- dispatch-center-click! [client {:strs [centerX centerY]}]
  (dispatch-click! client {"x" centerX
                           "y" centerY}))

(defn- dispatch-i-key! [client]
  (doseq [event-type ["keyDown" "keyUp"]]
    (cdp-command! client
                  "Input.dispatchKeyEvent"
                  {"type" event-type
                   "key" "i"
                   "code" "KeyI"
                   "windowsVirtualKeyCode" 73
                   "nativeVirtualKeyCode" 73})))

(defn- run-happy-viewport! [http-client chrome url viewport]
  (println (format "Smoke checking %s viewport at %dx%d."
                   (:name viewport)
                   (:width viewport)
                   (:height viewport)))
  (let [client (open-page! http-client chrome viewport (major-icons-smoke-url url) nil)]
    (try
      (let [stats (wait-for! client
                             (str (:name viewport) " Three.js board render")
                             happy-stats-js
                             happy-ready?)
            rect (evaluate! client canvas-rect-js)
            pixel-stats (when rect
                          (screenshot-pixel-stats! client rect))]
        (when-not rect
          (throw (ex-info "Three.js canvas bounds could not be measured."
                          {:viewport viewport
                           :stats stats})))
        (when-not (pixel-ok? pixel-stats)
          (throw (ex-info "Three.js canvas screenshot did not contain visible board content."
                          {:viewport viewport
                           :stats stats
                           :pixel-stats pixel-stats})))
        (dispatch-i-key! client)
        (let [popup-stats (wait-for! client
                                      (str (:name viewport) " popup icon mode")
                                      popup-mode-js
                                      popup-mode-ready?)
              updated-rect (evaluate! client canvas-rect-js)]
          (when-not updated-rect
            (throw (ex-info "Three.js canvas bounds could not be remeasured after popup mode."
                            {:viewport viewport
                             :stats stats
                             :popup-stats popup-stats})))
          (dispatch-center-click! client updated-rect)
          (let [selection (wait-for! client
                                     (str (:name viewport) " center-card selection")
                                     selection-js
                                     #(str/includes? (or (get % "panelText") "")
                                                    "Row 2, Column 2"))]
            {:viewport (:name viewport)
             :stats stats
             :popup-stats popup-stats
             :pixel-stats pixel-stats
             :selection selection})))
      (catch Exception error
        (throw (ex-info (str "3D board smoke failed in the " (:name viewport) " viewport.")
                        {:viewport viewport
                         :url url
                         :browser-diagnostics (browser-diagnostics client)
                         :cause (ex-message error)
                         :data (ex-data error)}
                        error)))
      (finally
        (close-cdp! client)))))

(defn- run-missing-three-fallback! [http-client chrome url]
  (println "Smoke checking missing-Three.js fallback path.")
  (let [client (open-page! http-client
                           chrome
                           (first viewports)
                           (major-icons-smoke-url url)
                           ["*cdn.jsdelivr.net/npm/three@0.128.0*"])]
    (try
      (wait-for! client
                 "CSS fallback after blocked Three.js CDN globals"
                 fallback-stats-js
                 fallback-ready?)
      (dispatch-i-key! client)
      (wait-for! client
                 "CSS fallback popup icon mode"
                 popup-mode-js
                 popup-mode-ready?)
      (catch Exception error
        (throw (ex-info "3D board fallback smoke failed when Three.js CDN scripts were blocked."
                        {:url url
                         :browser-diagnostics (browser-diagnostics client)
                         :cause (ex-message error)
                         :data (ex-data error)}
                        error)))
      (finally
        (close-cdp! client)))))

(defn- run-mismatched-three-fallback! [http-client chrome url]
  (println "Smoke checking mismatched-Three.js fallback path.")
  (let [client (open-page! http-client
                           chrome
                           (first viewports)
                           (major-icons-smoke-url url)
                           ["*cdn.jsdelivr.net/npm/three@0.128.0*"]
                           mismatched-three-js)]
    (try
      (wait-for! client
                 "CSS fallback after mismatched Three.js globals"
                 fallback-stats-js
                 mismatch-ready?)
      (catch Exception error
        (throw (ex-info "3D board fallback smoke failed when mismatched Three.js globals were present."
                        {:url url
                         :browser-diagnostics (browser-diagnostics client)
                         :cause (ex-message error)
                         :data (ex-data error)}
                        error)))
      (finally
        (close-cdp! client)))))

(defn- start-local-server! []
  (let [port (free-port)
        server (jetty/run-jetty #'app-server/app {:port port :join? false})]
    {:url (str "http://127.0.0.1:" port "/index.html")
     :stop #(.stop server)}))

(defn- target-url! []
  (if-let [url (getenv "SMOKE_URL")]
    {:url url
     :stop (constantly nil)}
    (start-local-server!)))

(defn- run-smoke! []
  (let [http-client (-> (HttpClient/newBuilder)
                        (.connectTimeout (Duration/ofSeconds 5))
                        .build)
        target (target-url!)]
    (try
      (let [chrome (launch-chrome!)]
        (try
          (wait-for-cdp! http-client chrome)
          (println (str "Smoke target: " (:url target)))
          (doseq [viewport viewports]
            (run-happy-viewport! http-client chrome (:url target) viewport))
          (run-missing-three-fallback! http-client chrome (:url target))
          (run-mismatched-three-fallback! http-client chrome (:url target))
          (println "3D board smoke passed.")
          (finally
            (stop-chrome! chrome))))
      (finally
        ((:stop target))))))

(defn -main [& _]
  (try
    (run-smoke!)
    (catch Throwable error
      (binding [*out* *err*]
        (println (str "3D board smoke failed: " (ex-message error)))
        (when-let [data (ex-data error)]
          (println (pr-str data)))
        (when (getenv "SMOKE_DEBUG")
          (.printStackTrace error)))
      (System/exit 1))))
