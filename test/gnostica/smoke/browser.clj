(ns gnostica.smoke.browser
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]
           [java.lang ProcessBuilder ProcessBuilder$Redirect]
           [java.net ServerSocket URI URLEncoder]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers WebSocket WebSocket$Listener]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.time Duration]
           [java.util.concurrent CompletableFuture TimeUnit]
           [java.util.concurrent.atomic AtomicLong]))

(def cdp-timeout-ms 10000)
(def wait-timeout-ms 20000)

(defn getenv [k]
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

(defn free-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn- delete-recursive! [path]
  (let [file (.toFile path)]
    (when (.exists file)
      (doseq [child (reverse (file-seq file))]
        (io/delete-file child true)))))

(defn launch-chrome! []
  (let [executable (or (chrome-executable)
                       (throw (ex-info "Chrome or Chromium executable not found. Set SMOKE_CHROME or CHROME_PATH to the browser path."
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

(defn stop-chrome! [{:keys [process profile-dir]}]
  (when process
    (.destroy process)
    (when-not (.waitFor process 5 TimeUnit/SECONDS)
      (.destroyForcibly process)
      (.waitFor process 5 TimeUnit/SECONDS)))
  (when profile-dir
    (delete-recursive! profile-dir)))

(defn http-client []
  (-> (HttpClient/newBuilder)
      (.connectTimeout (Duration/ofSeconds 5))
      .build))

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

(defn wait-for-cdp! [client {:keys [port process]}]
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
        partial (atom "")
        listener (reify WebSocket$Listener
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
     :events events}))

(defn close-cdp! [{:keys [websocket]}]
  (when websocket
    (.join (.sendClose websocket WebSocket/NORMAL_CLOSURE "done"))))

(defn cdp-command!
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

(defn evaluate! [client expression]
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

(defn wait-for! [client description expression ready?]
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
  [{:keys [width height mobile] :as _viewport} client blocked-urls init-script]
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
                 "mobile" (boolean mobile)})
  (cdp-command! client
                "Emulation.setTouchEmulationEnabled"
                (cond-> {"enabled" (boolean mobile)}
                  mobile
                  (assoc "maxTouchPoints" 1))))

(defn open-page!
  [http-client chrome viewport {:keys [url blocked-urls init-script
                                       ready-description ready-expression ready?]
                                :or {blocked-urls []}}]
  (let [websocket-url (new-page-websocket! http-client (:port chrome))
        client (connect-cdp! http-client websocket-url)]
    (prepare-page! viewport client blocked-urls init-script)
    (cdp-command! client "Page.navigate" {"url" url})
    (when ready-expression
      (wait-for! client
                 (or ready-description "page readiness")
                 ready-expression
                 (or ready? true?)))
    client))

(defn browser-diagnostics [client]
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

(defn dispatch-click! [client {:strs [x y]}]
  (doseq [event-type ["mouseMoved" "mousePressed" "mouseReleased"]]
    (cdp-command! client
                  "Input.dispatchMouseEvent"
                  (cond-> {"type" event-type
                           "x" x
                           "y" y}
                    (not= "mouseMoved" event-type)
                    (assoc "button" "left"
                           "clickCount" 1)))))

(defn dispatch-center-click! [client {:strs [centerX centerY]}]
  (dispatch-click! client {"x" centerX
                           "y" centerY}))

(defn dispatch-mouse-move!
  ([client point]
   (dispatch-mouse-move! client point false))
  ([client {:strs [x y]} pressed?]
   (cdp-command! client
                 "Input.dispatchMouseEvent"
                 (cond-> {"type" "mouseMoved"
                          "x" (double x)
                          "y" (double y)}
                   pressed?
                   (assoc "button" "left"
                          "buttons" 1)))))

(defn dispatch-mouse-press! [client {:strs [x y]}]
  (cdp-command! client
                "Input.dispatchMouseEvent"
                {"type" "mousePressed"
                 "x" (double x)
                 "y" (double y)
                 "button" "left"
                 "buttons" 1
                 "clickCount" 1}))

(defn dispatch-mouse-release! [client {:strs [x y]}]
  (cdp-command! client
                "Input.dispatchMouseEvent"
                {"type" "mouseReleased"
                 "x" (double x)
                 "y" (double y)
                 "button" "left"
                 "clickCount" 1}))

(defn dispatch-drag! [client start-point end-point]
  (let [start-x (double (get start-point "x"))
        start-y (double (get start-point "y"))
        end-x (double (get end-point "x"))
        end-y (double (get end-point "y"))
        mid-x (/ (+ start-x end-x) 2)
        mid-y (/ (+ start-y end-y) 2)]
    (cdp-command! client
                  "Input.dispatchMouseEvent"
                  {"type" "mouseMoved"
                   "x" start-x
                   "y" start-y})
    (cdp-command! client
                  "Input.dispatchMouseEvent"
                  {"type" "mousePressed"
                   "x" start-x
                   "y" start-y
                   "button" "left"
                   "clickCount" 1})
    (doseq [[x y] [[mid-x mid-y] [end-x end-y]]]
      (cdp-command! client
                    "Input.dispatchMouseEvent"
                    {"type" "mouseMoved"
                     "x" x
                     "y" y
                     "button" "left"
                     "buttons" 1}))
    (cdp-command! client
                  "Input.dispatchMouseEvent"
                  {"type" "mouseReleased"
                   "x" end-x
                   "y" end-y
                   "button" "left"
                   "clickCount" 1})))

(defn- touch-point
  ([point]
   (touch-point point 1))
  ([point id]
   {"x" (double (get point "x"))
    "y" (double (get point "y"))
    "radiusX" 6
    "radiusY" 6
    "rotationAngle" 0
    "force" 1
    "id" id}))

(defn- dispatch-touch-event! [client event-type points]
  (cdp-command! client
                "Input.dispatchTouchEvent"
                {"type" event-type
                 "touchPoints" (vec points)}))

(defn dispatch-touch-tap! [client point]
  (dispatch-touch-event! client "touchStart" [(touch-point point)])
  (Thread/sleep 50)
  (dispatch-touch-event! client "touchEnd" []))

(defn dispatch-touch-drag! [client start-point end-point]
  (let [start-x (double (get start-point "x"))
        start-y (double (get start-point "y"))
        end-x (double (get end-point "x"))
        end-y (double (get end-point "y"))
        mid-point {"x" (/ (+ start-x end-x) 2)
                   "y" (/ (+ start-y end-y) 2)}]
    (dispatch-touch-event! client "touchStart" [(touch-point start-point)])
    (Thread/sleep 50)
    (doseq [point [mid-point end-point]]
      (dispatch-touch-event! client "touchMove" [(touch-point point)])
      (Thread/sleep 50))
    (dispatch-touch-event! client "touchEnd" [])))

(defn dispatch-wheel! [client {:strs [centerX centerY]} delta-y]
  (cdp-command! client
                "Input.dispatchMouseEvent"
                {"type" "mouseMoved"
                 "x" centerX
                 "y" centerY})
  (cdp-command! client
                "Input.dispatchMouseEvent"
                {"type" "mouseWheel"
                 "x" centerX
                 "y" centerY
                 "deltaX" 0
                 "deltaY" delta-y}))

(defn dispatch-key! [client {:keys [key code key-code modifiers]}]
  (doseq [event-type ["keyDown" "keyUp"]]
    (cdp-command! client
                  "Input.dispatchKeyEvent"
                  (cond-> {"type" event-type
                           "key" key
                           "code" code
                           "windowsVirtualKeyCode" key-code
                           "nativeVirtualKeyCode" key-code}
                    modifiers
                    (assoc "modifiers" modifiers)))))

(defn dispatch-i-key! [client]
  (dispatch-key! client {:key "i"
                         :code "KeyI"
                         :key-code 73}))

(defn dispatch-g-key! [client]
  (dispatch-key! client {:key "g"
                         :code "KeyG"
                         :key-code 71}))

(defn dispatch-w-key! [client]
  (dispatch-key! client {:key "w"
                         :code "KeyW"
                         :key-code 87}))

(defn dispatch-arrow-right-key! [client]
  (dispatch-key! client {:key "ArrowRight"
                         :code "ArrowRight"
                         :key-code 39}))

(defn dispatch-enter-key! [client]
  (dispatch-key! client {:key "Enter"
                         :code "Enter"
                         :key-code 13}))

(defn dispatch-tab-key!
  ([client]
   (dispatch-tab-key! client false))
  ([client shift?]
   (dispatch-key! client (cond-> {:key "Tab"
                                  :code "Tab"
                                  :key-code 9}
                           shift?
                           (assoc :modifiers 8)))))

(defn dispatch-question-mark-key! [client]
  (dispatch-key! client {:key "?"
                         :code "Slash"
                         :key-code 191
                         :modifiers 8}))

(defn dispatch-escape-key! [client]
  (doseq [event-type ["rawKeyDown" "keyUp"]]
    (cdp-command! client
                  "Input.dispatchKeyEvent"
                  {"type" event-type
                   "key" "Escape"
                   "code" "Escape"
                   "windowsVirtualKeyCode" 27
                   "nativeVirtualKeyCode" 27})))
