(ns gnostica.smoke-runner
  (:require [gnostica.server :as app-server]
            [gnostica.smoke.browser :as browser]
            [gnostica.smoke.page-stats :as stats]
            [ring.adapter.jetty :as jetty]))

(defn- open-gnostica-page!
  ([http-client chrome viewport url blocked-urls]
   (open-gnostica-page! http-client chrome viewport url blocked-urls nil))
  ([http-client chrome viewport url blocked-urls init-script]
   (browser/open-page! http-client
                       chrome
                       viewport
                       {:url url
                        :blocked-urls blocked-urls
                        :init-script init-script
                        :ready-description "the Gnostica app shell"
                        :ready-expression stats/app-ready-js
                        :ready? true?})))

(defn- run-happy-viewport! [http-client chrome url viewport]
  (println (format "Smoke checking %s viewport at %dx%d."
                   (:name viewport)
                   (:width viewport)
                   (:height viewport)))
  (let [client (open-gnostica-page! http-client
                                    chrome
                                    viewport
                                    (stats/major-icons-smoke-url url)
                                    nil)]
    (try
      (let [initial-stats (browser/wait-for! client
                                             (str (:name viewport) " Three.js board render")
                                             stats/happy-stats-js
                                             stats/happy-ready?)
            rect (browser/evaluate! client stats/canvas-rect-js)
            pixel-stats (when rect
                          (stats/screenshot-pixel-stats! client rect))]
        (when-not rect
          (throw (ex-info "Three.js canvas bounds could not be measured."
                          {:viewport viewport
                           :stats initial-stats})))
        (when-not (stats/pixel-ok? pixel-stats)
          (throw (ex-info "Three.js canvas screenshot did not contain visible board content."
                          {:viewport viewport
                           :stats initial-stats
                           :pixel-stats pixel-stats})))
        (stats/focus-three-board! client)
        (browser/dispatch-w-key! client)
        (let [wasd-stats (browser/wait-for! client
                                            (str (:name viewport) " WASD board movement")
                                            stats/happy-stats-js
                                            #(and (stats/happy-ready? %)
                                                  (stats/camera-target-y-changed? initial-stats %)))
              _ (browser/dispatch-arrow-right-key! client)
              keyboard-stats (browser/wait-for! client
                                                (str (:name viewport) " arrow-key board movement")
                                                stats/happy-stats-js
                                                #(and (stats/happy-ready? %)
                                                      (stats/camera-target-x-changed? wasd-stats %)))]
          (browser/dispatch-wheel! client rect -720)
          (let [zoomed-stats (browser/wait-for! client
                                                (str (:name viewport) " changed 3D camera distance")
                                                stats/happy-stats-js
                                                #(and (stats/happy-ready? %)
                                                      (stats/camera-distance-changed? keyboard-stats %)))]
            (browser/dispatch-question-mark-key! client)
            (let [hotkey-help (browser/wait-for! client
                                                 (str (:name viewport) " hotkey help dialog")
                                                 stats/hotkey-help-js
                                                 stats/hotkey-help-open-ready?)]
              (browser/dispatch-escape-key! client)
              (browser/wait-for! client
                                 (str (:name viewport) " hotkey help close")
                                 stats/hotkey-help-js
                                 stats/hotkey-help-closed-ready?)
              (browser/dispatch-g-key! client)
              (let [icon-help (browser/wait-for! client
                                                 (str (:name viewport) " icon help dialog")
                                                 stats/icon-help-js
                                                 stats/icon-help-open-ready?)]
                (browser/dispatch-escape-key! client)
                (browser/wait-for! client
                                   (str (:name viewport) " icon help close")
                                   stats/icon-help-js
                                   stats/icon-help-closed-ready?)
                (browser/dispatch-i-key! client)
                (let [popup-stats (browser/wait-for! client
                                                     (str (:name viewport) " popup icon mode")
                                                     stats/popup-mode-js
                                                     stats/popup-mode-ready?)
                      updated-rect (browser/evaluate! client stats/canvas-rect-js)]
                  (when-not (stats/camera-distance-preserved? zoomed-stats popup-stats)
                    (throw (ex-info "The I hotkey reset the 3D camera view."
                                    {:viewport viewport
                                     :zoomed-stats zoomed-stats
                                     :popup-stats popup-stats})))
                  (when-not updated-rect
                    (throw (ex-info "Three.js canvas bounds could not be remeasured after popup mode."
                                    {:viewport viewport
                                     :stats initial-stats
                                     :popup-stats popup-stats})))
                  (browser/dispatch-center-click! client updated-rect)
                  (let [selection (browser/wait-for! client
                                                     (str (:name viewport) " center-card selection")
                                                     stats/selection-js
                                                     stats/center-card-selected?)]
                    {:viewport (:name viewport)
                     :stats initial-stats
                     :keyboard-stats keyboard-stats
                     :zoomed-stats zoomed-stats
                     :hotkey-help hotkey-help
                     :icon-help icon-help
                     :popup-stats popup-stats
                     :pixel-stats pixel-stats
                     :selection selection})))))))
      (catch Exception error
        (throw (ex-info (str "3D board smoke failed in the " (:name viewport) " viewport.")
                        {:viewport viewport
                         :url url
                         :browser-diagnostics (browser/browser-diagnostics client)
                         :cause (ex-message error)
                         :data (ex-data error)}
                        error)))
      (finally
        (browser/close-cdp! client)))))

(defn- run-missing-three-fallback! [http-client chrome url]
  (println "Smoke checking missing-Three.js fallback path.")
  (let [client (open-gnostica-page! http-client
                                    chrome
                                    (first stats/viewports)
                                    (stats/major-icons-smoke-url url)
                                    ["*cdn.jsdelivr.net/npm/three@0.128.0*"])]
    (try
      (browser/wait-for! client
                         "CSS fallback after blocked Three.js CDN globals"
                         stats/fallback-stats-js
                         stats/fallback-ready?)
      (browser/dispatch-i-key! client)
      (browser/wait-for! client
                         "CSS fallback popup icon mode"
                         stats/popup-mode-js
                         stats/popup-mode-ready?)
      (catch Exception error
        (throw (ex-info "3D board fallback smoke failed when Three.js CDN scripts were blocked."
                        {:url url
                         :browser-diagnostics (browser/browser-diagnostics client)
                         :cause (ex-message error)
                         :data (ex-data error)}
                        error)))
      (finally
        (browser/close-cdp! client)))))

(defn- run-mismatched-three-fallback! [http-client chrome url]
  (println "Smoke checking mismatched-Three.js fallback path.")
  (let [client (open-gnostica-page! http-client
                                    chrome
                                    (first stats/viewports)
                                    (stats/major-icons-smoke-url url)
                                    ["*cdn.jsdelivr.net/npm/three@0.128.0*"]
                                    stats/mismatched-three-js)]
    (try
      (browser/wait-for! client
                         "CSS fallback after mismatched Three.js globals"
                         stats/fallback-stats-js
                         stats/mismatch-ready?)
      (catch Exception error
        (throw (ex-info "3D board fallback smoke failed when mismatched Three.js globals were present."
                        {:url url
                         :browser-diagnostics (browser/browser-diagnostics client)
                         :cause (ex-message error)
                         :data (ex-data error)}
                        error)))
      (finally
        (browser/close-cdp! client)))))

(defn- start-local-server! []
  (let [port (browser/free-port)
        server (jetty/run-jetty #'app-server/app {:port port :join? false})]
    {:url (str "http://127.0.0.1:" port "/index.html")
     :stop #(.stop server)}))

(defn- target-url! []
  (if-let [url (browser/getenv "SMOKE_URL")]
    {:url url
     :stop (constantly nil)}
    (start-local-server!)))

(defn- run-smoke! []
  (let [http-client (browser/http-client)
        target (target-url!)]
    (try
      (let [chrome (browser/launch-chrome!)]
        (try
          (browser/wait-for-cdp! http-client chrome)
          (println (str "Smoke target: " (:url target)))
          (doseq [viewport stats/viewports]
            (run-happy-viewport! http-client chrome (:url target) viewport))
          (run-missing-three-fallback! http-client chrome (:url target))
          (run-mismatched-three-fallback! http-client chrome (:url target))
          (println "3D board smoke passed.")
          (finally
            (browser/stop-chrome! chrome))))
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
        (when (browser/getenv "SMOKE_DEBUG")
          (.printStackTrace error)))
      (System/exit 1))))
