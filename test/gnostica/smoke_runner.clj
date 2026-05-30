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

(defn- assert-drag-started! [description result]
  (when-not (true? (get result "started"))
    (throw (ex-info (str description " did not start.")
                    {:result result})))
  (when-not (seq (get result "payload"))
    (throw (ex-info (str description " did not write a gesture payload.")
                    {:result result}))))

(defn- assert-drop-started! [description result]
  (when-not (true? (get result "dropped"))
    (throw (ex-info (str description " did not drop.")
                    {:result result})))
  (when-not (seq (get result "payload"))
    (throw (ex-info (str description " did not carry a gesture payload.")
                    {:result result}))))

(defn- assert-first-piece-ghost! [description result]
  (when-not (true? (get result "ghostVisible"))
    (throw (ex-info (str description " did not show a first-piece drag ghost.")
                    {:result result})))
  (when-not (= "small" (get result "ghostPieceSize"))
    (throw (ex-info (str description " drag ghost was not a small piece.")
                    {:result result}))))

(defn- assert-css-single-drag-highlight! [description result]
  (when-not (true? (get result "boardDragActiveBeforeDrop"))
    (throw (ex-info (str description " did not mark the board drag as active before drop.")
                    {:result result})))
  (when-not (= "territory" (get result "dragHoverKindBeforeDrop"))
    (throw (ex-info (str description " did not hover a territory target before drop.")
                    {:result result})))
  (when-not (= "legal" (get result "dragHoverStatusBeforeDrop"))
    (throw (ex-info (str description " did not preserve the hovered target legal status.")
                    {:result result})))
  (when-not (= 1 (long (or (get result "dragHoverTargetCountBeforeDrop") -1)))
    (throw (ex-info (str description " highlighted more than one active drag target.")
                    {:result result})))
  (when-not (= 1 (long (or (get result "visibleLegalTargetCountBeforeDrop") -1)))
    (throw (ex-info (str description " rendered more than the hovered target as a visible legal target.")
                    {:result result})))
  (when-not (zero? (long (or (get result "visibleDisabledTargetCountBeforeDrop") -1)))
    (throw (ex-info (str description " rendered disabled targets during a legal first-piece hover.")
                    {:result result})))
  (when-not (< 1 (long (or (get result "dropTargetCountBeforeDrop") 0)))
    (throw (ex-info (str description " did not keep non-hover drop targets available for hit testing.")
                    {:result result}))))

(defn- assert-three-single-drag-highlight! [description result]
  (when-not (true? (get result "boardDragActiveBeforeDrop"))
    (throw (ex-info (str description " did not mark the board drag as active before drop.")
                    {:result result})))
  (when-not (= "territory" (get result "dragTargetKindBeforeDrop"))
    (throw (ex-info (str description " did not hover a territory target before drop.")
                    {:result result})))
  (when-not (= "legal" (get result "dragTargetStatusBeforeDrop"))
    (throw (ex-info (str description " did not preserve the hovered target legal status.")
                    {:result result})))
  (when-not (= 1 (long (or (get result "dragTargetHighlightCountBeforeDrop") -1)))
    (throw (ex-info (str description " rendered more than one Three.js drag target highlight.")
                    {:result result}))))

(defn- assert-clicked! [description result]
  (when-not (true? (get result "clicked"))
    (throw (ex-info (str description " was not clicked.")
                    {:result result}))))

(defn- assert-selected! [description result]
  (when-not (true? (get result "selected"))
    (throw (ex-info (str description " was not selected.")
                    {:result result}))))

(defn- assert-touch-input! [description result ready?]
  (when-not (ready? result)
    (throw (ex-info (str description " did not produce touch input events.")
                    {:touch-input result}))))

(defn- cancel-pending-tray! [client description]
  (browser/evaluate! client stats/cancel-pending-move-js)
  (browser/wait-for! client
                     (str description " pending tray close")
                     stats/pending-tray-stats-js
                     stats/pending-tray-closed?)
  (browser/evaluate! client stats/close-move-panel-js))

(defn- run-three-direct-gesture-smoke! [client viewport]
  (let [points (browser/evaluate! client stats/three-piece-drag-points-js)]
    (when-not (true? (get points "ok"))
      (throw (ex-info "Could not calculate an in-canvas Three.js piece drag path."
                      {:viewport viewport
                       :points points})))
    (browser/dispatch-drag! client (get points "source") (get points "target"))
    (let [pending (browser/wait-for! client
                                     (str (:name viewport) " Three.js piece drag pending tray")
                                     stats/pending-tray-stats-js
                                     stats/pending-tray-needs-choice-ready?)]
      (cancel-pending-tray! client (str (:name viewport) " Three.js piece drag"))
      pending)))

(defn- run-three-touch-direct-gesture-smoke! [client viewport]
  (let [points (browser/evaluate! client stats/three-piece-drag-points-js)]
    (when-not (true? (get points "ok"))
      (throw (ex-info "Could not calculate an in-canvas Three.js touch piece drag path."
                      {:viewport viewport
                       :points points})))
    (browser/evaluate! client stats/reset-touch-input-probe-js)
    (browser/dispatch-touch-drag! client (get points "source") (get points "target"))
    (let [pending (browser/wait-for! client
                                     (str (:name viewport) " Three.js touch piece drag pending tray")
                                     stats/pending-tray-stats-js
                                     stats/pending-tray-needs-choice-ready?)
          touch-input (browser/evaluate! client stats/touch-input-probe-stats-js)]
      (assert-touch-input! "Three.js touch piece drag" touch-input stats/touch-drag-input-ready?)
      (cancel-pending-tray! client (str (:name viewport) " Three.js touch piece drag"))
      {:pending pending
       :touch-input touch-input})))

(defn- run-hand-card-direct-gesture-smoke! [client viewport]
  (let [drag-result (browser/evaluate! client stats/hand-card-drag-js)]
    (assert-drag-started! "Hand-card drag" drag-result)
    (let [pending (browser/wait-for! client
                                     (str (:name viewport) " hand-card drag pending tray")
                                     stats/pending-tray-stats-js
                                     stats/pending-tray-needs-choice-ready?)]
      (browser/evaluate! client stats/open-detailed-entry-js)
      (let [detailed (browser/wait-for! client
                                        (str (:name viewport) " hand-card drag Detailed entry")
                                        stats/pending-tray-stats-js
                                        stats/pending-tray-detailed-open-ready?)]
        (cancel-pending-tray! client (str (:name viewport) " hand-card drag"))
        {:drag-result drag-result
         :pending pending
         :detailed detailed}))))

(defn- run-hand-card-touch-gesture-smoke! [client viewport]
  (let [point (browser/evaluate! client stats/hand-card-touch-point-js)]
    (when-not (true? (get point "ok"))
      (throw (ex-info "Could not calculate a hand-card touch point."
                      {:viewport viewport
                       :point point})))
    (browser/evaluate! client stats/reset-touch-input-probe-js)
    (browser/dispatch-touch-tap! client point)
    (let [pending (browser/wait-for! client
                                     (str (:name viewport) " hand-card touch pending tray")
                                     stats/pending-tray-stats-js
                                     stats/pending-tray-needs-choice-ready?)
          touch-input (browser/evaluate! client stats/touch-input-probe-stats-js)]
      (assert-touch-input! "Hand-card touch" touch-input stats/touch-input-ready?)
      (browser/evaluate! client stats/open-detailed-entry-js)
      (let [detailed (browser/wait-for! client
                                        (str (:name viewport) " hand-card touch Detailed entry")
                                        stats/pending-tray-stats-js
                                        stats/pending-tray-detailed-open-ready?)]
        (cancel-pending-tray! client (str (:name viewport) " hand-card touch"))
        {:point point
         :pending pending
         :detailed detailed
         :touch-input touch-input}))))

(defn- run-happy-viewport! [http-client chrome url viewport]
  (println (format "Smoke checking %s viewport at %dx%d."
                   (:name viewport)
                   (:width viewport)
                   (:height viewport)))
  (let [client (open-gnostica-page! http-client
                                    chrome
                                    viewport
                                    (stats/major-icons-smoke-url url)
                                    nil
                                    (when (:mobile viewport)
                                      stats/touch-input-probe-init-js))]
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
        (let [three-piece-drag (run-three-direct-gesture-smoke! client viewport)
              hand-card-drag (run-hand-card-direct-gesture-smoke! client viewport)
              touch-piece-drag (when (:mobile viewport)
                                 (run-three-touch-direct-gesture-smoke! client viewport))
              touch-hand-card (when (:mobile viewport)
                                (run-hand-card-touch-gesture-smoke! client viewport))]
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
                                                        stats/center-card-selected?)
                          move-panel (browser/wait-for! client
                                                        (str (:name viewport) " move-panel hand-card controls")
                                                        stats/move-panel-hand-card-step-js
                                                        stats/move-panel-hand-card-step-ready?)]
                      {:viewport (:name viewport)
                       :stats initial-stats
                       :keyboard-stats keyboard-stats
                       :zoomed-stats zoomed-stats
                       :hotkey-help hotkey-help
                       :icon-help icon-help
                       :popup-stats popup-stats
                       :pixel-stats pixel-stats
                       :three-piece-drag three-piece-drag
                       :hand-card-drag hand-card-drag
                       :touch-piece-drag touch-piece-drag
                       :touch-hand-card touch-hand-card
                       :selection selection
                       :move-panel move-panel}))))))))
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

(defn- run-fallback-direct-gesture-smoke! [client description]
  (let [drag-result (browser/evaluate! client stats/fallback-piece-drag-js)]
    (assert-drag-started! "CSS fallback piece drag" drag-result)
    (let [pending (browser/wait-for! client
                                     (str description " CSS piece drag pending tray")
                                     stats/pending-tray-stats-js
                                     stats/pending-tray-needs-choice-ready?)]
      (browser/evaluate! client stats/open-detailed-entry-js)
      (let [detailed (browser/wait-for! client
                                        (str description " CSS piece drag Detailed entry")
                                        stats/pending-tray-stats-js
                                        stats/pending-tray-detailed-open-ready?)]
        (cancel-pending-tray! client (str description " CSS piece drag"))
        {:drag-result drag-result
         :pending pending
         :detailed detailed}))))

(defn- run-confirmed-fallback-direct-drop! [http-client chrome url]
  (println "Smoke checking confirmed CSS direct drop.")
  (let [client (open-gnostica-page! http-client
                                    chrome
                                    (first stats/viewports)
                                    (stats/direct-drop-smoke-url url)
                                    ["*cdn.jsdelivr.net/npm/three@0.128.0*"])]
    (try
      (browser/wait-for! client
                         "direct-drop CSS fallback setup"
                         stats/direct-drop-fallback-stats-js
                         stats/direct-drop-fallback-ready?)
      (let [drop-result (browser/evaluate! client stats/initial-placement-drop-js)]
        (assert-drop-started! "CSS initial-placement source-to-board drop" drop-result)
        (assert-css-single-drag-highlight! "CSS initial-placement source-to-board drop" drop-result)
        (let [pending (browser/wait-for! client
                                         "CSS initial-placement pending tray"
                                         stats/pending-tray-stats-js
                                         stats/pending-tray-needs-choice-ready?)]
          (browser/evaluate! client stats/open-detailed-entry-js)
          (browser/wait-for! client
                             "CSS initial-placement Detailed entry"
                             stats/pending-tray-stats-js
                             stats/pending-tray-detailed-open-ready?)
          (let [orientation-result (browser/evaluate! client
                                                      stats/choose-north-orientation-js)]
            (assert-selected! "North orientation" orientation-result)
            (let [ready (browser/wait-for! client
                                           "CSS initial-placement ready tray"
                                           stats/pending-tray-stats-js
                                           stats/pending-tray-ready?)
                  confirm-result (browser/evaluate! client
                                                    stats/confirm-pending-move-js)]
              (assert-clicked! "CSS initial-placement Confirm" confirm-result)
              (let [confirmed (browser/wait-for! client
                                                 "CSS initial-placement confirmed board state"
                                                 stats/direct-drop-fallback-stats-js
                                                 stats/direct-drop-confirmed?)]
                {:drop-result drop-result
                 :pending pending
                 :orientation-result orientation-result
                 :ready ready
                 :confirm-result confirm-result
                 :confirmed confirmed})))))
      (catch Exception error
        (throw (ex-info "Confirmed CSS direct drop smoke failed."
                        {:url url
                         :browser-diagnostics (browser/browser-diagnostics client)
                         :cause (ex-message error)
                         :data (ex-data error)}
                        error)))
      (finally
        (browser/close-cdp! client)))))

(defn- run-confirmed-three-direct-drop! [http-client chrome url]
  (println "Smoke checking confirmed Three.js direct drop.")
  (let [client (open-gnostica-page! http-client
                                    chrome
                                    (first stats/viewports)
                                    (stats/direct-drop-smoke-url url)
                                    [])]
    (try
      (browser/wait-for! client
                         "direct-drop Three.js setup"
                         stats/direct-drop-three-stats-js
                         stats/direct-drop-three-ready?)
      (let [drop-result (browser/evaluate! client stats/initial-placement-three-drop-js)]
        (assert-drop-started! "Three.js initial-placement source-to-board drop" drop-result)
        (assert-first-piece-ghost! "Three.js initial-placement source-to-board drop" drop-result)
        (assert-three-single-drag-highlight! "Three.js initial-placement source-to-board drop" drop-result)
        (let [pending (browser/wait-for! client
                                         "Three.js initial-placement pending tray"
                                         stats/pending-tray-stats-js
                                         stats/pending-tray-needs-choice-ready?)]
          (browser/evaluate! client stats/open-detailed-entry-js)
          (browser/wait-for! client
                             "Three.js initial-placement Detailed entry"
                             stats/pending-tray-stats-js
                             stats/pending-tray-detailed-open-ready?)
          (let [orientation-result (browser/evaluate! client
                                                      stats/choose-north-orientation-js)]
            (assert-selected! "North orientation" orientation-result)
            (let [ready (browser/wait-for! client
                                           "Three.js initial-placement ready tray"
                                           stats/pending-tray-stats-js
                                           stats/pending-tray-ready?)
                  confirm-result (browser/evaluate! client
                                                    stats/confirm-pending-move-js)]
              (assert-clicked! "Three.js initial-placement Confirm" confirm-result)
              (let [confirmed (browser/wait-for! client
                                                 "Three.js initial-placement confirmed board state"
                                                 stats/direct-drop-three-stats-js
                                                 stats/direct-drop-three-confirmed?)]
                {:drop-result drop-result
                 :pending pending
                 :orientation-result orientation-result
                 :ready ready
                 :confirm-result confirm-result
                 :confirmed confirmed})))))
      (catch Exception error
        (throw (ex-info "Confirmed Three.js direct drop smoke failed."
                        {:url url
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
      (run-fallback-direct-gesture-smoke! client "blocked Three.js")
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
          (run-confirmed-three-direct-drop! http-client chrome (:url target))
          (run-missing-three-fallback! http-client chrome (:url target))
          (run-confirmed-fallback-direct-drop! http-client chrome (:url target))
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
