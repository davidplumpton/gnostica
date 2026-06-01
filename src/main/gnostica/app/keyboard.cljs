(ns gnostica.app.keyboard
  (:require [clojure.string :as str]
            [gnostica.app-state :as app-state]
            [gnostica.gesture-input :as gesture-input]
            [gnostica.keyboard-shortcuts :as shortcuts]
            [re-frame.core :as rf]
            [re-frame.db :as rf-db]))

(def install-global-shortcuts-fx :gnostica.app.keyboard/install-global-shortcuts)

(def set-gesture-drag-orientation-event
  :gnostica.app/set-gesture-drag-orientation)

(def set-pending-placement-orientation-event
  :gnostica.app/set-pending-placement-orientation)

(defonce global-shortcut-listener
  (atom nil))

;; Capture lets drag-orientation keys run before focused board controls stop propagation.
(def global-shortcut-listener-capture? true)

(defn- editable-target? [target]
  (let [tag-name (some-> target .-tagName str/lower-case)]
    (or (and target (.-isContentEditable target))
        (#{"input" "select" "textarea"} tag-name))))

(defn- event-info [event]
  {:key (.-key event)
   :code (.-code event)
   :shift? (.-shiftKey event)
   :alt? (.-altKey event)
   :ctrl? (.-ctrlKey event)
   :meta? (.-metaKey event)})

(defn- dispatch-orientation-change! []
  (.dispatchEvent js/window
                  (js/CustomEvent.
                   gesture-input/orientation-change-event
                   #js {:bubbles false
                        :cancelable false})))

(defn- handle-drag-orientation-key! [event]
  (when-let [request (gesture-input/orientation-key-request (event-info event))]
    (when-let [input (gesture-input/active-gesture-input)]
      (when-let [result (app-state/gesture-drag-orientation-result
                         @rf-db/app-db
                         input
                         request)]
        (.preventDefault event)
        (.stopPropagation event)
        (when-let [orientation (:orientation result)]
          (gesture-input/set-active-gesture-input!
           (gesture-input/with-drag-orientation input orientation)))
        (dispatch-orientation-change!)
        (rf/dispatch [set-gesture-drag-orientation-event result])
        true))))

(defn- handle-pending-placement-orientation-key! [event]
  (when-let [request (gesture-input/orientation-key-request (event-info event))]
    (when-let [result (app-state/pending-placement-orientation-result
                       @rf-db/app-db
                       request)]
      (.preventDefault event)
      (.stopPropagation event)
      (dispatch-orientation-change!)
      (rf/dispatch [set-pending-placement-orientation-event result])
      true)))

(defn uninstall! []
  (when-let [listener @global-shortcut-listener]
    (.removeEventListener js/window
                          "keydown"
                          listener
                          global-shortcut-listener-capture?)
    (reset! global-shortcut-listener nil)))

(defn install! []
  (uninstall!)
  (let [listener (fn [event]
                   (when-not (editable-target? (.-target event))
                     (or (handle-drag-orientation-key! event)
                         (handle-pending-placement-orientation-key! event)
                         (when-let [shortcut-event (shortcuts/global-shortcut-event
                                                    (event-info event))]
                           (.preventDefault event)
                           (rf/dispatch [shortcut-event])))))]
    (reset! global-shortcut-listener listener)
    (.addEventListener js/window
                       "keydown"
                       listener
                       global-shortcut-listener-capture?)))

(rf/reg-fx
 install-global-shortcuts-fx
 (fn [enabled?]
   (if enabled?
     (install!)
     (uninstall!))))
