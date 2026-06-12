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

(def move-keyboard-placement-target-event
  :gnostica.app/move-keyboard-placement-target)

(def accept-keyboard-placement-target-event
  :gnostica.app/accept-keyboard-placement-target)

(def cancel-gesture-intent-event
  :gnostica.app/cancel-gesture-intent)

(def confirm-move-event
  :gnostica.app/confirm-move)

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
   :key-code (.-keyCode event)
   :which (.-which event)
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

(defn- keyboard-placement-direction [event-info]
  (when-not (or (:alt? event-info)
                (:ctrl? event-info)
                (:meta? event-info))
    (case (some-> (:key event-info) str/lower-case)
      "arrowup" :north
      "arrowright" :east
      "arrowdown" :south
      "arrowleft" :west
      nil)))

(defn- accept-key? [event-info]
  (and (not (or (:alt? event-info)
                (:ctrl? event-info)
                (:meta? event-info)))
       (or (contains? #{"enter" " "} (some-> (:key event-info) str/lower-case))
           (= "space" (some-> (:code event-info) str/lower-case)))))

(defn- cancel-key? [event-info]
  (and (not (or (:alt? event-info)
                (:ctrl? event-info)
                (:meta? event-info)))
       (or (= "escape" (some-> (:key event-info) str/lower-case))
           (= "escape" (some-> (:code event-info) str/lower-case))
           (= 27 (:key-code event-info))
           (= 27 (:which event-info)))))

(defn- help-dialog-open? [db]
  (or (app-state/hotkey-help-open? db)
      (app-state/icon-help-open? db)))

(defn- handle-help-dialog-key! [event]
  (let [info (event-info event)]
    (when (cancel-key? info)
      (.preventDefault event)
      (when-let [close-button (.querySelector
                               js/document
                               ".hotkey-help-dialog__close, .icon-help-dialog__close")]
        (.click close-button)))
    true))

(defn- handle-keyboard-placement-key! [event]
  (let [db @rf-db/app-db]
    (when (app-state/keyboard-placement-targeting-active? db)
      (let [info (event-info event)
            mode (app-state/keyboard-placement-targeting-mode db)]
        (cond
          (cancel-key? info)
          (do
            (.preventDefault event)
            (.stopPropagation event)
            (rf/dispatch [cancel-gesture-intent-event])
            true)

          (and (= :target mode)
               (keyboard-placement-direction info))
          (do
            (.preventDefault event)
            (.stopPropagation event)
            (rf/dispatch-sync [move-keyboard-placement-target-event
                               (keyboard-placement-direction info)])
            true)

          (and (= :target mode)
               (accept-key? info))
          (do
            (.preventDefault event)
            (.stopPropagation event)
            (rf/dispatch-sync [accept-keyboard-placement-target-event])
            true)

          (= :orientation mode)
          (or (when-let [request (gesture-input/orientation-key-request info)]
                (when-let [result (app-state/pending-placement-orientation-result
                                   db
                                   request)]
                  (.preventDefault event)
                  (.stopPropagation event)
                  (dispatch-orientation-change!)
                  (rf/dispatch-sync [set-pending-placement-orientation-event result])
                  true))
              (when (accept-key? info)
                (.preventDefault event)
                (.stopPropagation event)
                (when (app-state/move-ready? db)
                  (rf/dispatch [confirm-move-event]))
                true))

          :else
          nil)))))

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
                   (let [db @rf-db/app-db]
                     (if (help-dialog-open? db)
                       (handle-help-dialog-key! event)
                       (when-not (editable-target? (.-target event))
                         (or (handle-keyboard-placement-key! event)
                             (handle-drag-orientation-key! event)
                             (handle-pending-placement-orientation-key! event)
                             (when-let [shortcut-event (shortcuts/global-shortcut-event
                                                        (event-info event)
                                                        {:dev-demo-hotkeys?
                                                         (app-state/dev-demo-hotkeys?
                                                          db)})]
                               (.preventDefault event)
                               (rf/dispatch [shortcut-event])))))))]
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
