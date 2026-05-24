(ns gnostica.app.keyboard
  (:require [clojure.string :as str]
            [gnostica.keyboard-shortcuts :as shortcuts]
            [re-frame.core :as rf]))

(def install-global-shortcuts-fx :gnostica.app.keyboard/install-global-shortcuts)

(defonce global-shortcut-listener
  (atom nil))

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

(defn uninstall! []
  (when-let [listener @global-shortcut-listener]
    (.removeEventListener js/window "keydown" listener)
    (reset! global-shortcut-listener nil)))

(defn install! []
  (uninstall!)
  (let [listener (fn [event]
                   (when-not (editable-target? (.-target event))
                     (when-let [shortcut-event (shortcuts/global-shortcut-event
                                                (event-info event))]
                       (.preventDefault event)
                       (rf/dispatch [shortcut-event]))))]
    (reset! global-shortcut-listener listener)
    (.addEventListener js/window "keydown" listener)))

(rf/reg-fx
 install-global-shortcuts-fx
 (fn [enabled?]
   (if enabled?
     (install!)
     (uninstall!))))
