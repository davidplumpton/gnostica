(ns gnostica.app.keyboard
  (:require [clojure.string :as str]
            [gnostica.app.events :as events]
            [re-frame.core :as rf]))

(defonce keyboard-shortcut-listener
  (atom nil))

(defn- editable-target? [target]
  (let [tag-name (some-> target .-tagName str/lower-case)]
    (or (and target (.-isContentEditable target))
        (#{"input" "select" "textarea"} tag-name))))

(defn- modified-shortcut? [event]
  (or (.-altKey event)
      (.-ctrlKey event)
      (.-metaKey event)))

(defn- question-mark-key? [event key]
  (or (= "?" key)
      (and (= "Slash" (.-code event))
           (.-shiftKey event))))

(defn install! []
  (when-let [listener @keyboard-shortcut-listener]
    (.removeEventListener js/window "keydown" listener))
  (let [listener (fn [event]
                   (let [key (.-key event)
                         lower-key (str/lower-case key)]
                     (when (and (not (modified-shortcut? event))
                                (not (editable-target? (.-target event))))
                       (cond
                         (question-mark-key? event key)
                         (do
                           (.preventDefault event)
                           (rf/dispatch [events/open-hotkey-help]))

                         (= "escape" lower-key)
                         (do
                           (.preventDefault event)
                           (rf/dispatch [events/close-help-dialogs]))

                         (= "i" lower-key)
                         (do
                           (.preventDefault event)
                           (rf/dispatch [events/toggle-card-icon-mode]))

                         (= "g" lower-key)
                         (do
                           (.preventDefault event)
                           (rf/dispatch [events/open-icon-help]))))))]
    (reset! keyboard-shortcut-listener listener)
    (.addEventListener js/window "keydown" listener)))
