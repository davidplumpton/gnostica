(ns gnostica.keyboard-shortcuts
  (:require [clojure.string :as str]))

(def hotkey-commands
  [{:id :open-hotkey-help
    :scope :global
    :keys ["?"]
    :command "Show keyboard commands"
    :event :gnostica.app/open-hotkey-help
    :matches [{:key "?"}
              {:code "Slash" :shift? true}]}
   {:id :open-icon-help
    :scope :global
    :keys ["G"]
    :command "Show special move icon guide"
    :event :gnostica.app/open-icon-help
    :matches [{:key "g"}]}
   {:id :toggle-card-icon-mode
    :scope :global
    :keys ["I"]
    :command "Toggle card icon overlays"
    :event :gnostica.app/toggle-card-icon-mode
    :matches [{:key "i"}]}
   {:id :cycle-drag-orientation
    :scope :direct-manipulation
    :keys ["O"]
    :command "Cycle dragged or pending piece orientation"}
   {:id :pan-board-view
    :scope :board
    :keys ["W/A/S/D" "Arrow keys"]
    :command "Move the 3D board view when the board is focused"}
   {:id :close-help-dialogs
    :scope :global
    :keys ["Esc"]
    :command "Close open help dialogs"
    :event :gnostica.app/close-help-dialogs
    :matches [{:key "escape"}]}])

(def global-shortcuts
  (filterv #(= :global (:scope %)) hotkey-commands))

(defn command-by-id [id]
  (some #(when (= id (:id %)) %)
        hotkey-commands))

(defn hotkey-command-labels []
  (mapcat :keys hotkey-commands))

(defn- normalize-key [key]
  (some-> key str/lower-case))

(defn- normalize-event [event-info]
  (-> event-info
      (update :key normalize-key)
      (update :shift? true?)
      (update :alt? true?)
      (update :ctrl? true?)
      (update :meta? true?)))

(defn- modified-shortcut? [{:keys [alt? ctrl? meta?]}]
  (or alt? ctrl? meta?))

(defn- match-value? [expected actual]
  (if (set? expected)
    (contains? expected actual)
    (= expected actual)))

(defn- event-match? [event-info match]
  (every? (fn [[key expected]]
            (match-value? expected (get event-info key)))
          match))

(defn- shortcut-match? [event-info shortcut]
  (some #(event-match? event-info %)
        (:matches shortcut)))

(defn global-shortcut-for-event [event-info]
  (let [event-info (normalize-event event-info)]
    (when-not (modified-shortcut? event-info)
      (some #(when (shortcut-match? event-info %) %)
            global-shortcuts))))

(defn global-shortcut-event [event-info]
  (:event (global-shortcut-for-event event-info)))
