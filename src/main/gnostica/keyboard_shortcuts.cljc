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
   {:id :keyboard-first-placement-targeting
    :scope :direct-manipulation
    :keys ["Enter" "Arrow keys" "Esc"]
    :command "Choose first-piece placement target by keyboard"}
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

(def dev-demo-hotkey-commands
  [{:id :layout-shuffled-deck-territories
    :scope :global
    :keys ["D"]
    :command "Lay out shuffled deck as territories"
    :event :gnostica.app/layout-shuffled-deck-territories
    :matches [{:key "d"}]}])

(defn hotkey-commands-for
  ([] hotkey-commands)
  ([opts]
   (cond-> hotkey-commands
     (true? (:dev-demo-hotkeys? opts))
     (into dev-demo-hotkey-commands))))

(def global-shortcuts
  (filterv #(= :global (:scope %)) hotkey-commands))

(defn global-shortcuts-for
  ([] global-shortcuts)
  ([opts]
   (filterv #(= :global (:scope %))
            (hotkey-commands-for opts))))

(defn command-by-id
  ([id] (command-by-id id nil))
  ([id opts]
   (some #(when (= id (:id %)) %)
         (hotkey-commands-for opts))))

(defn hotkey-command-labels
  ([] (hotkey-command-labels nil))
  ([opts]
   (mapcat :keys (hotkey-commands-for opts))))

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

(defn global-shortcut-for-event
  ([event-info] (global-shortcut-for-event event-info nil))
  ([event-info opts]
   (let [event-info (normalize-event event-info)]
     (when-not (modified-shortcut? event-info)
       (some #(when (shortcut-match? event-info %) %)
             (global-shortcuts-for opts))))))

(defn global-shortcut-event
  ([event-info] (global-shortcut-event event-info nil))
  ([event-info opts]
   (:event (global-shortcut-for-event event-info opts))))
