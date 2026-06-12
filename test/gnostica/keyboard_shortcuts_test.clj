(ns gnostica.keyboard-shortcuts-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.keyboard-shortcuts :as shortcuts]))

(deftest hotkey-command-catalog-is-the-shared-command-model
  (is (= ["?" "G" "I" "O" "Enter" "Arrow keys" "Esc" "W/A/S/D" "Arrow keys" "Esc"]
         (vec (shortcuts/hotkey-command-labels))))
  (is (= "Cycle dragged or pending piece orientation"
         (:command (shortcuts/command-by-id :cycle-drag-orientation))))
  (is (= "Move the 3D board view when the board is focused"
         (:command (shortcuts/command-by-id :pan-board-view))))
  (is (= [:open-hotkey-help
          :open-icon-help
          :toggle-card-icon-mode
          :cycle-drag-orientation
          :keyboard-first-placement-targeting
          :pan-board-view
          :close-help-dialogs]
         (mapv :id shortcuts/hotkey-commands))))

(deftest dev-demo-hotkey-is-only-in-dev-catalog
  (is (nil? (shortcuts/command-by-id :layout-shuffled-deck-territories)))
  (is (= "Lay out shuffled deck as territories"
         (:command (shortcuts/command-by-id
                    :layout-shuffled-deck-territories
                    {:dev-demo-hotkeys? true}))))
  (is (= ["?" "G" "I" "O" "Enter" "Arrow keys" "Esc" "W/A/S/D"
          "Arrow keys" "Esc" "D"]
         (vec (shortcuts/hotkey-command-labels
               {:dev-demo-hotkeys? true})))))

(deftest global-shortcut-events-are-derived-from-the-catalog
  (is (= :gnostica.app/open-hotkey-help
         (shortcuts/global-shortcut-event {:key "?"})))
  (is (= :gnostica.app/open-hotkey-help
         (shortcuts/global-shortcut-event {:key "/" :code "Slash" :shift? true})))
  (is (= :gnostica.app/open-icon-help
         (shortcuts/global-shortcut-event {:key "G"})))
  (is (= :gnostica.app/toggle-card-icon-mode
         (shortcuts/global-shortcut-event {:key "i"})))
  (is (nil? (shortcuts/global-shortcut-event {:key "D"})))
  (is (= :gnostica.app/layout-shuffled-deck-territories
         (shortcuts/global-shortcut-event {:key "D"}
                                          {:dev-demo-hotkeys? true})))
  (is (= :gnostica.app/close-help-dialogs
         (shortcuts/global-shortcut-event {:key "Escape"})))
  (is (nil? (shortcuts/global-shortcut-event {:key "i" :ctrl? true})))
  (is (nil? (shortcuts/global-shortcut-event {:key "/" :code "Slash"})))
  (is (nil? (shortcuts/global-shortcut-event {:key "w"}))))
