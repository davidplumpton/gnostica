(ns gnostica.keyboard-shortcuts-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.keyboard-shortcuts :as shortcuts]))

(deftest hotkey-command-catalog-is-the-shared-command-model
  (is (= ["?" "G" "I" "W/A/S/D" "Arrow keys" "Esc"]
         (vec (shortcuts/hotkey-command-labels))))
  (is (= "Move the 3D board view when the board is focused"
         (:command (shortcuts/command-by-id :pan-board-view))))
  (is (= [:open-hotkey-help
          :open-icon-help
          :toggle-card-icon-mode
          :pan-board-view
          :close-help-dialogs]
         (mapv :id shortcuts/hotkey-commands))))

(deftest global-shortcut-events-are-derived-from-the-catalog
  (is (= :gnostica.app/open-hotkey-help
         (shortcuts/global-shortcut-event {:key "?"})))
  (is (= :gnostica.app/open-hotkey-help
         (shortcuts/global-shortcut-event {:key "/" :code "Slash" :shift? true})))
  (is (= :gnostica.app/open-icon-help
         (shortcuts/global-shortcut-event {:key "G"})))
  (is (= :gnostica.app/toggle-card-icon-mode
         (shortcuts/global-shortcut-event {:key "i"})))
  (is (= :gnostica.app/close-help-dialogs
         (shortcuts/global-shortcut-event {:key "Escape"})))
  (is (nil? (shortcuts/global-shortcut-event {:key "i" :ctrl? true})))
  (is (nil? (shortcuts/global-shortcut-event {:key "/" :code "Slash"})))
  (is (nil? (shortcuts/global-shortcut-event {:key "w"}))))
