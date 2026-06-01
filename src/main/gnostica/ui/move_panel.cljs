(ns gnostica.ui.move-panel
  (:require [gnostica.ui.move-panel.pending-tray :as pending-tray]
            [gnostica.ui.move-panel.shell :as shell]))

(def pending-move-tray pending-tray/pending-move-tray)

(def move-panel shell/move-panel)
