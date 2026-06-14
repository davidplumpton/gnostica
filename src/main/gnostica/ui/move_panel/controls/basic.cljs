(ns gnostica.ui.move-panel.controls.basic
  (:require [gnostica.app.events :as events]
            [gnostica.ui.move-panel.choices :as choices]))

(defn render-source-board-control
  [{:keys [params]} {:keys [source-board-options legal-targets]}]
  [choices/board-choice-grid "Source territory" source-board-options
   (:source-board-index params)
   (:territories legal-targets)])

(defn render-hand-card-control
  [{:keys [params]} {:keys [hand-options]}]
  [choices/hand-card-choices hand-options (:hand-card-id params)])

(defn render-piece-control
  [{:keys [params]} {:keys [board piece-options]}]
  [choices/piece-choices board piece-options (:piece-id params)])

(defn render-power-control
  [_ {:keys [power power-options]}]
  [choices/power-choices power-options power])

(defn render-major-action-count-control
  [_ {:keys [major-action-count-options major-action-count]}]
  [choices/major-action-count-choices major-action-count-options major-action-count])

(defn render-sword-action-count-control
  [{:keys [params]} {:keys [sword-action-count-options]}]
  [choices/sword-action-count-choices sword-action-count-options (:sword-action-count params)])

(defn render-target-piece-control
  [{:keys [params]} {:keys [board target-piece-options]}]
  [choices/target-piece-choices board target-piece-options (:target-piece-id params)])

(defn render-minion-orientation-control
  [{:keys [params]} {:keys [orientation-options]}]
  [choices/orientation-choices "Minion orientation"
   events/set-move-minion-orientation
   orientation-options
   (:minion-orientation params)])

(defn render-discard-cards-control
  [{:keys [params]} {:keys [discard-card-options]}]
  [choices/discard-card-choices discard-card-options (:discard-card-ids params)])

(defn render-draw-count-control
  [{:keys [params]} {:keys [draw-options]}]
  [choices/draw-count-choices draw-options (:draw-count params)])

(defn render-orientation-control
  [{:keys [params]} {:keys [orientation-options]}]
  [choices/orientation-choices orientation-options (:orientation params)])

(defn render-target-space-control
  [{:keys [params]} {:keys [target-board-options target-wasteland-options legal-targets]}]
  [choices/target-choice-grid target-board-options
   target-wasteland-options
   (:target-board-index params)
   (:target-wasteland params)
   legal-targets])

(def control-renderers
  {:source-board render-source-board-control
   :hand-card render-hand-card-control
   :piece render-piece-control
   :power render-power-control
   :major-action-count render-major-action-count-control
   :sword-action-count render-sword-action-count-control
   :target-piece render-target-piece-control
   :minion-orientation render-minion-orientation-control
   :discard-cards render-discard-cards-control
   :draw-count render-draw-count-control
   :orientation render-orientation-control
   :target-space render-target-space-control})
