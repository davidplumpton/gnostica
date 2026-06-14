(ns gnostica.ui.move-panel.controls.special
  (:require [gnostica.ui.move-panel.choices :as choices]))

(defn render-world-copy-control
  [{:keys [params]} {:keys [world-copy-options legal-targets]}]
  [choices/world-copy-choices world-copy-options (:copied-board-index params)
   (:territories legal-targets)])

(defn render-world-copied-power-control
  [_ {:keys [world-copied-power-options world-copied-power]}]
  [choices/power-choices world-copied-power-options world-copied-power])

(defn render-fool-reveal-count-control
  [{:keys [params]} {:keys [fool-reveal-count-options]}]
  [choices/fool-reveal-count-choices
   fool-reveal-count-options
   (:fool-reveal-count params)])

(defn render-fool-reveal-card-control
  [_ {:keys [fool-reveal-state]}]
  [choices/fool-reveal-card-control fool-reveal-state])

(defn render-fool-reveal-decision-control
  [_ {:keys [fool-reveal-state]}]
  [choices/fool-reveal-decision-control fool-reveal-state])

(defn render-fool-play-power-control
  [_ {:keys [fool-play-power-options fool-play-power]}]
  [choices/fool-play-power-choices fool-play-power-options fool-play-power])

(defn render-high-priestess-redraw-count-control
  [{:keys [params]} {:keys [high-priestess-redraw-count-options]}]
  [choices/high-priestess-redraw-count-choices
   high-priestess-redraw-count-options
   (:high-priestess-redraw-count params)])

(defn render-high-priestess-redraws-control
  [_ {:keys [high-priestess-redraw-options]}]
  [choices/high-priestess-redraw-controls high-priestess-redraw-options])

(defn render-judgement-card-selection-control
  [{:keys [params]} {:keys [judgement-card-options judgement-card-maximum]}]
  [choices/judgement-card-choices
   judgement-card-options
   (:judgement-card-ids params)
   judgement-card-maximum])

(def control-renderers
  {:world-copy render-world-copy-control
   :world-copied-power render-world-copied-power-control
   :fool-reveal-count render-fool-reveal-count-control
   :fool-reveal-card render-fool-reveal-card-control
   :fool-reveal-decision render-fool-reveal-decision-control
   :fool-play-power render-fool-play-power-control
   :high-priestess-redraw-count render-high-priestess-redraw-count-control
   :high-priestess-redraws render-high-priestess-redraws-control
   :judgement-card-selection render-judgement-card-selection-control})
