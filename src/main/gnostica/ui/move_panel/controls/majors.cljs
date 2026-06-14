(ns gnostica.ui.move-panel.controls.majors
  (:require [gnostica.ui.move-panel.choices :as choices]))

(defn- piece-orientation-major-controls
  [label params board target-piece-options orientation-options]
  [:<>
   [choices/target-piece-choices board target-piece-options (:target-piece-id params)]
   (when (:target-piece-id params)
     [choices/orientation-choices label orientation-options (:orientation params)])])

(defn- devil-move-controls
  [params board devil-action-count-options target-piece-options orientation-options]
  [:<>
   [choices/devil-action-count-choices devil-action-count-options (:devil-action-count params)]
   (when (:devil-action-count params)
     [piece-orientation-major-controls
      "Target orientation"
      params
      board
      target-piece-options
      orientation-options])])

(defn- hermit-move-controls
  [params board target-piece-options target-board-options target-wasteland-options
   orientation-options orientation-required? legal-targets]
  (let [target-selected? (or (:target-piece-id params)
                             (some? (:target-board-index params)))]
    [:<>
     (if target-selected?
       [:div.move-step
        [:div.move-step__header
         [:span "Hermit target"]
         [:strong
          (if-let [piece (some #(when (= (:target-piece-id params) (:id %)) %)
                               (concat target-piece-options []))]
            (choices/piece-choice-label board piece)
            (if-let [cell (some #(when (= (:target-board-index params) (:index %)) %)
                                board)]
              (:title (:card cell))
              "Selected"))]]]
       [:<>
        [choices/target-choice-grid "Hermit target" target-board-options [] (:target-board-index params) nil
         legal-targets]
        [choices/target-piece-choices board target-piece-options (:target-piece-id params)]])
     (when target-selected?
       [choices/target-choice-grid "Destination"
        target-board-options
        target-wasteland-options
        (:hermit-destination-board-index params)
        (:hermit-destination-wasteland params)
        legal-targets])
     (when (and orientation-required?
                (or (:hermit-destination-board-index params)
                    (:hermit-destination-wasteland params)))
       [choices/orientation-choices orientation-options (:orientation params)])]))

(defn render-piece-orientation-major-control
  [{:keys [params]} {:keys [board target-piece-options orientation-options]}]
  [piece-orientation-major-controls
   "Replacement orientation"
   params
   board
   target-piece-options
   orientation-options])

(defn render-hermit-control
  [{:keys [params]} {:keys [board target-piece-options target-board-options
                            target-wasteland-options orientation-options
                            orientation-required? legal-targets]}]
  [hermit-move-controls params
   board
   target-piece-options
   target-board-options
   target-wasteland-options
   orientation-options
   orientation-required?
   legal-targets])

(defn render-devil-control
  [{:keys [params]} {:keys [board devil-action-count-options target-piece-options
                            orientation-options]}]
  [devil-move-controls
   params
   board
   devil-action-count-options
   target-piece-options
   orientation-options])

(def control-renderers
  {:piece-orientation-major render-piece-orientation-major-control
   :hermit render-hermit-control
   :devil render-devil-control})
