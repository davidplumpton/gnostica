(ns gnostica.ui.move-panel.controls
  (:require [gnostica.app.events :as events]
            [gnostica.move-selection.registry :as move-registry]
            [gnostica.ui.move-panel.choices :as choices]))

(defn- cup-move-controls
  [params board target-piece-options target-board-options target-wasteland-options
   territory-card-source-options one-point-card-options orientation-options legal-targets]
  [:<>
   [choices/target-choice-grid target-board-options
    target-wasteland-options
    (:target-board-index params)
    (:target-wasteland params)
    legal-targets]
   [choices/target-piece-choices board target-piece-options (:target-piece-id params)]
   (when (:target-board-index params)
     [choices/orientation-choices orientation-options (:orientation params)])
   (when (:target-wasteland params)
     [choices/territory-card-source-choices territory-card-source-options
      (:territory-card-source params)])
   (when (and (:target-wasteland params)
              (or (= 1 (count territory-card-source-options))
                  (= :hand (:territory-card-source params))))
     [choices/one-point-card-choices one-point-card-options (:one-point-card-id params)])])

(defn- rod-move-controls
  [params board rod-mode-options target-piece-options target-board-options distance-options
   orientation-options orientation-required? legal-targets]
  [:<>
   [choices/rod-mode-choices rod-mode-options (:rod-mode params)]
   (case (:rod-mode params)
     :move-minion
     [:<>
      [choices/distance-choices distance-options (:distance params)]
      (when (and orientation-required? (:distance params))
        [choices/orientation-choices orientation-options (:orientation params)])]

     :push-piece
     [:<>
      [choices/target-piece-choices board target-piece-options (:target-piece-id params)]
      (when (:target-piece-id params)
        [choices/distance-choices distance-options (:distance params)])
      (when (and orientation-required? (:distance params))
        [choices/orientation-choices orientation-options (:orientation params)])]

     :push-territory
     [:<>
      [choices/board-choice-grid "Target territory" target-board-options (:target-board-index params)
       (:territories legal-targets)]
      (when (:target-board-index params)
        [choices/distance-choices distance-options (:distance params)])]

     nil)])

(defn- sun-move-controls
  [params board sun-disc-mode-options target-piece-options target-board-options
   target-wasteland-options one-point-card-options replacement-card-options
   orientation-options sun-cup-target-piece sun-disc-orientation-available?
   legal-targets]
  (let [cup-target-ready? (or (:target-piece-id params)
                              (:target-wasteland params)
                              (and (some? (:target-board-index params))
                                   (:orientation params)))
        normal-wasteland-cup? (and (:target-wasteland params)
                                   (:sun-disc-mode params)
                                   (not= :created-territory
                                         (:sun-disc-mode params)))]
    [:<>
     [choices/target-choice-grid board
      target-wasteland-options
      (:target-board-index params)
      (:target-wasteland params)
      legal-targets]
     (when (and (not (:sun-disc-mode params))
                (not (:target-board-index params))
                (not (:target-wasteland params)))
       [choices/target-piece-choices "Cup target piece"
        board
        target-piece-options
        (:target-piece-id params)])
     (when (and (:sun-disc-mode params)
                (:target-piece-id params))
       [choices/target-piece-summary "Cup target piece" board sun-cup-target-piece])
     (when (:target-board-index params)
       [choices/orientation-choices orientation-options (:orientation params)])
     (when cup-target-ready?
       [choices/sun-disc-mode-choices sun-disc-mode-options (:sun-disc-mode params)])
     (when normal-wasteland-cup?
       [choices/one-point-card-choices one-point-card-options (:one-point-card-id params)])
     (case (:sun-disc-mode params)
       :created-piece
       nil

       :created-territory
       [choices/replacement-card-choices replacement-card-options
        (:sun-disc-replacement-card-id params)]

       :piece
       [:<>
        [choices/target-piece-choices "Disc target piece"
         board
         target-piece-options
         (:sun-disc-target-piece-id params)]
        (when (and (:sun-disc-target-piece-id params)
                   sun-disc-orientation-available?)
          [choices/orientation-choices "Disc orientation"
           events/set-move-sun-disc-orientation
           orientation-options
           (:sun-disc-orientation params)])]

       :territory
       [:<>
        [choices/board-choice-grid "Disc target territory"
         target-board-options
         (:sun-disc-target-board-index params)
         (:territories legal-targets)]
        (when (:sun-disc-target-board-index params)
          [choices/replacement-card-choices replacement-card-options
           (:sun-disc-replacement-card-id params)])]

       nil)]))

(defn- disc-move-controls
  [params board disc-action-count-options disc-minion-orientation-required?
   disc-target-kind-options target-piece-options target-board-options
   replacement-source-options replacement-card-options orientation-options
   orientation-available? legal-targets]
  (let [action-count-ready? (or (empty? disc-action-count-options)
                                (:disc-action-count params))
        minion-orientation-ready? (or (not disc-minion-orientation-required?)
                                      (:minion-orientation params))
        target-ready? (and action-count-ready? minion-orientation-ready?)]
    [:<>
     (when (seq disc-action-count-options)
       [choices/disc-action-count-choices disc-action-count-options (:disc-action-count params)])
     (when disc-minion-orientation-required?
       [choices/orientation-choices "Minion orientation"
        events/set-move-minion-orientation
        orientation-options
        (:minion-orientation params)])
     (when target-ready?
       [choices/disc-target-kind-choices disc-target-kind-options (:disc-target-kind params)])
     (when target-ready?
       (case (:disc-target-kind params)
         :piece
         [:<>
          [choices/target-piece-choices board target-piece-options (:target-piece-id params)]
          (when orientation-available?
            [choices/orientation-choices orientation-options (:orientation params)])]

         :territory
         [:<>
          [choices/board-choice-grid "Target territory" target-board-options (:target-board-index params)
           (:territories legal-targets)]
          (when (:target-board-index params)
            [choices/territory-card-source-choices "Replacement source"
             replacement-source-options
             (:replacement-card-source params)])
          (when (and (:target-board-index params)
                     (or (= 1 (count replacement-source-options))
                         (:replacement-card-source params)))
            [choices/replacement-card-choices replacement-card-options (:replacement-card-id params)])]

         nil))]))

(defn- sword-move-controls
  [params board sword-target-kind-options target-piece-options target-board-options
   replacement-source-options replacement-card-options orientation-options
   orientation-available? damage-options legal-targets]
  [:<>
   [choices/sword-target-kind-choices sword-target-kind-options (:sword-target-kind params)]
   (case (:sword-target-kind params)
     :piece
     [:<>
      [choices/target-piece-choices board target-piece-options (:target-piece-id params)]
      (when (:target-piece-id params)
        [choices/damage-choices damage-options (:damage params)])
      (when (and orientation-available? (:damage params))
        [choices/orientation-choices orientation-options (:orientation params)])]

     :territory
     [:<>
      [choices/board-choice-grid "Target territory" target-board-options (:target-board-index params)
       (:territories legal-targets)]
      (when (:target-board-index params)
        [choices/damage-choices damage-options (:damage params)])
      (when (and (:target-board-index params)
                 (:damage params))
        [choices/territory-card-source-choices "Replacement source"
         replacement-source-options
         (:replacement-card-source params)])
      (when (and (:target-board-index params)
                 (:damage params)
                 (or (= 1 (count replacement-source-options))
                     (:replacement-card-source params)))
        [choices/replacement-card-choices replacement-card-options (:replacement-card-id params)])]

     nil)])

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

(defn- render-source-board-control
  [{:keys [params]} {:keys [source-board-options legal-targets]}]
  [choices/board-choice-grid "Source territory" source-board-options
   (:source-board-index params)
   (:territories legal-targets)])

(defn- render-hand-card-control
  [{:keys [params]} {:keys [hand-options]}]
  [choices/hand-card-choices hand-options (:hand-card-id params)])

(defn- render-piece-control
  [{:keys [params]} {:keys [board piece-options]}]
  [choices/piece-choices board piece-options (:piece-id params)])

(defn- render-power-control
  [_ {:keys [power power-options]}]
  [choices/power-choices power-options power])

(defn- render-major-action-count-control
  [_ {:keys [major-action-count-options major-action-count]}]
  [choices/major-action-count-choices major-action-count-options major-action-count])

(defn- render-sword-action-count-control
  [{:keys [params]} {:keys [sword-action-count-options]}]
  [choices/sword-action-count-choices sword-action-count-options (:sword-action-count params)])

(defn- render-world-copy-control
  [{:keys [params]} {:keys [world-copy-options legal-targets]}]
  [choices/world-copy-choices world-copy-options (:copied-board-index params)
   (:territories legal-targets)])

(defn- render-world-copied-power-control
  [_ {:keys [world-copied-power-options world-copied-power]}]
  [choices/power-choices world-copied-power-options world-copied-power])

(defn- render-rod-control
  [{:keys [params]} {:keys [board rod-mode-options target-piece-options
                            target-board-options distance-options orientation-options
                            orientation-required? legal-targets]}]
  [rod-move-controls params
   board
   rod-mode-options
   target-piece-options
   target-board-options
   distance-options
   orientation-options
   orientation-required?
   legal-targets])

(defn- render-cup-control
  [{:keys [params]} {:keys [board target-piece-options target-board-options
                            target-wasteland-options territory-card-source-options
                            one-point-card-options orientation-options legal-targets]}]
  [cup-move-controls params
   board
   target-piece-options
   target-board-options
   target-wasteland-options
   territory-card-source-options
   one-point-card-options
   orientation-options
   legal-targets])

(defn- render-disc-control
  [{:keys [params]} {:keys [board disc-action-count-options
                            disc-minion-orientation-required?
                            disc-target-kind-options target-piece-options
                            target-board-options territory-card-source-options
                            replacement-card-options orientation-options
                            disc-orientation-available? legal-targets]}]
  [disc-move-controls params
   board
   disc-action-count-options
   disc-minion-orientation-required?
   disc-target-kind-options
   target-piece-options
   target-board-options
   territory-card-source-options
   replacement-card-options
   orientation-options
   disc-orientation-available?
   legal-targets])

(defn- render-sun-control
  [{:keys [params]} {:keys [board sun-disc-mode-options target-piece-options
                            target-board-options target-wasteland-options
                            one-point-card-options replacement-card-options
                            orientation-options sun-cup-target-piece
                            sun-disc-orientation-available? legal-targets]}]
  [sun-move-controls params
   board
   sun-disc-mode-options
   target-piece-options
   target-board-options
   target-wasteland-options
   one-point-card-options
   replacement-card-options
   orientation-options
   sun-cup-target-piece
   sun-disc-orientation-available?
   legal-targets])

(defn- render-sword-control
  [{:keys [params]} {:keys [board sword-target-kind-options target-piece-options
                            target-board-options territory-card-source-options
                            replacement-card-options orientation-options
                            sword-orientation-available? damage-options legal-targets]}]
  [sword-move-controls params
   board
   sword-target-kind-options
   target-piece-options
   target-board-options
   territory-card-source-options
   replacement-card-options
   orientation-options
   sword-orientation-available?
   damage-options
   legal-targets])

(defn- render-fool-reveal-count-control
  [{:keys [params]} {:keys [fool-reveal-count-options]}]
  [choices/fool-reveal-count-choices
   fool-reveal-count-options
   (:fool-reveal-count params)])

(defn- render-fool-reveal-card-control
  [_ {:keys [fool-reveal-state]}]
  [choices/fool-reveal-card-control fool-reveal-state])

(defn- render-fool-reveal-decision-control
  [_ {:keys [fool-reveal-state]}]
  [choices/fool-reveal-decision-control fool-reveal-state])

(defn- render-fool-play-power-control
  [_ {:keys [fool-play-power-options fool-play-power]}]
  [choices/fool-play-power-choices fool-play-power-options fool-play-power])

(defn- render-high-priestess-redraw-count-control
  [{:keys [params]} {:keys [high-priestess-redraw-count-options]}]
  [choices/high-priestess-redraw-count-choices
   high-priestess-redraw-count-options
   (:high-priestess-redraw-count params)])

(defn- render-high-priestess-redraws-control
  [_ {:keys [high-priestess-redraw-options]}]
  [choices/high-priestess-redraw-controls high-priestess-redraw-options])

(defn- render-judgement-card-selection-control
  [{:keys [params]} {:keys [judgement-card-options judgement-card-maximum]}]
  [choices/judgement-card-choices
   judgement-card-options
   (:judgement-card-ids params)
   judgement-card-maximum])

(defn- render-piece-orientation-major-control
  [{:keys [params]} {:keys [board target-piece-options orientation-options]}]
  [piece-orientation-major-controls
   "Replacement orientation"
   params
   board
   target-piece-options
   orientation-options])

(defn- render-hermit-control
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

(defn- render-devil-control
  [{:keys [params]} {:keys [board devil-action-count-options target-piece-options
                            orientation-options]}]
  [devil-move-controls
   params
   board
   devil-action-count-options
   target-piece-options
   orientation-options])

(defn- render-target-piece-control
  [{:keys [params]} {:keys [board target-piece-options]}]
  [choices/target-piece-choices board target-piece-options (:target-piece-id params)])

(defn- render-minion-orientation-control
  [{:keys [params]} {:keys [orientation-options]}]
  [choices/orientation-choices "Minion orientation"
   events/set-move-minion-orientation
   orientation-options
   (:minion-orientation params)])

(defn- render-discard-cards-control
  [{:keys [params]} {:keys [discard-card-options]}]
  [choices/discard-card-choices discard-card-options (:discard-card-ids params)])

(defn- render-draw-count-control
  [{:keys [params]} {:keys [draw-options]}]
  [choices/draw-count-choices draw-options (:draw-count params)])

(defn- render-orientation-control
  [{:keys [params]} {:keys [orientation-options]}]
  [choices/orientation-choices orientation-options (:orientation params)])

(defn- render-target-space-control
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
   :world-copy render-world-copy-control
   :world-copied-power render-world-copied-power-control
   :rod render-rod-control
   :cup render-cup-control
   :disc render-disc-control
   :sun render-sun-control
   :sword render-sword-control
   :fool-reveal-count render-fool-reveal-count-control
   :fool-reveal-card render-fool-reveal-card-control
   :fool-reveal-decision render-fool-reveal-decision-control
   :fool-play-power render-fool-play-power-control
   :high-priestess-redraw-count render-high-priestess-redraw-count-control
   :high-priestess-redraws render-high-priestess-redraws-control
   :judgement-card-selection render-judgement-card-selection-control
   :piece-orientation-major render-piece-orientation-major-control
   :hermit render-hermit-control
   :devil render-devil-control
   :target-piece render-target-piece-control
   :minion-orientation render-minion-orientation-control
   :discard-cards render-discard-cards-control
   :draw-count render-draw-count-control
   :orientation render-orientation-control
   :target-space render-target-space-control})

(defn- render-control-group [{:keys [type]} selection controls]
  (when-let [renderer (get control-renderers
                           (move-registry/control-renderer-key type))]
    [renderer selection controls]))

(defn active-controls [selection controls control-groups]
  [:<>
   (for [[index group] (map-indexed vector control-groups)]
     ^{:key (str index "-" (:type group) "-" (:power group) "-" (:action-power group))}
     [render-control-group group selection controls])])
