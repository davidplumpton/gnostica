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

(defn- render-control-group [{:keys [type]} selection controls]
  (let [{:keys [params]} selection
        {:keys [board power power-options rod-mode-options piece-options
                world-copy-options world-copied-power-options world-copied-power
                disc-action-count-options major-action-count-options major-action-count
                sword-action-count-options devil-action-count-options
                sun-disc-mode-options
                sun-cup-target-piece
                fool-reveal-count-options fool-reveal-state
                fool-play-power-options fool-play-power
                high-priestess-redraw-count-options
                high-priestess-redraw-options judgement-card-options
                judgement-card-maximum
                disc-minion-orientation-required?
                disc-target-kind-options sword-target-kind-options
                target-piece-options hand-options discard-card-options source-board-options
                target-board-options target-wasteland-options
                territory-card-source-options one-point-card-options
                replacement-card-options orientation-options orientation-required?
                disc-orientation-available? sun-disc-orientation-available?
                sword-orientation-available?
                distance-options damage-options draw-options legal-targets]} controls]
    (case (move-registry/control-renderer-key type)
      :source-board
      [choices/board-choice-grid "Source territory" source-board-options (:source-board-index params)
       (:territories legal-targets)]

      :hand-card
      [choices/hand-card-choices hand-options (:hand-card-id params)]

      :piece
      [choices/piece-choices board piece-options (:piece-id params)]

      :power
      [choices/power-choices power-options power]

      :major-action-count
      [choices/major-action-count-choices major-action-count-options major-action-count]

      :sword-action-count
      [choices/sword-action-count-choices sword-action-count-options (:sword-action-count params)]

      :world-copy
      [choices/world-copy-choices world-copy-options (:copied-board-index params)
       (:territories legal-targets)]

      :world-copied-power
      [choices/power-choices world-copied-power-options world-copied-power]

      :rod
      [rod-move-controls params
       board
       rod-mode-options
       target-piece-options
       target-board-options
       distance-options
       orientation-options
       orientation-required?
       legal-targets]

      :cup
      [cup-move-controls params
       board
       target-piece-options
       target-board-options
       target-wasteland-options
       territory-card-source-options
       one-point-card-options
       orientation-options
       legal-targets]

      :disc
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
       legal-targets]

      :sun
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
       legal-targets]

      :sword
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
       legal-targets]

      :fool-reveal-count
      [choices/fool-reveal-count-choices
       fool-reveal-count-options
       (:fool-reveal-count params)]

      :fool-reveal-card
      [choices/fool-reveal-card-control fool-reveal-state]

      :fool-reveal-decision
      [choices/fool-reveal-decision-control fool-reveal-state]

      :fool-play-power
      [choices/fool-play-power-choices fool-play-power-options fool-play-power]

      :high-priestess-redraw-count
      [choices/high-priestess-redraw-count-choices
       high-priestess-redraw-count-options
       (:high-priestess-redraw-count params)]

      :high-priestess-redraws
      [choices/high-priestess-redraw-controls high-priestess-redraw-options]

      :judgement-card-selection
      [choices/judgement-card-choices
       judgement-card-options
       (:judgement-card-ids params)
       judgement-card-maximum]

      :piece-orientation-major
      [piece-orientation-major-controls
       "Replacement orientation"
       params
       board
       target-piece-options
       orientation-options]

      :hermit
      [hermit-move-controls params
       board
       target-piece-options
       target-board-options
       target-wasteland-options
       orientation-options
       orientation-required?
       legal-targets]

      :devil
      [devil-move-controls
       params
       board
       devil-action-count-options
       target-piece-options
       orientation-options]

      :target-piece
      [choices/target-piece-choices board target-piece-options (:target-piece-id params)]

      :minion-orientation
      [choices/orientation-choices "Minion orientation"
       events/set-move-minion-orientation
       orientation-options
       (:minion-orientation params)]

      :discard-cards
      [choices/discard-card-choices discard-card-options (:discard-card-ids params)]

      :draw-count
      [choices/draw-count-choices draw-options (:draw-count params)]

      :orientation
      [choices/orientation-choices orientation-options (:orientation params)]

      :target-space
      [choices/target-choice-grid target-board-options
       target-wasteland-options
       (:target-board-index params)
       (:target-wasteland params)
       legal-targets]

      nil)))

(defn active-controls [selection controls control-groups]
  [:<>
   (for [[index group] (map-indexed vector control-groups)]
     ^{:key (str index "-" (:type group) "-" (:power group) "-" (:action-power group))}
     [render-control-group group selection controls])])
