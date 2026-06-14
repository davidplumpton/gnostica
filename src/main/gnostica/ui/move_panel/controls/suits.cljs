(ns gnostica.ui.move-panel.controls.suits
  (:require [gnostica.app.events :as events]
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

(defn render-rod-control
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

(defn render-cup-control
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

(defn render-disc-control
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

(defn render-sun-control
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

(defn render-sword-control
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

(def control-renderers
  {:rod render-rod-control
   :cup render-cup-control
   :disc render-disc-control
   :sun render-sun-control
   :sword render-sword-control})
