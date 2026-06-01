(ns gnostica.move-selection.options)

(def rod-mode-order
  [:move-minion
   :push-piece
   :push-territory])

(def rod-modes
  (set rod-mode-order))

(def rod-mode-definitions
  {:move-minion {:id :move-minion
                 :label "Move minion"}
   :push-piece {:id :push-piece
                :label "Push piece"}
   :push-territory {:id :push-territory
                    :label "Push territory"}})

(def disc-target-kind-order
  [:piece
   :territory])

(def disc-target-kinds
  (set disc-target-kind-order))

(def disc-target-kind-definitions
  {:piece {:id :piece
           :label "Grow piece"}
   :territory {:id :territory
               :label "Grow territory"}})

(def sun-disc-mode-order
  [:skip
   :created-piece
   :created-territory
   :piece
   :territory])

(def sun-disc-modes
  (set sun-disc-mode-order))

(def sun-disc-mode-definitions
  {:skip {:id :skip
          :label "Cup only"}
   :created-piece {:id :created-piece
                   :label "Grow created piece"}
   :created-territory {:id :created-territory
                       :label "Grow created territory"}
   :piece {:id :piece
           :label "Grow existing piece"}
   :territory {:id :territory
               :label "Grow existing territory"}})

(def sword-target-kind-order
  [:piece
   :territory])

(def sword-target-kinds
  (set sword-target-kind-order))

(def sword-target-kind-definitions
  {:piece {:id :piece
           :label "Attack piece"}
   :territory {:id :territory
               :label "Attack territory"}})

(def fool-reveal-count-order
  [0 1 2])

(def high-priestess-redraw-count-order
  [0 1 2])

(def strength-disc-action-count-order
  [1 2])

(def death-sword-action-count-order
  [1 2])

(def hand-trade-major-action-count-order
  [1 2])

(def hand-trade-major-action-count-definitions
  {1 {:id 1
      :label "Trade only"}
   2 {:id 2
      :label "Use both"}})

(def devil-action-count-order
  [1 2 3])

(def territory-card-source-order
  [:hand :draw-pile-top])

(def territory-card-source-definitions
  {:hand {:id :hand
          :label "Hand one-point card"}
   :draw-pile-top {:id :draw-pile-top
                   :label "Top draw-pile card"}})

(def disc-replacement-card-source-order
  [:hand :discard-pile])

(def disc-replacement-card-source-definitions
  {:hand {:id :hand
          :label "Hand card"}
   :discard-pile {:id :discard-pile
                  :label "Discard pile card"}})

(def requirement-prompts
  {:source-board-index "Choose a source territory with one of your pieces."
   :hand-card-id "Choose a card from the current player's hand."
   :power "Choose the card power."
   :piece-id "Choose a minion."
   :rod-mode "Choose a Rod move."
   :disc-action-count "Choose how many Disc actions to apply."
   :sword-action-count "Choose how many Sword actions to apply."
   :devil-action-count "Choose how many Devil orientations to apply."
   :minion-orientation "Choose the minion orientation."
   :sun-disc-mode "Choose whether Sun applies Disc."
   :disc-target-kind "Choose piece or territory growth."
   :sword-target-kind "Choose piece or territory attack."
   :fool-reveal-count "Choose how many cards Fool reveals."
   :fool-reveal-card "Reveal the next Fool card."
   :fool-reveal-choice "Choose whether to skip or play the revealed card."
   :fool-play-power "Choose the revealed card power."
   :high-priestess-redraw-count "Choose how many redraw passes High Priestess applies."
   :high-priestess-redraws "Choose cards and draw counts for each redraw pass."
   :judgement-card-selection "Choose discard-pile cards for Judgement."
   :copied-board-index "Choose a major territory for World to copy."
   :copied-power "Choose the copied power."
   :target-piece-id "Choose a target piece."
   :target-board-index "Choose a target territory."
   :target-space "Choose a target territory, enemy piece, or wasteland."
   :hermit-target-space "Choose a targeted piece or territory."
   :hermit-destination-space "Choose a destination territory or wasteland."
   :initial-target-space "Choose an empty territory or wasteland."
   :territory-card-source "Choose where the new territory card comes from."
   :one-point-card-id "Choose a one-point card from the current player's hand."
   :replacement-card-source "Choose where the replacement card comes from."
   :replacement-card-id "Choose a replacement card."
   :orientation "Choose an orientation."
   :distance "Choose a distance."
   :damage "Choose damage."
   :draw-count "Choose cards to discard, then how many cards to draw."})
