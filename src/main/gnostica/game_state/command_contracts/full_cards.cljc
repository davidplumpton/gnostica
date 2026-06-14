(ns gnostica.game-state.command-contracts.full-cards
  (:require [gnostica.game-state.command-contracts.primitives :as primitives]
            [gnostica.game-state.command-contracts.suits :as suits]
            [gnostica.game-state.command-contracts.targets :as targets]))

(def RedrawPass
  (primitives/closed-map
   [:discard-card-ids {:optional true} [:vector primitives/CardId]]
   [:draw-count primitives/NonNegativeInt]))

(def HighPriestessCommand
  (apply primitives/closed-map
         (concat targets/source-command-entries
                 [[:redraws {:optional true} [:vector {:max 2} RedrawPass]]
                  [:shuffle-fn {:optional true} primitives/ShuffleFn]])))

(def JudgementCommand
  (apply primitives/closed-map
         (concat targets/source-command-entries
                 [[:piece-id primitives/PieceId]
                  [:card-ids [:vector primitives/CardId]]])))

(def SunCupAction
  (primitives/closed-map
   [:target suits/CupTarget]
   [:orientation {:optional true} primitives/Orientation]
   [:territory-card-source {:optional true} [:enum :hand :draw-pile-top]]
   [:one-point-card-id {:optional true} primitives/CardId]))

(def SunDiscTarget
  [:or
   targets/PieceTarget
   targets/TerritoryTarget
   targets/CreatedPieceTarget
   targets/CreatedTerritoryTarget])

(def SunDiscAction
  (primitives/closed-map
   [:target SunDiscTarget]
   [:orientation {:optional true} primitives/Orientation]
   [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
   [:replacement-card-id {:optional true} primitives/CardId]))

(def SunCommand
  (apply primitives/closed-map
         (concat targets/acting-source-command-entries
                 [[:cup SunCupAction]
                  [:disc {:optional true} SunDiscAction]])))

(def MajorAction
  (primitives/closed-map
   [:power :keyword]
   [:piece-id {:optional true} primitives/PieceId]
   [:target {:optional true} [:or targets/PieceTarget
                              targets/TerritoryTarget
                              targets/WastelandTarget]]
   [:destination {:optional true} [:or targets/TerritoryDestination
                                   targets/WastelandDestination]]
   [:orientation {:optional true} primitives/Orientation]
   [:distance {:optional true} primitives/PositiveInt]
   [:damage {:optional true} primitives/PositiveInt]
   [:mode {:optional true} [:enum :move-minion :push-piece :push-territory]]
   [:direction {:optional true} [:enum :north :east :south :west]]
   [:one-point-card-id {:optional true} primitives/CardId]
   [:territory-card-source {:optional true} [:enum :hand :draw-pile-top]]
   [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
   [:replacement-card-id {:optional true} primitives/CardId]))

(def CompositeMajorCommand
  (apply primitives/closed-map
         (concat targets/source-command-entries
                 [[:actions {:optional true} [:vector MajorAction]]
                  [:cup {:optional true} suits/MajorCupAction]
                  [:cup-actions {:optional true} [:vector suits/MajorCupAction]]
                  [:rod {:optional true} suits/MajorRodAction]
                  [:rod-actions {:optional true} [:vector suits/MajorRodAction]]
                  [:minion-orientation {:optional true} primitives/Orientation]
                  [:hand-trade-target {:optional true} targets/PieceTarget]
                  [:hand-trade-target-piece-id {:optional true} primitives/PieceId]])))

(def HierophantCommand
  [:or
   (apply primitives/closed-map
          (concat targets/source-command-entries
                  [[:target targets/PieceTarget]
                   [:orientation primitives/Orientation]]))
   (apply primitives/closed-map
          (concat targets/source-command-entries
                  [[:actions [:vector {:min 1} MajorAction]]]))])

(def HermitCommand
  [:or
   (apply primitives/closed-map
          (concat targets/source-command-entries
                  [[:target [:or targets/PieceTarget targets/TerritoryTarget]]
                   [:destination [:or targets/TerritoryDestination
                                  targets/WastelandDestination]]
                   [:orientation {:optional true} primitives/Orientation]]))
   (apply primitives/closed-map
          (concat targets/source-command-entries
                  [[:actions [:vector {:min 1} MajorAction]]]))])

(def DevilCommand
  [:or
   (apply primitives/closed-map
          (concat targets/source-command-entries
                  [[:target targets/PieceTarget]
                   [:orientation primitives/Orientation]]))
   (apply primitives/closed-map
          (concat targets/source-command-entries
                  [[:orientations [:vector {:min 1 :max 3} MajorAction]]]))
   (apply primitives/closed-map
          (concat targets/source-command-entries
                  [[:actions [:vector {:min 1 :max 3} MajorAction]]]))])
