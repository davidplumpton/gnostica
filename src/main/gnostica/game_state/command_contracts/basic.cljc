(ns gnostica.game-state.command-contracts.basic
  (:require [gnostica.game-state.command-contracts.primitives :as primitives]
            [gnostica.game-state.command-contracts.targets :as targets]))

(def DrawCommand
  (primitives/closed-map
   [:player-id primitives/PlayerId]
   [:discard-card-ids {:optional true} [:vector primitives/CardId]]
   [:draw-count primitives/NonNegativeInt]
   [:shuffle-fn {:optional true} primitives/ShuffleFn]))

(def OrientCommand
  (primitives/closed-map
   [:player-id primitives/PlayerId]
   [:piece-id primitives/PieceId]
   [:orientation primitives/Orientation]))

(def InitialPlacementCommand
  (primitives/closed-map
   [:player-id primitives/PlayerId]
   [:target [:or targets/TerritoryTarget targets/WastelandTarget]]
   [:orientation primitives/Orientation]))
