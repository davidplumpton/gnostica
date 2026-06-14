(ns gnostica.game-state.command-contracts.targets
  (:require [gnostica.game-state.command-contracts.primitives :as primitives]
            [gnostica.power-taxonomy :as taxonomy]))

(def SuitPower
  (primitives/enum-schema taxonomy/suit-power-values))

(def WorldCopiedPower
  (primitives/enum-schema taxonomy/world-copied-power-values))

(def TerritorySource
  (primitives/closed-map
   [:kind [:enum :territory]]
   [:board-index primitives/BoardIndex]
   [:piece-id {:optional true} primitives/PieceId]))

(def HandCardSource
  (primitives/closed-map
   [:kind [:enum :hand-card]]
   [:card-id primitives/CardId]
   [:piece-id {:optional true} primitives/PieceId]))

(def MoveSource
  [:or TerritorySource HandCardSource])

(def source-command-entries
  [[:player-id primitives/PlayerId]
   [:source MoveSource]])

(def SourceCommand
  (apply primitives/closed-map source-command-entries))

(def ActingTerritorySource
  (primitives/closed-map
   [:kind [:enum :territory]]
   [:board-index primitives/BoardIndex]
   [:piece-id primitives/PieceId]))

(def ActingHandCardSource
  (primitives/closed-map
   [:kind [:enum :hand-card]]
   [:card-id primitives/CardId]
   [:piece-id primitives/PieceId]))

(def ActingMoveSource
  [:or ActingTerritorySource ActingHandCardSource])

(def acting-source-command-entries
  [[:player-id primitives/PlayerId]
   [:source ActingMoveSource]])

(def ActingSourceCommand
  (apply primitives/closed-map acting-source-command-entries))

(def TerritoryTarget
  (primitives/closed-map
   [:kind [:enum :territory]]
   [:board-index primitives/BoardIndex]))

(def PieceTarget
  (primitives/closed-map
   [:kind [:enum :piece]]
   [:piece-id primitives/PieceId]))

(def WastelandTarget
  (primitives/closed-map
   [:kind [:enum :wasteland]]
   [:row :int]
   [:col :int]))

(def CreatedPieceTarget
  (primitives/closed-map
   [:kind [:enum :created-piece]]))

(def CreatedTerritoryTarget
  (primitives/closed-map
   [:kind [:enum :created-territory]]))

(def TerritoryDestination
  TerritoryTarget)

(def WastelandDestination
  WastelandTarget)
