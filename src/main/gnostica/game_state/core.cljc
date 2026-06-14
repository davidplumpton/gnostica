(ns gnostica.game-state.core
  (:require [gnostica.game-state.board-pieces :as board-pieces]
            [gnostica.game-state.collections :as collections]
            [gnostica.game-state.constants :as constants]
            [gnostica.game-state.deck :as deck]
            [gnostica.game-state.hands :as hands]
            [gnostica.game-state.pieces :as piece-state]
            [gnostica.game-state.players :as players]
            [gnostica.game-state.result :as result]
            [gnostica.game-state.score :as score]
            [gnostica.game-state.setup :as setup]
            [gnostica.game-state.sources :as sources]
            [gnostica.game-state.spatial :as spatial]
            [gnostica.game-state.turn :as turn]))

(def min-players constants/min-players)
(def max-players constants/max-players)
(def starting-hand-size constants/starting-hand-size)
(def pieces-per-size-in-stash constants/pieces-per-size-in-stash)
(def initial-phase constants/initial-phase)
(def finished-phase constants/finished-phase)
(def default-target-score constants/default-target-score)
(def allowed-target-scores constants/allowed-target-scores)
(def required-player-fields constants/required-player-fields)
(def required-card-fields constants/required-card-fields)

(def success result/success)
(def failure result/failure)

(def valid-player-count? players/valid-player-count?)
(def current-player players/current-player)
(def append-history players/append-history)
(def update-player players/update-player)
(def duplicate-values collections/duplicate-values)

(def validate-board-pieces board-pieces/validate-board-pieces)
(def with-board-pieces-result board-pieces/with-board-pieces-result)
(def with-board-pieces board-pieces/with-board-pieces)

(def board-cell-by-index spatial/board-cell-by-index)
(def board-cell-at spatial/board-cell-at)
(def piece-by-id piece-state/piece-by-id)
(def pieces-at-board-index piece-state/pieces-at-board-index)
(def piece-coordinate piece-state/piece-coordinate)
(def player-hand-card hands/player-hand-card)
(def remove-card-from-hand hands/remove-card-from-hand)
(def remove-cards-from-hand hands/remove-cards-from-hand)
(def discard-card hands/discard-card)
(def discard-cards hands/discard-cards)
(def append-cards-to-hand hands/append-cards-to-hand)

(def refresh-draw-pile deck/refresh-draw-pile)

(def apply-starting-bids setup/apply-starting-bids)
(def resolve-starting-bid-rounds setup/resolve-starting-bid-rounds)
(def create-game setup/create-game)

(def stash-count piece-state/stash-count)
(def update-stash-count piece-state/update-stash-count)
(def increment-stash piece-state/increment-stash)
(def decrement-stash piece-state/decrement-stash)
(def small-stash-count piece-state/small-stash-count)
(def decrement-small-stash piece-state/decrement-small-stash)
(def next-piece-id piece-state/next-piece-id)
(def source-summary sources/source-summary)
(def current-player-id? players/current-player-id?)
(def apply-source-cost sources/apply-source-cost)
(def wasteland-target spatial/wasteland-target)
(def wasteland-target? spatial/wasteland-target?)
(def legal-piece-coordinate? spatial/legal-piece-coordinate?)
(def void-pieces piece-state/void-pieces)
(def enemy-pieces-at-coordinate piece-state/enemy-pieces-at-coordinate)
(def move-wasteland-pieces-to-board-index piece-state/move-wasteland-pieces-to-board-index)
(def move-board-index-pieces-to-wasteland piece-state/move-board-index-pieces-to-wasteland)
(def pieces-at-coordinate piece-state/pieces-at-coordinate)
(def target-piece-territory-cell piece-state/target-piece-territory-cell)
(def next-board-index spatial/next-board-index)
(def remove-piece-by-id piece-state/remove-piece-by-id)
(def replace-piece-by-id piece-state/replace-piece-by-id)
(def replace-piece piece-state/replace-piece)
(def player-pieces piece-state/player-pieces)
(def move-territory-cell spatial/move-territory-cell)

(def player-score score/player-score)
(def scores score/scores)
(def with-current-scores score/with-current-scores)

(def target-score turn/target-score)
(def finished? turn/finished?)
(def player-eliminated? turn/player-eliminated?)
(def turn-action-unavailable-result turn/turn-action-unavailable-result)
(def active-challenge-player-id turn/active-challenge-player-id)
(def can-announce-challenge? turn/can-announce-challenge?)
(def challenge-unavailable-reason turn/challenge-unavailable-reason)
(def can-end-turn? turn/can-end-turn?)
(def announce-challenge turn/announce-challenge)
(def return-pieces-to-stash turn/return-pieces-to-stash)
(def return-void-pieces-to-stash turn/return-void-pieces-to-stash)
(def advance-turn turn/advance-turn)
(def end-turn turn/end-turn)
