(ns gnostica.game-state
  (:require [gnostica.game-state.core :as core]
            [gnostica.game-state.cup :as cup]
            [gnostica.game-state.disc :as disc]
            [gnostica.game-state.draw :as draw]
            [gnostica.game-state.placement :as placement]
            [gnostica.game-state.rod :as rod]
            [gnostica.game-state.sword :as sword]))

(def min-players core/min-players)
(def max-players core/max-players)
(def starting-hand-size core/starting-hand-size)
(def pieces-per-size-in-stash core/pieces-per-size-in-stash)
(def initial-phase core/initial-phase)
(def finished-phase core/finished-phase)
(def default-target-score core/default-target-score)
(def allowed-target-scores core/allowed-target-scores)
(def required-player-fields core/required-player-fields)
(def required-card-fields core/required-card-fields)

(defn success
  ([state]
   (core/success state))
  ([state events]
   (core/success state events)))

(defn failure [code message data]
  (core/failure code message data))

(defn valid-player-count? [player-specs]
  (core/valid-player-count? player-specs))

(defn current-player [state]
  (core/current-player state))

(defn player-score [state player-id]
  (core/player-score state player-id))

(defn scores [state]
  (core/scores state))

(defn with-current-scores [state]
  (core/with-current-scores state))

(defn target-score [state]
  (core/target-score state))

(defn finished? [state]
  (core/finished? state))

(defn active-challenge-player-id [state]
  (core/active-challenge-player-id state))

(defn can-announce-challenge? [state player-id]
  (core/can-announce-challenge? state player-id))

(defn challenge-unavailable-reason [state player-id]
  (core/challenge-unavailable-reason state player-id))

(defn announce-challenge [state command]
  (core/announce-challenge state command))

(defn append-history [state event]
  (core/append-history state event))

(defn with-board-pieces [state board-pieces]
  (core/with-board-pieces state board-pieces))

(defn create-game
  ([player-specs]
   (core/create-game player-specs))
  ([player-specs opts]
   (core/create-game player-specs opts)))

(defn advance-turn [state]
  (core/advance-turn state))

(defn end-turn [state command]
  (core/end-turn state command))

(def cup-territory-card-sources cup/cup-territory-card-sources)
(defn apply-cup-move [state command]
  (cup/apply-cup-move state command))

(def rod-modes rod/rod-modes)
(def rod-direction-offsets rod/rod-direction-offsets)
(defn rod-destination-coordinate [coordinate direction distance]
  (rod/rod-destination-coordinate coordinate direction distance))

(defn resolve-rod-command [state command]
  (rod/resolve-rod-command state command))

(defn apply-rod-move [state command]
  (rod/apply-rod-move state command))

(def disc-territory-card-sources disc/disc-territory-card-sources)
(def disc-direction-offsets disc/disc-direction-offsets)
(defn disc-target-coordinate [coordinate orientation]
  (disc/disc-target-coordinate coordinate orientation))

(defn resolve-disc-command [state command]
  (disc/resolve-disc-command state command))

(defn apply-disc-move [state command]
  (disc/apply-disc-move state command))

(defn apply-sun-move [state command]
  (disc/apply-sun-move state command))

(def sword-territory-card-sources sword/sword-territory-card-sources)
(def sword-direction-offsets sword/sword-direction-offsets)
(defn sword-target-coordinate [coordinate orientation]
  (sword/sword-target-coordinate coordinate orientation))

(defn resolve-sword-command [state command]
  (sword/resolve-sword-command state command))

(defn apply-sword-move [state command]
  (sword/apply-sword-move state command))

(defn apply-draw-move [state command]
  (draw/apply-draw-move state command))

(defn apply-orient-move [state command]
  (placement/apply-orient-move state command))

(defn apply-initial-placement [state command]
  (placement/apply-initial-placement state command))
