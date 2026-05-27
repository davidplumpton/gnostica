(ns gnostica.game-state
  (:require [gnostica.game-state.composite :as composite]
            [gnostica.game-state.core :as core]
            [gnostica.game-state.cup :as cup]
            [gnostica.game-state.disc :as disc]
            [gnostica.game-state.draw :as draw]
            [gnostica.game-state.major :as major]
            [gnostica.game-state.manipulation :as manipulation]
            [gnostica.game-state.placement :as placement]
            [gnostica.game-state.rod :as rod]
            [gnostica.game-state.sword :as sword]
            [gnostica.game-state.world :as world]))

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

(defn apply-starting-bids [state command]
  (core/apply-starting-bids state command))

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

(defn apply-moon-move [state command]
  (sword/apply-moon-move state command))

(defn apply-draw-move [state command]
  (draw/apply-draw-move state command))

(defn apply-fool-move [state command]
  (draw/apply-fool-move state command))

(defn apply-high-priestess-move [state command]
  (draw/apply-high-priestess-move state command))

(defn apply-judgement-move [state command]
  (draw/apply-judgement-move state command))

(defn target-coordinate [coordinate orientation]
  (manipulation/target-coordinate coordinate orientation))

(defn apply-hierophant-move [state command]
  (manipulation/apply-hierophant-move state command))

(defn apply-hermit-move [state command]
  (manipulation/apply-hermit-move state command))

(defn apply-devil-move [state command]
  (manipulation/apply-devil-move state command))

(defn apply-empress-move [state command]
  (composite/apply-empress-move state command))

(defn apply-emperor-move [state command]
  (composite/apply-emperor-move state command))

(defn apply-lovers-move [state command]
  (composite/apply-lovers-move state command))

(defn apply-chariot-move [state command]
  (composite/apply-chariot-move state command))

(defn apply-hanged-man-move [state command]
  (composite/apply-hanged-man-move state command))

(defn apply-temperance-move [state command]
  (composite/apply-temperance-move state command))

(defn world-major-territories [state]
  (world/world-major-territories state))

(defn apply-world-move [state command]
  (world/apply-world-move state command))

(defn resolve-major-source [state command]
  (major/resolve-major-source state command))

(defn charge-major-source-once [state source-result]
  (major/charge-source-once state source-result))

(defn major-paid-source-opts [source-result]
  (major/paid-source-opts source-result))

(defn major-action-source [source-result piece-id]
  (major/action-source source-result piece-id))

(defn apply-major-sequence [state command spec]
  (major/apply-major-sequence state command spec))

(defn apply-orient-move [state command]
  (placement/apply-orient-move state command))

(defn apply-initial-placement [state command]
  (placement/apply-initial-placement state command))
