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

(defn turn-action-unavailable-result [state player-id action]
  (core/turn-action-unavailable-result state player-id action))

(defn- command-turn-action [command default-action]
  (case (get-in command [:source :kind])
    :hand-card :play-hand-card
    :territory :activate-territory
    default-action))

(defn- apply-turn-action [state command action f]
  (if-let [result (when (map? command)
                    (turn-action-unavailable-result state
                                                    (:player-id command)
                                                    action))]
    result
    (f state command)))

(defn- apply-source-turn-action [state command f]
  (apply-turn-action state
                     command
                     (command-turn-action command :activate-territory)
                     f))

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
  (apply-source-turn-action state command cup/apply-cup-move))

(def rod-modes rod/rod-modes)
(def rod-direction-offsets rod/rod-direction-offsets)
(defn rod-destination-coordinate [coordinate direction distance]
  (rod/rod-destination-coordinate coordinate direction distance))

(defn resolve-rod-command [state command]
  (rod/resolve-rod-command state command))

(defn apply-rod-move [state command]
  (apply-source-turn-action state command rod/apply-rod-move))

(def disc-territory-card-sources disc/disc-territory-card-sources)
(def disc-direction-offsets disc/disc-direction-offsets)
(defn disc-target-coordinate [coordinate orientation]
  (disc/disc-target-coordinate coordinate orientation))

(defn resolve-disc-command
  ([state command]
   (disc/resolve-disc-command state command))
  ([state command source-opts]
   (disc/resolve-disc-command state command source-opts)))

(defn apply-disc-move [state command]
  (apply-source-turn-action state command disc/apply-disc-move))

(defn apply-sun-move [state command]
  (apply-source-turn-action state command disc/apply-sun-move))

(def sword-territory-card-sources sword/sword-territory-card-sources)
(def sword-direction-offsets sword/sword-direction-offsets)
(defn sword-target-coordinate [coordinate orientation]
  (sword/sword-target-coordinate coordinate orientation))

(defn resolve-sword-command
  ([state command]
   (sword/resolve-sword-command state command))
  ([state command source-opts]
   (sword/resolve-sword-command state command source-opts)))

(defn apply-sword-move [state command]
  (apply-source-turn-action state command sword/apply-sword-move))

(defn apply-moon-move [state command]
  (apply-source-turn-action state command sword/apply-moon-move))

(defn apply-draw-move [state command]
  (apply-turn-action state command :draw-cards draw/apply-draw-move))

(defn apply-fool-move [state command]
  (apply-source-turn-action state command draw/apply-fool-move))

(defn apply-high-priestess-move [state command]
  (apply-source-turn-action state command draw/apply-high-priestess-move))

(defn apply-judgement-move [state command]
  (apply-source-turn-action state command draw/apply-judgement-move))

(defn target-coordinate [coordinate orientation]
  (manipulation/target-coordinate coordinate orientation))

(defn apply-hierophant-move [state command]
  (apply-source-turn-action state command manipulation/apply-hierophant-move))

(defn apply-hermit-move [state command]
  (apply-source-turn-action state command manipulation/apply-hermit-move))

(defn apply-devil-move [state command]
  (apply-source-turn-action state command manipulation/apply-devil-move))

(defn apply-empress-move [state command]
  (apply-source-turn-action state command composite/apply-empress-move))

(defn apply-emperor-move [state command]
  (apply-source-turn-action state command composite/apply-emperor-move))

(defn apply-lovers-move [state command]
  (apply-source-turn-action state command composite/apply-lovers-move))

(defn apply-chariot-move [state command]
  (apply-source-turn-action state command composite/apply-chariot-move))

(defn apply-hanged-man-move [state command]
  (apply-source-turn-action state command composite/apply-hanged-man-move))

(defn apply-temperance-move [state command]
  (apply-source-turn-action state command composite/apply-temperance-move))

(defn world-major-territories [state]
  (world/world-major-territories state))

(defn apply-world-move [state command]
  (apply-source-turn-action state command world/apply-world-move))

(defn resolve-major-source
  ([state command]
   (major/resolve-major-source state command))
  ([state command source-opts]
   (major/resolve-major-source state command source-opts)))

(defn charge-major-source-once [state source-result]
  (major/charge-source-once state source-result))

(defn major-paid-source-opts [source-result]
  (major/paid-source-opts source-result))

(defn major-action-source [source-result piece-id]
  (major/action-source source-result piece-id))

(defn apply-major-sequence [state command spec]
  (major/apply-major-sequence state command spec))

(defn apply-orient-move [state command]
  (apply-turn-action state command :orient-piece placement/apply-orient-move))

(defn apply-initial-placement [state command]
  (apply-turn-action state
                     command
                     :place-initial-small
                     placement/apply-initial-placement))
