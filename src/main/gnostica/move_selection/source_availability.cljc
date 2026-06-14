(ns gnostica.move-selection.source-availability
  (:require [gnostica.game-state :as game-state]
            [gnostica.move-selection.state :as selection]))

(defn small-stash-count [db]
  (or (get-in (selection/current-player db) [:stash :small])
      (get-in db [:game :pieces :stashes (selection/current-player-id db) :small])
      0))

(defn- max-potential-draw-count [db]
  (let [discard-count (count (selection/current-player-hand db))
        hand-slots game-state/starting-hand-size
        available-cards (+ (count (get-in db [:game :draw-pile] []))
                           (count (get-in db [:game :discard-pile] []))
                           discard-count)]
    (max 0 (min hand-slots available-cards))))

(defn source-unavailable-reason [db source-id]
  (let [player (selection/current-player db)
        owned-pieces (selection/current-player-pieces db)
        hand (selection/current-player-hand db)
        max-draw (max-potential-draw-count db)
        turn-action-result (when player
                             (game-state/turn-action-unavailable-result
                              (selection/game db)
                              (:id player)
                              source-id))]
    (cond
      (nil? player)
      "No current player is available."

      (game-state/finished? (selection/game db))
      "The game is finished."

      (selection/turn-action-consumed? db)
      "The current player has already taken a turn action."

      turn-action-result
      (get-in turn-action-result [:error :message])

      (= :activate-territory source-id)
      (cond
        (empty? owned-pieces)
        "The current player has no pieces on the board."

        (empty? (selection/current-player-territory-source-indexes db))
        "The current player has no pieces on a territory.")

      (= :play-hand-card source-id)
      (cond
        (empty? hand) "The current player has no hand cards."
        (empty? owned-pieces) "The current player needs a piece on the board.")

      (= :draw-cards source-id)
      (cond
        (empty? owned-pieces) "The current player has no pieces on the board."
        (zero? max-draw) "The current player cannot draw more cards.")

      (= :orient-piece source-id)
      (when-not (seq owned-pieces)
        "The current player has no pieces to orient.")

      (= :place-initial-small source-id)
      (cond
        (seq owned-pieces) "The current player already has pieces on the board."
        (not (pos? (small-stash-count db)))
        "The current player has no small pieces in stash.")

      :else
      "Unknown move source.")))
