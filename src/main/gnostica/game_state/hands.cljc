(ns gnostica.game-state.hands
  (:require [gnostica.game-state.players :as players]))

(defn player-hand-card [state player-id card-id]
  (some (fn [card]
          (when (= card-id (:id card))
            card))
        (get-in state [:players-by-id player-id :hand])))

(defn remove-card-from-hand [state player-id card-id]
  (players/update-player state player-id update :hand
                         (fn [hand]
                           (vec (remove #(= card-id (:id %)) hand)))))

(defn remove-cards-from-hand [state player-id card-ids]
  (let [card-id-set (set card-ids)]
    (players/update-player state player-id update :hand
                           (fn [hand]
                             (vec (remove #(contains? card-id-set (:id %))
                                          hand))))))

(defn discard-card [state card]
  (update state :discard-pile conj card))

(defn discard-cards [state cards]
  (update state :discard-pile into (vec cards)))

(defn append-cards-to-hand [state player-id cards]
  (players/update-player state player-id update :hand
                         (fn [hand]
                           (vec (concat hand cards)))))
