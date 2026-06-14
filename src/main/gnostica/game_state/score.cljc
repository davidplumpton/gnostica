(ns gnostica.game-state.score
  (:require [gnostica.cards :as cards]
            [gnostica.game-state.pieces :as piece-state]
            [gnostica.game-state.players :as players]))

(defn- scoreable-state? [state]
  (and (map? state)
       (sequential? (:players state))
       (sequential? (:board state))
       (map? (:pieces state))))

(defn- controlled-territory-player-id [pieces]
  (let [player-ids (set (map :player-id pieces))]
    (when (and (seq pieces)
               (= 1 (count player-ids)))
      (first player-ids))))

(defn player-score [state player-id]
  (reduce
   (fn [score cell]
     (let [pieces (piece-state/pieces-at-board-index state (:index cell))]
       (if (= player-id (controlled-territory-player-id pieces))
         (+ score (or (cards/card-point-value (:card cell)) 0))
         score)))
   0
   (:board state)))

(defn scores [state]
  (into {}
        (map (fn [{:keys [id eliminated?]}]
               [id (if eliminated?
                     0
                     (player-score state id))]))
        (:players state)))

(defn with-current-scores [state]
  (if-not (scoreable-state? state)
    state
    (let [scores-by-player-id (scores state)
          players (mapv (fn [player]
                          (assoc player
                                 :score (get scores-by-player-id
                                             (:id player)
                                             0)))
                        (:players state))]
      (assoc state
             :players players
             :players-by-id (players/rebuild-players-by-id players)))))
