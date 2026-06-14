(ns gnostica.game-state.players
  (:require [gnostica.game-state.constants :as constants]))

(defn valid-player-count? [player-specs]
  (<= constants/min-players (count player-specs) constants/max-players))

(defn current-player [state]
  (get-in state [:players-by-id (get-in state [:turn :current-player-id])]))

(defn append-history [state event]
  (update state :history conj event))

(defn rebuild-players-by-id [players]
  (into {} (map (juxt :id identity)) players))

(defn update-player [state player-id f & args]
  (let [players (mapv (fn [player]
                        (if (= player-id (:id player))
                          (apply f player args)
                          player))
                      (:players state))]
    (assoc state
           :players players
           :players-by-id (rebuild-players-by-id players))))

(defn initial-turn [players]
  (let [order (mapv :id players)]
    {:order order
     :current-player-index 0
     :current-player-id (first order)
     :round 1}))

(defn current-player-id? [state player-id]
  (= player-id (get-in state [:turn :current-player-id])))
