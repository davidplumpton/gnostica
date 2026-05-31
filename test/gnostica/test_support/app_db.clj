(ns gnostica.test-support.app-db
  (:require [gnostica.app-state :as app-state]))

(defn piece-by-id [db piece-id]
  (some #(when (= piece-id (:id %)) %)
        (app-state/board-pieces db)))

(defn board-cell-by-index [db board-index]
  (some #(when (= board-index (:index %)) %)
        (app-state/board db)))

(defn territory-target [targets board-index]
  (some #(when (= board-index (:board-index %)) %)
        (:territories targets)))

(defn piece-target [targets piece-id]
  (some #(when (= piece-id (:piece-id %)) %)
        (:pieces targets)))

(defn hand-card-target [targets card-id]
  (some #(when (= card-id (:card-id %)) %)
        (:hand-cards targets)))

(defn discard-card-target [targets card-id]
  (some #(when (= card-id (:card-id %)) %)
        (:discard-cards targets)))

(defn- with-game-players [db players]
  (assoc-in (assoc-in db [:game :players] players)
            [:game :players-by-id]
            (into {} (map (juxt :id identity) players))))

(defn remove-board-cell [db board-index]
  (let [cell (board-cell-by-index db board-index)]
    (cond-> (update-in db [:game :board]
                       (fn [cells]
                         (vec (remove #(= board-index (:index %)) cells))))
      cell
      (update-in [:game :discard-pile] conj (:card cell)))))

(defn replace-game-player-hand [db player-id hand]
  (with-game-players db
    (mapv (fn [player]
            (if (= player-id (:id player))
              (assoc player :hand (vec hand))
              player))
          (get-in db [:game :players]))))

(defn mark-game-player-eliminated [db player-id]
  (with-game-players db
    (mapv (fn [player]
            (if (= player-id (:id player))
              (assoc player :eliminated? true)
              player))
          (get-in db [:game :players]))))
