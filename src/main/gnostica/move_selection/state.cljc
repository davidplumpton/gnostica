(ns gnostica.move-selection.state
  (:require [gnostica.board-layout :as layout]
            [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]
            [gnostica.game-state.spatial :as spatial]
            [gnostica.move-selection.staging :as staging]
            [gnostica.pieces :as pieces]))

(defn game [db]
  (:game db))

(defn board [db]
  (get-in db [:game :board] []))

(defn board-cell-by-index [db index]
  (layout/cell-by-index (board db) index))

(defn board-pieces [db]
  (get-in db [:game :pieces :on-board] []))

(defn selected-board-index [db]
  (:selected-board-index db))

(defn current-player [db]
  (some-> (game db) game-state/current-player))

(defn current-player-id [db]
  (get-in db [:game :turn :current-player-id]))

(defn current-player-hand [db]
  (vec (:hand (current-player db))))

(defn discard-pile [db]
  (vec (get-in db [:game :discard-pile] [])))

(defn current-player-pieces [db]
  (let [player-id (current-player-id db)]
    (->> (board-pieces db)
         (filter #(= player-id (:player-id %)))
         vec)))

(defn game-turn-key [state]
  (select-keys (:turn state)
               [:current-player-id :current-player-index :round]))

(defn turn-action-consumed? [db]
  (let [record (:turn-action db)]
    (boolean
     (and (true? (:consumed? record))
          (= (:turn-key record)
             (game-turn-key (game db)))))))

(defn piece-coordinate [db piece]
  (if-let [{:keys [row col]} (:space piece)]
    [row col]
    (when-let [{:keys [row col]} (board-cell-by-index db (:space-index piece))]
      [row col])))

(defn pieces-at-coordinate [db row col]
  (filterv #(= [row col] (piece-coordinate db %))
           (board-pieces db)))

(defn minion-target-coordinate [db piece]
  (when-let [coordinate (piece-coordinate db piece)]
    (when-let [{:keys [row col]} (game-state/target-coordinate coordinate
                                                               (:orientation piece))]
      [row col])))

(defn targetable-piece? [db minion piece]
  (let [target-coordinate (piece-coordinate db piece)]
    (and minion
         piece
         target-coordinate
         (or (= (:id minion) (:id piece))
             (spatial/same-coordinate? target-coordinate
                                       (minion-target-coordinate db minion))))))

(defn targetable-territory? [db minion cell]
  (and minion
       cell
       (spatial/same-coordinate? [(:row cell) (:col cell)]
                                 (minion-target-coordinate db minion))))

(defn current-player-piece? [db piece]
  (= (current-player-id db) (:player-id piece)))

(defn piece-by-id [db piece-id]
  (some #(when (= piece-id (:id %)) %)
        (board-pieces db)))

(defn current-player-piece-by-id [db piece-id]
  (let [piece (piece-by-id db piece-id)]
    (when (current-player-piece? db piece)
      piece)))

(defn current-player-pieces-on-space [db space-index]
  (->> (pieces/pieces-for-space (board-pieces db) space-index)
       (filter #(current-player-piece? db %))
       vec))

(defn current-player-territory-source-indexes [db]
  (->> (current-player-pieces db)
       (keep :space-index)
       set))

(defn current-player-territory-source-options [db]
  (let [owned-spaces (current-player-territory-source-indexes db)]
    (filterv #(contains? owned-spaces (:index %))
             (board db))))

(defn hand-card-by-id [db card-id]
  (some #(when (= card-id (:id %)) %)
        (current-player-hand db)))

(defn gameplay-move-source? [source]
  (contains? #{:activate-territory :play-hand-card} source))

(defn source-hand-card-id [source-id params]
  (when (= :play-hand-card source-id)
    (:hand-card-id params)))

(defn source-board-card [db params]
  (:card (board-cell-by-index db (:source-board-index params))))

(defn source-card [db source-id params]
  (case source-id
    :activate-territory (source-board-card db params)
    :play-hand-card (hand-card-by-id db (:hand-card-id params))
    nil))

(defn source-command [source params]
  (case source
    :activate-territory
    {:kind :territory
     :board-index (:source-board-index params)
     :piece-id (:piece-id params)}

    :play-hand-card
    {:kind :hand-card
     :card-id (:hand-card-id params)
     :piece-id (:piece-id params)}))

(defn one-point-card-options-for [db source-id params]
  (let [source-card-id (source-hand-card-id source-id params)]
    (->> (current-player-hand db)
         (filter #(and (cards/one-point-card? %)
                       (not= source-card-id (:id %))))
         vec)))

(defn one-point-card-by-id [db source-id params card-id]
  (some #(when (= card-id (:id %)) %)
        (one-point-card-options-for db source-id params)))

(defn valid-board-index? [db index]
  (some? (board-cell-by-index db index)))

(defn target-board-cell [db params]
  (board-cell-by-index db (:target-board-index params)))

(defn move-selection [db]
  (:move-selection db (staging/empty-selection)))

(defn move-source [db]
  (:source (move-selection db)))

(defn move-params [db]
  (:params (move-selection db)))
