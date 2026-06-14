(ns gnostica.game-state.pieces
  (:require [gnostica.game-state.constants :as constants]
            [gnostica.game-state.players :as players]
            [gnostica.game-state.spatial :as spatial]
            [gnostica.pieces :as piece-registry]))

(defn initial-stash []
  (into {}
        (map (fn [size]
               [size constants/pieces-per-size-in-stash]))
        (keys piece-registry/piece-sizes)))

(defn initial-stashes [players]
  (into {}
        (map (fn [player]
               [(:id player) (:stash player)]))
        players))

(defn piece-by-id [state piece-id]
  (some (fn [piece]
          (when (= piece-id (:id piece))
            piece))
        (get-in state [:pieces :on-board])))

(defn pieces-at-board-index [state board-index]
  (filterv #(= board-index (:space-index %))
           (get-in state [:pieces :on-board])))

(defn piece-coordinate [state piece]
  (if-let [{:keys [row col]} (:space piece)]
    [row col]
    (when-let [{:keys [row col]} (spatial/board-cell-by-index
                                  state
                                  (:space-index piece))]
      [row col])))

(defn stash-count [state player-id size]
  (or (get-in state [:players-by-id player-id :stash size])
      (get-in state [:pieces :stashes player-id size])
      0))

(defn update-stash-count [state player-id size f]
  (-> state
      (players/update-player player-id update-in [:stash size] f)
      (update-in [:pieces :stashes player-id size] f)))

(defn increment-stash [state player-id size]
  (update-stash-count state player-id size inc))

(defn decrement-stash [state player-id size]
  (update-stash-count state player-id size dec))

(defn small-stash-count [state player-id]
  (stash-count state player-id :small))

(defn decrement-small-stash [state player-id]
  (decrement-stash state player-id :small))

(defn next-piece-id [state player-id size]
  (let [prefix (str (name player-id) "-" (name size) "-")
        used-ids (set (map :id (get-in state [:pieces :on-board])))]
    (->> (iterate inc 1)
         (map #(keyword (str prefix %)))
         (remove used-ids)
         first)))

(defn void-pieces [state]
  (->> (get-in state [:pieces :on-board])
       (filterv (fn [piece]
                  (let [coordinate (piece-coordinate state piece)]
                    (or (nil? coordinate)
                        (not (spatial/legal-piece-coordinate?
                              state
                              coordinate))))))))

(defn enemy-pieces-at-coordinate [state player-id row col]
  (->> (get-in state [:pieces :on-board])
       (filter (fn [piece]
                 (and (not= player-id (:player-id piece))
                      (= [row col] (piece-coordinate state piece)))))
       vec))

(defn move-wasteland-pieces-to-board-index [state row col board-index]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (mapv (fn [piece]
                       (if (= [row col] (piece-coordinate state piece))
                         (-> piece
                             (dissoc :space)
                             (assoc :space-index board-index))
                         piece))
                     board-pieces))))

(defn move-board-index-pieces-to-wasteland [state board-index row col]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (mapv (fn [piece]
                       (if (= board-index (:space-index piece))
                         (-> piece
                             (dissoc :space-index)
                             (assoc :space {:kind :wasteland
                                            :row row
                                            :col col}))
                         piece))
                     board-pieces))))

(defn pieces-at-coordinate [state row col]
  (filterv #(= [row col] (piece-coordinate state %))
           (get-in state [:pieces :on-board])))

(defn target-piece-territory-cell [state piece]
  (if-let [space-index (:space-index piece)]
    (spatial/board-cell-by-index state space-index)
    (when-let [{:keys [row col]} (:space piece)]
      (spatial/board-cell-at state row col))))

(defn remove-piece-by-id [state piece-id]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (vec (remove #(= piece-id (:id %)) board-pieces)))))

(defn replace-piece-by-id [state piece-id piece]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (mapv (fn [board-piece]
                       (if (= piece-id (:id board-piece))
                         piece
                         board-piece))
                     board-pieces))))

(defn replace-piece [state piece]
  (replace-piece-by-id state (:id piece) piece))

(defn player-pieces [state player-id]
  (filterv #(= player-id (:player-id %))
           (get-in state [:pieces :on-board])))
