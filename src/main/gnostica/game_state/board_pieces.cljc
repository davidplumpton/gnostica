(ns gnostica.game-state.board-pieces
  (:require [gnostica.board-layout :as board-layout]
            [gnostica.game-state.collections :as collections]
            [gnostica.game-state.constants :as constants]
            [gnostica.game-state.pieces :as piece-state]
            [gnostica.game-state.players :as players]
            [gnostica.game-state.result :as result]
            [gnostica.pieces :as pieces]))

(defn- board-piece-counts [board-pieces]
  (reduce (fn [counts {:keys [player-id size]}]
            (if (and player-id size)
              (update-in counts [player-id size] (fnil inc 0))
              counts))
          {}
          board-pieces))

(defn- player-ids [state]
  (mapv :id (:players state)))

(defn- player-id-set [state]
  (set (player-ids state)))

(defn- board-index-set [state]
  (set (map :index (:board state))))

(defn- sorted-board-indexes [state]
  (vec (sort (board-index-set state))))

(defn- wasteland-space-key [space]
  (select-keys space [:kind :row :col]))

(defn- wasteland-space-set [state]
  (->> (board-layout/wasteland-spaces (:board state))
       (map wasteland-space-key)
       set))

(defn- sorted-wasteland-spaces [state]
  (->> (board-layout/wasteland-spaces (:board state))
       (map wasteland-space-key)
       (sort-by (juxt :row :col))
       vec))

(defn- invalid-board-piece-shape-errors [board-pieces]
  (->> board-pieces
       (map-indexed
        (fn [index piece]
          (when-not (map? piece)
            {:code :invalid-board-piece
             :message "Seeded board pieces must be maps."
             :data {:index index
                    :piece piece}})))
       (remove nil?)
       vec))

(defn- map-board-pieces [board-pieces]
  (filterv map? board-pieces))

(defn- invalid-board-piece-field-errors [board-pieces]
  (->> board-pieces
       (mapcat
        (fn [{:keys [id] :as piece}]
          (cond-> []
            (not (keyword? id))
            (conj {:code :invalid-piece-id
                   :message "Seeded board pieces must have keyword ids."
                   :data {:piece-id id
                          :piece piece}}))))
       vec))

(defn- duplicate-board-piece-id-errors [board-pieces]
  (let [duplicates (collections/duplicate-values (keep :id board-pieces))]
    (when (seq duplicates)
      [{:code :duplicate-active-piece-ids
        :message "Active pieces must have unique ids."
        :data {:piece-ids duplicates}}])))

(defn- board-piece-owner-errors [state board-pieces]
  (let [ids (player-id-set state)]
    (->> board-pieces
         (keep (fn [{:keys [id player-id]}]
                 (when-not (contains? ids player-id)
                   {:code :unknown-piece-player
                    :message "Pieces on the board must belong to a player in the game."
                    :data {:piece-id id
                           :player-id player-id
                           :player-ids (player-ids state)}})))
         vec)))

(defn- board-piece-location-errors [board-pieces]
  (->> board-pieces
       (keep (fn [{:keys [id] :as piece}]
               (let [has-space-index? (contains? piece :space-index)
                     has-space? (contains? piece :space)]
                 (cond
                   (and has-space-index? has-space?)
                   {:code :ambiguous-piece-location
                    :message "Pieces must use either :space-index or :space, not both."
                    :data {:piece-id id
                           :space-index (:space-index piece)
                           :space (:space piece)}}

                   (not (or has-space-index? has-space?))
                   {:code :missing-piece-location
                    :message "Pieces must include exactly one location field: :space-index or :space."
                    :data {:piece-id id}}

                   (and has-space?
                        (not (and (map? (:space piece))
                                  (= :wasteland (get-in piece [:space :kind])))))
                   {:code :invalid-piece-space
                    :message "Piece :space must describe a wasteland coordinate."
                    :data {:piece-id id
                           :space (:space piece)}}))))
       vec))

(defn- board-piece-space-index-errors [state board-pieces]
  (let [board-indexes (board-index-set state)
        sorted-indexes (delay (sorted-board-indexes state))]
    (->> board-pieces
         (keep (fn [{:keys [id space-index] :as piece}]
                 (when (and (contains? piece :space-index)
                            (not (contains? board-indexes space-index)))
                   {:code :piece-space-missing
                    :message "Pieces with a space index must reference an existing board cell."
                    :data {:piece-id id
                           :space-index space-index
                           :board-indexes @sorted-indexes}})))
         vec)))

(defn- board-piece-wasteland-errors [state board-pieces]
  (let [valid-spaces (wasteland-space-set state)
        sorted-spaces (delay (sorted-wasteland-spaces state))]
    (->> board-pieces
         (keep (fn [{:keys [id space] :as piece}]
                 (when (and (contains? piece :space)
                            (map? space)
                            (= :wasteland (:kind space))
                            (not (contains? valid-spaces (wasteland-space-key space))))
                   {:code :piece-wasteland-missing
                    :message "Pieces in wasteland space must reference a current wasteland coordinate."
                    :data {:piece-id id
                           :space (wasteland-space-key space)
                           :wasteland-spaces @sorted-spaces}})))
         vec)))

(defn- board-piece-stash-overflow-errors [state board-pieces]
  (let [valid-player-ids (player-id-set state)
        piece-counts (board-piece-counts board-pieces)]
    (vec
     (for [[player-id size-counts] piece-counts
           [size active-piece-count] size-counts
           :when (and (contains? valid-player-ids player-id)
                      (contains? pieces/piece-sizes size)
                      (< constants/pieces-per-size-in-stash active-piece-count))]
       {:code :piece-count-exceeds-stash
        :message "Seeded board pieces cannot use more pieces than a player's starting stash."
        :data {:player-id player-id
               :size size
               :active-piece-count active-piece-count
               :available-count constants/pieces-per-size-in-stash}}))))

(defn- invalid-board-piece-collection-result [board-pieces]
  (when-not (or (nil? board-pieces)
                (sequential? board-pieces))
    (result/failure
     :invalid-board-pieces
     "Board pieces cannot be seeded into game state."
     {:errors [{:code :invalid-board-piece-collection
                :message "Board pieces must be a sequential collection."
                :data {:board-pieces board-pieces}}]})))

(defn validate-board-pieces
  "Return a structured error result when seeded pieces would create impossible state."
  [state board-pieces]
  (or (invalid-board-piece-collection-result board-pieces)
      (let [board-pieces (vec (or board-pieces []))
            map-pieces (map-board-pieces board-pieces)
            errors (vec (concat
                         (invalid-board-piece-shape-errors board-pieces)
                         (invalid-board-piece-field-errors map-pieces)
                         (duplicate-board-piece-id-errors map-pieces)
                         (board-piece-owner-errors state map-pieces)
                         (board-piece-location-errors map-pieces)
                         (board-piece-space-index-errors state map-pieces)
                         (board-piece-wasteland-errors state map-pieces)
                         (board-piece-stash-overflow-errors state map-pieces)))]
        (when (seq errors)
          (result/failure
           :invalid-board-pieces
           "Board pieces cannot be seeded into game state."
           {:errors errors})))))

(defn- stash-after-board-pieces [piece-counts player-id]
  (into {}
        (map (fn [size]
               [size (- constants/pieces-per-size-in-stash
                        (get-in piece-counts [player-id size] 0))]))
        (keys pieces/piece-sizes)))

(defn- install-board-pieces [state board-pieces]
  (let [board-pieces (vec board-pieces)
        piece-counts (board-piece-counts board-pieces)
        players (mapv (fn [player]
                        (assoc player
                               :stash (stash-after-board-pieces piece-counts
                                                                (:id player))))
                      (:players state))]
    (assoc state
           :players players
           :players-by-id (players/rebuild-players-by-id players)
           :pieces (assoc (:pieces state)
                          :on-board board-pieces
                          :stashes (piece-state/initial-stashes players)))))

(defn with-board-pieces-result
  "Replace active pieces with a structured success/error result."
  [state board-pieces]
  (if-let [invalid-result (validate-board-pieces state board-pieces)]
    invalid-result
    {:ok? true
     :state (install-board-pieces state (vec (or board-pieces [])))
     :events []}))

(defn with-board-pieces
  "Replace active pieces and rebuild both stash mirrors from the starting stash size."
  [state board-pieces]
  (let [result (with-board-pieces-result state board-pieces)]
    (if (:ok? result)
      (:state result)
      (throw (ex-info "Board pieces failed validation before being seeded."
                      (:error result))))))
