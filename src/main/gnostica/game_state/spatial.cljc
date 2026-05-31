(ns gnostica.game-state.spatial)

(def cardinal-direction-offsets
  {:north [-1 0]
   :east [0 1]
   :south [1 0]
   :west [0 -1]})

(defn coordinate-map [coordinate]
  (cond
    (map? coordinate)
    (when (and (int? (:row coordinate))
               (int? (:col coordinate)))
      (select-keys coordinate [:row :col]))

    (sequential? coordinate)
    (let [[row col] coordinate]
      (when (and (int? row)
                 (int? col))
        {:row row
         :col col}))))

(defn same-coordinate? [left right]
  (= (coordinate-map left)
     (coordinate-map right)))

(defn offset-coordinate
  ([coordinate direction distance]
   (offset-coordinate coordinate direction distance cardinal-direction-offsets))
  ([coordinate direction distance direction-offsets]
   (when-let [[row-offset col-offset] (get direction-offsets direction)]
     (when-let [{:keys [row col]} (coordinate-map coordinate)]
       (when (int? distance)
         {:row (+ row (* row-offset distance))
          :col (+ col (* col-offset distance))})))))

(defn target-coordinate
  ([coordinate orientation]
   (target-coordinate coordinate orientation cardinal-direction-offsets))
  ([coordinate orientation direction-offsets]
   (when-let [{:keys [row col]} (coordinate-map coordinate)]
     (cond
       (= :up orientation)
       {:row row
        :col col}

       :else
       (when-let [[row-offset col-offset] (get direction-offsets orientation)]
         {:row (+ row row-offset)
          :col (+ col col-offset)})))))

(defn target-summary [target]
  (select-keys target [:kind :piece-id :board-index :row :col]))

(defn move-piece-to-space [piece piece-space orientation]
  (let [piece (cond-> piece
                orientation (assoc :orientation orientation))]
    (if-let [space-index (:space-index piece-space)]
      (-> piece
          (dissoc :space)
          (assoc :space-index space-index))
      (-> piece
          (dissoc :space-index)
          (assoc :space (:space piece-space))))))
