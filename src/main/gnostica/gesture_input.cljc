(ns gnostica.gesture-input)

(def mime-type "application/gnostica-gesture")

(defn gesture-input-string [input]
  (pr-str input))

(defn- id-value [value id-key]
  (if (map? value)
    (get value id-key)
    value))

(defn territory-source-input [cell-or-index]
  {:source {:kind :territory
            :board-index (id-value cell-or-index :index)}})

(defn territory-target [cell-or-index]
  {:kind :territory
   :board-index (id-value cell-or-index :index)})

(defn hand-card-source-input [card-or-id]
  {:source {:kind :hand-card
            :card-id (id-value card-or-id :id)}})

(defn draw-pile-source-input []
  {:source {:kind :draw-pile}})

(defn piece-source-input [piece-or-id]
  {:source {:kind :piece
            :piece-id (id-value piece-or-id :id)}})

(defn piece-target [piece-or-id]
  {:kind :piece
   :piece-id (id-value piece-or-id :id)})

(defn stash-piece-source-input [player-or-id]
  {:source {:kind :stash-piece
            :player-id (id-value player-or-id :id)
            :size :small}})

(defn wasteland-target
  ([space]
   {:kind :wasteland
    :row (:row space)
    :col (:col space)})
  ([row col]
   {:kind :wasteland
    :row row
    :col col}))

(defn replacement-card-choice-input [card-or-id descriptor]
  {:preserve-selection? true
   :fields (cond-> {:replacement-card-id (id-value card-or-id :id)}
             (:replacement-card-source descriptor)
             (assoc :replacement-card-source (:replacement-card-source descriptor)))})

(defn draggable-card-source? [descriptor]
  (not (contains? #{:discard :territory-card :replacement-card}
                  (:role descriptor))))

(defn hand-card-drag-input [card descriptor]
  (cond
    (and (= :replacement-card (:role descriptor))
         (:enabled? descriptor))
    (replacement-card-choice-input card descriptor)

    (draggable-card-source? descriptor)
    (hand-card-source-input card)

    :else
    nil))

(defn discard-card-drag-input [card descriptor]
  (when (and card
             (= :replacement-card (:role descriptor))
             (:enabled? descriptor))
    (replacement-card-choice-input card descriptor)))

(defn territory-drag-input [cell descriptor]
  (if (and (:active? descriptor)
           (= :target (:role descriptor)))
    {:preserve-selection? true
     :fields {:target-board-index (:index cell)}}
    (territory-source-input cell)))

(defn piece-drag-input [piece descriptor]
  (case (:role descriptor)
    :minion
    (when (:enabled? descriptor)
      {:preserve-selection? true
       :fields {:piece-id (:id piece)}})

    :target
    (when (:enabled? descriptor)
      {:preserve-selection? true
       :fields {:target-piece-id (:id piece)}})

    (when (:source-enabled? descriptor)
      (piece-source-input piece))))
