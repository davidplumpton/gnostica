(ns gnostica.game-state.collections)

(defn duplicate-values [values]
  (->> values
       frequencies
       (filter (fn [[_ occurrences]]
                 (> occurrences 1)))
       (map first)
       vec))

(defn rotate-vector-from-index [values index]
  (vec (concat (drop index values)
               (take index values))))
