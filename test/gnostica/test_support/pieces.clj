(ns gnostica.test-support.pieces)

(defn piece-by-id [state piece-id]
  (some #(when (= piece-id (:id %)) %)
        (get-in state [:pieces :on-board])))
