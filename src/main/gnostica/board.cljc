(ns gnostica.board)

(def board-size 3)

(def board-card-count
  (* board-size board-size))

(defn position-for-index [index]
  {:row (quot index board-size)
   :col (mod index board-size)})

(defn orientation-for [row col]
  (if (even? (+ row col))
    :portrait
    :landscape))

(defn initial-board
  ([deck]
   (initial-board deck shuffle))
  ([deck shuffle-deck]
   (->> deck
        shuffle-deck
        (take board-card-count)
        (map-indexed
         (fn [index card]
           (let [{:keys [row col]} (position-for-index index)]
             {:index index
              :row row
              :col col
              :orientation (orientation-for row col)
              :face :up
              :card card})))
        vec)))
