(ns gnostica.test-support.board
  (:require [gnostica.board :as board]
            [gnostica.cards :as cards]))

(defn board-card-ids [state]
  (mapv (comp :id :card) (:board state)))

(defn board-cell-by-index [state board-index]
  (some #(when (= board-index (:index %)) %)
        (:board state)))

(defn board-cell-at [state row col]
  (some #(when (and (= row (:row %))
                    (= col (:col %)))
           %)
        (:board state)))

(defn state-with-board-card [state board-index card-id]
  (update state :board
          (fn [cells]
            (mapv (fn [cell]
                    (if (= board-index (:index cell))
                      (assoc cell :card (cards/card-by-id card-id))
                      cell))
                  cells))))

(defn with-board-cells-at [state index-coordinates]
  (let [kept-indexes (set (map first index-coordinates))
        removed-cards (keep (fn [cell]
                              (when-not (contains? kept-indexes (:index cell))
                                (:card cell)))
                            (:board state))]
    (-> state
        (assoc :board
               (mapv (fn [[board-index {:keys [row col]}]]
                       (assoc (board-cell-by-index state board-index)
                              :row row
                              :col col
                              :orientation (board/orientation-for row col)))
                     index-coordinates))
        (update :draw-pile into removed-cards))))
