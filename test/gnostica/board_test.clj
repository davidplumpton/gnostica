(ns gnostica.board-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.board :as board]
            [gnostica.cards :as cards]))

(defn- cell-at [cells row col]
  (some
   (fn [cell]
     (when (and (= row (:row cell))
                (= col (:col cell)))
       cell))
   cells))

(deftest initial-board-has-nine-face-up-cards
  (let [cells (board/initial-board cards/deck)]
    (is (= board/board-card-count (count cells)))
    (is (every? #(= :up (:face %)) cells))
    (is (= (map :id (take board/board-card-count cards/deck))
           (map (comp :id :card) cells)))))

(deftest adjacent-cards-have-opposite-orientations
  (let [cells (board/initial-board cards/deck)]
    (doseq [{:keys [row col orientation]} cells
            [dr dc] [[1 0] [0 1]]
            :let [neighbor (cell-at cells (+ row dr) (+ col dc))]
            :when neighbor]
      (is (not= orientation (:orientation neighbor))
          (str "Expected opposite orientations at "
               [row col]
               " and "
               [(+ row dr) (+ col dc)])))))
