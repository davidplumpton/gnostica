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
  (let [cells (board/initial-board cards/deck identity)]
    (is (= board/board-card-count (count cells)))
    (is (every? #(= :up (:face %)) cells))))

(deftest initial-board-shuffles-before-dealing
  (let [cells (board/initial-board cards/deck reverse)
        dealt-card-ids (map (comp :id :card) cells)]
    (is (= (map :id (take board/board-card-count (reverse cards/deck)))
           dealt-card-ids))
    (is (not= (map :id (take board/board-card-count cards/deck))
              dealt-card-ids))))

(deftest adjacent-cards-have-opposite-orientations
  (let [cells (board/initial-board cards/deck identity)]
    (doseq [{:keys [row col orientation]} cells
            [dr dc] [[1 0] [0 1]]
            :let [neighbor (cell-at cells (+ row dr) (+ col dc))]
            :when neighbor]
      (is (not= orientation (:orientation neighbor))
          (str "Expected opposite orientations at "
               [row col]
               " and "
               [(+ row dr) (+ col dc)])))))
