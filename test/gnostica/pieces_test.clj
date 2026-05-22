(ns gnostica.pieces-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.board :as board]
            [gnostica.pieces :as pieces]))

(deftest initial-pieces-reference-known-rendering-data
  (doseq [piece pieces/initial-pieces]
    (is (contains? pieces/players-by-id (:player-id piece)))
    (is (contains? pieces/piece-sizes (:size piece)))
    (is (contains? pieces/legal-orientations (:orientation piece)))
    (is (<= 0 (:space-index piece) (dec board/board-card-count)))))

(deftest initial-pieces-fit-space-capacity
  (doseq [[space-index space-pieces] (pieces/pieces-by-space pieces/initial-pieces)]
    (is (<= (count space-pieces) pieces/max-pieces-per-space)
        (str "Expected no more than "
             pieces/max-pieces-per-space
             " pieces at space "
             space-index))))

(deftest initial-pieces-exercise-three-piece-layout
  (is (some #(= pieces/max-pieces-per-space (count %))
            (vals (pieces/pieces-by-space pieces/initial-pieces)))))
