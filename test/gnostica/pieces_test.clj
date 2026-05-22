(ns gnostica.pieces-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.board :as board]
            [gnostica.pieces :as pieces]))

(defn- roughly= [expected actual]
  (< (Math/abs (- expected actual)) 1.0e-12))

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

(deftest lying-piece-geometry-rests-on-side-face
  (doseq [[size piece-size] pieces/piece-sizes
          :let [face-apothem (/ (:radius piece-size) (Math/sqrt 2))
                correction-angle (Math/atan (/ face-apothem (:height piece-size)))
                center-height (* (/ (:height piece-size) 2)
                                 (Math/sin correction-angle))]]
    (is (roughly= face-apothem
                  (pieces/side-face-apothem piece-size))
        (str "Expected " size " face apothem to use the square pyramid side plane"))
    (is (roughly= correction-angle
                  (pieces/lying-correction-angle piece-size))
        (str "Expected " size " lying correction to make the side plane parallel"))
    (is (roughly= center-height
                  (pieces/lying-center-height-above-surface piece-size))
        (str "Expected " size " center height to lift the flush face onto the card"))
    (is (< (pieces/lying-center-height-above-surface piece-size)
           (:radius piece-size))
        (str "Expected " size " lying piece to sit lower than edge-balanced placement"))))
