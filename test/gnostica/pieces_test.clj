(ns gnostica.pieces-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.pieces :as pieces]))

(defn- roughly= [expected actual]
  (< (Math/abs (- expected actual)) 1.0e-12))

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
