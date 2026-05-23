(ns gnostica.board-layout-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.board :as board]
            [gnostica.board-layout :as layout]
            [gnostica.cards :as cards]
            [gnostica.pieces :as pieces]))

(defn- roughly= [expected actual]
  (< (Math/abs (- expected actual)) 1.0e-12))

(defn- roughly-vector= [expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map roughly= expected actual))))

(deftest card-positions-center-the-three-by-three-board
  (is (roughly-vector= [(- layout/card-step) layout/card-step]
                       (layout/card-position {:row 0 :col 0})))
  (is (roughly-vector= [0 0]
                       (layout/card-position {:row 1 :col 1})))
  (is (roughly-vector= [layout/card-step (- layout/card-step)]
                       (layout/card-position {:row 2 :col 2})))
  (is (roughly= (+ (* 2 layout/card-step) layout/card-long (* 2 layout/card-gap))
                layout/board-plane-size)))

(deftest cells-can-be-addressed-by-board-index
  (let [cells (board/initial-board cards/deck identity)
        cells-by-index (layout/cells-by-index cells)]
    (is (= (set (range board/board-card-count))
           (set (keys cells-by-index))))
    (doseq [cell cells]
      (is (= cell (get cells-by-index (:index cell)))))))

(deftest piece-slots-are-stable-and-limited
  (let [space-pieces (mapv #(assoc (first pieces/initial-pieces) :id %)
                           [:a :b :c :d])]
    (is (= [[0 (space-pieces 0)]
            [1 (space-pieces 1)]
            [2 (space-pieces 2)]]
           (vec (layout/visible-piece-slots space-pieces))))
    (is (= [0 -0.03] (layout/piece-slot-offset 0 1)))
    (is (= [-0.17 0.11] (layout/piece-slot-offset 0 2)))
    (is (= [0.17 -0.11] (layout/piece-slot-offset 1 2)))
    (is (= [-0.21 -0.17] (layout/piece-slot-offset 0 3)))
    (is (= [0.21 -0.17] (layout/piece-slot-offset 1 3)))
    (is (= [0 0.18] (layout/piece-slot-offset 2 3)))
    (is (= [0 0] (layout/piece-slot-offset 3 3)))))

(deftest piece-rotation-and-height-math-is-browser-free
  (let [piece-size (:medium pieces/piece-sizes)]
    (is (roughly= 0 (layout/piece-compass-z-rotation :north)))
    (is (roughly= (- (/ Math/PI 2))
                  (layout/piece-compass-z-rotation :east)))
    (is (roughly= Math/PI (layout/piece-compass-z-rotation :south)))
    (is (roughly= (/ Math/PI 2)
                  (layout/piece-compass-z-rotation :west)))
    (is (nil? (layout/piece-compass-z-rotation :up)))
    (is (roughly= (/ Math/PI 4) layout/piece-side-face-roll))
    (is (roughly= (+ layout/piece-surface-z (/ (:height piece-size) 2))
                  (layout/piece-center-z piece-size :up)))
    (is (roughly= (+ layout/piece-surface-z
                     (pieces/lying-center-height-above-surface piece-size))
                  (layout/piece-center-z piece-size :north)))))

(deftest piece-pip-markers-follow-size-counts
  (doseq [[size piece-size] pieces/piece-sizes
          :let [positions (layout/piece-pip-local-positions piece-size)
                xs (map first positions)
                ys (map second positions)
                zs (map #(nth % 2) positions)]]
    (is (= (:pips piece-size) (count positions))
        (str "Expected " size " marker count to match pip count"))
    (is (roughly= 0 (reduce + xs))
        (str "Expected " size " marker row to be centered"))
    (is (every? neg? ys)
        (str "Expected " size " markers near the pyramid base"))
    (is (every? pos? zs)
        (str "Expected " size " markers on the visible front face"))
    (is (every? #(roughly= (first ys) %) ys))
    (is (every? #(roughly= (first zs) %) zs))))
