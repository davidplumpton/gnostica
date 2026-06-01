(ns gnostica.board-layout-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.board :as board]
            [gnostica.board-layout :as layout]
            [gnostica.cards :as cards]
            [gnostica.fixtures :as fixtures]
            [gnostica.pieces :as pieces]))

(defn- roughly= [expected actual]
  (< (Math/abs (- expected actual)) 1.0e-12))

(defn- roughly-vector= [expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map roughly= expected actual))))

(defn- distance [[ax ay az] [bx by bz]]
  (Math/sqrt (+ (Math/pow (- ax bx) 2)
                (Math/pow (- ay by) 2)
                (Math/pow (- az bz) 2))))

(deftest card-positions-center-the-three-by-three-board
  (is (roughly-vector= [(- layout/card-step) layout/card-step]
                       (layout/card-position {:row 0 :col 0})))
  (is (roughly-vector= [0 0]
                       (layout/card-position {:row 1 :col 1})))
  (is (roughly-vector= [layout/card-step (- layout/card-step)]
                       (layout/card-position {:row 2 :col 2})))
  (is (roughly= (+ (* 2 layout/card-step) layout/card-long (* 2 layout/card-gap))
                layout/board-plane-size)))

(deftest wasteland-spaces-are-empty-orthogonal-neighbors
  (let [cells (board/initial-board cards/deck identity)
        wastelands (layout/wasteland-spaces cells)]
    (is (= [{:kind :wasteland :row -1 :col 0 :orientation :landscape}
            {:kind :wasteland :row -1 :col 1 :orientation :portrait}
            {:kind :wasteland :row -1 :col 2 :orientation :landscape}
            {:kind :wasteland :row 0 :col -1 :orientation :landscape}
            {:kind :wasteland :row 0 :col 3 :orientation :landscape}
            {:kind :wasteland :row 1 :col -1 :orientation :portrait}
            {:kind :wasteland :row 1 :col 3 :orientation :portrait}
            {:kind :wasteland :row 2 :col -1 :orientation :landscape}
            {:kind :wasteland :row 2 :col 3 :orientation :landscape}
            {:kind :wasteland :row 3 :col 0 :orientation :landscape}
            {:kind :wasteland :row 3 :col 1 :orientation :portrait}
            {:kind :wasteland :row 3 :col 2 :orientation :landscape}]
           (mapv #(select-keys % [:kind :row :col :orientation]) wastelands)))
    (is (empty? (filter (set (map (juxt :row :col) cells))
                        (map (juxt :row :col) wastelands))))
    (is (= {:min-row -1 :max-row 3 :min-col -1 :max-col 3}
           (layout/space-bounds (layout/board-spaces cells))))
    (is (roughly= (+ (* 4 layout/card-step) layout/card-long (* 2 layout/card-gap))
                  (:width (layout/board-plane (layout/board-spaces cells)))))
    (is (roughly= (+ (* 4 layout/card-step) layout/card-long (* 2 layout/card-gap))
                  (:height (layout/board-plane (layout/board-spaces cells)))))
    (is (roughly= (+ (:width (layout/board-plane (layout/board-spaces cells)))
                     (* 2 layout/table-void-margin))
                  (:width (layout/table-plane (layout/board-spaces cells)))))
    (is (roughly= (:width (layout/board-plane (layout/board-spaces cells)))
                  (:velvet-width (layout/table-plane (layout/board-spaces cells)))))
    (is (roughly= (/ layout/table-void-margin 2)
                  (:fade-distance (layout/table-plane (layout/board-spaces cells)))))))

(deftest wasteland-detection-fills-internal-empty-spaces
  (let [cells [{:row 0 :col 0 :orientation :portrait}
               {:row 0 :col 2 :orientation :portrait}]
        wastelands (layout/wasteland-spaces cells)]
    (is (some #(= {:row 0 :col 1} (select-keys % [:row :col])) wastelands))
    (is (not-any? #(= {:row 0 :col 0} (select-keys % [:row :col])) wastelands))
    (is (not-any? #(= {:row 0 :col 2} (select-keys % [:row :col])) wastelands))))

(deftest cells-can-be-addressed-by-board-index
  (let [cells (board/initial-board cards/deck identity)
        cells-by-index (layout/cells-by-index cells)]
    (is (= (set (range board/board-card-count))
           (set (keys cells-by-index))))
    (doseq [cell cells]
      (is (= cell (get cells-by-index (:index cell))))
      (is (= cell (layout/cell-by-index cells (:index cell)))))
    (is (nil? (layout/cell-by-index cells 99)))))

(deftest piece-slots-are-stable-and-overflow-aware
  (let [space-pieces (mapv #(assoc (first fixtures/demo-board-pieces) :id %)
                           [:a :b :c :d])]
    (is (= [[0 (space-pieces 0)]
            [1 (space-pieces 1)]
            [2 (space-pieces 2)]
            [3 (space-pieces 3)]]
           (vec (layout/visible-piece-slots space-pieces))))
    (is (= [0 -0.03] (layout/piece-slot-offset 0 1)))
    (is (= [-0.17 0.11] (layout/piece-slot-offset 0 2)))
    (is (= [0.17 -0.11] (layout/piece-slot-offset 1 2)))
    (is (= [-0.21 -0.17] (layout/piece-slot-offset 0 3)))
    (is (= [0.21 -0.17] (layout/piece-slot-offset 1 3)))
    (is (= [0 0.18] (layout/piece-slot-offset 2 3)))
    (is (= [0 0] (layout/piece-slot-offset 3 3)))
    (is (not= [0 0] (layout/piece-slot-offset 3 4)))
    (is (= 0.84 (layout/piece-slot-scale 4)))
    (is (roughly-vector= [50 52]
                         (layout/piece-slot-css-position 0 1 :portrait)))))

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
    (is (roughly= (+ layout/piece-upright-surface-z (/ (:height piece-size) 2))
                  (layout/piece-center-z piece-size :up)))
    (is (roughly= (+ layout/card-surface-z layout/piece-cardinal-surface-lift)
                  layout/piece-cardinal-surface-z))
    (is (< layout/piece-cardinal-surface-z layout/piece-upright-surface-z))
    (is (roughly= (+ layout/piece-cardinal-surface-z
                     (pieces/lying-center-height-above-surface piece-size))
                  (layout/piece-center-z piece-size :north)))))

(deftest piece-pip-markers-follow-size-counts
  (doseq [[size piece-size] pieces/piece-sizes
          :let [markers (layout/piece-pip-local-markers piece-size)
                positions (mapv :position markers)
                normals (mapv :normal markers)
                surface-positions (mapv (fn [position normal]
                                          (mapv (fn [value normal-component]
                                                  (- value
                                                     (* layout/piece-pip-marker-surface-lift
                                                        normal-component)))
                                                position
                                                normal))
                                        positions
                                        normals)
                distances (map distance positions (rest positions))]]
    (is (= (:pips piece-size) (count markers))
        (str "Expected " size " marker count to match pip count"))
    (is (every? (fn [[normal-x normal-y normal-z]]
                  (and (neg? normal-x)
                       (pos? normal-y)
                       (pos? normal-z)
                       (roughly= 1 (distance [0 0 0] [normal-x normal-y normal-z]))))
                normals)
        (str "Expected " size " marker normals to face out from one side face"))
    (is (every? (fn [[surface-x surface-y surface-z]]
                  (let [face-radius (* (:radius piece-size)
                                       (/ (- (/ (:height piece-size) 2) surface-y)
                                          (:height piece-size)))]
                    (roughly= face-radius (+ (- surface-x) surface-z))))
                surface-positions)
        (str "Expected " size " markers to sit on the front-left pyramid side face"))
    (is (every? neg? (map second surface-positions))
        (str "Expected " size " markers near the pyramid base"))
    (is (every? #(> % (* 2 layout/piece-pip-marker-radius)) distances)
        (str "Expected " size " marker spacing to keep pip counts legible"))
    (is (every? #(roughly= (* (:radius piece-size)
                              layout/piece-pip-marker-spacing-ratio)
                           %)
                distances))))
