(ns gnostica.board-layout
  (:require [gnostica.board :as board]
            [gnostica.pieces :as pieces]))

(def pi
  #?(:clj Math/PI
     :cljs js/Math.PI))

(defn- sqrt [value]
  #?(:clj (Math/sqrt value)
     :cljs (js/Math.sqrt value)))

(def card-short 1)
(def card-long 1.5)
(def card-gap 0.14)
(def selected-card-padding 0.14)
(def piece-surface-z 0.07)
(def piece-pip-marker-radius 0.024)
(def piece-pip-marker-surface-lift 0.01)
(def piece-pip-marker-base-inset-ratio 0.22)
(def piece-pip-marker-spacing-ratio 0.42)

;; Cardinal pyramids lie on one triangular side face rather than balancing on an edge.
(def piece-side-face-roll (/ pi 4))

(def card-step
  (+ (/ (+ card-short card-long) 2) card-gap))

(def board-plane-size
  (+ (* 2 card-step) card-long (* 2 card-gap)))

(def table-void-margin
  (* card-step 0.35))

(def table-fade-distance
  (/ table-void-margin 2))

(defn card-position [{:keys [row col]}]
  [(* (- col 1) card-step)
   (* (- 1 row) card-step)])

(def orthogonal-neighbor-offsets
  [[-1 0] [0 1] [1 0] [0 -1]])

(defn- space-key [{:keys [row col]}]
  [row col])

(defn space-dimensions [{:keys [orientation]}]
  (if (= :landscape orientation)
    [card-long card-short]
    [card-short card-long]))

(defn space-extents [space]
  (let [[x y] (card-position space)
        [width height] (space-dimensions space)
        half-width (/ width 2)
        half-height (/ height 2)]
    {:left (- x half-width)
     :right (+ x half-width)
     :bottom (- y half-height)
     :top (+ y half-height)}))

(defn space-outline-segments [space]
  (let [{:keys [left right bottom top]} (space-extents space)]
    [[[left bottom] [right bottom]]
     [[right bottom] [right top]]
     [[right top] [left top]]
     [[left top] [left bottom]]]))

(defn wasteland-spaces [cells]
  (let [occupied (set (map space-key cells))]
    (->> cells
         (mapcat (fn [{:keys [row col]}]
                   (map (fn [[row-offset col-offset]]
                          [(+ row row-offset) (+ col col-offset)])
                        orthogonal-neighbor-offsets)))
         (remove occupied)
         set
         sort
         (mapv (fn [[row col]]
                 {:kind :wasteland
                  :id (str "wasteland-" row "-" col)
                  :row row
                  :col col
                  :orientation (board/orientation-for row col)})))))

(defn board-spaces [cells]
  (vec (concat cells (wasteland-spaces cells))))

(defn space-bounds [spaces]
  (when (seq spaces)
    {:min-row (apply min (map :row spaces))
     :max-row (apply max (map :row spaces))
     :min-col (apply min (map :col spaces))
     :max-col (apply max (map :col spaces))}))

(defn board-plane [spaces]
  (let [{:keys [min-row max-row min-col max-col]} (space-bounds spaces)
        center-col (/ (+ min-col max-col) 2)
        center-row (/ (+ min-row max-row) 2)]
    {:width (+ (* (- max-col min-col) card-step) card-long (* 2 card-gap))
     :height (+ (* (- max-row min-row) card-step) card-long (* 2 card-gap))
     :center [(* (- center-col 1) card-step)
              (* (- 1 center-row) card-step)]}))

(defn table-plane [spaces]
  (let [{:keys [width height center] :as playable-plane} (board-plane spaces)]
    (assoc playable-plane
           :width (+ width (* 2 table-void-margin))
           :height (+ height (* 2 table-void-margin))
           :velvet-width width
           :velvet-height height
           :void-margin table-void-margin
           :fade-distance table-fade-distance
           :center center)))

(defn cells-by-index [cells]
  (into {} (map (juxt :index identity) cells)))

(defn cell-by-index [cells index]
  (get (cells-by-index cells) index))

(defn piece-slot-offset [slot piece-count]
  (get (case piece-count
         1 [[0 -0.03]]
         2 [[-0.17 0.11] [0.17 -0.11]]
         [[-0.21 -0.17] [0.21 -0.17] [0 0.18]])
       slot
       [0 0]))

(defn visible-piece-slots [space-pieces]
  (map-indexed vector (take pieces/max-pieces-per-space space-pieces)))

(defn piece-compass-z-rotation [orientation]
  (case orientation
    :north 0
    :east (- (/ pi 2))
    :south pi
    :west (/ pi 2)
    nil))

(defn piece-center-z [piece-size orientation]
  (+ piece-surface-z
     (if (= :up orientation)
       (/ (:height piece-size) 2)
       (pieces/lying-center-height-above-surface piece-size))))

(defn- piece-radius-at-local-y [piece-size y]
  (* (:radius piece-size)
     (/ (- (/ (:height piece-size) 2) y)
        (:height piece-size))))

(defn- piece-front-side-face-normal [piece-size]
  (let [slope (/ (:radius piece-size) (:height piece-size))
        length (sqrt (+ 2 (* slope slope)))]
    [(/ -1 length) (/ slope length) (/ 1 length)]))

(defn piece-pip-local-markers [piece-size]
  (let [pip-count (or (:pips piece-size) 0)
        inset (* (:height piece-size) piece-pip-marker-base-inset-ratio)
        y (+ (- (/ (:height piece-size) 2))
             inset)
        face-radius (piece-radius-at-local-y piece-size y)
        face-center [(- (/ face-radius 2)) y (/ face-radius 2)]
        edge-direction [(/ -1 (sqrt 2)) 0 (/ -1 (sqrt 2))]
        normal (piece-front-side-face-normal piece-size)
        x-step (* (:radius piece-size) piece-pip-marker-spacing-ratio)
        center-offset (/ (dec pip-count) 2)]
    (mapv (fn [index]
            (let [edge-offset (* (- index center-offset) x-step)
                  position (mapv (fn [center edge normal-component]
                                   (+ center
                                      (* edge-offset edge)
                                      (* piece-pip-marker-surface-lift normal-component)))
                                 face-center
                                 edge-direction
                                 normal)]
              {:position position
               :normal normal}))
          (range pip-count))))

(defn piece-pip-local-positions [piece-size]
  (mapv :position (piece-pip-local-markers piece-size)))
