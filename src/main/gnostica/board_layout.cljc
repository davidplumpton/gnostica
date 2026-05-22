(ns gnostica.board-layout
  (:require [gnostica.pieces :as pieces]))

(def pi
  #?(:clj Math/PI
     :cljs js/Math.PI))

(def card-short 1)
(def card-long 1.5)
(def card-gap 0.14)
(def selected-card-padding 0.14)
(def piece-surface-z 0.07)

;; Cardinal pyramids lie on one triangular side face rather than balancing on an edge.
(def piece-side-face-roll (/ pi 4))

(def card-step
  (+ (/ (+ card-short card-long) 2) card-gap))

(def board-plane-size
  (+ (* 2 card-step) card-long (* 2 card-gap)))

(defn card-position [{:keys [row col]}]
  [(* (- col 1) card-step)
   (* (- 1 row) card-step)])

(defn cells-by-index [cells]
  (into {} (map (juxt :index identity) cells)))

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
