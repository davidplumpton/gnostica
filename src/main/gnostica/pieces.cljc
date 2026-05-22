(ns gnostica.pieces)

(def max-pieces-per-space 3)

(def players
  [{:id :rose
    :name "Rose"
    :color 0xe85d75
    :css-color "#e85d75"}
   {:id :indigo
    :name "Indigo"
    :color 0x4f7cff
    :css-color "#4f7cff"}
   {:id :gold
    :name "Gold"
    :color 0xf2c94c
    :css-color "#f2c94c"}
   {:id :teal
    :name "Teal"
    :color 0x2fc3b0
    :css-color "#2fc3b0"}])

(def players-by-id
  (into {} (map (juxt :id identity) players)))

(def piece-sizes
  {:small {:label "Small"
           :pips 1
           :radius 0.105
           :height 0.34}
   :medium {:label "Medium"
            :pips 2
            :radius 0.135
            :height 0.46}
   :large {:label "Large"
           :pips 3
           :radius 0.165
           :height 0.58}})

(def legal-orientations #{:up :north :east :south :west})

(def initial-pieces
  [{:id :rose-scout
    :player-id :rose
    :space-index 0
    :size :small
    :orientation :east}
   {:id :indigo-minion
    :player-id :indigo
    :space-index 4
    :size :medium
    :orientation :up}
   {:id :gold-charger
    :player-id :gold
    :space-index 4
    :size :large
    :orientation :north}
   {:id :teal-guard
    :player-id :teal
    :space-index 4
    :size :small
    :orientation :west}
   {:id :rose-striker
    :player-id :rose
    :space-index 8
    :size :medium
    :orientation :south}])

(defn player-for [piece]
  (get players-by-id (:player-id piece)))

(defn size-data [piece]
  (get piece-sizes (:size piece)))

(defn pips [piece]
  (:pips (size-data piece)))

(defn size-label [size]
  (get-in piece-sizes [size :label] "Unknown"))

(defn orientation-label [orientation]
  (case orientation
    :up "Upright"
    :north "North"
    :east "East"
    :south "South"
    :west "West"
    "Unknown"))

(defn pieces-by-space [pieces]
  (group-by :space-index pieces))

(defn pieces-for-space [pieces space-index]
  (get (pieces-by-space pieces) space-index []))
