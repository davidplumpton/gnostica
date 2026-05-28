(ns gnostica.pieces)

(def max-pieces-per-space 3)

(def ^:private sqrt-two
  #?(:clj (Math/sqrt 2)
     :cljs (js/Math.sqrt 2)))

(defn- atan [value]
  #?(:clj (Math/atan value)
     :cljs (js/Math.atan value)))

(defn- sin [value]
  #?(:clj (Math/sin value)
     :cljs (js/Math.sin value)))

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
    :css-color "#2fc3b0"}
   {:id :violet
    :name "Violet"
    :color 0x9b5de5
    :css-color "#9b5de5"}
   {:id :slate
    :name "Slate"
    :color 0x6b7280
    :css-color "#6b7280"}])

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

(def cardinal-orientations #{:north :east :south :west})

(def legal-orientations (conj cardinal-orientations :up))

(defn player-for [piece]
  (get players-by-id (:player-id piece)))

(defn size-data [piece]
  (get piece-sizes (:size piece)))

(defn side-face-apothem [piece-size]
  (/ (:radius piece-size) sqrt-two))

(defn lying-correction-angle [piece-size]
  (atan (/ (side-face-apothem piece-size) (:height piece-size))))

(defn lying-center-height-above-surface [piece-size]
  (* (/ (:height piece-size) 2)
     (sin (lying-correction-angle piece-size))))

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

(defn territory-space [space-index]
  (when (int? space-index)
    {:kind :territory
     :index space-index}))

(defn wasteland-space [row col]
  (when (and (int? row)
             (int? col))
    {:kind :wasteland
     :row row
     :col col}))

(defn space-key [space]
  (cond
    (contains? space :space-index)
    (territory-space (:space-index space))

    (contains? space :index)
    (territory-space (:index space))

    (= :wasteland (:kind space))
    (wasteland-space (:row space) (:col space))))

(defn piece-space-key [piece]
  (if-let [space (:space piece)]
    (space-key space)
    (space-key piece)))

(defn pieces-by-space [pieces]
  (reduce (fn [groups piece]
            (if-let [space (piece-space-key piece)]
              (update groups space (fnil conj []) piece)
              groups))
          {}
          pieces))

(defn pieces-for-space [pieces space-index]
  (get (pieces-by-space pieces) (territory-space space-index) []))

(defn pieces-for-wasteland [pieces row col]
  (get (pieces-by-space pieces) (wasteland-space row col) []))
