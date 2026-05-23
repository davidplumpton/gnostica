(ns gnostica.icon-view
  (:require [gnostica.icons :as icons]))

(defn- cup-symbol []
  [:g
   [:path {:d "M28 24 H72 L65 56 Q50 67 35 56 Z"}]
   [:path {:d "M50 66 V80"}]
   [:path {:d "M36 82 H64"}]])

(defn- rod-symbol []
  [:g
   [:path {:d "M34 76 L60 20"}]
   [:path {:d "M53 34 Q70 32 74 45 Q61 47 53 34 Z"}]
   [:path {:d "M47 48 Q33 45 30 34 Q43 34 47 48 Z"}]])

(defn- sword-symbol []
  [:g
   [:path {:d "M31 73 L60 24 L70 17 L66 30 L37 78 Z"}]
   [:path {:d "M29 61 L47 76"}]
   [:path {:d "M24 76 L40 84"}]])

(defn- disc-symbol []
  [:polygon {:points "50,16 60,41 87,41 65,57 73,84 50,68 27,84 35,57 13,41 40,41"}])

(defn- triangle-symbol [attrs]
  [:polygon (merge {:points "50,16 77,76 23,76"
                    :fill "currentColor"}
                   attrs)])

(defn- card-stack-symbol []
  [:g
   [:rect {:x 28 :y 25 :width 30 :height 43 :rx 3}]
   [:path {:d "M34 22 L66 29 L58 72"}]
   [:path {:d "M40 19 L72 29 L62 72"}]])

(defn- curved-arrow-symbol []
  [:g
   [:path {:d "M26 36 Q51 12 75 34"}]
   [:path {:d "M71 21 L77 35 L62 35"}]])

(defn- swap-arrows-symbol []
  [:g
   [:path {:d "M26 34 Q50 12 73 35"}]
   [:path {:d "M68 23 L74 36 L59 35"}]
   [:path {:d "M74 66 Q50 88 27 65"}]
   [:path {:d "M32 77 L26 64 L41 65"}]])

(defn- mini-triangles-symbol []
  [:g
   [:polygon {:points "20,23 28,41 12,41" :fill "currentColor"}]
   [:polygon {:points "80,23 88,41 72,41" :fill "currentColor"}]
   [:polygon {:points "20,77 28,59 12,59" :fill "currentColor"}]
   [:polygon {:points "80,77 88,59 72,59" :fill "currentColor"}]])

(defn- icon-symbol [icon-id]
  (case icon-id
    :question-card
    [:g
     [:rect {:x 30 :y 22 :width 40 :height 56 :rx 4}]
     [:text {:x 50 :y 65 :text-anchor "middle"} "?"]]

    :wild-suits
    [:g
     [:path {:d "M20 78 L80 22"}]
     [:path {:d "M20 22 L80 78"}]
     [:g {:transform "translate(50 8) scale(0.35) translate(-50 -50)"}
      (cup-symbol)]
     [:g {:transform "translate(77 50) scale(0.33) translate(-50 -50)"}
      (sword-symbol)]
     [:g {:transform "translate(23 50) scale(0.33) translate(-50 -50)"}
      (rod-symbol)]
     [:g {:transform "translate(50 82) scale(0.35) translate(-50 -50)"}
      (disc-symbol)]]

    :draw-hand
    (card-stack-symbol)

    :orient-minion
    [:g
     (triangle-symbol {:fill "none"})
     [:polygon {:points "64,48 79,76 49,76" :fill "none"}]
     (curved-arrow-symbol)]

    :cup-unbounded
    [:g
     (mini-triangles-symbol)
     (cup-symbol)]

    :rod-unbounded
    [:g
     (mini-triangles-symbol)
     (rod-symbol)]

    :convert-piece
    [:g
     (triangle-symbol {})
     [:polygon {:points "68,22 84,56 52,56" :fill "none"}]
     [:path {:d "M45 40 H65"}]
     [:path {:d "M60 32 L68 40 L60 48"}]]

    :rod
    (rod-symbol)

    :cup
    (cup-symbol)

    :trade-hand
    [:g
     [:rect {:x 26 :y 28 :width 23 :height 32 :rx 3}]
     [:rect {:x 52 :y 40 :width 23 :height 32 :rx 3}]
     (swap-arrows-symbol)]

    :sword
    (sword-symbol)

    :relocate
    [:g
     [:path {:d "M28 70 Q50 54 72 70"}]
     [:path {:d "M50 70 L50 23"}]
     [:path {:d "M50 23 L70 45"}]
     [:path {:d "M50 23 L30 45"}]
     [:path {:d "M30 45 Q18 42 14 30 Q30 31 39 51"}]
     [:path {:d "M70 45 Q82 42 86 30 Q70 31 61 51"}]]

    :wheel-cup
    [:g
     (cup-symbol)
     [:text {:x 50 :y 56 :text-anchor "middle"} "?"]]

    :disc
    (disc-symbol)

    :orient-target
    [:g
     (triangle-symbol {})
     [:polygon {:points "68,30 88,76 48,76" :fill "currentColor"}]
     (curved-arrow-symbol)]

    :sword-from-discard
    [:g
     (swap-arrows-symbol)
     (sword-symbol)]

    :disc-from-discard
    [:g
     (swap-arrows-symbol)
     (disc-symbol)]

    :judgement
    [:g
     (card-stack-symbol)
     [:polygon {:points "67,30 83,66 51,66" :fill "none"}]
     [:path {:d "M76 75 V51"}]
     [:path {:d "M68 59 L76 50 L84 59"}]]

    :world
    [:g
     [:ellipse {:cx 50 :cy 50 :rx 35 :ry 21}]
     [:path {:d "M16 50 H84"}]
     [:path {:d "M32 37 Q50 48 68 37"}]
     [:path {:d "M32 63 Q50 52 68 63"}]]

    nil))

(defn gnostica-icon [icon-id]
  [:svg.gnostica-icon
   {:class (str "is-" (name icon-id))
    :viewBox "0 0 100 100"
    :focusable "false"
    :aria-hidden "true"
    :data-icon-id (name icon-id)}
   [:circle.gnostica-icon__base {:cx 50 :cy 50 :r 45}]
   [:g.gnostica-icon__mark
    {:fill "none"
     :stroke "currentColor"
     :stroke-linecap "round"
     :stroke-linejoin "round"}
    (icon-symbol icon-id)]])

(defn card-icon-popover
  ([card] (card-icon-popover card {}))
  ([card {:keys [show-title?]}]
   (when-let [icon-ids (seq (icons/present-icon-ids (:gnostica-icons card)))]
     [:span.card-icon-popover
      {:role "tooltip"
       :data-icon-count (count icon-ids)}
      (when show-title?
        [:span.card-icon-popover__title (:title card)])
      [:span.card-icon-popover__items
       (for [[position icon-id] (map-indexed vector icon-ids)]
         ^{:key (str (:id card) "-popover-" position "-" (name icon-id))}
         [:span.card-icon-popover__item
          [gnostica-icon icon-id]
          [:span.card-icon-popover__label (icons/icon-label icon-id)]])]])))
