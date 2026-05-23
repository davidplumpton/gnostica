(ns gnostica.icon-layout)

(def card-icon-scale
  "Scale applied to Gnostica special-move icons over card art."
  2)

(def max-card-icon-count 3)

(def dom-card-icon-base-width-percent 18)
(def dom-card-icon-width-percent
  (* dom-card-icon-base-width-percent card-icon-scale))

(def dom-card-icon-stack-top-percent 4)
(def dom-card-icon-stack-left-percent 5)
(def dom-card-icon-base-gap-px 3)
(def dom-card-icon-gap-px
  (* dom-card-icon-base-gap-px card-icon-scale))

(def card-texture-width 1024)
(def card-texture-height 1536)
(def texture-card-icon-base-size 132)
(def texture-card-icon-size
  (* texture-card-icon-base-size card-icon-scale))
(def texture-card-icon-margin-x 58)
(def texture-card-icon-margin-y 58)
(def texture-card-icon-base-gap 18)
(def texture-card-icon-gap
  (* texture-card-icon-base-gap card-icon-scale))

(defn icon-stack-span
  "Returns the total height/width consumed by a one-dimensional icon stack."
  [icon-count icon-size icon-gap]
  (if (pos? icon-count)
    (+ (* icon-count icon-size)
       (* (dec icon-count) icon-gap))
    0))

(defn texture-icon-stack-span [icon-count]
  (icon-stack-span icon-count texture-card-icon-size texture-card-icon-gap))

(defn texture-icon-stack-fits?
  "Returns true when the texture icon stack fits inside the card texture bounds."
  [icon-count]
  (and (<= (+ texture-card-icon-margin-x texture-card-icon-size)
           card-texture-width)
       (<= (+ texture-card-icon-margin-y
              (texture-icon-stack-span icon-count))
           card-texture-height)))

(defn dom-icon-stack-style []
  {"--gnostica-icon-stack-top" (str dom-card-icon-stack-top-percent "%")
   "--gnostica-icon-stack-left" (str dom-card-icon-stack-left-percent "%")
   "--gnostica-icon-stack-width" (str dom-card-icon-width-percent "%")
   "--gnostica-icon-stack-gap" (str dom-card-icon-gap-px "px")
   "--gnostica-icon-scale" (str card-icon-scale)})
