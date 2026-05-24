(ns gnostica.three-card-textures
  (:require [gnostica.board-layout :as layout]
            [gnostica.icon-layout :as icon-layout]
            [gnostica.icons :as icons]))

(def card-texture-width icon-layout/card-texture-width)
(def card-texture-height icon-layout/card-texture-height)
(def card-icon-size icon-layout/texture-card-icon-size)
(def card-icon-margin-x icon-layout/texture-card-icon-margin-x)
(def card-icon-margin-y icon-layout/texture-card-icon-margin-y)
(def card-icon-gap icon-layout/texture-card-icon-gap)

(def supported-icon-order
  [:question-card
   :wild-suits
   :draw-hand
   :orient-minion
   :cup-unbounded
   :rod-unbounded
   :convert-piece
   :rod
   :cup
   :trade-hand
   :sword
   :relocate
   :wheel-cup
   :disc
   :orient-target
   :sword-from-discard
   :disc-from-discard
   :judgement
   :world])

(def supported-icon-ids
  (set supported-icon-order))

(defn texture-renderer-metadata []
  {:card-icon-scale icon-layout/card-icon-scale
   :card-icon-size card-icon-size
   :max-card-icon-count icon-layout/max-card-icon-count
   :supported-icon-count (count supported-icon-ids)
   :icon-stack-fits? (icon-layout/texture-icon-stack-fits?
                      icon-layout/max-card-icon-count)})

(defn- active-now? [active?]
  (or (nil? active?) @active?))

(defn- begin-path! [context]
  (.beginPath context))

(defn- stroke! [context]
  (.stroke context))

(defn- fill! [context]
  (.fill context))

(defn- polygon! [context points fill?]
  (when-let [[[x y] & more-points] (seq points)]
    (begin-path! context)
    (.moveTo context x y)
    (doseq [[point-x point-y] more-points]
      (.lineTo context point-x point-y))
    (.closePath context)
    (if fill?
      (fill! context)
      (stroke! context))))

(defn- path-lines! [context points]
  (when-let [[[x y] & more-points] (seq points)]
    (begin-path! context)
    (.moveTo context x y)
    (doseq [[point-x point-y] more-points]
      (.lineTo context point-x point-y))
    (stroke! context)))

(defn- draw-cup! [context]
  (begin-path! context)
  (.moveTo context 28 24)
  (.lineTo context 72 24)
  (.lineTo context 65 56)
  (.quadraticCurveTo context 50 67 35 56)
  (.closePath context)
  (stroke! context)
  (path-lines! context [[50 66] [50 80]])
  (path-lines! context [[36 82] [64 82]]))

(defn- draw-rod! [context]
  (path-lines! context [[34 76] [60 20]])
  (begin-path! context)
  (.moveTo context 53 34)
  (.quadraticCurveTo context 70 32 74 45)
  (.quadraticCurveTo context 61 47 53 34)
  (stroke! context)
  (begin-path! context)
  (.moveTo context 47 48)
  (.quadraticCurveTo context 33 45 30 34)
  (.quadraticCurveTo context 43 34 47 48)
  (stroke! context))

(defn- draw-sword! [context]
  (polygon! context [[31 73] [60 24] [70 17] [66 30] [37 78]] false)
  (path-lines! context [[29 61] [47 76]])
  (path-lines! context [[24 76] [40 84]]))

(defn- draw-disc! [context]
  (polygon! context [[50 16] [60 41] [87 41] [65 57] [73 84]
                     [50 68] [27 84] [35 57] [13 41] [40 41]]
            false))

(defn- draw-triangle! [context points fill?]
  (polygon! context points fill?))

(defn- draw-card-stack! [context]
  (doseq [[x y w h] [[28 25 30 43]]]
    (begin-path! context)
    (.rect context x y w h)
    (stroke! context))
  (path-lines! context [[34 22] [66 29] [58 72]])
  (path-lines! context [[40 19] [72 29] [62 72]]))

(defn- draw-curved-arrow! [context]
  (begin-path! context)
  (.moveTo context 26 36)
  (.quadraticCurveTo context 51 12 75 34)
  (stroke! context)
  (path-lines! context [[71 21] [77 35] [62 35]]))

(defn- draw-swap-arrows! [context]
  (begin-path! context)
  (.moveTo context 26 34)
  (.quadraticCurveTo context 50 12 73 35)
  (stroke! context)
  (path-lines! context [[68 23] [74 36] [59 35]])
  (begin-path! context)
  (.moveTo context 74 66)
  (.quadraticCurveTo context 50 88 27 65)
  (stroke! context)
  (path-lines! context [[32 77] [26 64] [41 65]]))

(defn- draw-mini-triangles! [context]
  (doseq [points [[[20 23] [28 41] [12 41]]
                  [[80 23] [88 41] [72 41]]
                  [[20 77] [28 59] [12 59]]
                  [[80 77] [88 59] [72 59]]]]
    (draw-triangle! context points true)))

(defn- with-mini-symbol! [context x y scale draw!]
  (.save context)
  (.translate context x y)
  (.scale context scale scale)
  (.translate context -50 -50)
  (draw! context)
  (.restore context))

(defn- draw-question-card! [context]
  (begin-path! context)
  (.rect context 30 22 40 56)
  (stroke! context)
  (.fillText context "?" 50 55))

(defn- draw-icon-mark! [context icon-id]
  (case icon-id
    :question-card
    (draw-question-card! context)

    :wild-suits
    (do
      (path-lines! context [[20 78] [80 22]])
      (path-lines! context [[20 22] [80 78]])
      (with-mini-symbol! context 50 8 0.35 draw-cup!)
      (with-mini-symbol! context 77 50 0.33 draw-sword!)
      (with-mini-symbol! context 23 50 0.33 draw-rod!)
      (with-mini-symbol! context 50 82 0.35 draw-disc!))

    :draw-hand
    (draw-card-stack! context)

    :orient-minion
    (do
      (draw-triangle! context [[50 16] [77 76] [23 76]] false)
      (draw-triangle! context [[64 48] [79 76] [49 76]] false)
      (draw-curved-arrow! context))

    :cup-unbounded
    (do
      (draw-mini-triangles! context)
      (draw-cup! context))

    :rod-unbounded
    (do
      (draw-mini-triangles! context)
      (draw-rod! context))

    :convert-piece
    (do
      (draw-triangle! context [[50 16] [77 76] [23 76]] true)
      (draw-triangle! context [[68 22] [84 56] [52 56]] false)
      (path-lines! context [[45 40] [65 40]])
      (path-lines! context [[60 32] [68 40] [60 48]]))

    :rod
    (draw-rod! context)

    :cup
    (draw-cup! context)

    :trade-hand
    (do
      (begin-path! context)
      (.rect context 26 28 23 32)
      (stroke! context)
      (begin-path! context)
      (.rect context 52 40 23 32)
      (stroke! context)
      (draw-swap-arrows! context))

    :sword
    (draw-sword! context)

    :relocate
    (do
      (begin-path! context)
      (.moveTo context 28 70)
      (.quadraticCurveTo context 50 54 72 70)
      (stroke! context)
      (path-lines! context [[50 70] [50 23] [70 45]])
      (path-lines! context [[50 23] [30 45]])
      (begin-path! context)
      (.moveTo context 30 45)
      (.quadraticCurveTo context 18 42 14 30)
      (.quadraticCurveTo context 30 31 39 51)
      (stroke! context)
      (begin-path! context)
      (.moveTo context 70 45)
      (.quadraticCurveTo context 82 42 86 30)
      (.quadraticCurveTo context 70 31 61 51)
      (stroke! context))

    :wheel-cup
    (do
      (draw-cup! context)
      (.fillText context "?" 50 51))

    :disc
    (draw-disc! context)

    :orient-target
    (do
      (draw-triangle! context [[50 16] [77 76] [23 76]] true)
      (draw-triangle! context [[68 30] [88 76] [48 76]] true)
      (draw-curved-arrow! context))

    :sword-from-discard
    (do
      (draw-swap-arrows! context)
      (draw-sword! context))

    :disc-from-discard
    (do
      (draw-swap-arrows! context)
      (draw-disc! context))

    :judgement
    (do
      (draw-card-stack! context)
      (draw-triangle! context [[67 30] [83 66] [51 66]] false)
      (path-lines! context [[76 75] [76 51]])
      (path-lines! context [[68 59] [76 50] [84 59]]))

    :world
    (do
      (begin-path! context)
      (.save context)
      (.translate context 50 50)
      (.scale context 1 0.6)
      (.arc context 0 0 35 0 (* 2 js/Math.PI))
      (.restore context)
      (stroke! context)
      (path-lines! context [[16 50] [84 50]])
      (begin-path! context)
      (.moveTo context 32 37)
      (.quadraticCurveTo context 50 48 68 37)
      (stroke! context)
      (begin-path! context)
      (.moveTo context 32 63)
      (.quadraticCurveTo context 50 52 68 63)
      (stroke! context))

    nil))

(defn- draw-gnostica-icon! [context icon-id x y size]
  (.save context)
  (.translate context x y)
  (.scale context (/ size 100) (/ size 100))
  (set! (.-fillStyle context) "#fffaf0")
  (set! (.-strokeStyle context) "#16100d")
  (set! (.-lineWidth context) 5)
  (begin-path! context)
  (.arc context 50 50 45 0 (* 2 js/Math.PI))
  (fill! context)
  (stroke! context)
  (set! (.-fillStyle context) "#16100d")
  (set! (.-strokeStyle context) "#16100d")
  (set! (.-lineWidth context) 6)
  (set! (.-lineCap context) "round")
  (set! (.-lineJoin context) "round")
  (set! (.-font context) "700 52px Georgia, serif")
  (set! (.-textAlign context) "center")
  (set! (.-textBaseline context) "middle")
  (draw-icon-mark! context icon-id)
  (.restore context))

(defn- draw-icon-stack! [context icon-ids]
  (doseq [[position icon-id] (map-indexed vector (icons/present-icon-ids icon-ids))]
    (draw-gnostica-icon! context
                         icon-id
                         card-icon-margin-x
                         (+ card-icon-margin-y
                            (* position (+ card-icon-size card-icon-gap)))
                         card-icon-size)))

(defn- draw-card-image-cover! [context image]
  (let [image-width (.-width image)
        image-height (.-height image)
        image-aspect (/ image-width image-height)
        card-aspect (/ card-texture-width card-texture-height)
        [source-x source-y source-width source-height]
        (if (> image-aspect card-aspect)
          (let [width (* image-height card-aspect)]
            [(/ (- image-width width) 2) 0 width image-height])
          (let [height (/ image-width card-aspect)]
            [0 (/ (- image-height height) 2) image-width height]))]
    (.drawImage context
                image
                source-x
                source-y
                source-width
                source-height
                0
                0
                card-texture-width
                card-texture-height)))

(defn- create-card-canvas-texture! []
  (let [canvas (.createElement js/document "canvas")
        context (.getContext canvas "2d")
        texture (js/THREE.Texture. canvas)]
    (set! (.-width canvas) card-texture-width)
    (set! (.-height canvas) card-texture-height)
    (set! (.-fillStyle context) "#efe5d0")
    (.fillRect context 0 0 card-texture-width card-texture-height)
    (set! (.-encoding texture) js/THREE.sRGBEncoding)
    (set! (.-minFilter texture) js/THREE.LinearFilter)
    (set! (.-magFilter texture) js/THREE.LinearFilter)
    (set! (.-needsUpdate texture) true)
    {:context context
     :texture texture}))

(defn- configure-loaded-card-texture! [texture]
  (let [image (.-image texture)
        image-width (.-width image)
        image-height (.-height image)
        card-aspect (/ layout/card-short layout/card-long)]
    (when (and (pos? image-width) (pos? image-height))
      (let [image-aspect (/ image-width image-height)
            [repeat-x repeat-y] (if (> image-aspect card-aspect)
                                  [(/ card-aspect image-aspect) 1]
                                  [1 (/ image-aspect card-aspect)])]
        (.set (.-repeat texture) repeat-x repeat-y)
        (.set (.-offset texture)
              (/ (- 1 repeat-x) 2)
              (/ (- 1 repeat-y) 2)))))
  (set! (.-encoding texture) js/THREE.sRGBEncoding)
  (set! (.-minFilter texture) js/THREE.LinearFilter)
  (set! (.-magFilter texture) js/THREE.LinearFilter)
  (set! (.-needsUpdate texture) true)
  texture)

(defn- unsupported-icon-ids [ids]
  (vec (remove supported-icon-ids (icons/present-icon-ids ids))))

(defn load-card-icon-texture!
  "Creates a canvas-backed texture for a card image plus Gnostica icon stack.

  Returns {:ok? true :texture ...} after creating the placeholder texture, then
  invokes on-ready with that texture after image and icon drawing completes.
  Returns {:ok? false :error-message ...} when the card references unsupported
  icon ids."
  [{:keys [card active? on-ready on-error]}]
  (let [{:keys [image title gnostica-icons]} card
        unsupported-icons (unsupported-icon-ids gnostica-icons)]
    (if (seq unsupported-icons)
      {:ok? false
       :unsupported-icons unsupported-icons
       :error-message (str "Missing Gnostica icon asset"
                           (when (not= 1 (count unsupported-icons)) "s")
                           " for "
                           title
                           ": "
                           (pr-str unsupported-icons))}
      (let [card-image (js/Image.)
            {:keys [context texture]} (create-card-canvas-texture!)]
        (set! (.-decoding card-image) "async")
        (set! (.-onload card-image)
              (fn []
                (when (active-now? active?)
                  (.clearRect context 0 0 card-texture-width card-texture-height)
                  (draw-card-image-cover! context card-image)
                  (draw-icon-stack! context gnostica-icons)
                  (set! (.-needsUpdate texture) true)
                  (when on-ready
                    (on-ready texture)))))
        (set! (.-onerror card-image)
              (fn [error]
                (when (active-now? active?)
                  (when on-error
                    (on-error {:card card
                               :image image
                               :title title
                               :error error})))))
        (set! (.-src card-image) image)
        {:ok? true
         :texture texture}))))

(defn load-card-art-texture!
  "Starts loading a normal card art texture and returns the Three.js texture."
  [{:keys [loader card active? on-ready on-error]}]
  (let [{:keys [image title]} card]
    (.load loader
           image
           (fn [loaded-texture]
             (when (active-now? active?)
               (configure-loaded-card-texture! loaded-texture)
               (when on-ready
                 (on-ready loaded-texture))))
           nil
           (fn [error]
             (when (active-now? active?)
               (when on-error
                 (on-error {:card card
                            :image image
                            :title title
                            :error error})))))))
