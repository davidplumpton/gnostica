(ns gnostica.three-board.controls
  (:require [gnostica.board-layout :as layout]
            [reagent.core :as r]))

(def min-distance 3.2)
(def max-distance 10)
(def min-polar-angle 0.28)
(def max-polar-angle 1.34)
(def zoom-speed 0.78)
(def rotate-speed 0.62)
(def keyboard-pan-step 0.32)
(def keyboard-pan-boost 2)
(def camera-metadata-precision 1000)

(defn- round-camera-number [value]
  (/ (.round js/Math (* value camera-metadata-precision))
     camera-metadata-precision))

(defn- vector3-coords [point]
  [(.-x point) (.-y point) (.-z point)])

(defn- camera-distance [camera controls]
  (let [position (.-position camera)
        target (.-target controls)
        dx (- (.-x position) (.-x target))
        dy (- (.-y position) (.-y target))
        dz (- (.-z position) (.-z target))]
    (js/Math.hypot dx dy dz)))

(defn camera-view-metadata [camera controls]
  {:camera-distance (round-camera-number (camera-distance camera controls))
   :camera-target-x (round-camera-number (.. controls -target -x))
   :camera-target-y (round-camera-number (.. controls -target -y))})

(defn sync-camera-metadata! [this camera controls]
  (when (and camera controls)
    (let [metadata (camera-view-metadata camera controls)
          state (r/state this)]
      (when (not= (select-keys state (keys metadata)) metadata)
        (r/replace-state this (merge state metadata))))))

(defn configure-camera! [camera]
  (.set (.-up camera) 0 0 1)
  (.set (.-position camera) 0 -4.8 5.9)
  (.lookAt camera 0 0 0)
  camera)

(defn restore-view-state! [camera controls {:keys [position target zoom]}]
  (when (and (seq position) (seq target))
    (let [[position-x position-y position-z] position
          [target-x target-y target-z] target]
      (.set (.-position camera) position-x position-y position-z)
      (.set (.-target controls) target-x target-y target-z)
      (when (number? zoom)
        (set! (.-zoom camera) zoom))
      (.updateProjectionMatrix camera)
      (.update controls))))

(defn configure-controls! [camera controls preserved-view]
  (.set (.-target controls) 0 0 0)
  (set! (.-enableDamping controls) false)
  (set! (.-enablePan controls) false)
  (set! (.-enableRotate controls) true)
  (set! (.-enableZoom controls) true)
  (set! (.-minDistance controls) min-distance)
  (set! (.-maxDistance controls) max-distance)
  (set! (.-minPolarAngle controls) min-polar-angle)
  (set! (.-maxPolarAngle controls) max-polar-angle)
  (set! (.-zoomSpeed controls) zoom-speed)
  (set! (.-rotateSpeed controls) rotate-speed)
  (.update controls)
  (.saveState controls)
  (when preserved-view
    (restore-view-state! camera controls preserved-view))
  controls)

(defn control-change-listener [this camera controls render!]
  (fn [_]
    (sync-camera-metadata! this camera controls)
    (render!)))

(defn- clamp-number [value min-value max-value]
  (-> value
      (max min-value)
      (min max-value)))

(defn keyboard-pan-bounds [cells]
  (let [{:keys [width height center]} (layout/board-plane (layout/board-spaces cells))
        [center-x center-y] center
        max-offset-x (/ width 4)
        max-offset-y (/ height 4)]
    {:min-x (- center-x max-offset-x)
     :max-x (+ center-x max-offset-x)
     :min-y (- center-y max-offset-y)
     :max-y (+ center-y max-offset-y)}))

(defn- camera-board-forward [camera controls]
  (let [position (.-position camera)
        target (.-target controls)
        x (- (.-x target) (.-x position))
        y (- (.-y target) (.-y position))
        length (js/Math.hypot x y)]
    (if (> length 0.0001)
      [(/ x length) (/ y length)]
      [0 1])))

(defn- keyboard-pan-delta [event]
  (when-not (or (.-altKey event)
                (.-ctrlKey event)
                (.-metaKey event))
    (let [key (some-> (.-key event) .toLowerCase)
          multiplier (if (.-shiftKey event) keyboard-pan-boost 1)]
      (when-let [[right forward] (case key
                                   "w" [0 1]
                                   "arrowup" [0 1]
                                   "s" [0 -1]
                                   "arrowdown" [0 -1]
                                   "d" [1 0]
                                   "arrowright" [1 0]
                                   "a" [-1 0]
                                   "arrowleft" [-1 0]
                                   nil)]
        [(* right multiplier) (* forward multiplier)]))))

(defn- pan-camera-view! [this right-steps forward-steps]
  (let [{:keys [camera controls render! keyboard-pan-bounds]} (r/state this)]
    (when (and camera controls render! keyboard-pan-bounds)
      (let [[forward-x forward-y] (camera-board-forward camera controls)
            right-x forward-y
            right-y (- forward-x)
            target (.-target controls)
            position (.-position camera)
            intended-x (+ (.-x target)
                          (* keyboard-pan-step
                             (+ (* right-steps right-x)
                                (* forward-steps forward-x))))
            intended-y (+ (.-y target)
                          (* keyboard-pan-step
                             (+ (* right-steps right-y)
                                (* forward-steps forward-y))))
            clamped-x (clamp-number intended-x
                                    (:min-x keyboard-pan-bounds)
                                    (:max-x keyboard-pan-bounds))
            clamped-y (clamp-number intended-y
                                    (:min-y keyboard-pan-bounds)
                                    (:max-y keyboard-pan-bounds))
            dx (- clamped-x (.-x target))
            dy (- clamped-y (.-y target))]
        (when (or (not= 0 dx) (not= 0 dy))
          (.set position (+ (.-x position) dx) (+ (.-y position) dy) (.-z position))
          (.set target clamped-x clamped-y (.-z target))
          (.update controls)
          (sync-camera-metadata! this camera controls)
          (render!))))))

(defn handle-board-key-down! [this event]
  (when-let [[right-steps forward-steps] (when-not (true? (get-in (r/state this)
                                                                  [:drag-preview :active?]))
                                           (keyboard-pan-delta event))]
    (.preventDefault event)
    (.stopPropagation event)
    (pan-camera-view! this right-steps forward-steps)))

(defn capture-view-state [this]
  (let [{:keys [camera controls]} (r/state this)]
    (when (and camera controls)
      {:position (vector3-coords (.-position camera))
       :target (vector3-coords (.-target controls))
       :zoom (.-zoom camera)})))

(defn reset-view! [this]
  (let [{:keys [camera controls]} (r/state this)]
    (when controls
      (.reset controls)
      (.update controls)
      (sync-camera-metadata! this camera controls))))
