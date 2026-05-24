(ns gnostica.three-board
  (:require [gnostica.board-layout :as layout]
            [gnostica.icon-view :as icon-view]
            [gnostica.icons :as icons]
            [gnostica.pieces :as pieces]
            [gnostica.three-card-textures :as card-textures]
            [reagent.core :as r]))

(def pointer-click-threshold 8)
(def board-index-user-data-key "gnosticaBoardIndex")
(def expected-three-revision "128")
(def table-surface-color 0x1c0715)
(def table-surface-css-color "#1c0715")
(def table-clear-color 0x0a0308)
(def table-clear-css-color "#0a0308")
(def pip-marker-color 0xfff4d3)
(def piece-edge-outline-color 0x050505)
(def piece-edge-outline-opacity 0.9)
(def wasteland-outline-color 0xd8fff3)
(def wasteland-outline-opacity 0.42)
(def wasteland-outline-z -0.005)
(def wasteland-outline-dash-size 0.035)
(def wasteland-outline-gap-size 0.055)
(def renderer-antialias-requested? true)
(def controls-min-distance 3.2)
(def controls-max-distance 10)
(def controls-min-polar-angle 0.28)
(def controls-max-polar-angle 1.34)
(def controls-zoom-speed 0.78)
(def controls-rotate-speed 0.62)
(def keyboard-pan-step 0.32)
(def keyboard-pan-boost 2)
(def camera-metadata-precision 1000)

(defn three-runtime []
  (when (exists? js/THREE)
    js/THREE))

(defn three-revision []
  (some-> (three-runtime) (.-REVISION)))

(defn orbit-controls-runtime []
  (when-let [three (three-runtime)]
    (.-OrbitControls three)))

(defn runtime-status []
  (cond
    (not (three-runtime))
    {:ok? false
     :code :three-missing
     :message "Three.js is unavailable; check the pinned CDN script before /js/main.js."}

    (not= expected-three-revision (three-revision))
    {:ok? false
     :code :three-revision-mismatch
     :message (str "Three.js revision "
                   (or (three-revision) "unknown")
                   " is incompatible; expected r"
                   expected-three-revision
                   " from the pinned CDN script before /js/main.js.")}

    (not (orbit-controls-runtime))
    {:ok? false
     :code :orbit-controls-missing
     :message "Three.js OrbitControls are unavailable; check the pinned CDN control script before /js/main.js."}

    :else
    {:ok? true
     :code :ready
     :message "Three.js r128 runtime is ready."}))

(defn available? []
  (true? (:ok? (runtime-status))))

(defn- invoke-callback [callbacks key & args]
  (when-let [callback (get callbacks key)]
    (apply callback args)))

(defn- piece-tilt-axis [orientation]
  (let [axis (js/THREE.Vector3.)]
    (case orientation
      :north (.set axis -1 0 0)
      :east (.set axis 0 1 0)
      :south (.set axis 1 0 0)
      :west (.set axis 0 -1 0)
      nil)
    axis))

(defn- set-piece-rotation! [mesh orientation piece-size]
  (cond
    (= :up orientation)
    (set! (.. mesh -rotation -x) (/ js/Math.PI 2))

    (contains? pieces/cardinal-orientations orientation)
    (do
      (.rotateZ mesh (layout/piece-compass-z-rotation orientation))
      (.rotateY mesh layout/piece-side-face-roll)
      (.rotateOnWorldAxis mesh
                          (piece-tilt-axis orientation)
                          (pieces/lying-correction-angle piece-size))))
  mesh)

(defn- apply-card-texture! [material texture render!]
  (set! (.-map material) texture)
  (set! (.-needsUpdate material) true)
  (when render!
    (render!)))

(defn- mark-card-texture-load-error!
  [material callbacks {:keys [image title error]} render!]
  (js/console.error (str "Failed to load tarot texture for " title ": " image) error)
  (.set (.-color material) 0xff8a7a)
  (set! (.-needsUpdate material) true)
  (invoke-callback callbacks :on-texture-error image)
  (render!))

(defn- mark-card-renderer-error! [material callbacks message render!]
  (js/console.error message)
  (.set (.-color material) 0xff8a7a)
  (set! (.-needsUpdate material) true)
  (invoke-callback callbacks :on-renderer-error message)
  (render!))

(defn- pointer-event->board-pointer! [pointer target event]
  (let [rect (.getBoundingClientRect target)
        width (.-width rect)
        height (.-height rect)]
    (when (and (pos? width) (pos? height))
      (.set pointer
            (- (* 2 (/ (- (.-clientX event) (.-left rect)) width)) 1)
            (- 1 (* 2 (/ (- (.-clientY event) (.-top rect)) height))))
      true)))

(defn- set-selection! [this selected-index]
  (let [{:keys [active? render! selection-meshes]} (r/state this)]
    (when (and active? @active? selection-meshes)
      (doseq [[index mesh] selection-meshes]
        (set! (.-visible mesh) (= index selected-index)))
      (when render!
        (render!)))))

(defn- dispose! [this]
  (let [{:keys [renderer
                resize-listener
                controls
                control-change-listener
                pointer-down-listener
                pointer-up-listener
                pointer-cancel-listener
                pointer-move-listener
                pointer-leave-listener
                active?
                geometries
                materials
                textures]} (r/state this)]
    (when active?
      (reset! active? false))
    (when resize-listener
      (.removeEventListener js/window "resize" resize-listener))
    (when (and controls control-change-listener)
      (.removeEventListener controls "change" control-change-listener))
    (when controls
      (.dispose controls))
    (doseq [texture textures]
      (.dispose texture))
    (doseq [material materials]
      (.dispose material))
    (doseq [geometry geometries]
      (.dispose geometry))
    (when renderer
      (let [canvas (.-domElement renderer)
            parent (.-parentNode canvas)]
        (when pointer-down-listener
          (.removeEventListener canvas "pointerdown" pointer-down-listener))
        (when pointer-up-listener
          (.removeEventListener canvas "pointerup" pointer-up-listener))
        (when pointer-cancel-listener
          (.removeEventListener canvas "pointercancel" pointer-cancel-listener))
        (when pointer-move-listener
          (.removeEventListener canvas "pointermove" pointer-move-listener))
        (when pointer-leave-listener
          (.removeEventListener canvas "pointerleave" pointer-leave-listener))
        (when parent
          (.removeChild parent canvas)))
      (.dispose renderer)))
  (r/replace-state this {}))

(defn- add-table-plane! [scene geometries materials spaces]
  (let [{:keys [width height center]} (layout/board-plane spaces)
        [center-x center-y] center
        geometry (js/THREE.PlaneGeometry. width height)
        material (js/THREE.MeshBasicMaterial. #js {:color table-surface-color
                                                   :side js/THREE.DoubleSide})
        mesh (js/THREE.Mesh. geometry material)]
    (.set (.-position mesh) center-x center-y -0.03)
    (.add scene mesh)
    (swap! geometries conj geometry)
    (swap! materials conj material)))

(defn- vector3 [x y z]
  (let [point (js/THREE.Vector3.)]
    (.set point x y z)
    point))

(defn- align-local-z-to-normal! [mesh [normal-x normal-y normal-z]]
  (.setFromUnitVectors (.-quaternion mesh)
                       (vector3 0 0 1)
                       (vector3 normal-x normal-y normal-z))
  mesh)

(defn- add-wasteland-line! [scene geometries material [[start-x start-y] [end-x end-y]]]
  (let [geometry (js/THREE.BufferGeometry.)
        line (js/THREE.Line. geometry material)]
    (.setFromPoints geometry
                    (to-array [(vector3 start-x start-y wasteland-outline-z)
                               (vector3 end-x end-y wasteland-outline-z)]))
    (.computeLineDistances line)
    (.add scene line)
    (swap! geometries conj geometry)))

(defn- add-wasteland-outlines! [scene geometries materials spaces]
  (let [material (js/THREE.LineDashedMaterial.
                  #js {:color wasteland-outline-color
                       :transparent true
                       :opacity wasteland-outline-opacity
                       :dashSize wasteland-outline-dash-size
                       :gapSize wasteland-outline-gap-size})]
    (doseq [space spaces
            segment (layout/space-outline-segments space)]
      (add-wasteland-line! scene geometries material segment))
    (swap! materials conj material)))

(defn- add-piece-lights! [scene]
  (let [ambient (js/THREE.AmbientLight. 0xffffff 0.72)
        directional (js/THREE.DirectionalLight. 0xffffff 0.68)]
    (.set (.-position directional) -2.5 -3.5 5)
    (.add scene ambient)
    (.add scene directional)))

(defn- add-piece-edge-outline! [mesh geometries materials geometry]
  (let [edge-geometry (js/THREE.EdgesGeometry. geometry)
        edge-material (js/THREE.LineBasicMaterial.
                       #js {:color piece-edge-outline-color
                            :transparent true
                            :opacity piece-edge-outline-opacity})
        edge-lines (js/THREE.LineSegments. edge-geometry edge-material)]
    (.add mesh edge-lines)
    (swap! geometries conj edge-geometry)
    (swap! materials conj edge-material)))

(defn- add-piece-mesh!
  [scene geometries materials cells-by-index slot piece-count piece]
  (when-let [cell (get cells-by-index (:space-index piece))]
    (let [piece-size (pieces/size-data piece)
          {:keys [radius height]} piece-size
          player (pieces/player-for piece)
          geometry (js/THREE.ConeGeometry. radius height 4)
          material (js/THREE.MeshLambertMaterial.
                    #js {:color (or (:color player) 0xffffff)})
          mesh (js/THREE.Mesh. geometry material)
          [card-x card-y] (layout/card-position cell)
          [offset-x offset-y] (layout/piece-slot-offset slot piece-count)
          z (layout/piece-center-z piece-size (:orientation piece))]
      (add-piece-edge-outline! mesh geometries materials geometry)
      (doseq [{:keys [position normal]} (layout/piece-pip-local-markers piece-size)]
        (let [pip-geometry (js/THREE.CircleGeometry. layout/piece-pip-marker-radius 16)
              pip-material (js/THREE.MeshBasicMaterial.
                            #js {:color pip-marker-color
                                 :side js/THREE.DoubleSide})
              pip-mesh (js/THREE.Mesh. pip-geometry pip-material)
              [x y marker-z] position]
          (align-local-z-to-normal! pip-mesh normal)
          (.set (.-position pip-mesh) x y marker-z)
          (.add mesh pip-mesh)
          (swap! geometries conj pip-geometry)
          (swap! materials conj pip-material)))
      (.set (.-position mesh) (+ card-x offset-x) (+ card-y offset-y) z)
      (set-piece-rotation! mesh (:orientation piece) piece-size)
      (.add scene mesh)
      (swap! geometries conj geometry)
      (swap! materials conj material)
      true)))

(defn- add-piece-meshes! [scene geometries materials cells board-pieces]
  (let [indexed-cells (layout/cells-by-index cells)
        edge-outline-count (atom 0)]
    (doseq [[_ space-pieces] (pieces/pieces-by-space board-pieces)
            [slot piece] (layout/visible-piece-slots space-pieces)]
      (when (add-piece-mesh! scene geometries materials indexed-cells slot (count space-pieces) piece)
        (swap! edge-outline-count inc)))
    @edge-outline-count))

(defn- visible-piece-count [board-pieces]
  (reduce (fn [total [_ space-pieces]]
            (+ total (count (layout/visible-piece-slots space-pieces))))
          0
          (pieces/pieces-by-space board-pieces)))

(defn- create-renderer [callbacks]
  (try
    (js/THREE.WebGLRenderer. #js {:antialias renderer-antialias-requested?})
    (catch js/Error error
      (js/console.error "Three.js WebGL renderer is unavailable." error)
      (invoke-callback callbacks :on-renderer-error (.-message error))
      nil)))

(defn- renderer-antialias-enabled? [renderer]
  (try
    (let [context (.getContext renderer)
          attributes (when context
                       (.getContextAttributes context))]
      (boolean (and attributes (.-antialias attributes))))
    (catch :default _
      false)))

(defn- show-card-icon-overlays? [card-icon-mode]
  (not= :popup card-icon-mode))

(defn- assoc-component-state! [this key value]
  (let [state (r/state this)]
    (when (not= (get state key) value)
      (r/replace-state this (assoc state key value)))))

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

(defn- camera-view-metadata [camera controls]
  {:camera-distance (round-camera-number (camera-distance camera controls))
   :camera-target-x (round-camera-number (.. controls -target -x))
   :camera-target-y (round-camera-number (.. controls -target -y))})

(defn- sync-camera-metadata! [this camera controls]
  (when (and camera controls)
    (let [metadata (camera-view-metadata camera controls)
          state (r/state this)]
      (when (not= (select-keys state (keys metadata)) metadata)
        (r/replace-state this (merge state metadata))))))

(defn- clamp-number [value min-value max-value]
  (-> value
      (max min-value)
      (min max-value)))

(defn- keyboard-pan-bounds [cells]
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

(defn- handle-board-key-down! [this event]
  (when-let [[right-steps forward-steps] (keyboard-pan-delta event)]
    (.preventDefault event)
    (.stopPropagation event)
    (pan-camera-view! this right-steps forward-steps)))

(defn- focus-board-on-pointer-down! [event]
  (let [target (.-target event)
        class-list (some-> target .-classList)]
    (when (and class-list
               (.contains class-list "board-three__canvas"))
      (.focus (.-currentTarget event)))))

(defn- capture-view-state [this]
  (let [{:keys [camera controls]} (r/state this)]
    (when (and camera controls)
      {:position (vector3-coords (.-position camera))
       :target (vector3-coords (.-target controls))
       :zoom (.-zoom camera)})))

(defn- restore-view-state! [camera controls {:keys [position target zoom]}]
  (when (and (seq position) (seq target))
    (let [[position-x position-y position-z] position
          [target-x target-y target-z] target]
      (.set (.-position camera) position-x position-y position-z)
      (.set (.-target controls) target-x target-y target-z)
      (when (number? zoom)
        (set! (.-zoom camera) zoom))
      (.updateProjectionMatrix camera)
      (.update controls))))

(defn- add-card-plane!
  [scene
   loader
   render!
   active?
   geometries
   materials
   textures
   card-meshes
   selection-meshes
   card-icon-mode
   callbacks
   {:keys [index orientation card] :as cell}]
  (let [selection-geometry (js/THREE.PlaneGeometry.
                            (+ layout/card-short layout/selected-card-padding)
                            (+ layout/card-long layout/selected-card-padding))
        selection-material (js/THREE.MeshBasicMaterial. #js {:color 0x9ff7e7
                                                             :side js/THREE.DoubleSide})
        selection-mesh (js/THREE.Mesh. selection-geometry selection-material)
        geometry (js/THREE.PlaneGeometry. layout/card-short layout/card-long)
        material (js/THREE.MeshBasicMaterial. #js {:color 0xffffff
                                                   :side js/THREE.DoubleSide})
        mesh (js/THREE.Mesh. geometry material)
        [x y] (layout/card-position cell)]
    (.set (.-position selection-mesh) x y 0.01)
    (set! (.-visible selection-mesh) false)
    (.set (.-position mesh) x y 0.02)
    (when (= :landscape orientation)
      (set! (.. selection-mesh -rotation -z) (/ js/Math.PI 2))
      (set! (.. mesh -rotation -z) (/ js/Math.PI 2)))
    (.add scene selection-mesh)
    (.add scene mesh)
    (aset (.-userData mesh) board-index-user-data-key index)
    (swap! geometries conj selection-geometry)
    (swap! materials conj selection-material)
    (swap! selection-meshes assoc index selection-mesh)
    (swap! geometries conj geometry)
    (swap! materials conj material)
    (swap! card-meshes conj mesh)
    (if (and (show-card-icon-overlays? card-icon-mode)
             (seq (icons/present-icon-ids (:gnostica-icons card))))
      (let [{:keys [ok? texture error-message]}
            (card-textures/load-card-icon-texture!
             {:card card
              :active? active?
              :on-ready #(apply-card-texture! material % render!)
              :on-error #(mark-card-texture-load-error! material callbacks % render!)})]
        (if ok?
          (do
            (swap! textures conj texture)
            (apply-card-texture! material texture nil))
          (mark-card-renderer-error! material callbacks error-message render!)))
      (let [texture (card-textures/load-card-art-texture!
                     {:loader loader
                      :card card
                      :active? active?
                      :on-ready #(apply-card-texture! material % render!)
                      :on-error #(mark-card-texture-load-error! material callbacks % render!)})]
        (swap! textures conj texture)))))

(defn- reset-view! [this]
  (let [{:keys [camera controls]} (r/state this)]
    (when controls
      (.reset controls)
      (.update controls)
      (sync-camera-metadata! this camera controls))))

(defn- mount!
  ([this]
   (mount! this nil))
  ([this preserved-view]
   (dispose! this)
   (let [[_ cells board-pieces selected-index card-icon-mode _texture-errors callbacks] (r/argv this)]
     (invoke-callback callbacks :on-clear-texture-errors)
     (when (available?)
       (let [mount-node (.-boardMountNode ^js this)]
         (when mount-node
           (let [scene (js/THREE.Scene.)
                 camera (js/THREE.PerspectiveCamera. 45 1 0.1 100)
                 renderer (create-renderer callbacks)
                 loader (js/THREE.TextureLoader.)
                 raycaster (js/THREE.Raycaster.)
                 pointer (js/THREE.Vector2.)
                 active? (atom true)
                 pointer-down (atom nil)
                 geometries (atom [])
                 materials (atom [])
                 textures (atom [])
                 card-meshes (atom [])
                 selection-meshes (atom {})]
             (when renderer
               (.setPixelRatio renderer (min 2 (or (.-devicePixelRatio js/window) 1)))
               (set! (.-outputEncoding renderer) js/THREE.sRGBEncoding)
               (.setClearColor renderer table-clear-color 1)
               (set! (.. renderer -domElement -className) "board-three__canvas")
               (.appendChild mount-node (.-domElement renderer))
               (.set (.-up camera) 0 0 1)
               (.set (.-position camera) 0 -4.8 5.9)
               (.lookAt camera 0 0 0)
               (letfn [(render! []
                         (when @active?
                           (.render renderer scene camera)))
                       (resize! []
                         (when @active?
                           (let [rect (.getBoundingClientRect mount-node)
                                 width (max 1 (.-width rect))
                                 height (max 1 (.-height rect))]
                             (.setSize renderer width height false)
                             (set! (.-aspect camera) (/ width height))
                             (.updateProjectionMatrix camera)
                             (render!))))
                       (board-index-at! [event]
                         (when (pointer-event->board-pointer! pointer (.-domElement renderer) event)
                           (.setFromCamera raycaster pointer camera)
                           (let [intersections (.intersectObjects raycaster (to-array @card-meshes) false)
                                 intersection (aget intersections 0)
                                 picked-object (some-> intersection (aget "object"))
                                 picked-index (some-> picked-object
                                                      (.-userData)
                                                      (aget board-index-user-data-key))]
                             (when (number? picked-index)
                               picked-index))))
                       (select-card-at! [event]
                         (when-let [picked-index (board-index-at! event)]
                           (invoke-callback callbacks :on-card-select picked-index)))
                       (hover-card-at! [event]
                         (assoc-component-state! this :hovered-index (board-index-at! event)))
                       (pointer-down-listener [event]
                         (reset! pointer-down {:id (.-pointerId event)
                                               :x (.-clientX event)
                                               :y (.-clientY event)}))
                       (pointer-up-listener [event]
                         (when-let [{:keys [id x y]} @pointer-down]
                           (reset! pointer-down nil)
                           (let [distance (js/Math.hypot (- (.-clientX event) x)
                                                         (- (.-clientY event) y))]
                             (when (and (= id (.-pointerId event))
                                        (<= distance pointer-click-threshold))
                               (select-card-at! event)))))
                       (pointer-cancel-listener [_]
                         (reset! pointer-down nil))
                       (pointer-leave-listener [_]
                         (assoc-component-state! this :hovered-index nil))]
                 (let [controls (js/THREE.OrbitControls. camera (.-domElement renderer))
                       canvas (.-domElement renderer)
                       control-change-listener (fn [_]
                                                 (sync-camera-metadata! this camera controls)
                                                 (render!))]
                   (.set (.-target controls) 0 0 0)
                   (set! (.-enableDamping controls) false)
                   (set! (.-enablePan controls) false)
                   (set! (.-enableRotate controls) true)
                   (set! (.-enableZoom controls) true)
                   (set! (.-minDistance controls) controls-min-distance)
                   (set! (.-maxDistance controls) controls-max-distance)
                   (set! (.-minPolarAngle controls) controls-min-polar-angle)
                   (set! (.-maxPolarAngle controls) controls-max-polar-angle)
                   (set! (.-zoomSpeed controls) controls-zoom-speed)
                   (set! (.-rotateSpeed controls) controls-rotate-speed)
                   (.update controls)
                   (.saveState controls)
                   (when preserved-view
                     (restore-view-state! camera controls preserved-view))
                   (.addEventListener controls "change" control-change-listener)
                   (.addEventListener canvas "pointerdown" pointer-down-listener)
                   (.addEventListener canvas "pointerup" pointer-up-listener)
                   (.addEventListener canvas "pointercancel" pointer-cancel-listener)
                   (.addEventListener canvas "pointermove" hover-card-at!)
                   (.addEventListener canvas "pointerleave" pointer-leave-listener)
                   (add-piece-lights! scene)
                   (let [wastelands (layout/wasteland-spaces cells)
                         board-spaces (vec (concat cells wastelands))]
                     (add-table-plane! scene geometries materials board-spaces)
                     (add-wasteland-outlines! scene geometries materials wastelands))
                   (doseq [cell cells]
                     (add-card-plane! scene
                                      loader
                                      render!
                                      active?
                                      geometries
                                      materials
                                      textures
                                      card-meshes
                                      selection-meshes
                                      card-icon-mode
                                      callbacks
                                      cell))
                   (let [piece-edge-outline-count (add-piece-meshes! scene
                                                                       geometries
                                                                       materials
                                                                       cells
                                                                       board-pieces)
                         antialias-enabled? (renderer-antialias-enabled? renderer)]
                     (.addEventListener js/window "resize" resize!)
                     (resize!)
                     (r/replace-state this (merge {:renderer renderer
                                                   :resize-listener resize!
                                                   :camera camera
                                                   :controls controls
                                                   :control-change-listener control-change-listener
                                                   :pointer-down-listener pointer-down-listener
                                                   :pointer-up-listener pointer-up-listener
                                                   :pointer-cancel-listener pointer-cancel-listener
                                                   :pointer-move-listener hover-card-at!
                                                   :pointer-leave-listener pointer-leave-listener
                                                   :active? active?
                                                   :render! render!
                                                   :geometries @geometries
                                                   :materials @materials
                                                   :textures @textures
                                                   :selection-meshes @selection-meshes
                                                   :piece-edge-outline-count piece-edge-outline-count
                                                   :antialias-enabled? antialias-enabled?
                                                   :keyboard-pan-bounds (keyboard-pan-bounds cells)}
                                                  (camera-view-metadata camera controls)))
                     (set-selection! this selected-index))))))))))))

(def scene
  (r/create-class
   {:display-name "three-board-scene"
    :component-did-mount mount!
    :component-did-update
    (fn [this old-argv _ _]
      (let [[_ old-cells old-pieces old-selected-index old-card-icon-mode] old-argv
            [_ new-cells new-pieces new-selected-index new-card-icon-mode] (r/argv this)]
        (cond
          (or (not= old-cells new-cells)
              (not= old-pieces new-pieces))
          (mount! this)

          (not= old-card-icon-mode new-card-icon-mode)
          (mount! this (capture-view-state this))

          (not= old-selected-index new-selected-index)
          (set-selection! this new-selected-index))))
    :component-will-unmount dispose!
    :reagent-render
    (fn [_cells _pieces _selected-index card-icon-mode texture-errors _callbacks]
      (let [component (r/current-component)
            state (r/state component)
            cells-by-index (layout/cells-by-index _cells)
            selected-card (get-in cells-by-index [_selected-index :card])
            texture-metadata (card-textures/texture-renderer-metadata)
            popover-index (or (:hovered-index state)
                              (when (:board-focused? state)
                                _selected-index))
            popover-card (get-in cells-by-index [popover-index :card])]
        [:div.board-three
         {:role "img"
          :tabIndex 0
          :aria-label (str "Three-dimensional Gnostica board with nine face-up tarot territory cards and Icehouse pieces. "
                           "Use W, A, S, D, or arrow keys to move the board view when focused"
                           (when-let [summary (and (= :popup card-icon-mode)
                                                   (icons/icon-stack-label (:gnostica-icons selected-card)))]
                             (when (seq summary)
                               (str ". Selected card special moves: " summary))))
          :on-focus #(assoc-component-state! component :board-focused? true)
          :on-blur #(assoc-component-state! component :board-focused? false)
          :on-pointer-down focus-board-on-pointer-down!
          :on-key-down #(handle-board-key-down! component %)
          :data-board-card-count (count _cells)
          :data-major-icon-card-count (count (filter #(seq (icons/present-icon-ids
                                                            (get-in % [:card :gnostica-icons])))
                                                      _cells))
          :data-major-icon-count (reduce + (map #(count (icons/present-icon-ids
                                                         (get-in % [:card :gnostica-icons])))
                                                _cells))
          :data-card-icon-mode (name card-icon-mode)
          :data-card-icon-scale (:card-icon-scale texture-metadata)
          :data-card-icon-size (:card-icon-size texture-metadata)
          :data-card-texture-supported-icon-count (:supported-icon-count texture-metadata)
          :data-card-texture-max-icon-count (:max-card-icon-count texture-metadata)
          :data-card-texture-icon-stack-fits (:icon-stack-fits? texture-metadata)
          :data-wasteland-count (count (layout/wasteland-spaces _cells))
          :data-visible-piece-count (visible-piece-count _pieces)
          :data-piece-edge-outline-count (or (:piece-edge-outline-count state) 0)
          :data-antialias-requested renderer-antialias-requested?
          :data-antialias-enabled (true? (:antialias-enabled? state))
          :data-min-zoom-distance controls-min-distance
          :data-max-zoom-distance controls-max-distance
          :data-camera-distance (or (:camera-distance state) "")
          :data-camera-target-x (or (:camera-target-x state) "")
          :data-camera-target-y (or (:camera-target-y state) "")
          :data-selected-board-index _selected-index
          :data-table-surface-color table-surface-css-color
          :data-table-clear-color table-clear-css-color
          :data-texture-error-count (count texture-errors)}
         [:div.board-three__mount
          {:aria-hidden "true"
           :ref #(set! (.-boardMountNode ^js component) %)}]
         [:button.board-three__reset
          {:type "button"
           :on-click #(reset-view! component)}
          "Reset view"]
         (when (and (= :popup card-icon-mode)
                    (seq (icons/present-icon-ids (:gnostica-icons popover-card))))
           [:div.board-three-icon-popover
            [icon-view/card-icon-popover popover-card {:show-title? true}]])
         (when (seq texture-errors)
           [:p.board-3d-status.is-error
            (str "Texture load failed for "
                 (count texture-errors)
                 " card"
                 (when (not= 1 (count texture-errors)) "s")
                 ". Check the console for image paths.")])]))}))
