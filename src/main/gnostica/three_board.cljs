(ns gnostica.three-board
  (:require [gnostica.board-layout :as layout]
            [gnostica.pieces :as pieces]
            [reagent.core :as r]))

(def pointer-click-threshold 8)
(def board-index-user-data-key "gnosticaBoardIndex")

(defn three-runtime []
  (when (exists? js/THREE)
    js/THREE))

(defn three-revision []
  (some-> (three-runtime) (.-REVISION)))

(defn orbit-controls-runtime []
  (when-let [three (three-runtime)]
    (.-OrbitControls three)))

(defn available? []
  (boolean (and (three-runtime) (orbit-controls-runtime))))

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

(defn- texture-cover! [texture]
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
  texture)

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
        (when parent
          (.removeChild parent canvas)))
      (.dispose renderer)))
  (r/replace-state this {}))

(defn- add-table-plane! [scene geometries materials]
  (let [geometry (js/THREE.PlaneGeometry. layout/board-plane-size layout/board-plane-size)
        material (js/THREE.MeshBasicMaterial. #js {:color 0x4d9a87
                                                   :side js/THREE.DoubleSide})
        mesh (js/THREE.Mesh. geometry material)]
    (set! (.. mesh -position -z) -0.03)
    (.add scene mesh)
    (swap! geometries conj geometry)
    (swap! materials conj material)))

(defn- add-piece-lights! [scene]
  (let [ambient (js/THREE.AmbientLight. 0xffffff 0.72)
        directional (js/THREE.DirectionalLight. 0xffffff 0.68)]
    (.set (.-position directional) -2.5 -3.5 5)
    (.add scene ambient)
    (.add scene directional)))

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
      (.set (.-position mesh) (+ card-x offset-x) (+ card-y offset-y) z)
      (set-piece-rotation! mesh (:orientation piece) piece-size)
      (.add scene mesh)
      (swap! geometries conj geometry)
      (swap! materials conj material))))

(defn- add-piece-meshes! [scene geometries materials cells board-pieces]
  (let [indexed-cells (layout/cells-by-index cells)]
    (doseq [[_ space-pieces] (pieces/pieces-by-space board-pieces)
            [slot piece] (layout/visible-piece-slots space-pieces)]
      (add-piece-mesh! scene geometries materials indexed-cells slot (count space-pieces) piece))))

(defn- create-renderer [callbacks]
  (try
    (js/THREE.WebGLRenderer. #js {:antialias true})
    (catch js/Error error
      (js/console.error "Three.js WebGL renderer is unavailable." error)
      (invoke-callback callbacks :on-renderer-error (.-message error))
      nil)))

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
   callbacks
   {:keys [index orientation card] :as cell}]
  (let [{:keys [image title]} card
        selection-geometry (js/THREE.PlaneGeometry.
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
    (let [texture (.load loader
                         image
                         (fn [loaded-texture]
                           (when @active?
                             (texture-cover! loaded-texture)
                             (set! (.-encoding loaded-texture) js/THREE.sRGBEncoding)
                             (set! (.-minFilter loaded-texture) js/THREE.LinearFilter)
                             (set! (.-magFilter loaded-texture) js/THREE.LinearFilter)
                             (set! (.-needsUpdate loaded-texture) true)
                             (set! (.-map material) loaded-texture)
                             (set! (.-needsUpdate material) true)
                             (render!)))
                         nil
                         (fn [error]
                           (when @active?
                             (js/console.error (str "Failed to load tarot texture for " title ": " image) error)
                             (.set (.-color material) 0xff8a7a)
                             (set! (.-needsUpdate material) true)
                             (invoke-callback callbacks :on-texture-error image)
                             (render!))))]
      (swap! textures conj texture))))

(defn- reset-view! [this]
  (when-let [controls (:controls (r/state this))]
    (.reset controls)
    (.update controls)))

(defn- mount! [this]
  (dispose! this)
  (let [[_ cells board-pieces selected-index _texture-errors callbacks] (r/argv this)]
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
              (.setClearColor renderer 0x45786d 1)
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
                      (select-card-at! [event]
                        (when (pointer-event->board-pointer! pointer (.-domElement renderer) event)
                          (.setFromCamera raycaster pointer camera)
                          (let [intersections (.intersectObjects raycaster (to-array @card-meshes) false)
                                intersection (aget intersections 0)
                                picked-object (some-> intersection (aget "object"))
                                picked-index (some-> picked-object
                                                     (.-userData)
                                                     (aget board-index-user-data-key))]
                            (when (number? picked-index)
                              (invoke-callback callbacks :on-card-select picked-index)))))
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
                        (reset! pointer-down nil))]
                (let [controls (js/THREE.OrbitControls. camera (.-domElement renderer))
                      canvas (.-domElement renderer)
                      control-change-listener (fn [_] (render!))]
                  (.set (.-target controls) 0 0 0)
                  (set! (.-enableDamping controls) false)
                  (set! (.-enablePan controls) false)
                  (set! (.-enableRotate controls) true)
                  (set! (.-enableZoom controls) true)
                  (set! (.-minDistance controls) 5.2)
                  (set! (.-maxDistance controls) 10)
                  (set! (.-minPolarAngle controls) 0.28)
                  (set! (.-maxPolarAngle controls) 1.34)
                  (set! (.-zoomSpeed controls) 0.78)
                  (set! (.-rotateSpeed controls) 0.62)
                  (.update controls)
                  (.saveState controls)
                  (.addEventListener controls "change" control-change-listener)
                  (.addEventListener canvas "pointerdown" pointer-down-listener)
                  (.addEventListener canvas "pointerup" pointer-up-listener)
                  (.addEventListener canvas "pointercancel" pointer-cancel-listener)
                  (add-piece-lights! scene)
                  (add-table-plane! scene geometries materials)
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
                                     callbacks
                                     cell))
                  (add-piece-meshes! scene geometries materials cells board-pieces)
                  (.addEventListener js/window "resize" resize!)
                  (resize!)
                  (r/replace-state this {:renderer renderer
                                         :resize-listener resize!
                                         :controls controls
                                         :control-change-listener control-change-listener
                                         :pointer-down-listener pointer-down-listener
                                         :pointer-up-listener pointer-up-listener
                                         :pointer-cancel-listener pointer-cancel-listener
                                         :active? active?
                                         :render! render!
                                         :geometries @geometries
                                         :materials @materials
                                         :textures @textures
                                         :selection-meshes @selection-meshes})
                  (set-selection! this selected-index))))))))))

(def scene
  (r/create-class
   {:display-name "three-board-scene"
    :component-did-mount mount!
    :component-did-update
    (fn [this old-argv _ _]
      (let [[_ old-cells old-pieces old-selected-index] old-argv
            [_ new-cells new-pieces new-selected-index] (r/argv this)]
        (if (or (not= old-cells new-cells)
                (not= old-pieces new-pieces))
          (mount! this)
          (when (not= old-selected-index new-selected-index)
            (set-selection! this new-selected-index)))))
    :component-will-unmount dispose!
    :reagent-render
    (fn [_cells _pieces _selected-index texture-errors _callbacks]
      (let [component (r/current-component)]
        [:div.board-three
         {:role "img"
          :aria-label "Three-dimensional Gnostica board with nine face-up tarot territory cards and Icehouse pieces"}
         [:div.board-three__mount
          {:aria-hidden "true"
           :ref #(set! (.-boardMountNode ^js component) %)}]
         [:button.board-three__reset
          {:type "button"
           :on-click #(reset-view! component)}
          "Reset view"]
         (when (seq texture-errors)
           [:p.board-3d-status.is-error
            (str "Texture load failed for "
                 (count texture-errors)
                 " card"
                 (when (not= 1 (count texture-errors)) "s")
                 ". Check the console for image paths.")])]))}))
