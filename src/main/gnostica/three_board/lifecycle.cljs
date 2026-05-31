(ns gnostica.three-board.lifecycle
  (:require [gnostica.gesture-input :as gesture-input]
            [gnostica.three-board.controls :as controls]
            [gnostica.three-board.pointer :as pointer]
            [gnostica.three-board.resources :as resources]
            [gnostica.three-board.runtime :as runtime]
            [gnostica.three-board.scene-graph :as scene-graph]
            [reagent.core :as r]))

(declare set-selection!)

(defn assoc-component-state! [this key value]
  (let [state (r/state this)]
    (when (not= (get state key) value)
      (r/replace-state this (assoc state key value))
      (when (= :drag-preview key)
        (let [[_ _cells _board-pieces selected-index _card-icon-mode
               _texture-errors legal-targets] (r/argv this)]
          (set-selection! this selected-index legal-targets))))))

(defn- territory-targets-by-index [legal-targets]
  (into {}
        (map (juxt :board-index identity))
        (:territories legal-targets)))

(defn- wasteland-targets-by-coordinate [legal-targets]
  (into {}
        (map (fn [{:keys [row col] :as descriptor}]
               [[row col] descriptor]))
        (:wastelands legal-targets)))

(defn- piece-targets-by-id [legal-targets]
  (into {}
        (map (juxt :piece-id identity))
        (:pieces legal-targets)))

(defn- board-space-drag-preview? [{:keys [source]}]
  (gesture-input/board-space-drag-source? source))

(defn- drag-target-key [kind {:keys [target] :as drag-preview}]
  (when (and (board-space-drag-preview? drag-preview)
             (= kind (:kind target)))
    (case kind
      :territory (:board-index target)
      :wasteland [(:row target) (:col target)]
      nil)))

(defn- placement-target-key [kind {:keys [placement]}]
  (let [target-space (:target-space placement)]
    (when (= kind (:kind target-space))
      (case kind
        :territory (:board-index target-space)
        :wasteland [(:row target-space) (:col target-space)]
        nil))))

(defn- highlighted-target-style [status]
  (case status
    :disabled {:visible? true
               :color 0xff8a7a
               :opacity 0.56}
    :legal {:visible? true
            :color 0xfff2a6
            :opacity 0.9}
    {:visible? true
     :color 0xffe08a
     :opacity 0.74}))

(defn- target-mesh-style
  ([descriptor]
   (target-mesh-style nil nil false nil nil nil descriptor))
  ([selected-key key drag-active? drag-key preview-key preview-status descriptor]
    (cond
      (and (some? drag-key)
           (= key drag-key))
      (assoc (highlighted-target-style (:status descriptor))
             :drag-target? true)

      (and (some? preview-key)
           (= key preview-key))
      (highlighted-target-style preview-status)

      (and (some? selected-key)
           (= key selected-key))
      {:visible? true
       :color 0x9ff7e7
       :opacity 0.82}

      drag-active?
      {:visible? false
       :color 0x9ff7e7
       :opacity 0.82}

      (and (:active? descriptor)
           (:enabled? descriptor))
      {:visible? true
       :color 0xffe08a
       :opacity 0.68}

      (and (:active? descriptor)
           (not (:enabled? descriptor)))
      {:visible? true
       :color 0xff8a7a
       :opacity 0.28}

      :else
      {:visible? false
       :color 0x9ff7e7
       :opacity 0.82})))

(defn- style-highlight-mesh! [mesh {:keys [visible? color opacity]}]
  (let [^js material (.-material ^js mesh)]
    (set! (.-visible ^js mesh) visible?)
    (.set (.-color material) color)
    (set! (.-opacity material) opacity)
    (set! (.-transparent material) (< opacity 1))
    (set! (.-needsUpdate material) true)))

(defn- assoc-state-when-changed! [this key value]
  (let [state (r/state this)]
    (when (not= (get state key) value)
      (r/replace-state this (assoc state key value)))))

(defn- style-and-track-drag-target! [mesh style]
  (style-highlight-mesh! mesh style)
  (true? (:drag-target? style)))

(defn set-placement-preview! [this move-preview]
  (let [{:keys [active? placement-preview render!]} (r/state this)]
    (when (and active? @active? placement-preview)
      (scene-graph/set-placement-preview! placement-preview (:placement move-preview))
      (when render!
        (render!)))))

(defn set-selection!
  ([this selected-index]
   (set-selection! this selected-index nil))
  ([this selected-index legal-targets]
   (let [{:keys [active? render! selection-meshes wasteland-selection-meshes
                 piece-selection-meshes legal-targets-ref drag-preview]} (r/state this)]
     (when (and active? @active? selection-meshes)
      (when legal-targets-ref
        (reset! legal-targets-ref legal-targets))
      (let [[_ _cells _board-pieces _selected-index _card-icon-mode
             _texture-errors _legal-targets move-preview] (r/argv this)
            territory-targets (territory-targets-by-index legal-targets)
            wasteland-targets (wasteland-targets-by-coordinate legal-targets)
            piece-targets (piece-targets-by-id legal-targets)
            board-space-drag-active? (board-space-drag-preview? drag-preview)
            drag-territory-key (drag-target-key :territory drag-preview)
            drag-wasteland-key (drag-target-key :wasteland drag-preview)
            preview-territory-key (placement-target-key :territory move-preview)
            preview-wasteland-key (placement-target-key :wasteland move-preview)
            preview-status (:status move-preview)
            drag-target-count (atom 0)]
        (doseq [[index mesh] selection-meshes]
          (when (style-and-track-drag-target!
                 mesh
                 (target-mesh-style selected-index
                                    index
                                    board-space-drag-active?
                                    drag-territory-key
                                    preview-territory-key
                                    preview-status
                                    (get territory-targets index)))
            (swap! drag-target-count inc)))
        (doseq [[coordinate mesh] wasteland-selection-meshes]
          (when (style-and-track-drag-target!
                 mesh
                 (target-mesh-style nil
                                    coordinate
                                    board-space-drag-active?
                                    drag-wasteland-key
                                    preview-wasteland-key
                                    preview-status
                                    (get wasteland-targets coordinate)))
            (swap! drag-target-count inc)))
        (doseq [[piece-id mesh] piece-selection-meshes]
          (style-highlight-mesh! mesh
                                 (target-mesh-style (get piece-targets piece-id))))
        (assoc-state-when-changed! this
                                   :drag-target-highlight-count
                                   @drag-target-count)
        (when render!
          (render!)))))))

(defn dispose! [this]
  (resources/dispose-board! (r/state this))
  (r/replace-state this {}))

(defn mount!
  ([this]
   (mount! this nil))
  ([this preserved-view]
   (dispose! this)
   (let [[_ cells board-pieces selected-index card-icon-mode _texture-errors
          legal-targets _move-preview direct-manipulation callbacks] (r/argv this)]
     (resources/invoke-callback callbacks :on-clear-texture-errors)
     (when (runtime/available?)
       (when-let [mount-node (.-boardMountNode ^js this)]
         (when-let [renderer (resources/create-renderer callbacks)]
           (let [scene (js/THREE.Scene.)
                 camera (js/THREE.PerspectiveCamera. 45 1 0.1 100)
                 loader (js/THREE.TextureLoader.)
                 active? (atom true)
                 legal-targets-ref (atom legal-targets)]
             (.setPixelRatio renderer (min 2 (or (.-devicePixelRatio js/window) 1)))
             (set! (.-outputEncoding renderer) js/THREE.sRGBEncoding)
             (.setClearColor renderer scene-graph/table-clear-color 1)
             (set! (.. renderer -domElement -className) "board-three__canvas")
             (.appendChild mount-node (.-domElement renderer))
             (controls/configure-camera! camera)
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
                           (render!))))]
               (let [controls (js/THREE.OrbitControls. camera (.-domElement renderer))
                     canvas (.-domElement renderer)]
                 (controls/configure-controls! camera controls preserved-view)
                 (let [control-change-listener (controls/control-change-listener this
                                                                                  camera
                                                                                  controls
                                                                                  render!)
                       scene-data (scene-graph/assemble-board-scene!
                                   {:scene scene
                                    :loader loader
                                    :render! render!
                                    :active? active?
                                    :cells cells
                                    :board-pieces board-pieces
                                    :card-icon-mode card-icon-mode
                                    :callbacks callbacks})
                       pointer-listeners (pointer/install-board-pointer-listeners!
                                          {:canvas canvas
                                           :camera camera
                                           :controls controls
                                           :card-meshes (:card-meshes scene-data)
                                           :object-meshes (:object-meshes scene-data)
                                           :target-meshes (:target-meshes scene-data)
                                           :legal-targets-ref legal-targets-ref
                                           :drag-enabled? (:pointer-drag-enabled?
                                                           direct-manipulation)
                                           :callbacks callbacks
                                           :assoc-state! #(assoc-component-state! this %1 %2)})
                       antialias-enabled? (resources/renderer-antialias-enabled? renderer)]
                   (.addEventListener controls "change" control-change-listener)
                   (.addEventListener js/window "resize" resize!)
                   (resize!)
                   (r/replace-state this (merge {:renderer renderer
                                                 :resize-listener resize!
                                                 :camera camera
                                                 :controls controls
                                                 :control-change-listener control-change-listener
                                                 :active? active?
                                                 :render! render!
                                                 :geometries (:geometries scene-data)
                                                 :materials (:materials scene-data)
                                                 :textures (:textures scene-data)
                                                 :selection-meshes (:selection-meshes scene-data)
                                                 :wasteland-selection-meshes (:wasteland-selection-meshes scene-data)
                                                 :piece-selection-meshes (:piece-selection-meshes scene-data)
                                                 :placement-preview (:placement-preview scene-data)
                                                 :legal-targets-ref legal-targets-ref
                                                 :piece-edge-outline-count (:piece-edge-outline-count scene-data)
                                                 :antialias-enabled? antialias-enabled?
                                                 :keyboard-pan-bounds (controls/keyboard-pan-bounds cells)}
                                                pointer-listeners
                                                (controls/camera-view-metadata camera controls)))
                   (set-placement-preview! this _move-preview)
                   (set-selection! this selected-index legal-targets)))))))))))
