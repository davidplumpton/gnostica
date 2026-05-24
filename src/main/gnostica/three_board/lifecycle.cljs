(ns gnostica.three-board.lifecycle
  (:require [gnostica.three-board.controls :as controls]
            [gnostica.three-board.pointer :as pointer]
            [gnostica.three-board.resources :as resources]
            [gnostica.three-board.runtime :as runtime]
            [gnostica.three-board.scene-graph :as scene-graph]
            [reagent.core :as r]))

(defn assoc-component-state! [this key value]
  (let [state (r/state this)]
    (when (not= (get state key) value)
      (r/replace-state this (assoc state key value)))))

(defn set-selection! [this selected-index]
  (let [{:keys [active? render! selection-meshes]} (r/state this)]
    (when (and active? @active? selection-meshes)
      (doseq [[index mesh] selection-meshes]
        (set! (.-visible mesh) (= index selected-index)))
      (when render!
        (render!)))))

(defn dispose! [this]
  (resources/dispose-board! (r/state this))
  (r/replace-state this {}))

(defn mount!
  ([this]
   (mount! this nil))
  ([this preserved-view]
   (dispose! this)
   (let [[_ cells board-pieces selected-index card-icon-mode _texture-errors callbacks] (r/argv this)]
     (resources/invoke-callback callbacks :on-clear-texture-errors)
     (when (runtime/available?)
       (when-let [mount-node (.-boardMountNode ^js this)]
         (when-let [renderer (resources/create-renderer callbacks)]
           (let [scene (js/THREE.Scene.)
                 camera (js/THREE.PerspectiveCamera. 45 1 0.1 100)
                 loader (js/THREE.TextureLoader.)
                 active? (atom true)]
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
                       pointer-listeners (pointer/install-card-pointer-listeners!
                                          {:canvas canvas
                                           :camera camera
                                           :card-meshes (:card-meshes scene-data)
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
                                                 :piece-edge-outline-count (:piece-edge-outline-count scene-data)
                                                 :antialias-enabled? antialias-enabled?
                                                 :keyboard-pan-bounds (controls/keyboard-pan-bounds cells)}
                                                pointer-listeners
                                                (controls/camera-view-metadata camera controls)))
                   (set-selection! this selected-index)))))))))))
