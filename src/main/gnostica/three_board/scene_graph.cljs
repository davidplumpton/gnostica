(ns gnostica.three-board.scene-graph
  (:require [gnostica.board-layout :as layout]
            [gnostica.icons :as icons]
            [gnostica.pieces :as pieces]
            [gnostica.three-board.pointer :as pointer]
            [gnostica.three-board.resources :as resources]
            [gnostica.three-card-textures :as card-textures]))

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
  (resources/invoke-callback callbacks :on-texture-error image)
  (render!))

(defn- mark-card-renderer-error! [material callbacks message render!]
  (js/console.error message)
  (.set (.-color material) 0xff8a7a)
  (set! (.-needsUpdate material) true)
  (resources/invoke-callback callbacks :on-renderer-error message)
  (render!))

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

(defn visible-piece-count [board-pieces]
  (reduce (fn [total [_ space-pieces]]
            (+ total (count (layout/visible-piece-slots space-pieces))))
          0
          (pieces/pieces-by-space board-pieces)))

(defn- show-card-icon-overlays? [card-icon-mode]
  (not= :popup card-icon-mode))

(defn- add-card-plane!
  [{:keys [scene
           loader
           render!
           active?
           geometries
           materials
           textures
           card-meshes
           selection-meshes
           card-icon-mode
           callbacks]}
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
    (pointer/mark-board-index! mesh index)
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

(defn assemble-board-scene!
  [{:keys [scene loader render! active? cells board-pieces card-icon-mode callbacks]}]
  (let [geometries (atom [])
        materials (atom [])
        textures (atom [])
        card-meshes (atom [])
        selection-meshes (atom {})
        card-context {:scene scene
                      :loader loader
                      :render! render!
                      :active? active?
                      :geometries geometries
                      :materials materials
                      :textures textures
                      :card-meshes card-meshes
                      :selection-meshes selection-meshes
                      :card-icon-mode card-icon-mode
                      :callbacks callbacks}]
    (add-piece-lights! scene)
    (let [wastelands (layout/wasteland-spaces cells)
          board-spaces (vec (concat cells wastelands))]
      (add-table-plane! scene geometries materials board-spaces)
      (add-wasteland-outlines! scene geometries materials wastelands))
    (doseq [cell cells]
      (add-card-plane! card-context cell))
    (let [piece-edge-outline-count (add-piece-meshes! scene
                                                       geometries
                                                       materials
                                                       cells
                                                       board-pieces)]
      {:geometries @geometries
       :materials @materials
       :textures @textures
       :card-meshes @card-meshes
       :selection-meshes @selection-meshes
       :piece-edge-outline-count piece-edge-outline-count})))
