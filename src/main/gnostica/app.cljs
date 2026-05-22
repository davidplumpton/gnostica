(ns gnostica.app
  (:require [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.pieces :as pieces]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(rf/reg-event-db
 ::initialize
 (fn [_ _]
   {:board (board/initial-board cards/deck)
    :pieces pieces/initial-pieces
    :selected-board-index 0}))

(rf/reg-event-db
 ::select-board-card
 (fn [db [_ index]]
   (assoc db :selected-board-index index)))

(rf/reg-event-db
 ::clear-three-texture-errors
 (fn [db _]
   (assoc db :three-texture-errors [])))

(rf/reg-event-db
 ::three-texture-error
 (fn [db [_ image]]
   (update db :three-texture-errors (fnil conj []) image)))

(rf/reg-event-db
 ::three-renderer-error
 (fn [db [_ message]]
   (assoc db :three-renderer-error message)))

(rf/reg-sub
 ::board
 (fn [db _]
   (:board db)))

(rf/reg-sub
 ::pieces
 (fn [db _]
   (:pieces db)))

(rf/reg-sub
 ::selected-board-index
 (fn [db _]
   (:selected-board-index db)))

(rf/reg-sub
 ::three-texture-errors
 (fn [db _]
   (:three-texture-errors db)))

(rf/reg-sub
 ::three-renderer-error
 (fn [db _]
   (:three-renderer-error db)))

(rf/reg-sub
 ::selected-board-cell
 :<- [::board]
 :<- [::selected-board-index]
 (fn [[board selected-index] _]
   (get board selected-index)))

(rf/reg-sub
 ::selected-board-pieces
 :<- [::pieces]
 :<- [::selected-board-index]
 (fn [[pieces selected-index] _]
   (pieces/pieces-for-space pieces selected-index)))

(defn orientation-label [orientation]
  (case orientation
    :portrait "Portrait"
    :landscape "Landscape"
    "Unknown"))

(defn three-runtime []
  (when (exists? js/THREE)
    js/THREE))

(defn three-revision []
  (some-> (three-runtime) (.-REVISION)))

(defn orbit-controls-runtime []
  (when-let [three (three-runtime)]
    (.-OrbitControls three)))

(def card-short 1)
(def card-long 1.5)
(def card-gap 0.14)
(def selected-card-padding 0.14)
(def pointer-click-threshold 8)
(def board-index-user-data-key "gnosticaBoardIndex")
(def piece-surface-z 0.07)
;; Cardinal pyramids lie on one triangular side face rather than balancing on an edge.
(def piece-side-face-roll (/ js/Math.PI 4))
(def card-step
  (+ (/ (+ card-short card-long) 2) card-gap))

(def board-plane-size
  (+ (* 2 card-step) card-long (* 2 card-gap)))

(defn- card-position [{:keys [row col]}]
  [(* (- col 1) card-step)
   (* (- 1 row) card-step)])

(defn- piece-slot-offset [slot piece-count]
  (get (case piece-count
         1 [[0 -0.03]]
         2 [[-0.17 0.11] [0.17 -0.11]]
         [[-0.21 -0.17] [0.21 -0.17] [0 0.18]])
       slot
       [0 0]))

(defn- piece-compass-z-rotation [orientation]
  (case orientation
    :north 0
    :east (- (/ js/Math.PI 2))
    :south js/Math.PI
    :west (/ js/Math.PI 2)
    nil))

(defn- piece-tilt-axis [orientation]
  (let [axis (js/THREE.Vector3.)]
    (case orientation
      :north (.set axis -1 0 0)
      :east (.set axis 0 1 0)
      :south (.set axis 1 0 0)
      :west (.set axis 0 -1 0)
      nil)
    axis))

(defn- piece-center-z [piece-size orientation]
  (+ piece-surface-z
     (if (= :up orientation)
       (/ (:height piece-size) 2)
       (pieces/lying-center-height-above-surface piece-size))))

(defn- set-piece-rotation! [mesh orientation piece-size]
  (cond
    (= :up orientation)
    (set! (.. mesh -rotation -x) (/ js/Math.PI 2))

    (contains? pieces/cardinal-orientations orientation)
    (do
      (.rotateZ mesh (piece-compass-z-rotation orientation))
      (.rotateY mesh piece-side-face-roll)
      (.rotateOnWorldAxis mesh
                          (piece-tilt-axis orientation)
                          (pieces/lying-correction-angle piece-size))))
  mesh)

(defn- texture-cover! [texture]
  (let [image (.-image texture)
        image-width (.-width image)
        image-height (.-height image)
        card-aspect (/ card-short card-long)]
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

(defn- set-three-selection! [this selected-index]
  (let [{:keys [active? render! selection-meshes]} (r/state this)]
    (when (and active? @active? selection-meshes)
      (doseq [[index mesh] selection-meshes]
        (set! (.-visible mesh) (= index selected-index)))
      (when render!
        (render!)))))

(defn- dispose-three-board! [this]
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
  (let [geometry (js/THREE.PlaneGeometry. board-plane-size board-plane-size)
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
  [scene geometries materials cells slot piece-count piece]
  (when-let [cell (get cells (:space-index piece))]
    (let [piece-size (pieces/size-data piece)
          {:keys [radius height]} piece-size
          player (pieces/player-for piece)
          geometry (js/THREE.ConeGeometry. radius height 4)
          material (js/THREE.MeshLambertMaterial.
                    #js {:color (or (:color player) 0xffffff)})
          mesh (js/THREE.Mesh. geometry material)
          [card-x card-y] (card-position cell)
          [offset-x offset-y] (piece-slot-offset slot piece-count)
          z (piece-center-z piece-size (:orientation piece))]
      (.set (.-position mesh) (+ card-x offset-x) (+ card-y offset-y) z)
      (set-piece-rotation! mesh (:orientation piece) piece-size)
      (.add scene mesh)
      (swap! geometries conj geometry)
      (swap! materials conj material))))

(defn- add-piece-meshes! [scene geometries materials cells board-pieces]
  (doseq [[_ space-pieces] (pieces/pieces-by-space board-pieces)
          [slot piece] (map-indexed vector (take pieces/max-pieces-per-space space-pieces))]
    (add-piece-mesh! scene geometries materials cells slot (count space-pieces) piece)))

(defn- create-three-renderer []
  (try
    (js/THREE.WebGLRenderer. #js {:antialias true})
    (catch js/Error error
      (js/console.error "Three.js WebGL renderer is unavailable." error)
      (rf/dispatch [::three-renderer-error (.-message error)])
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
   {:keys [index orientation card] :as cell}]
  (let [{:keys [image title]} card
        selection-geometry (js/THREE.PlaneGeometry.
                            (+ card-short selected-card-padding)
                            (+ card-long selected-card-padding))
        selection-material (js/THREE.MeshBasicMaterial. #js {:color 0x9ff7e7
                                                             :side js/THREE.DoubleSide})
        selection-mesh (js/THREE.Mesh. selection-geometry selection-material)
        geometry (js/THREE.PlaneGeometry. card-short card-long)
        material (js/THREE.MeshBasicMaterial. #js {:color 0xffffff
                                                   :side js/THREE.DoubleSide})
        mesh (js/THREE.Mesh. geometry material)
        [x y] (card-position cell)]
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
                             (rf/dispatch [::three-texture-error image])
                             (render!))))]
      (swap! textures conj texture))))

(defn- reset-three-board-view! [this]
  (when-let [controls (:controls (r/state this))]
    (.reset controls)
    (.update controls)))

(defn- mount-three-board! [this]
  (dispose-three-board! this)
  (rf/dispatch-sync [::clear-three-texture-errors])
  (when (and (three-runtime) (orbit-controls-runtime))
    (let [[_ cells board-pieces selected-index] (r/argv this)
          mount-node (.-boardMountNode ^js this)]
      (when mount-node
        (let [scene (js/THREE.Scene.)
              camera (js/THREE.PerspectiveCamera. 45 1 0.1 100)
              renderer (create-three-renderer)
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
                            (rf/dispatch [::select-board-card picked-index])))))
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
                (set-three-selection! this selected-index)))))))))

(def three-board-scene
  (r/create-class
   {:display-name "three-board-scene"
    :component-did-mount mount-three-board!
    :component-did-update
    (fn [this old-argv _ _]
      (let [[_ old-cells old-pieces old-selected-index] old-argv
            [_ new-cells new-pieces new-selected-index] (r/argv this)]
        (if (or (not= old-cells new-cells)
                (not= old-pieces new-pieces))
          (mount-three-board! this)
          (when (not= old-selected-index new-selected-index)
            (set-three-selection! this new-selected-index)))))
    :component-will-unmount dispose-three-board!
    :reagent-render
    (fn [_cells _pieces _selected-index texture-errors]
      (let [component (r/current-component)]
        [:div.board-three
         {:role "img"
          :aria-label "Three-dimensional Gnostica board with nine face-up tarot territory cards and Icehouse pieces"}
         [:div.board-three__mount
          {:aria-hidden "true"
           :ref #(set! (.-boardMountNode ^js component) %)}]
         [:button.board-three__reset
          {:type "button"
           :on-click #(reset-three-board-view! component)}
          "Reset view"]
         (when (seq texture-errors)
           [:p.board-3d-status.is-error
            (str "Texture load failed for "
                 (count texture-errors)
                 " card"
                 (when (not= 1 (count texture-errors)) "s")
                 ". Check the console for image paths.")])]))}))

(defn- piece-summary [piece]
  (let [player (pieces/player-for piece)]
    (str (:name player)
         " "
         (pieces/size-label (:size piece))
         ", "
         (pieces/orientation-label (:orientation piece)))))

(defn- board-piece-marker [slot piece]
  (let [pips (pieces/pips piece)
        player (pieces/player-for piece)]
    ^{:key (:id piece)}
    [:span.board-piece
     {:class (str "is-slot-" slot
                  " is-" (name (:size piece))
                  " is-" (name (:orientation piece)))
      :style {"--piece-color" (:css-color player)}}
     [:span.board-piece__body]
     [:span.board-piece__pips
      (for [pip (range pips)]
        ^{:key pip}
        [:span.board-piece__pip])]]))

(defn board-card [{:keys [index row col orientation card]} selected? board-pieces]
  (let [{:keys [image title]} card]
    [:button.board-card
     {:type "button"
      :class (str "is-" (name orientation)
                  " is-row-" row
                  " is-col-" col
                  (when selected? " is-selected"))
      :aria-label (str title
                       ", "
                       (orientation-label orientation)
                       ", row "
                       (inc row)
                       ", column "
                       (inc col)
                       (when (seq board-pieces)
                         (str ", pieces: "
                              (apply str (interpose "; " (map piece-summary board-pieces))))))
      :on-click #(rf/dispatch [::select-board-card index])}
     [:img {:src image
            :alt title
            :draggable "false"}]
     (when (seq board-pieces)
       [:div.board-card__pieces
        {:aria-hidden "true"}
        (for [[slot piece] (map-indexed vector (take pieces/max-pieces-per-space board-pieces))]
          (board-piece-marker slot piece))])]))

(defn board-stage []
  (let [cells @(rf/subscribe [::board])
        board-pieces @(rf/subscribe [::pieces])
        pieces-by-space (pieces/pieces-by-space board-pieces)
        selected-index @(rf/subscribe [::selected-board-index])
        texture-errors @(rf/subscribe [::three-texture-errors])
        renderer-error @(rf/subscribe [::three-renderer-error])]
    [:section.board-area
     {:data-three-revision (or (three-revision) "unavailable")}
     (if (and (three-runtime) (orbit-controls-runtime) (not renderer-error))
       [three-board-scene cells board-pieces selected-index texture-errors]
       [:div.board-fallback
        [:p.board-3d-status.is-error
         (cond
           renderer-error
           (str "Three.js WebGL rendering is unavailable; using the CSS board. " renderer-error)

           (three-runtime)
           "Three.js OrbitControls are unavailable; check the pinned CDN control script before /js/main.js."

           :else
           "Three.js is unavailable; check the pinned CDN script before /js/main.js.")]
        [:div.board-stage
         {:role "group"
          :aria-label "Gnostica board"}
         (for [cell cells]
           ^{:key (:index cell)}
           [board-card
            cell
            (= selected-index (:index cell))
            (get pieces-by-space (:index cell))])]])]))

(defn territory-panel []
  (let [{:keys [row col orientation card]} @(rf/subscribe [::selected-board-cell])
        selected-pieces @(rf/subscribe [::selected-board-pieces])
        {:keys [title group rank]} card]
    [:aside.territory-panel
     [:p.eyebrow "Territory"]
     [:h1 title]
     [:dl.territory-facts
      [:div
       [:dt "Arcana"]
       [:dd group]]
      (when rank
        [:div
         [:dt "Rank"]
         [:dd rank]])
      [:div
       [:dt "Orientation"]
       [:dd (orientation-label orientation)]]
      [:div
       [:dt "Position"]
       [:dd (str "Row " (inc row) ", Column " (inc col))]]
      [:div
       [:dt "Pieces"]
       [:dd
        (if (seq selected-pieces)
          [:ul.territory-pieces
           (for [piece selected-pieces]
             (let [player (pieces/player-for piece)]
               ^{:key (:id piece)}
               [:li
                [:span.territory-piece-swatch
                 {:style {"--piece-color" (:css-color player)}}]
                [:span (piece-summary piece)]]))]
          "None")]]]]))

(defn app []
  [:<>
   [:header.app-header
    [:div.brand
     [:span.brand__mark "G"]
     [:span.brand__name "Gnostica"]]]
   [:main.app-shell
    [board-stage]
    [territory-panel]]])

(defn mount! []
  (rdom/render [app] (.getElementById js/document "app")))

(defn reload! []
  (mount!))

(defn init []
  (rf/dispatch-sync [::initialize])
  (mount!))
