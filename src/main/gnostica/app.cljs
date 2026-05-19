(ns gnostica.app
  (:require [gnostica.board :as board]
            [gnostica.cards :as cards]
            [re-frame.core :as rf]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

(rf/reg-event-db
 ::initialize
 (fn [_ _]
   {:board (board/initial-board cards/deck)
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

(rf/reg-sub
 ::board
 (fn [db _]
   (:board db)))

(rf/reg-sub
 ::selected-board-index
 (fn [db _]
   (:selected-board-index db)))

(rf/reg-sub
 ::three-texture-errors
 (fn [db _]
   (:three-texture-errors db)))

(rf/reg-sub
 ::selected-board-cell
 :<- [::board]
 :<- [::selected-board-index]
 (fn [[board selected-index] _]
   (get board selected-index)))

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
(def card-step
  (+ (/ (+ card-short card-long) 2) card-gap))

(def board-plane-size
  (+ (* 2 card-step) card-long (* 2 card-gap)))

(defn- card-position [{:keys [row col]}]
  [(* (- col 1) card-step)
   (* (- 1 row) card-step)])

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

(defn- dispose-three-board! [this]
  (let [{:keys [renderer resize-listener controls control-change-listener active? geometries materials textures]} (r/state this)]
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

(defn- add-card-plane! [scene loader render! active? geometries materials textures {:keys [orientation card] :as cell}]
  (let [{:keys [image title]} card
        geometry (js/THREE.PlaneGeometry. card-short card-long)
        material (js/THREE.MeshBasicMaterial. #js {:color 0xffffff
                                                   :side js/THREE.DoubleSide})
        mesh (js/THREE.Mesh. geometry material)
        [x y] (card-position cell)]
    (.set (.-position mesh) x y 0.02)
    (when (= :landscape orientation)
      (set! (.. mesh -rotation -z) (/ js/Math.PI 2)))
    (.add scene mesh)
    (swap! geometries conj geometry)
    (swap! materials conj material)
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
    (let [[_ cells] (r/argv this)
          mount-node (.-boardMountNode ^js this)]
      (when mount-node
        (let [scene (js/THREE.Scene.)
              camera (js/THREE.PerspectiveCamera. 45 1 0.1 100)
              renderer (js/THREE.WebGLRenderer. #js {:antialias true})
              loader (js/THREE.TextureLoader.)
              active? (atom true)
              geometries (atom [])
              materials (atom [])
              textures (atom [])]
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
                        (render!))))]
            (let [controls (js/THREE.OrbitControls. camera (.-domElement renderer))
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
              (add-table-plane! scene geometries materials)
              (doseq [cell cells]
                (add-card-plane! scene loader render! active? geometries materials textures cell))
              (.addEventListener js/window "resize" resize!)
              (resize!)
              (r/replace-state this {:renderer renderer
                                     :resize-listener resize!
                                     :controls controls
                                     :control-change-listener control-change-listener
                                     :active? active?
                                     :geometries @geometries
                                     :materials @materials
                                     :textures @textures}))))))))

(def three-board-scene
  (r/create-class
   {:display-name "three-board-scene"
    :component-did-mount mount-three-board!
    :component-did-update
    (fn [this old-argv _ _]
      (let [old-cells (second old-argv)
            new-cells (second (r/argv this))]
        (when (not= old-cells new-cells)
          (mount-three-board! this))))
    :component-will-unmount dispose-three-board!
    :reagent-render
    (fn [_cells texture-errors]
      (let [component (r/current-component)]
        [:div.board-three
         {:role "img"
          :aria-label "Three-dimensional Gnostica board with nine face-up tarot territory cards"}
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

(defn board-card [{:keys [index row col orientation card]} selected?]
  (let [{:keys [image title]} card]
    [:button.board-card
     {:type "button"
      :class (str "is-" (name orientation)
                  " is-row-" row
                  " is-col-" col
                  (when selected? " is-selected"))
      :aria-label (str title ", " (orientation-label orientation) ", row " (inc row) ", column " (inc col))
      :on-click #(rf/dispatch [::select-board-card index])}
     [:img {:src image
            :alt title
            :draggable "false"}]]))

(defn board-stage []
  (let [cells @(rf/subscribe [::board])
        selected-index @(rf/subscribe [::selected-board-index])
        texture-errors @(rf/subscribe [::three-texture-errors])]
    [:section.board-area
     {:data-three-revision (or (three-revision) "unavailable")}
     (if (and (three-runtime) (orbit-controls-runtime))
       [three-board-scene cells texture-errors]
       [:div.board-fallback
        [:p.board-3d-status.is-error
         (if (three-runtime)
           "Three.js OrbitControls are unavailable; check the pinned CDN control script before /js/main.js."
           "Three.js is unavailable; check the pinned CDN script before /js/main.js.")]
        [:div.board-stage
         {:role "group"
          :aria-label "Gnostica board"}
         (for [cell cells]
           ^{:key (:index cell)}
           [board-card cell (= selected-index (:index cell))])]])]))

(defn territory-panel []
  (let [{:keys [row col orientation card]} @(rf/subscribe [::selected-board-cell])
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
       [:dd (str "Row " (inc row) ", Column " (inc col))]]]]))

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
