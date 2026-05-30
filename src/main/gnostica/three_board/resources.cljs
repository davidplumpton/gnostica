(ns gnostica.three-board.resources)

(def renderer-antialias-requested? true)

(defn invoke-callback [callbacks key & args]
  (when-let [callback (get callbacks key)]
    (apply callback args)))

(defn create-renderer [callbacks]
  (try
    (js/THREE.WebGLRenderer. #js {:antialias renderer-antialias-requested?})
    (catch js/Error error
      (js/console.error "Three.js WebGL renderer is unavailable." error)
      (invoke-callback callbacks :on-renderer-error (.-message error))
      nil)))

(defn renderer-antialias-enabled? [renderer]
  (try
    (let [context (.getContext renderer)
          attributes (when context
                       (.getContextAttributes context))]
      (boolean (and attributes (.-antialias attributes))))
    (catch :default _
      false)))

(defn dispose-board!
  [{:keys [renderer
           resize-listener
           controls
           control-change-listener
           pointer-down-listener
           pointer-up-listener
           pointer-cancel-listener
           pointer-move-listener
           pointer-leave-listener
           pointer-listener-capture?
           active?
           geometries
           materials
           textures]}]
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
        (.removeEventListener canvas "pointerdown" pointer-down-listener pointer-listener-capture?))
      (when pointer-up-listener
        (.removeEventListener canvas "pointerup" pointer-up-listener pointer-listener-capture?))
      (when pointer-cancel-listener
        (.removeEventListener canvas "pointercancel" pointer-cancel-listener pointer-listener-capture?))
      (when pointer-move-listener
        (.removeEventListener canvas "pointermove" pointer-move-listener pointer-listener-capture?))
      (when pointer-leave-listener
        (.removeEventListener canvas "pointerleave" pointer-leave-listener pointer-listener-capture?))
      (when parent
        (.removeChild parent canvas)))
    (.dispose renderer)))
