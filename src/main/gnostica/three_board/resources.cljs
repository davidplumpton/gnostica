(ns gnostica.three-board.resources
  (:require [gnostica.gesture-input :as gesture-input]))

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
           external-drag-over-listener
           external-drop-listener
           external-drag-leave-listener
           external-pointer-move-listener
           external-pointer-drop-listener
           external-pointer-cancel-listener
           orientation-change-listener
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
      (when external-drag-over-listener
        (.removeEventListener canvas "dragover" external-drag-over-listener pointer-listener-capture?))
      (when external-drop-listener
        (.removeEventListener canvas "drop" external-drop-listener pointer-listener-capture?))
      (when external-drag-leave-listener
        (.removeEventListener canvas "dragleave" external-drag-leave-listener pointer-listener-capture?))
      (when external-pointer-move-listener
        (.removeEventListener canvas
                              gesture-input/pointer-drag-move-event
                              external-pointer-move-listener
                              pointer-listener-capture?))
      (when external-pointer-drop-listener
        (.removeEventListener canvas
                              gesture-input/pointer-drag-drop-event
                              external-pointer-drop-listener
                              pointer-listener-capture?))
      (when external-pointer-cancel-listener
        (.removeEventListener js/window
                              gesture-input/pointer-drag-cancel-event
                              external-pointer-cancel-listener
                              pointer-listener-capture?))
      (when orientation-change-listener
        (.removeEventListener js/window
                              gesture-input/orientation-change-event
                              orientation-change-listener
                              pointer-listener-capture?))
      (when parent
        (.removeChild parent canvas)))
    (.dispose renderer)))
