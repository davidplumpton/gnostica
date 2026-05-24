(ns gnostica.three-board.pointer
  (:require [gnostica.three-board.resources :as resources]))

(def click-threshold 8)
(def board-index-user-data-key "gnosticaBoardIndex")

(defn mark-board-index! [mesh index]
  (aset (.-userData mesh) board-index-user-data-key index)
  mesh)

(defn- pointer-event->board-pointer! [pointer target event]
  (let [rect (.getBoundingClientRect target)
        width (.-width rect)
        height (.-height rect)]
    (when (and (pos? width) (pos? height))
      (.set pointer
            (- (* 2 (/ (- (.-clientX event) (.-left rect)) width)) 1)
            (- 1 (* 2 (/ (- (.-clientY event) (.-top rect)) height))))
      true)))

(defn- board-index-at!
  [raycaster pointer camera card-meshes canvas event]
  (when (pointer-event->board-pointer! pointer canvas event)
    (.setFromCamera raycaster pointer camera)
    (let [intersections (.intersectObjects raycaster card-meshes false)
          intersection (aget intersections 0)
          picked-object (some-> intersection (aget "object"))
          picked-index (some-> picked-object
                               (.-userData)
                               (aget board-index-user-data-key))]
      (when (number? picked-index)
        picked-index))))

(defn install-card-pointer-listeners!
  [{:keys [canvas camera card-meshes callbacks assoc-state!]}]
  (let [raycaster (js/THREE.Raycaster.)
        pointer (js/THREE.Vector2.)
        pointer-down (atom nil)
        card-mesh-array (to-array card-meshes)
        board-index-at! #(board-index-at! raycaster pointer camera card-mesh-array canvas %)
        select-card-at! (fn [event]
                          (when-let [picked-index (board-index-at! event)]
                            (resources/invoke-callback callbacks :on-card-select picked-index)))
        hover-card-at! #(assoc-state! :hovered-index (board-index-at! %))
        pointer-down-listener (fn [event]
                                (reset! pointer-down {:id (.-pointerId event)
                                                      :x (.-clientX event)
                                                      :y (.-clientY event)}))
        pointer-up-listener (fn [event]
                              (when-let [{:keys [id x y]} @pointer-down]
                                (reset! pointer-down nil)
                                (let [distance (js/Math.hypot (- (.-clientX event) x)
                                                              (- (.-clientY event) y))]
                                  (when (and (= id (.-pointerId event))
                                             (<= distance click-threshold))
                                    (select-card-at! event)))))
        pointer-cancel-listener (fn [_]
                                  (reset! pointer-down nil))
        pointer-leave-listener (fn [_]
                                 (assoc-state! :hovered-index nil))]
    (.addEventListener canvas "pointerdown" pointer-down-listener)
    (.addEventListener canvas "pointerup" pointer-up-listener)
    (.addEventListener canvas "pointercancel" pointer-cancel-listener)
    (.addEventListener canvas "pointermove" hover-card-at!)
    (.addEventListener canvas "pointerleave" pointer-leave-listener)
    {:pointer-down-listener pointer-down-listener
     :pointer-up-listener pointer-up-listener
     :pointer-cancel-listener pointer-cancel-listener
     :pointer-move-listener hover-card-at!
     :pointer-leave-listener pointer-leave-listener}))

(defn focus-board-on-pointer-down! [event]
  (let [target (.-target event)
        class-list (some-> target .-classList)]
    (when (and class-list
               (.contains class-list "board-three__canvas"))
      (.focus (.-currentTarget event)))))
