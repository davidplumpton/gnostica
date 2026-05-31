(ns gnostica.three-board.pointer
  (:require [gnostica.gesture-input :as gesture-input]
            [gnostica.three-board.resources :as resources]))

(def click-threshold 8)
(def board-index-user-data-key "gnosticaBoardIndex")
(def gesture-object-user-data-key "gnosticaGestureObject")

(defn mark-board-index! [mesh index]
  (aset (.-userData mesh) board-index-user-data-key index)
  mesh)

(defn mark-gesture-object! [mesh object]
  (aset (.-userData mesh) gesture-object-user-data-key object)
  mesh)

(defn- gesture-object [mesh]
  (some-> mesh
          (.-userData)
          (aget gesture-object-user-data-key)))

(defn- pointer-event->board-pointer! [pointer target event]
  (let [rect (.getBoundingClientRect target)
        width (.-width rect)
        height (.-height rect)]
    (when (and (pos? width) (pos? height))
      (.set pointer
            (- (* 2 (/ (- (.-clientX event) (.-left rect)) width)) 1)
            (- 1 (* 2 (/ (- (.-clientY event) (.-top rect)) height))))
      true)))

(defn- picked-mesh-at!
  [raycaster pointer camera meshes canvas event]
  (when (pointer-event->board-pointer! pointer canvas event)
    (.setFromCamera raycaster pointer camera)
    (some-> (.intersectObjects raycaster meshes false)
            (aget 0)
            (aget "object"))))

(defn- board-index-at!
  [raycaster pointer camera card-meshes canvas event]
  (let [picked-object (picked-mesh-at! raycaster pointer camera card-meshes canvas event)
        picked-index (some-> picked-object
                             (.-userData)
                             (aget board-index-user-data-key))]
    (when (number? picked-index)
      picked-index)))

(defn- gesture-object-at!
  [raycaster pointer camera object-meshes canvas event]
  (some-> (picked-mesh-at! raycaster pointer camera object-meshes canvas event)
          gesture-object))

(defn- descriptors-by [key descriptors]
  (into {}
        (map (juxt key identity))
        descriptors))

(defn- descriptor-for-object [legal-targets object]
  (case (:kind object)
    :territory
    (get (descriptors-by :board-index (:territories legal-targets))
         (:board-index object))

    :piece
    (get (descriptors-by :piece-id (:pieces legal-targets))
         (:piece-id object))

    :wasteland
    (get (into {}
               (map (fn [{:keys [row col] :as descriptor}]
                      [[row col] descriptor]))
               (:wastelands legal-targets))
         [(:row object) (:col object)])

    nil))

(defn- source-input-for-object [legal-targets object]
  (case (:kind object)
    :territory
    (gesture-input/territory-drag-input
     {:index (:board-index object)}
     (descriptor-for-object legal-targets object))

    :piece
    (when-let [descriptor (descriptor-for-object legal-targets object)]
      (gesture-input/piece-drag-input (:piece descriptor) descriptor))

    nil))

(defn- target-for-object [object]
  (case (:kind object)
    :territory (gesture-input/territory-target (:board-index object))
    :piece (gesture-input/piece-target (:piece-id object))
    :wasteland (gesture-input/wasteland-target (:row object) (:col object))
    nil))

(defn- preview-for [legal-targets source-object target-object]
  (let [descriptor (when target-object
                     (descriptor-for-object legal-targets target-object))]
    (cond-> {:active? true
             :source source-object
             :target target-object}
      descriptor
      (assoc :target-status (:status descriptor)
             :target-enabled? (:enabled? descriptor)
             :target-reason (or (:reason descriptor)
                                (get-in descriptor [:error :message]))))))

(defn- event-position [canvas event]
  (let [rect (.getBoundingClientRect canvas)]
    {:x (- (.-clientX event) (.-left rect))
     :y (- (.-clientY event) (.-top rect))}))

(defn- preview-at [legal-targets source-object target-object canvas event]
  (assoc (preview-for legal-targets source-object target-object)
         :pointer (event-position canvas event)))

(defn- pointer-distance [event {:keys [x y]}]
  (js/Math.hypot (- (.-clientX event) x)
                 (- (.-clientY event) y)))

(defn- set-controls-enabled! [controls enabled?]
  (when controls
    (set! (.-enabled controls) enabled?)))

(defn- stop-object-pointer-event! [event]
  (.preventDefault event)
  (.stopPropagation event))

(defn install-board-pointer-listeners!
  [{:keys [canvas camera controls card-meshes object-meshes target-meshes
           legal-targets-ref drag-enabled? callbacks assoc-state!]}]
  (let [raycaster (js/THREE.Raycaster.)
        pointer (js/THREE.Vector2.)
        pointer-down (atom nil)
        card-mesh-array (to-array card-meshes)
        object-mesh-array (to-array object-meshes)
        target-mesh-array (to-array target-meshes)
        board-index-at! #(board-index-at! raycaster pointer camera card-mesh-array canvas %)
        source-object-at! #(gesture-object-at! raycaster pointer camera object-mesh-array canvas %)
        target-object-at! #(gesture-object-at! raycaster pointer camera target-mesh-array canvas %)
        current-legal-targets #(or (some-> legal-targets-ref deref) {})
        select-card-at! (fn [event]
                          (when-let [picked-index (board-index-at! event)]
                            (resources/invoke-callback callbacks :on-card-select picked-index)))
        dispatch-gesture! (fn [input]
                            (resources/invoke-callback callbacks :on-gesture-intent input))
        clear-drag! (fn []
                      (assoc-state! :drag-preview nil)
                      (set-controls-enabled! controls true))
        hover-card-at! #(when-not @pointer-down
                          (assoc-state! :hovered-index (board-index-at! %)))
        pointer-down-listener (fn [event]
                                (let [click-state {:id (.-pointerId event)
                                                   :x (.-clientX event)
                                                   :y (.-clientY event)
                                                   :selection-only? true}]
                                  (if-let [source-object (when drag-enabled?
                                                           (source-object-at! event))]
                                    (if-let [input (source-input-for-object (current-legal-targets)
                                                                            source-object)]
                                      (do
                                        (stop-object-pointer-event! event)
                                        (set-controls-enabled! controls false)
                                        (when (.-setPointerCapture canvas)
                                          (.setPointerCapture canvas (.-pointerId event)))
                                        (reset! pointer-down {:id (.-pointerId event)
                                                              :x (.-clientX event)
                                                              :y (.-clientY event)
                                                              :source-object source-object
                                                              :input input
                                                              :dragging? false}))
                                      (reset! pointer-down click-state))
                                    (reset! pointer-down click-state))))
        pointer-move-listener (fn [event]
                                (if-let [{:keys [input source-object dragging?
                                                 selection-only?]
                                          :as state} @pointer-down]
                                  (if selection-only?
                                    nil
                                    (do
                                      (stop-object-pointer-event! event)
                                      (let [distance (pointer-distance event state)
                                            dragging-now? (or dragging?
                                                              (< click-threshold distance))
                                            target-object (when dragging-now?
                                                            (target-object-at! event))]
                                        (when (and dragging-now? (not dragging?))
                                          (dispatch-gesture! input))
                                        (when dragging-now?
                                          (swap! pointer-down assoc :dragging? true)
                                          (assoc-state! :drag-preview
                                                        (preview-at (current-legal-targets)
                                                                    source-object
                                                                    target-object
                                                                    canvas
                                                                    event))))))
                                  (hover-card-at! event)))
        pointer-up-listener (fn [event]
                              (when-let [{:keys [id input source-object dragging?
                                                 selection-only?]
                                          :as state}
                                         @pointer-down]
                                (when-not selection-only?
                                  (stop-object-pointer-event! event))
                                (reset! pointer-down nil)
                                (when (and (not selection-only?)
                                           (.-releasePointerCapture canvas))
                                  (try
                                    (.releasePointerCapture canvas (.-pointerId event))
                                    (catch :default _
                                      nil)))
                                (when-not selection-only?
                                  (clear-drag!))
                                (let [distance (pointer-distance event state)
                                      same-pointer? (= id (.-pointerId event))]
                                  (cond
                                    (and same-pointer?
                                         selection-only?
                                         (<= distance click-threshold))
                                    (select-card-at! event)

                                    (and same-pointer? dragging?)
                                    (let [target-object (target-object-at! event)
                                          target (target-for-object target-object)]
                                      (dispatch-gesture! (cond-> input
                                                           target
                                                           (assoc :target target))))

                                    (and same-pointer?
                                         (<= distance click-threshold)
                                         (= :territory (:kind source-object)))
                                    (select-card-at! event)

                                    (and same-pointer?
                                         (<= distance click-threshold)
                                         input)
                                    (dispatch-gesture! input)))))
        pointer-cancel-listener (fn [_]
                                  (reset! pointer-down nil)
                                  (clear-drag!))
        pointer-leave-listener (fn [_]
                                 (assoc-state! :hovered-index nil))
        external-drag-over-listener (fn [event]
                                      (when (and drag-enabled?
                                                 (gesture-input/gesture-data-transfer?
                                                  (.-dataTransfer event)))
                                        (.preventDefault event)
                                        (.stopPropagation event)
                                        (when-let [data-transfer (.-dataTransfer event)]
                                          (set! (.-dropEffect data-transfer) "move"))
                                        (when-let [input (gesture-input/gesture-input-from-data-transfer
                                                         (.-dataTransfer event))]
                                          (assoc-state! :drag-preview
                                                        (preview-at (current-legal-targets)
                                                                    (:source input)
                                                                    (target-object-at! event)
                                                                    canvas
                                                                    event)))))
        external-drop-listener (fn [event]
                                 (when-let [input (and drag-enabled?
                                                       (gesture-input/gesture-data-transfer?
                                                        (.-dataTransfer event))
                                                       (gesture-input/gesture-input-from-data-transfer
                                                        (.-dataTransfer event)))]
                                   (.preventDefault event)
                                   (.stopPropagation event)
                                   (let [target (target-for-object (target-object-at! event))]
                                     (dispatch-gesture! (cond-> input
                                                          target
                                                          (assoc :target target))))
                                   (gesture-input/clear-active-gesture-input!)
                                   (clear-drag!)))
        external-drag-leave-listener (fn [_]
                                      (clear-drag!))]
    (.addEventListener canvas "pointerdown" pointer-down-listener true)
    (.addEventListener canvas "pointerup" pointer-up-listener true)
    (.addEventListener canvas "pointercancel" pointer-cancel-listener true)
    (.addEventListener canvas "pointermove" pointer-move-listener true)
    (.addEventListener canvas "pointerleave" pointer-leave-listener true)
    (.addEventListener canvas "dragover" external-drag-over-listener true)
    (.addEventListener canvas "drop" external-drop-listener true)
    (.addEventListener canvas "dragleave" external-drag-leave-listener true)
    {:pointer-down-listener pointer-down-listener
     :pointer-up-listener pointer-up-listener
     :pointer-cancel-listener pointer-cancel-listener
     :pointer-move-listener pointer-move-listener
     :pointer-leave-listener pointer-leave-listener
     :external-drag-over-listener external-drag-over-listener
     :external-drop-listener external-drop-listener
     :external-drag-leave-listener external-drag-leave-listener
     :pointer-listener-capture? true}))

(def install-card-pointer-listeners! install-board-pointer-listeners!)

(defn focus-board-on-pointer-down! [event]
  (let [target (.-target event)
        class-list (some-> target .-classList)]
    (when (and class-list
               (.contains class-list "board-three__canvas"))
      (.focus (.-currentTarget event)))))
