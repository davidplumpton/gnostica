(ns gnostica.three-board.pointer
  (:require [gnostica.gesture-input :as gesture-input]
            [gnostica.legal-targets :as legal-targets]
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

(defn- event-client-x [event]
  (or (some-> event .-detail .-clientX)
      (.-clientX event)))

(defn- event-client-y [event]
  (or (some-> event .-detail .-clientY)
      (.-clientY event)))

(defn- pointer-event->board-pointer! [pointer target event]
  (let [rect (.getBoundingClientRect target)
        width (.-width rect)
        height (.-height rect)]
    (when (and (pos? width) (pos? height))
      (.set pointer
            (- (* 2 (/ (- (event-client-x event) (.-left rect)) width)) 1)
            (- 1 (* 2 (/ (- (event-client-y event) (.-top rect)) height))))
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

(defn- descriptor-for-object [legal-targets object]
  (legal-targets/descriptor-for-target legal-targets object))

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

(defn- active-drag-input [fallback]
  (or (gesture-input/active-gesture-input)
      fallback))

(defn- drag-source-object [source-object]
  (if-let [orientation (or (some-> (gesture-input/active-gesture-input)
                                   gesture-input/drag-orientation)
                           (:orientation source-object))]
    (assoc source-object :orientation orientation)
    source-object))

(defn- preview-for [legal-targets source-object target-object]
  (let [descriptor (when target-object
                     (descriptor-for-object legal-targets target-object))]
    (legal-targets/with-target-status
      {:active? true
       :source source-object
       :target target-object}
      descriptor)))

(defn- event-position [canvas event]
  (let [rect (.getBoundingClientRect canvas)]
    {:x (- (event-client-x event) (.-left rect))
     :y (- (event-client-y event) (.-top rect))}))

(defn- preview-at [legal-targets source-object target-object canvas event]
  (assoc (preview-for legal-targets source-object target-object)
         :pointer (event-position canvas event)))

(defn- pointer-distance [event {:keys [x y]}]
  (js/Math.hypot (- (event-client-x event) x)
                 (- (event-client-y event) y)))

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
        selectable-wasteland-target-at! (fn [event]
                                          (when-let [target (target-for-object
                                                             (target-object-at! event))]
                                            (let [descriptor (descriptor-for-object
                                                              (current-legal-targets)
                                                              target)]
                                              (when (and (= :wasteland (:kind target))
                                                         (legal-targets/active? descriptor))
                                                target))))
        select-board-target-at! (fn [event]
                                  (if-let [{:keys [row col]} (selectable-wasteland-target-at!
                                                              event)]
                                    (resources/invoke-callback callbacks
                                                               :on-wasteland-select
                                                               row
                                                               col)
                                    (select-card-at! event)))
        dispatch-gesture! (fn [input]
                            (resources/invoke-callback callbacks :on-gesture-intent input))
        last-drag-preview (atom nil)
        set-drag-preview! (fn [drag-preview]
                            (reset! last-drag-preview drag-preview)
                            (assoc-state! :drag-preview drag-preview))
        clear-drag! (fn []
                      (set-drag-preview! nil)
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
                                          (gesture-input/set-active-gesture-input!
                                           input)
                                          (dispatch-gesture! input))
                                        (when dragging-now?
                                          (swap! pointer-down assoc :dragging? true)
                                          (set-drag-preview!
                                           (preview-at (current-legal-targets)
                                                       (drag-source-object source-object)
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
                                      same-pointer? (= id (.-pointerId event))
                                      input (active-drag-input input)]
                                  (cond
                                    (and same-pointer?
                                         selection-only?
                                         (<= distance click-threshold))
                                    (select-board-target-at! event)

                                    (and same-pointer? dragging?)
                                    (let [target-object (target-object-at! event)
                                          target (target-for-object target-object)]
                                      (dispatch-gesture! (cond-> input
                                                           target
                                                           (assoc :target target))))

                                    (and same-pointer?
                                         (<= distance click-threshold)
                                         (= :territory (:kind source-object)))
                                    (select-board-target-at! event)

                                    (and same-pointer?
                                         (<= distance click-threshold)
                                         input)
                                    (dispatch-gesture! input))
                                  (when dragging?
                                    (gesture-input/clear-active-gesture-input!)))))
        pointer-cancel-listener (fn [_]
                                  (reset! pointer-down nil)
                                  (gesture-input/clear-active-gesture-input!)
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
                                          (let [input (active-drag-input input)]
                                            (set-drag-preview!
                                             (preview-at (current-legal-targets)
                                                         (drag-source-object (:source input))
                                                         (target-object-at! event)
                                                         canvas
                                                         event))))))
        external-drop-listener (fn [event]
                                 (when-let [input (and drag-enabled?
                                                       (gesture-input/gesture-data-transfer?
                                                        (.-dataTransfer event))
                                                       (gesture-input/gesture-input-from-data-transfer
                                                        (.-dataTransfer event)))]
                                   (.preventDefault event)
                                   (.stopPropagation event)
                                   (let [input (active-drag-input input)
                                         target (target-for-object (target-object-at! event))]
                                     (dispatch-gesture! (cond-> input
                                                          target
                                                          (assoc :target target))))
                                   (gesture-input/clear-active-gesture-input!)
                                   (clear-drag!)))
        external-drag-leave-listener (fn [_]
                                       (clear-drag!))
        external-pointer-input (fn [event]
                                 (some-> event .-detail .-input))
        external-pointer-move-listener (fn [event]
                                         (when-let [input (and drag-enabled?
                                                               (external-pointer-input event))]
                                           (.preventDefault event)
                                           (.stopPropagation event)
                                           (let [input (active-drag-input input)]
                                             (set-drag-preview!
                                              (preview-at (current-legal-targets)
                                                          (drag-source-object (:source input))
                                                          (target-object-at! event)
                                                          canvas
                                                          event)))))
        external-pointer-drop-listener (fn [event]
                                         (when-let [input (and drag-enabled?
                                                               (external-pointer-input event))]
                                           (.preventDefault event)
                                           (.stopPropagation event)
                                           (let [input (active-drag-input input)
                                                 target (target-for-object (target-object-at! event))]
                                             (dispatch-gesture! (cond-> input
                                                                  target
                                                                  (assoc :target target))))
                                           (gesture-input/clear-active-gesture-input!)
                                           (clear-drag!)))
        external-pointer-cancel-listener (fn [_]
                                           (clear-drag!))
        orientation-change-listener (fn [_]
                                      (when-let [drag-preview @last-drag-preview]
                                        (set-drag-preview!
                                         (update drag-preview
                                                 :source
                                                 drag-source-object))))]
    (.addEventListener canvas "pointerdown" pointer-down-listener true)
    (.addEventListener canvas "pointerup" pointer-up-listener true)
    (.addEventListener canvas "pointercancel" pointer-cancel-listener true)
    (.addEventListener canvas "pointermove" pointer-move-listener true)
    (.addEventListener canvas "pointerleave" pointer-leave-listener true)
    (.addEventListener canvas "dragover" external-drag-over-listener true)
    (.addEventListener canvas "drop" external-drop-listener true)
    (.addEventListener canvas "dragleave" external-drag-leave-listener true)
    (.addEventListener canvas
                       gesture-input/pointer-drag-move-event
                       external-pointer-move-listener
                       true)
    (.addEventListener canvas
                       gesture-input/pointer-drag-drop-event
                       external-pointer-drop-listener
                       true)
    (.addEventListener js/window
                       gesture-input/pointer-drag-cancel-event
                       external-pointer-cancel-listener
                       true)
    (.addEventListener js/window
                       gesture-input/orientation-change-event
                       orientation-change-listener
                       true)
    {:pointer-down-listener pointer-down-listener
     :pointer-up-listener pointer-up-listener
     :pointer-cancel-listener pointer-cancel-listener
     :pointer-move-listener pointer-move-listener
     :pointer-leave-listener pointer-leave-listener
     :external-drag-over-listener external-drag-over-listener
     :external-drop-listener external-drop-listener
     :external-drag-leave-listener external-drag-leave-listener
     :external-pointer-move-listener external-pointer-move-listener
     :external-pointer-drop-listener external-pointer-drop-listener
     :external-pointer-cancel-listener external-pointer-cancel-listener
     :orientation-change-listener orientation-change-listener
     :pointer-listener-capture? true}))

(def install-card-pointer-listeners! install-board-pointer-listeners!)

(defn focus-board-on-pointer-down! [event]
  (let [target (.-target event)
        class-list (some-> target .-classList)]
    (when (and class-list
               (.contains class-list "board-three__canvas"))
      (.focus (.-currentTarget event)))))
