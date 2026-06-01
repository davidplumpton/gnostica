(ns gnostica.ui.move-panel.source
  (:require [gnostica.app.events :as events]
            [gnostica.gesture-input :as gesture-input]
            [gnostica.pieces :as pieces]
            [re-frame.core :as rf]))

(defn- draw-drag-image-path! [context points]
  (.beginPath context)
  (let [[x y] (first points)]
    (.moveTo context x y))
  (doseq [[x y] (rest points)]
    (.lineTo context x y))
  (.closePath context))

(defn- fill-drag-image-face! [context color overlay points]
  (draw-drag-image-path! context points)
  (set! (.-fillStyle context) color)
  (.fill context)
  (when overlay
    (draw-drag-image-path! context points)
    (set! (.-fillStyle context) overlay)
    (.fill context)))

(defn- stash-piece-drag-image! [data-transfer color]
  (when (and data-transfer (.-setDragImage data-transfer))
    (let [size 72
          color (or color "#ffffff")
          scale (max 1 (or (.-devicePixelRatio js/window) 1))
          canvas (.createElement js/document "canvas")
          context (.getContext canvas "2d")]
      (set! (.-width canvas) (* size scale))
      (set! (.-height canvas) (* size scale))
      (set! (.. canvas -style -width) (str size "px"))
      (set! (.. canvas -style -height) (str size "px"))
      (set! (.. canvas -style -position) "fixed")
      (set! (.. canvas -style -left) "-1000px")
      (set! (.. canvas -style -top) "-1000px")
      (set! (.. canvas -style -pointerEvents) "none")
      (.scale context scale scale)
      (set! (.-shadowColor context) "rgba(0, 0, 0, 0.42)")
      (set! (.-shadowBlur context) 10)
      (set! (.-shadowOffsetY context) 5)
      (set! (.-fillStyle context) "rgba(0, 0, 0, 0.24)")
      (.beginPath context)
      (.ellipse context 36 56 21 7 0 0 (* 2 js/Math.PI))
      (.fill context)
      (set! (.-shadowBlur context) 0)
      (set! (.-shadowOffsetY context) 0)
      (set! (.-strokeStyle context) "rgba(255, 250, 240, 0.9)")
      (set! (.-lineWidth context) 1.3)
      (fill-drag-image-face! context color "rgba(255, 255, 255, 0.2)"
                             [[36 8] [62 55] [36 66]])
      (fill-drag-image-face! context color "rgba(0, 0, 0, 0.2)"
                             [[36 8] [36 66] [10 55]])
      (draw-drag-image-path! context [[36 8] [62 55] [36 66] [10 55]])
      (.stroke context)
      (set! (.-fillStyle context) "rgba(255, 250, 240, 0.95)")
      (.beginPath context)
      (.ellipse context 36 52 3.6 4.8 0 0 (* 2 js/Math.PI))
      (.fill context)
      (.appendChild (.-body js/document) canvas)
      (.setDragImage data-transfer canvas 36 54)
      (js/setTimeout
       #(when-let [parent (.-parentNode canvas)]
          (.removeChild parent canvas))
       0))))

(def source-pointer-drag-threshold 8)

(defonce suppress-next-source-click?
  (atom false))

(defn- left-button-pointer? [event]
  (zero? (.-button event)))

(defn- pointer-distance [event {:keys [x y]}]
  (js/Math.hypot (- (.-clientX event) x)
                 (- (.-clientY event) y)))

(defn- three-canvas-at [x y]
  (when-let [element (.elementFromPoint js/document x y)]
    (when (.-closest element)
      (.closest element ".board-three__canvas"))))

(defn- dispatch-pointer-gesture-event! [event-name input event]
  (when-let [canvas (three-canvas-at (.-clientX event) (.-clientY event))]
    (.dispatchEvent canvas
                    (js/CustomEvent.
                     event-name
                     #js {:bubbles true
                          :cancelable true
                          :detail #js {:input input
                                       :clientX (.-clientX event)
                                       :clientY (.-clientY event)}}))
    true))

(defn- cancel-pointer-gesture-preview! []
  (.dispatchEvent js/window
                  (js/CustomEvent.
                   gesture-input/pointer-drag-cancel-event
                   #js {:bubbles false
                        :cancelable false})))

(defn- begin-source-pointer-drag! [event input]
  (when (and input
             (left-button-pointer? event)
             (.querySelector js/document ".board-three__canvas"))
    (.preventDefault event)
    (.stopPropagation event)
    (reset! suppress-next-source-click? true)
    (js/setTimeout #(reset! suppress-next-source-click? false) 750)
    (gesture-input/set-active-gesture-input! input)
    (rf/dispatch-sync [events/start-gesture-intent input])
    (let [start {:x (.-clientX event)
                 :y (.-clientY event)}
          dragging? (atom false)
          move-listener* (atom nil)
          up-listener* (atom nil)
          cancel-listener* (atom nil)
          cleanup! (fn []
                     (when-let [listener @move-listener*]
                       (.removeEventListener js/window "pointermove" listener true))
                     (when-let [listener @up-listener*]
                       (.removeEventListener js/window "pointerup" listener true))
                     (when-let [listener @cancel-listener*]
                       (.removeEventListener js/window "pointercancel" listener true)))]
      (reset! move-listener*
              (fn [move-event]
                (when (or @dragging?
                          (< source-pointer-drag-threshold
                             (pointer-distance move-event start)))
                  (.preventDefault move-event)
                  (reset! dragging? true)
                  (let [input (or (gesture-input/active-gesture-input)
                                  input)]
                    (when-not (dispatch-pointer-gesture-event!
                               gesture-input/pointer-drag-move-event
                               input
                               move-event)
                      (cancel-pointer-gesture-preview!))))))
      (reset! up-listener*
              (fn [up-event]
                (.preventDefault up-event)
                (let [input (or (gesture-input/active-gesture-input)
                                input)]
                  (when @dragging?
                    (dispatch-pointer-gesture-event!
                     gesture-input/pointer-drag-drop-event
                     input
                     up-event)))
                (cleanup!)
                (gesture-input/clear-active-gesture-input!)
                (cancel-pointer-gesture-preview!)))
      (reset! cancel-listener*
              (fn [_]
                (cleanup!)
                (gesture-input/clear-active-gesture-input!)
                (cancel-pointer-gesture-preview!)))
      (.addEventListener js/window "pointermove" @move-listener* true)
      (.addEventListener js/window "pointerup" @up-listener* true)
      (.addEventListener js/window "pointercancel" @cancel-listener* true)
      true)))

(defn move-source-picker [options selected-source current-player direct-manipulation]
  [:div.move-source-list
   (for [{:keys [id label summary enabled? reason]} options]
     (let [stash-source? (and (= :place-initial-small id)
                              current-player)
           drag-enabled? (true? (:pointer-drag-enabled? direct-manipulation))
           player-color (or (:css-color current-player)
                            (get-in pieces/players-by-id
                                    [(:id current-player) :css-color]))
           stash-input (when (and stash-source? drag-enabled?)
                         (gesture-input/stash-piece-source-input current-player))]
       ^{:key id}
       [:button.move-source-option
        {:type "button"
         :class (when (= selected-source id) "is-selected")
         :disabled (not enabled?)
         :aria-pressed (= selected-source id)
         :draggable (if (and enabled? stash-input) "true" "false")
         :on-click (fn [event]
                     (if @suppress-next-source-click?
                       (do
                         (.preventDefault event)
                         (.stopPropagation event)
                         (reset! suppress-next-source-click? false))
                       (rf/dispatch [events/select-move-source id])))
         :on-pointer-down #(when (and enabled? stash-input)
                             (begin-source-pointer-drag! % stash-input))
         :on-drag-end #(gesture-input/clear-active-gesture-input!)
         :on-drag-start (fn [event]
                          (when (and enabled? stash-input)
                            (rf/dispatch [events/start-gesture-intent stash-input])
                            (let [data-transfer (.-dataTransfer event)]
                              (gesture-input/set-gesture-data! data-transfer
                                                               stash-input)
                              (stash-piece-drag-image! data-transfer player-color))))}
        [:span.move-source-option__label
         (when stash-source?
           [:span.move-source-option__piece
            {:aria-hidden "true"
             :data-piece-shape "small-pyramid"
             :draggable (if (and enabled? stash-input) "true" "false")
             :on-drag-end #(gesture-input/clear-active-gesture-input!)
             :on-drag-start (fn [event]
                              (when (and enabled? stash-input)
                                (.stopPropagation event)
                                (rf/dispatch [events/start-gesture-intent
                                              stash-input])
                                (let [data-transfer (.-dataTransfer event)]
                                  (gesture-input/set-gesture-data! data-transfer
                                                                   stash-input)
                                  (stash-piece-drag-image! data-transfer
                                                           player-color))))
             :style {"--piece-color" player-color}}
            [:span.move-source-option__piece-body]
            [:span.move-source-option__piece-pips
             [:span.move-source-option__piece-pip]]])
         label]
        [:span.move-source-option__summary (if enabled? summary reason)]]))])

(defn detailed-entry-default-toggle
  [{:keys [detailed-entry-available? detailed-entry-default?]}]
  (when detailed-entry-available?
    [:button.move-action.move-panel-mode-toggle
     {:type "button"
      :aria-pressed (true? detailed-entry-default?)
      :aria-label (if detailed-entry-default?
                    "Use direct gestures by default"
                    "Use Detailed entry by default")
      :on-click #(rf/dispatch [events/set-detailed-entry-default
                               (not detailed-entry-default?)])}
     "Detailed default"]))
