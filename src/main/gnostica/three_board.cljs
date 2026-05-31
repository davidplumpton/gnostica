(ns gnostica.three-board
  (:require [gnostica.board-layout :as layout]
            [gnostica.icon-view :as icon-view]
            [gnostica.icons :as icons]
            [gnostica.pieces :as pieces]
            [gnostica.three-board.controls :as controls]
            [gnostica.three-board.lifecycle :as lifecycle]
            [gnostica.three-board.pointer :as pointer]
            [gnostica.three-board.resources :as resources]
            [gnostica.three-board.runtime :as runtime]
            [gnostica.three-board.scene-graph :as scene-graph]
            [gnostica.three-card-textures :as card-textures]
            [reagent.core :as r]))

(def table-surface-css-color scene-graph/table-surface-css-color)
(def table-clear-css-color scene-graph/table-clear-css-color)

(defn- object-label [{:keys [kind board-index piece-id row col]}]
  (case kind
    :territory (str "territory " board-index)
    :piece (str "piece " (name piece-id))
    :wasteland (str "wasteland " row ", " col)
    :stash-piece "stash piece"
    "board object"))

(defn- drag-preview-class [{:keys [target-status]}]
  (case target-status
    :legal " is-legal-target"
    :disabled " is-disabled-target"
    ""))

(defn- drag-preview-summary [{:keys [source target target-status target-reason]}]
  (let [source-label (object-label source)]
    (cond
      (and target (= :legal target-status))
      (str "Dragging " source-label " to " (object-label target))

      (and target (= :disabled target-status))
      (str "Cannot drop on " (object-label target)
           (when target-reason
             (str ": " target-reason)))

      target
      (str "Dragging " source-label " over " (object-label target))

      :else
      (str "Dragging " source-label))))

(defn- stash-piece-drag-ghost? [{:keys [source pointer]}]
  (and pointer
       (= :stash-piece (:kind source))
       (= :small (:size source))))

(defn- drag-ghost-style [{:keys [source pointer]}]
  (let [{:keys [x y]} pointer
        color (get-in pieces/players-by-id [(:player-id source) :css-color])]
    (cond-> {"--drag-x" (str x "px")
             "--drag-y" (str y "px")}
      color
      (assoc "--piece-color" color))))

(defn- move-preview-class [{:keys [status]}]
  (case status
    :legal " is-preview-legal"
    :disabled " is-preview-disabled"
    :pending " is-preview-pending"
    ""))

(defn- move-preview-summary [{:keys [summary error movement mutation]}]
  (or (when-let [message (:message error)]
        (str (or summary "Move preview") ": " message))
      summary
      (:summary mutation)
      (:summary movement)
      "Move preview"))

(defn- preview-piece-class [{:keys [orientation status]}]
  (str "board-three__placement-piece-preview"
       " is-" (name (or orientation :up))
       (move-preview-class {:status status})))

(defn- projected-space-style [state space orientation]
  (let [{:keys [camera renderer]} state
        canvas (some-> renderer .-domElement)
        board-node (some-> canvas (.closest ".board-three"))]
    (when (and camera canvas board-node space)
      (let [[x y] (layout/card-position space)
            piece-size (pieces/size-data {:size :small})
            z (layout/piece-center-z piece-size (or orientation :up))
            point (js/THREE.Vector3. x y z)
            canvas-rect (.getBoundingClientRect canvas)
            board-rect (.getBoundingClientRect board-node)]
        (.project point camera)
        (when (and (<= -1 (.-x point) 1)
                   (<= -1 (.-y point) 1))
          (let [left (+ (- (.-left canvas-rect) (.-left board-rect))
                        (* (/ (+ (.-x point) 1) 2)
                           (.-width canvas-rect)))
                top (+ (- (.-top canvas-rect) (.-top board-rect))
                       (* (/ (- 1 (.-y point)) 2)
                          (.-height canvas-rect)))]
            {"--preview-x" (str left "px")
             "--preview-y" (str top "px")}))))))

(defn- placement-piece-preview [state {:keys [status placement]}]
  (let [{:keys [target-space player-id piece-size orientation]} placement
        color (get-in pieces/players-by-id [player-id :css-color])
        style (projected-space-style state target-space orientation)]
    (when style
      [:div
       {:class (preview-piece-class {:orientation orientation
                                     :status status})
        :aria-hidden "true"
        :data-player-id (some-> player-id name)
        :data-piece-size (some-> piece-size name)
        :data-preview-space-kind (some-> target-space :kind name)
        :data-preview-orientation (some-> orientation name)
        :style (cond-> style
                 color
                 (assoc "--piece-color" color))}
       [:span.board-three__placement-piece-preview-body]
       [:span.board-three__placement-piece-preview-pip]])))

(defn- orientation-callback-key [field]
  (case field
    :minion-orientation :on-minion-orientation-select
    :sun-disc-orientation :on-sun-disc-orientation-select
    :on-orientation-select))

(defn- orientation-compass [{:keys [field selected-orientation options]} callbacks]
  (when (and field (seq options))
    [:div.board-three__orientation-compass
     {:data-orientation-field (name field)
      :aria-label "Choose orientation"}
     (for [{:keys [id label]} options]
       ^{:key id}
       [:button.board-three__orientation-choice
        {:type "button"
         :class (str "is-" (name id)
                     (when (= selected-orientation id) " is-selected"))
         :aria-label label
         :aria-pressed (= selected-orientation id)
         :title label
         :on-click #(resources/invoke-callback callbacks
                                               (orientation-callback-key field)
                                               id)}
        (case id
          :up "U"
          :north "N"
          :east "E"
          :south "S"
          :west "W")])]))

(defn three-runtime []
  (runtime/three-runtime))

(defn three-revision []
  (runtime/three-revision))

(defn orbit-controls-runtime []
  (runtime/orbit-controls-runtime))

(defn runtime-status []
  (runtime/runtime-status))

(defn available? []
  (runtime/available?))

(def scene
  (r/create-class
   {:display-name "three-board-scene"
    :component-did-mount lifecycle/mount!
    :component-did-update
    (fn [this old-argv _ _]
      (let [[_ old-cells old-pieces old-selected-index old-card-icon-mode
             _old-texture-errors old-legal-targets _old-move-preview
             old-direct-manipulation] old-argv
            [_ new-cells new-pieces new-selected-index new-card-icon-mode
             _new-texture-errors new-legal-targets _new-move-preview
             new-direct-manipulation] (r/argv this)]
        (cond
          (or (not= old-cells new-cells)
              (not= old-pieces new-pieces)
              (not= old-direct-manipulation new-direct-manipulation))
          (lifecycle/mount! this)

          (not= old-card-icon-mode new-card-icon-mode)
          (lifecycle/mount! this (controls/capture-view-state this))

          (or (not= old-selected-index new-selected-index)
              (not= old-legal-targets new-legal-targets))
          (lifecycle/set-selection! this new-selected-index new-legal-targets))))
    :component-will-unmount lifecycle/dispose!
    :reagent-render
    (fn [_cells _pieces _selected-index card-icon-mode texture-errors legal-targets
         move-preview direct-manipulation callbacks]
      (let [component (r/current-component)
            state (r/state component)
            cells-by-index (layout/cells-by-index _cells)
            selected-card (get-in cells-by-index [_selected-index :card])
            texture-metadata (card-textures/texture-renderer-metadata)
            drag-preview (:drag-preview state)
            popover-index (or (:hovered-index state)
                              (when (:board-focused? state)
                                _selected-index))
            popover-card (get-in cells-by-index [popover-index :card])]
        [:div.board-three
         {:role "img"
          :tabIndex 0
          :aria-label (str "Three-dimensional Gnostica board with nine face-up tarot territory cards and Icehouse pieces. "
                           "Use W, A, S, D, or arrow keys to move the board view when focused"
                           (when-let [summary (and (= :popup card-icon-mode)
                                                   (icons/icon-stack-label (:gnostica-icons selected-card)))]
                             (when (seq summary)
                               (str ". Selected card special moves: " summary))))
          :on-focus #(lifecycle/assoc-component-state! component :board-focused? true)
          :on-blur #(lifecycle/assoc-component-state! component :board-focused? false)
          :on-pointer-down pointer/focus-board-on-pointer-down!
          :on-key-down #(controls/handle-board-key-down! component %)
          :data-board-card-count (count _cells)
          :data-major-icon-card-count (count (filter #(seq (icons/present-icon-ids
                                                            (get-in % [:card :gnostica-icons])))
                                                      _cells))
          :data-major-icon-count (reduce + (map #(count (icons/present-icon-ids
                                                         (get-in % [:card :gnostica-icons])))
                                                _cells))
          :data-card-icon-mode (name card-icon-mode)
          :data-card-icon-scale (:card-icon-scale texture-metadata)
          :data-card-icon-size (:card-icon-size texture-metadata)
          :data-card-texture-supported-icon-count (:supported-icon-count texture-metadata)
          :data-card-texture-max-icon-count (:max-card-icon-count texture-metadata)
          :data-card-texture-icon-stack-fits (:icon-stack-fits? texture-metadata)
          :data-wasteland-count (count (layout/wasteland-spaces _cells))
          :data-legal-target-count (count (filter :enabled?
                                                  (:territories legal-targets)))
          :data-pointer-drag-enabled (true? (:pointer-drag-enabled?
                                             direct-manipulation))
          :data-detailed-entry-available (true? (:detailed-entry-available?
                                                 direct-manipulation))
	          :data-drag-active (true? (:active? drag-preview))
	          :data-drag-target-kind (some-> drag-preview :target :kind name)
	          :data-drag-target-status (some-> drag-preview :target-status name)
	          :data-drag-target-highlight-count (or (:drag-target-highlight-count state) 0)
	          :data-move-preview-active (true? (:active? move-preview))
          :data-move-preview-status (some-> move-preview :status name)
          :data-visible-piece-count (scene-graph/visible-piece-count _cells _pieces)
          :data-piece-edge-outline-count (or (:piece-edge-outline-count state) 0)
          :data-antialias-requested resources/renderer-antialias-requested?
          :data-antialias-enabled (true? (:antialias-enabled? state))
          :data-min-zoom-distance controls/min-distance
          :data-max-zoom-distance controls/max-distance
          :data-camera-distance (or (:camera-distance state) "")
          :data-camera-target-x (or (:camera-target-x state) "")
          :data-camera-target-y (or (:camera-target-y state) "")
          :data-selected-board-index _selected-index
          :data-table-surface-color table-surface-css-color
          :data-table-clear-color table-clear-css-color
          :data-texture-error-count (count texture-errors)}
         [:div.board-three__mount
          {:aria-hidden "true"
           :ref #(set! (.-boardMountNode ^js component) %)}]
         [:button.board-three__reset
          {:type "button"
           :on-click #(controls/reset-view! component)}
          "Reset view"]
         (when (:active? drag-preview)
           [:div.board-three__drag-preview
            {:class (drag-preview-class drag-preview)
             :aria-live "polite"}
            (drag-preview-summary drag-preview)])
         (when (stash-piece-drag-ghost? drag-preview)
           [:div.board-three__drag-piece-ghost
            {:class (drag-preview-class drag-preview)
             :aria-hidden "true"
             :data-player-id (some-> drag-preview
                                      (get-in [:source :player-id])
                                      name)
             :data-piece-size (some-> drag-preview
                                      (get-in [:source :size])
                                      name)
             :style (drag-ghost-style drag-preview)}
            [:span.board-three__drag-piece-ghost-body]
            [:span.board-three__drag-piece-ghost-pip]])
         [placement-piece-preview state move-preview]
         (when (:active? move-preview)
           [:div.board-three__move-preview
            {:class (move-preview-class move-preview)
             :aria-live "polite"}
            [:strong (move-preview-summary move-preview)]
            (when-let [movement (:movement move-preview)]
              [:span.board-three__move-preview-detail
               (str (count (:path movement))
                    " path space"
                    (when (not= 1 (count (:path movement))) "s"))])
            [orientation-compass (:orientation-compass move-preview) callbacks]])
         (when (and (= :popup card-icon-mode)
                    (seq (icons/present-icon-ids (:gnostica-icons popover-card))))
           [:div.board-three-icon-popover
            [icon-view/card-icon-popover popover-card {:show-title? true}]])
         (when (seq texture-errors)
           [:p.board-3d-status.is-error
            (str "Texture load failed for "
                 (count texture-errors)
                 " card"
                 (when (not= 1 (count texture-errors)) "s")
                 ". Check the console for image paths.")])]))}))
