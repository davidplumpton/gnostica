(ns gnostica.three-board
  (:require [gnostica.board-layout :as layout]
            [gnostica.icon-view :as icon-view]
            [gnostica.icons :as icons]
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
      (let [[_ old-cells old-pieces old-selected-index old-card-icon-mode] old-argv
            [_ new-cells new-pieces new-selected-index new-card-icon-mode] (r/argv this)]
        (cond
          (or (not= old-cells new-cells)
              (not= old-pieces new-pieces))
          (lifecycle/mount! this)

          (not= old-card-icon-mode new-card-icon-mode)
          (lifecycle/mount! this (controls/capture-view-state this))

          (not= old-selected-index new-selected-index)
          (lifecycle/set-selection! this new-selected-index))))
    :component-will-unmount lifecycle/dispose!
    :reagent-render
    (fn [_cells _pieces _selected-index card-icon-mode texture-errors _callbacks]
      (let [component (r/current-component)
            state (r/state component)
            cells-by-index (layout/cells-by-index _cells)
            selected-card (get-in cells-by-index [_selected-index :card])
            texture-metadata (card-textures/texture-renderer-metadata)
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
          :data-visible-piece-count (scene-graph/visible-piece-count _pieces)
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
