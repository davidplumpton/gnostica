(ns gnostica.ui.board
  (:require [gnostica.app.events :as events]
            [gnostica.board-layout :as layout]
            [gnostica.gesture-input :as gesture-input]
            [gnostica.legal-targets :as legal-targets]
            [gnostica.pieces :as pieces]
            [gnostica.three-board :as three-board]
            [gnostica.ui.card :as card-ui]
            [gnostica.ui.common :as ui]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(defn- css-space-offset [step-count edge-offset]
  (str "calc(var(--board-void-margin) + "
       step-count
       " * var(--card-step) + "
       edge-offset
       ")"))

(defn- board-stage-length [min-value max-value]
  (str "calc("
       (- max-value min-value)
       " * var(--card-step) + var(--card-long) + 2 * var(--board-void-margin))"))

(defn- board-stage-style [{:keys [min-row max-row min-col max-col]}]
  {"width" (board-stage-length min-col max-col)
   "height" (board-stage-length min-row max-row)})

(defn- target-highlight-class [drag-hover target descriptor]
  (legal-targets/status-class
   (if drag-hover
     (gesture-input/show-target-highlight? drag-hover target)
     (legal-targets/highlighted? descriptor))
   descriptor))

(defn- drop-target-class [descriptor]
  (when (legal-targets/active? descriptor)
    " is-drop-target"))

(defn- gesture-input-from-event [event]
  (gesture-input/gesture-input-from-data-transfer (.-dataTransfer event)))

(defn- gesture-drag-event? [event]
  (gesture-input/gesture-data-transfer? (.-dataTransfer event)))

(defn- board-piece-drag-input? [input]
  (or (gesture-input/board-space-drag-source? input)
      (some? (get-in input [:fields :piece-id]))))

(defn- hoverable-board-drag? [input]
  (or (nil? input)
      (board-piece-drag-input? input)))

(defn- drag-hover-record [input target descriptor]
  (when target
    (cond-> {:target target
             :target-key (gesture-input/target-key target)
             :target-status (legal-targets/status descriptor)
             :target-enabled? (legal-targets/enabled? descriptor)}
      (:source input)
      (assoc :source (:source input)))))

(defn- drag-source-record [input]
  (when-let [source (:source input)]
    {:source source}))

(defn- set-drag-hover! [drag-hover input target descriptor]
  (let [input (or input
                  (when-let [source (:source @drag-hover)]
                    {:source source}))]
    (if (hoverable-board-drag? input)
      (reset! drag-hover
              (or (drag-hover-record input target descriptor)
                  (drag-source-record input)))
      (reset! drag-hover nil))))

(defn- drag-hover-target? [drag-hover target]
  (and (:target-key drag-hover)
       (= (:target-key drag-hover) (gesture-input/target-key target))))

(defn- current-drag-hover-status [drag-hover descriptor]
  (or (legal-targets/status descriptor)
      (:target-status drag-hover)))

(defn- drag-hover-status-class [drag-hover target descriptor]
  (when (drag-hover-target? drag-hover target)
    (case (current-drag-hover-status drag-hover descriptor)
      :legal " is-drag-hover-target is-drag-hover-legal"
      :disabled " is-drag-hover-target is-drag-hover-disabled"
      " is-drag-hover-target")))

(defn- on-drag-over-gesture [event]
  (when (gesture-drag-event? event)
    (.preventDefault event)
    (when-let [data-transfer (.-dataTransfer event)]
      (set! (.-dropEffect data-transfer) "move"))))

(defn- on-drag-over-target [event drag-hover target descriptor]
  (when (gesture-drag-event? event)
    (on-drag-over-gesture event)
    (.stopPropagation event)
    (set-drag-hover! drag-hover
                     (gesture-input-from-event event)
                     target
                     descriptor)))

(defn- on-drag-over-board-stage [event drag-hover]
  (when (gesture-drag-event? event)
    (on-drag-over-gesture event)
    (when (= (.-currentTarget event) (.-target event))
      (set-drag-hover! drag-hover
                       (gesture-input-from-event event)
                       nil
                       nil))))

(defn- on-board-stage-drag-leave [event drag-hover]
  (let [current (.-currentTarget event)
        related (.-relatedTarget event)]
    (when (or (nil? related)
              (not (.contains current related)))
      (reset! drag-hover nil))))

(defn- on-drop-gesture [event target drag-hover]
  (when-let [input (gesture-input-from-event event)]
    (.preventDefault event)
    (.stopPropagation event)
    (reset! drag-hover nil)
    (gesture-input/clear-active-gesture-input!)
    (rf/dispatch [events/start-gesture-intent (assoc input :target target)])))

(defn- board-space-style [{:keys [min-row min-col]} {:keys [row col orientation]}]
  (let [relative-row (- row min-row)
        relative-col (- col min-col)]
    {"left" (css-space-offset relative-col
                              (if (= :portrait orientation)
                                "var(--card-offset)"
                                "0px"))
     "top" (css-space-offset relative-row
                             (if (= :landscape orientation)
                               "var(--card-offset)"
                               "0px"))}))

(defn- orientation-event [field]
  (case field
    :minion-orientation events/set-move-minion-orientation
    :sun-disc-orientation events/set-move-sun-disc-orientation
    events/set-move-orientation))

(defn- preview-status-class [status]
  (case status
    :legal " is-preview-legal"
    :disabled " is-preview-disabled"
    :pending " is-preview-pending"
    ""))

(defn- preview-space-class [role {:keys [kind orientation]} status]
  (str "board-move-preview__space"
       " is-" (name role)
       " is-" (name kind)
       (when orientation
         (str " is-" (name orientation)))
       (preview-status-class status)))

(defn- preview-space [bounds role status space]
  (when space
    [:div
     {:class (preview-space-class role space status)
      :style (board-space-style bounds space)
      :data-preview-role (name role)
      :data-preview-space-kind (name (:kind space))
      :title (:label space)}
     [:span.board-move-preview__space-label
      (case role
        :source "Start"
        :destination "Land"
        :mutation "Change"
        "Path")]]))

(defn- preview-piece-class [{:keys [orientation]}]
  (str "board-move-preview__piece-marker"
       " is-" (name (or orientation :up))))

(defn- preview-piece [bounds status {:keys [target-space target player-id orientation]}]
  (when target-space
    (let [color (get-in pieces/players-by-id [player-id :css-color])]
      [:div.board-move-preview__piece
       {:class (str "is-" (name (:orientation target-space))
                    (preview-status-class status))
        :style (board-space-style bounds target-space)
        :data-preview-role "placement"
        :data-preview-space-kind (name (:kind target-space))
        :data-preview-board-index (some-> (or (:board-index target)
                                              (:board-index target-space))
                                          str)
        :data-preview-row (some-> (:row target-space) str)
        :data-preview-col (some-> (:col target-space) str)
        :data-preview-orientation (some-> orientation name)
        :title (str "Place small piece on " (:label target-space))}
       [:span
        {:class (preview-piece-class {:orientation orientation})
         :style (when color {"--piece-color" color})}
        [:span.board-move-preview__piece-marker-body]
        [:span.board-move-preview__piece-marker-pip]]])))

(defn- orientation-compass [bounds {:keys [field space selected-orientation options]}]
  (when (and field space (seq options))
    (let [event-id (orientation-event field)]
      [:div.board-orientation-compass
       {:style (board-space-style bounds space)
        :data-orientation-field (name field)
        :aria-label "Choose orientation"}
       (for [{:keys [id label]} options]
         ^{:key id}
         [:button.board-orientation-compass__choice
          {:type "button"
           :class (str "is-" (name id)
                       (when (= selected-orientation id) " is-selected"))
           :aria-label label
           :aria-pressed (= selected-orientation id)
           :title label
           :on-click (fn [event]
                       (.preventDefault event)
                       (.stopPropagation event)
                       (rf/dispatch [event-id id]))}
          (case id
            :up "U"
            :north "N"
            :east "E"
            :south "S"
            :west "W")])])))

(defn- board-move-preview [bounds {:keys [active? status movement mutation
                                          placement summary error]
                                   :as preview}]
  (when active?
    [:div.board-move-preview
     {:data-move-preview-status (some-> status name)}
     (when-let [source-space (:source-space movement)]
       (preview-space bounds :source status source-space))
     (for [[index space] (map-indexed vector (:path movement))]
       ^{:key (str "path-" index "-" (:row space) "-" (:col space))}
       (preview-space bounds :path status space))
     (when-let [destination-space (:destination-space movement)]
       (preview-space bounds :destination status destination-space))
     (when-let [target-space (:target-space mutation)]
       (preview-space bounds :mutation status target-space))
     (when-let [target-space (:target-space placement)]
       (preview-space bounds :destination status target-space))
     [preview-piece bounds status placement]
     [orientation-compass bounds (:orientation-compass preview)]
     (when summary
       [:div.board-move-preview__summary
        {:class (preview-status-class status)}
        (if-let [message (:message error)]
          (str summary ": " message)
          summary)])]))

(defn- piece-action-event [piece descriptor]
  (case (:role descriptor)
    :minion [events/select-move-piece (:id piece)]
    :target [events/select-move-target-piece (:id piece)]
    (when (:source-enabled? descriptor)
      [events/start-gesture-intent (gesture-input/piece-source-input piece)])))

(defn- piece-drag-input [piece descriptor]
  (gesture-input/piece-drag-input piece descriptor))

(defn- css-percent [value]
  (str value "%"))

(defn- board-piece-style [player slot piece-count space-orientation]
  (let [[left top] (layout/piece-slot-css-position slot
                                                   piece-count
                                                   space-orientation)]
    {"--piece-color" (:css-color player)
     "--piece-scale" (str (layout/piece-slot-scale piece-count))
     "left" (css-percent left)
     "top" (css-percent top)}))

(defn- board-piece-marker [slot piece-count space-orientation piece descriptor
                           drag-enabled? drag-hover]
  (let [pips (pieces/pips piece)
        player (pieces/player-for piece)
        drag-input (when drag-enabled?
                     (piece-drag-input piece descriptor))
        draggable? (some? drag-input)
        action-event (piece-action-event piece descriptor)]
    ^{:key (:id piece)}
    [:span.board-piece
     {:class (str "is-slot-" slot
                  " is-" (name (:size piece))
                  " is-" (name (:orientation piece))
                  (when draggable? " is-draggable")
                  (target-highlight-class @drag-hover
                                          {:kind :piece
                                           :piece-id (:id piece)}
                                          descriptor)
                  (when (legal-targets/selected? descriptor) " is-selected-target"))
      :style (board-piece-style player slot piece-count space-orientation)
      :title (legal-targets/reason descriptor)
      :data-piece-id (name (:id piece))
      :data-move-target-status (legal-targets/status-name descriptor)
      :data-move-target-role (some-> (:role descriptor) name)
      :draggable (if draggable? "true" "false")
      :on-click (fn [event]
                  (when action-event
                    (.preventDefault event)
                    (.stopPropagation event)
                    (rf/dispatch action-event)))
      :on-drag-start (fn [event]
                       (when draggable?
                         (.stopPropagation event)
                         (gesture-input/set-gesture-data! (.-dataTransfer event)
                                                          drag-input)
                         (rf/dispatch [events/start-gesture-intent drag-input])))
      :on-drag-end #(do
                      (gesture-input/clear-active-gesture-input!)
                      (reset! drag-hover nil))
      :on-drag-over on-drag-over-gesture
      :on-drop #(on-drop-gesture % {:kind :piece
                                    :piece-id (:id piece)}
                                 drag-hover)}
     [:span.board-piece__body
      [:span.board-piece__pips
       (for [pip (range pips)]
         ^{:key pip}
         [:span.board-piece__pip])]]]))

(defn- board-piece-markers [space-orientation board-pieces piece-targets
                            drag-enabled? drag-hover]
  (when-let [space-pieces (seq board-pieces)]
    (let [piece-count (count space-pieces)]
      [:div.board-card__pieces
       {:data-piece-count piece-count
        :data-overflow-piece-count (max 0
                                        (- piece-count
                                           pieces/max-pieces-per-space))}
       (for [[slot piece] (layout/visible-piece-slots space-pieces)]
         (board-piece-marker slot
                             piece-count
                             space-orientation
                             piece
                             (get piece-targets (:id piece))
                             drag-enabled?
                             drag-hover))])))

(defn- pieces-label [board-pieces]
  (apply str (interpose "; " (map ui/piece-summary board-pieces))))

(defn- board-wasteland [bounds {:keys [id orientation] :as space}
                        board-pieces descriptor piece-targets drag-enabled?
                        drag-hover]
  (let [target {:kind :wasteland
                :row (:row space)
                :col (:col space)}
        hovered? (drag-hover-target? @drag-hover target)]
    ^{:key id}
    [:div.board-wasteland
     {:class (str "is-" (name orientation)
                  (when (seq board-pieces) " has-pieces")
                  (drop-target-class descriptor)
                  (target-highlight-class @drag-hover target descriptor)
                  (drag-hover-status-class @drag-hover target descriptor)
                  (when (legal-targets/selected? descriptor) " is-selected-target"))
      :style (board-space-style bounds space)
      :data-piece-count (count board-pieces)
      :data-move-target-status (legal-targets/status-name descriptor)
      :data-move-target-role (some-> (:role descriptor) name)
      :data-drag-hover (when hovered? "true")
      :data-drag-hover-status (when hovered?
                                (some-> (current-drag-hover-status @drag-hover
                                                                   descriptor)
                                        name))
      :title (legal-targets/reason descriptor)
      :role (when (seq board-pieces) "img")
      :aria-label (when (seq board-pieces)
                    (str (ui/wasteland-label space)
                         ", pieces: "
                         (pieces-label board-pieces)))
      :aria-hidden (when-not (seq board-pieces) "true")
      :on-drag-over #(on-drag-over-target % drag-hover target descriptor)
      :on-drop #(on-drop-gesture % target drag-hover)}
     (board-piece-markers orientation
                          board-pieces
                          piece-targets
                          drag-enabled?
                          drag-hover)]))

(defn board-card [bounds {:keys [index row col orientation card] :as cell}
                  selected? board-pieces card-icon-mode descriptor piece-targets
                  drag-enabled? drag-hover]
  (let [{:keys [title]} card
        target {:kind :territory
                :board-index index}
        hovered? (drag-hover-target? @drag-hover target)
        drag-input (when drag-enabled?
                     (gesture-input/territory-drag-input cell descriptor))]
    [:button.board-card
     {:type "button"
      :class (str "is-" (name orientation)
                  " is-row-" row
                  " is-col-" col
                  (when selected? " is-selected")
                  (drop-target-class descriptor)
                  (target-highlight-class @drag-hover target descriptor)
                  (drag-hover-status-class @drag-hover target descriptor)
                  (when (legal-targets/selected? descriptor) " is-selected-target"))
      :style (board-space-style bounds cell)
      :data-move-target-status (legal-targets/status-name descriptor)
      :data-move-target-role (some-> (:role descriptor) name)
      :data-drag-hover (when hovered? "true")
      :data-drag-hover-status (when hovered?
                                (some-> (current-drag-hover-status @drag-hover
                                                                   descriptor)
                                        name))
      :draggable (if drag-input "true" "false")
      :title (legal-targets/reason descriptor)
      :aria-label (str title
                       ", "
                       (ui/orientation-label orientation)
                       ", row "
                       (inc row)
                       ", column "
                       (inc col)
                       (when-let [summary (card-ui/card-icon-summary card)]
                         (str ", special moves: " summary))
                       (when (seq board-pieces)
                         (str ", pieces: "
                              (pieces-label board-pieces))))
      :on-click #(rf/dispatch [events/select-board-card index])
      :on-double-click #(rf/dispatch [events/start-gesture-intent
                                      (gesture-input/territory-source-input cell)])
      :on-drag-start (fn [event]
                       (when drag-input
                         (gesture-input/set-gesture-data! (.-dataTransfer event)
                                                          drag-input)
                         (rf/dispatch [events/start-gesture-intent drag-input])))
      :on-drag-end #(do
                      (gesture-input/clear-active-gesture-input!)
                      (reset! drag-hover nil))
      :on-drag-over #(on-drag-over-target % drag-hover target descriptor)
      :on-drop #(on-drop-gesture % target drag-hover)}
     [card-ui/card-face card "board-card__face" card-icon-mode]
     (board-piece-markers orientation
                          board-pieces
                          piece-targets
                          drag-enabled?
                          drag-hover)]))

(defn- board-stage-content [drag-hover]
  (let [{:keys [cells board-pieces pieces-by-space wastelands space-bounds
                selected-index card-icon-mode texture-errors three-revision
                three-renderer-available? three-renderer-message legal-targets
                move-preview direct-manipulation]}
        @(rf/subscribe [events/board-view])]
    [:section.board-area
     {:data-three-revision (or three-revision "unavailable")}
     (cond
       (empty? cells)
       [:div.board-empty
        [:p "Board setup pending"]]

       three-renderer-available?
       [three-board/scene
        cells
        board-pieces
        selected-index
        card-icon-mode
        texture-errors
        legal-targets
        move-preview
        direct-manipulation
        {:on-card-select #(rf/dispatch [events/select-board-card %])
         :on-gesture-intent #(rf/dispatch [events/start-gesture-intent %])
         :on-orientation-select #(rf/dispatch [events/set-move-orientation %])
         :on-minion-orientation-select #(rf/dispatch [events/set-move-minion-orientation %])
         :on-sun-disc-orientation-select #(rf/dispatch [events/set-move-sun-disc-orientation %])
         :on-clear-texture-errors #(rf/dispatch [events/clear-three-texture-errors])
         :on-renderer-error #(rf/dispatch [events/three-renderer-error %])
         :on-texture-error #(rf/dispatch [events/three-texture-error %])}]

       :else
       [:div.board-fallback
        [:p.board-3d-status.is-error
         three-renderer-message]
        (let [target-indexes (legal-targets/target-indexes legal-targets)
              territory-targets (:territories target-indexes)
              wasteland-targets (:wastelands target-indexes)
              piece-targets (:pieces target-indexes)
              placement (:placement move-preview)
              placement-space (:target-space placement)
              placement-target (:target placement)
              drag-enabled? (true? (:pointer-drag-enabled? direct-manipulation))
              drag-hover* @drag-hover
              drag-hover-descriptor (legal-targets/descriptor-for-indexed-target
                                     target-indexes
                                     (:target drag-hover*))
              drag-hover-status (current-drag-hover-status drag-hover*
                                                           drag-hover-descriptor)]
          [:div.board-stage
           {:role "group"
            :aria-label "Gnostica board"
            :data-wasteland-count (count wastelands)
            :data-pointer-drag-enabled (true? (:pointer-drag-enabled?
                                               direct-manipulation))
            :data-detailed-entry-available (true? (:detailed-entry-available?
                                                   direct-manipulation))
            :data-table-surface-color three-board/table-surface-css-color
            :data-table-clear-color three-board/table-clear-css-color
            :data-drag-hover-kind (some-> drag-hover*
                                          :target
                                          :kind
                                          name)
            :data-drag-active (boolean drag-hover*)
            :data-drag-hover-status (some-> drag-hover-status name)
            :data-move-preview-target-kind (some-> placement-space :kind name)
            :data-move-preview-target-board-index (or (:board-index placement-target)
                                                      "")
            :data-move-preview-target-row (or (:row placement-space) "")
            :data-move-preview-target-col (or (:col placement-space) "")
            :data-move-preview-placement-orientation (some-> placement
                                                             :orientation
                                                             name)
            :style (board-stage-style space-bounds)
            :on-drag-over #(on-drag-over-board-stage % drag-hover)
            :on-drag-leave #(on-board-stage-drag-leave % drag-hover)}
           (for [space wastelands]
             (board-wasteland
              space-bounds
              space
              (get pieces-by-space (pieces/wasteland-space (:row space) (:col space)))
              (get wasteland-targets [(:row space) (:col space)])
              piece-targets
              drag-enabled?
              drag-hover))
           (for [cell cells]
             ^{:key (:index cell)}
             [board-card
              space-bounds
              cell
              (= selected-index (:index cell))
              (get pieces-by-space (pieces/territory-space (:index cell)))
              card-icon-mode
              (get territory-targets (:index cell))
              piece-targets
              drag-enabled?
              drag-hover])
           [board-move-preview space-bounds move-preview]])])]))

(defn board-stage []
  (let [drag-hover (r/atom nil)
        clear-drag-hover! (fn [_] (reset! drag-hover nil))
        start-drag-hover! (fn [event]
                            (when (gesture-drag-event? event)
                              (set-drag-hover! drag-hover
                                               (gesture-input-from-event event)
                                               nil
                                               nil)))]
    (r/create-class
     {:component-did-mount
      (fn [_]
        (.addEventListener js/window "dragstart" start-drag-hover! false)
        (.addEventListener js/window "dragend" clear-drag-hover! true)
        (.addEventListener js/window "drop" clear-drag-hover! true))
      :component-will-unmount
      (fn [_]
        (.removeEventListener js/window "dragstart" start-drag-hover! false)
        (.removeEventListener js/window "dragend" clear-drag-hover! true)
        (.removeEventListener js/window "drop" clear-drag-hover! true))
      :reagent-render
      (fn []
        [board-stage-content drag-hover])})))
