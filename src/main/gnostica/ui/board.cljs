(ns gnostica.ui.board
  (:require [gnostica.app.events :as events]
            [gnostica.gesture-input :as gesture-input]
            [gnostica.pieces :as pieces]
            [gnostica.three-board :as three-board]
            [gnostica.ui.card :as card-ui]
            [gnostica.ui.common :as ui]
            [re-frame.core :as rf]))

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

(defn- target-status-class [{:keys [active? status]}]
  (when active?
    (case status
      :legal " is-legal-target"
      :disabled " is-disabled-target"
      "")))

(defn- target-reason [descriptor]
  (or (:reason descriptor)
      (get-in descriptor [:error :message])))

(defn- gesture-input-from-event [event]
  (gesture-input/gesture-input-from-data-transfer (.-dataTransfer event)))

(defn- gesture-drag-event? [event]
  (gesture-input/gesture-data-transfer? (.-dataTransfer event)))

(defn- on-drag-over-gesture [event]
  (when (gesture-drag-event? event)
    (.preventDefault event)
    (when-let [data-transfer (.-dataTransfer event)]
      (set! (.-dropEffect data-transfer) "move"))))

(defn- on-drop-gesture [event target]
  (when-let [input (gesture-input-from-event event)]
    (.preventDefault event)
    (.stopPropagation event)
    (rf/dispatch [events/start-gesture-intent (assoc input :target target)])))

(defn- territory-targets-by-index [legal-targets]
  (into {}
        (map (juxt :board-index identity))
        (:territories legal-targets)))

(defn- wasteland-targets-by-coordinate [legal-targets]
  (into {}
        (map (fn [{:keys [row col] :as descriptor}]
               [[row col] descriptor]))
        (:wastelands legal-targets)))

(defn- piece-targets-by-id [legal-targets]
  (into {}
        (map (juxt :piece-id identity))
        (:pieces legal-targets)))

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
                                          summary error]
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

(defn- board-piece-marker [slot piece descriptor drag-enabled?]
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
                  (target-status-class descriptor)
                  (when (:selected? descriptor) " is-selected-target"))
      :style {"--piece-color" (:css-color player)}
      :title (target-reason descriptor)
      :data-piece-id (name (:id piece))
      :data-move-target-status (some-> (:status descriptor) name)
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
      :on-drag-over on-drag-over-gesture
      :on-drop #(on-drop-gesture % {:kind :piece
                                    :piece-id (:id piece)})}
     [:span.board-piece__body
      [:span.board-piece__pips
       (for [pip (range pips)]
         ^{:key pip}
         [:span.board-piece__pip])]]]))

(defn- board-piece-markers [board-pieces piece-targets drag-enabled?]
  (when (seq board-pieces)
    [:div.board-card__pieces
     (for [[slot piece] (map-indexed vector
                                     (take pieces/max-pieces-per-space board-pieces))]
       (board-piece-marker slot piece (get piece-targets (:id piece)) drag-enabled?))]))

(defn- pieces-label [board-pieces]
  (apply str (interpose "; " (map ui/piece-summary board-pieces))))

(defn- board-wasteland [bounds {:keys [id orientation] :as space}
                        board-pieces descriptor piece-targets drag-enabled?]
  ^{:key id}
  [:div.board-wasteland
   {:class (str "is-" (name orientation)
                (when (seq board-pieces) " has-pieces")
                (target-status-class descriptor)
                (when (:selected? descriptor) " is-selected-target"))
    :style (board-space-style bounds space)
    :data-piece-count (count board-pieces)
    :data-move-target-status (some-> (:status descriptor) name)
    :data-move-target-role (some-> (:role descriptor) name)
    :title (target-reason descriptor)
    :role (when (seq board-pieces) "img")
    :aria-label (when (seq board-pieces)
                  (str (ui/wasteland-label space)
                       ", pieces: "
                       (pieces-label board-pieces)))
    :aria-hidden (when-not (seq board-pieces) "true")
    :on-drag-over on-drag-over-gesture
    :on-drop #(on-drop-gesture % {:kind :wasteland
                                  :row (:row space)
                                  :col (:col space)})}
   (board-piece-markers board-pieces piece-targets drag-enabled?)])

(defn board-card [bounds {:keys [index row col orientation card] :as cell}
                  selected? board-pieces card-icon-mode descriptor piece-targets
                  drag-enabled?]
  (let [{:keys [title]} card
        drag-input (when drag-enabled?
                     (gesture-input/territory-drag-input cell descriptor))]
    [:button.board-card
     {:type "button"
      :class (str "is-" (name orientation)
                  " is-row-" row
                  " is-col-" col
                  (when selected? " is-selected")
                  (target-status-class descriptor)
                  (when (:selected? descriptor) " is-selected-target"))
      :style (board-space-style bounds cell)
      :data-move-target-status (some-> (:status descriptor) name)
      :data-move-target-role (some-> (:role descriptor) name)
      :draggable (if drag-input "true" "false")
      :title (target-reason descriptor)
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
      :on-drag-over on-drag-over-gesture
     :on-drop #(on-drop-gesture % {:kind :territory
                                    :board-index index})}
     [card-ui/card-face card "board-card__face" card-icon-mode]
     (board-piece-markers board-pieces piece-targets drag-enabled?)]))

(defn board-stage []
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
        (let [territory-targets (territory-targets-by-index legal-targets)
              wasteland-targets (wasteland-targets-by-coordinate legal-targets)
              piece-targets (piece-targets-by-id legal-targets)
              drag-enabled? (true? (:pointer-drag-enabled? direct-manipulation))]
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
            :style (board-stage-style space-bounds)}
           (for [space wastelands]
             (board-wasteland
              space-bounds
              space
              (get pieces-by-space (pieces/wasteland-space (:row space) (:col space)))
              (get wasteland-targets [(:row space) (:col space)])
              piece-targets
              drag-enabled?))
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
              drag-enabled?])
           [board-move-preview space-bounds move-preview]])])]))
