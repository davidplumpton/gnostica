(ns gnostica.app
  (:require [gnostica.app-state :as app-state]
            [gnostica.board-layout :as layout]
            [gnostica.pieces :as pieces]
            [gnostica.three-board :as three-board]
            [re-frame.core :as rf]
            [reagent.dom :as rdom]))

(rf/reg-event-db
 ::initialize
 (fn [_ [_ opts]]
   (app-state/initialize opts)))

(rf/reg-event-db
 ::select-board-card
 (fn [db [_ index]]
   (app-state/select-board-card db index)))

(rf/reg-event-db
 ::select-move-source
 (fn [db [_ source-id]]
   (app-state/select-move-source db source-id)))

(rf/reg-event-db
 ::select-move-piece
 (fn [db [_ piece-id]]
   (app-state/select-move-piece db piece-id)))

(rf/reg-event-db
 ::select-move-hand-card
 (fn [db [_ card-id]]
   (app-state/select-move-hand-card db card-id)))

(rf/reg-event-db
 ::set-move-orientation
 (fn [db [_ orientation]]
   (app-state/set-move-orientation db orientation)))

(rf/reg-event-db
 ::set-move-draw-count
 (fn [db [_ draw-count]]
   (app-state/set-move-draw-count db draw-count)))

(rf/reg-event-db
 ::confirm-move
 (fn [db _]
   (app-state/confirm-move db)))

(rf/reg-event-db
 ::cancel-move
 (fn [db _]
   (app-state/cancel-move db)))

(rf/reg-event-db
 ::clear-three-texture-errors
 (fn [db _]
   (assoc db :three-texture-errors [])))

(rf/reg-event-db
 ::three-texture-error
 (fn [db [_ image]]
   (update db :three-texture-errors (fnil conj []) image)))

(rf/reg-event-db
 ::three-renderer-error
 (fn [db [_ message]]
   (assoc db :three-renderer-error message)))

(rf/reg-sub
 ::game
 (fn [db _]
   (app-state/game db)))

(rf/reg-sub
 ::setup-error
 (fn [db _]
   (app-state/setup-error db)))

(rf/reg-sub
 ::board
 (fn [db _]
   (app-state/board db)))

(rf/reg-sub
 ::pieces
 (fn [db _]
   (app-state/board-pieces db)))

(rf/reg-sub
 ::selected-board-index
 (fn [db _]
   (app-state/selected-board-index db)))

(rf/reg-sub
 ::current-player
 (fn [db _]
   (app-state/current-player db)))

(rf/reg-sub
 ::three-texture-errors
 (fn [db _]
   (:three-texture-errors db)))

(rf/reg-sub
 ::three-renderer-error
 (fn [db _]
   (:three-renderer-error db)))

(rf/reg-sub
 ::selected-board-cell
 (fn [db _]
   (app-state/selected-board-cell db)))

(rf/reg-sub
 ::selected-board-pieces
 (fn [db _]
   (app-state/selected-board-pieces db)))

(rf/reg-sub
 ::move-selection
 (fn [db _]
   (app-state/move-selection db)))

(rf/reg-sub
 ::move-source-options
 (fn [db _]
   (app-state/move-source-options db)))

(rf/reg-sub
 ::move-prompt
 (fn [db _]
   (app-state/move-prompt db)))

(rf/reg-sub
 ::move-ready?
 (fn [db _]
   (app-state/move-ready? db)))

(rf/reg-sub
 ::move-piece-options
 (fn [db _]
   (app-state/move-piece-options db)))

(rf/reg-sub
 ::move-hand-card-options
 (fn [db _]
   (app-state/move-hand-card-options db)))

(rf/reg-sub
 ::move-source-board-options
 (fn [db _]
   (app-state/move-source-board-options db)))

(rf/reg-sub
 ::move-target-board-options
 (fn [db _]
   (app-state/move-target-board-options db)))

(rf/reg-sub
 ::move-orientation-options
 (fn [db _]
   (app-state/move-orientation-options db)))

(rf/reg-sub
 ::draw-count-options
 (fn [db _]
   (app-state/draw-count-options db)))

(defn orientation-label [orientation]
  (case orientation
    :portrait "Portrait"
    :landscape "Landscape"
    "Unknown"))

(defn- css-space-offset [step-count edge-offset]
  (str "calc(" step-count " * var(--card-step) + " edge-offset ")"))

(defn- board-stage-length [min-value max-value]
  (str "calc(" (- max-value min-value) " * var(--card-step) + var(--card-long))"))

(defn- board-stage-style [{:keys [min-row max-row min-col max-col]}]
  {"width" (board-stage-length min-col max-col)
   "height" (board-stage-length min-row max-row)})

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

(defn- piece-summary [piece]
  (let [player (pieces/player-for piece)]
    (str (:name player)
         " "
         (pieces/size-label (:size piece))
         ", "
         (pieces/orientation-label (:orientation piece)))))

(defn- board-piece-marker [slot piece]
  (let [pips (pieces/pips piece)
        player (pieces/player-for piece)]
    ^{:key (:id piece)}
    [:span.board-piece
     {:class (str "is-slot-" slot
                  " is-" (name (:size piece))
                  " is-" (name (:orientation piece)))
      :style {"--piece-color" (:css-color player)}}
     [:span.board-piece__body
      [:span.board-piece__pips
       (for [pip (range pips)]
         ^{:key pip}
         [:span.board-piece__pip])]]]))

(defn- board-wasteland [bounds {:keys [id orientation] :as space}]
  ^{:key id}
  [:div.board-wasteland
   {:class (str "is-" (name orientation))
    :style (board-space-style bounds space)
    :aria-hidden "true"}])

(defn board-card [bounds {:keys [index row col orientation card] :as cell} selected? board-pieces]
  (let [{:keys [image title]} card]
    [:button.board-card
     {:type "button"
      :class (str "is-" (name orientation)
                  " is-row-" row
                  " is-col-" col
                  (when selected? " is-selected"))
      :style (board-space-style bounds cell)
      :aria-label (str title
                       ", "
                       (orientation-label orientation)
                       ", row "
                       (inc row)
                       ", column "
                       (inc col)
                       (when (seq board-pieces)
                         (str ", pieces: "
                              (apply str (interpose "; " (map piece-summary board-pieces))))))
      :on-click #(rf/dispatch [::select-board-card index])}
     [:img {:src image
            :alt title
            :draggable "false"}]
     (when (seq board-pieces)
       [:div.board-card__pieces
        {:aria-hidden "true"}
        (for [[slot piece] (map-indexed vector (take pieces/max-pieces-per-space board-pieces))]
          (board-piece-marker slot piece))])]))

(defn board-stage []
  (let [cells @(rf/subscribe [::board])
        board-pieces @(rf/subscribe [::pieces])
        pieces-by-space (pieces/pieces-by-space board-pieces)
        wastelands (layout/wasteland-spaces cells)
        space-bounds (layout/space-bounds (concat cells wastelands))
        selected-index @(rf/subscribe [::selected-board-index])
        texture-errors @(rf/subscribe [::three-texture-errors])
        renderer-error @(rf/subscribe [::three-renderer-error])
        runtime-status (three-board/runtime-status)]
    [:section.board-area
     {:data-three-revision (or (three-board/three-revision) "unavailable")}
     (if (and (three-board/available?) (not renderer-error))
       [three-board/scene
        cells
        board-pieces
        selected-index
        texture-errors
        {:on-card-select #(rf/dispatch [::select-board-card %])
         :on-clear-texture-errors #(rf/dispatch-sync [::clear-three-texture-errors])
         :on-renderer-error #(rf/dispatch [::three-renderer-error %])
         :on-texture-error #(rf/dispatch [::three-texture-error %])}]
       [:div.board-fallback
        [:p.board-3d-status.is-error
         (if renderer-error
           (str "Three.js WebGL rendering is unavailable; using the CSS board. " renderer-error)
           (:message runtime-status))]
        [:div.board-stage
         {:role "group"
          :aria-label "Gnostica board"
          :data-wasteland-count (count wastelands)
          :data-table-surface-color three-board/table-surface-css-color
          :data-table-clear-color three-board/table-clear-css-color
          :style (board-stage-style space-bounds)}
         (for [space wastelands]
           (board-wasteland space-bounds space))
         (for [cell cells]
           ^{:key (:index cell)}
           [board-card
            space-bounds
            cell
            (= selected-index (:index cell))
            (get pieces-by-space (:index cell))])]])]))

(defn territory-panel []
  (let [{:keys [row col orientation card]} @(rf/subscribe [::selected-board-cell])
        selected-pieces @(rf/subscribe [::selected-board-pieces])
        {:keys [title group rank]} card]
    [:aside.territory-panel
     [:p.eyebrow "Territory"]
     [:h1 title]
     [:dl.territory-facts
      [:div
       [:dt "Arcana"]
       [:dd group]]
      (when rank
        [:div
         [:dt "Rank"]
         [:dd rank]])
      [:div
       [:dt "Orientation"]
       [:dd (orientation-label orientation)]]
      [:div
       [:dt "Position"]
       [:dd (str "Row " (inc row) ", Column " (inc col))]]
      [:div
       [:dt "Pieces"]
       [:dd
        (if (seq selected-pieces)
          [:ul.territory-pieces
           (for [piece selected-pieces]
             (let [player (pieces/player-for piece)]
               ^{:key (:id piece)}
               [:li
                [:span.territory-piece-swatch
                 {:style {"--piece-color" (:css-color player)}}]
                [:span (piece-summary piece)]]))]
          "None")]]]]))

(defn- move-source-picker [options selected-source]
  [:div.move-source-list
   (for [{:keys [id label summary enabled? reason]} options]
     ^{:key id}
     [:button.move-source-option
      {:type "button"
       :class (when (= selected-source id) "is-selected")
       :disabled (not enabled?)
       :aria-pressed (= selected-source id)
       :on-click #(rf/dispatch [::select-move-source id])}
      [:span.move-source-option__label label]
      [:span.move-source-option__summary (if enabled? summary reason)]])])

(defn- board-cell-label [{:keys [row col card]}]
  (str (:title card)
       ", row "
       (inc row)
       ", column "
       (inc col)))

(defn- board-choice-grid [label cells selected-index]
  [:div.move-step
   [:div.move-step__header
    [:span label]
    [:strong
     (if-let [selected-cell (some #(when (= selected-index (:index %)) %) cells)]
       (:title (:card selected-cell))
       "None")]]
   [:div.move-board-choice-grid
    (for [cell cells]
      ^{:key (:index cell)}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-index (:index cell)) "is-selected")
        :aria-pressed (= selected-index (:index cell))
        :on-click #(rf/dispatch [::select-board-card (:index cell)])}
       (board-cell-label cell)])]])

(defn- hand-card-choices [cards selected-card-id]
  [:div.move-step
   [:div.move-step__header
    [:span "Hand card"]
    [:strong
     (or (:title (some #(when (= selected-card-id (:id %)) %) cards))
         "None")]]
   [:div.move-choice-list
    (for [card cards]
      ^{:key (:id card)}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-card-id (:id card)) "is-selected")
        :aria-pressed (= selected-card-id (:id card))
        :on-click #(rf/dispatch [::select-move-hand-card (:id card)])}
       (:title card)])]])

(defn- piece-choice-label [board piece]
  (let [cell (get board (:space-index piece))]
    (str (piece-summary piece)
         " on "
         (:title (:card cell)))))

(defn- piece-choices [board pieces selected-piece-id]
  [:div.move-step
   [:div.move-step__header
    [:span "Minion"]
    [:strong
     (if-let [piece (some #(when (= selected-piece-id (:id %)) %) pieces)]
       (piece-summary piece)
       "None")]]
   (if (seq pieces)
     [:div.move-choice-list
      (for [piece pieces]
        ^{:key (:id piece)}
        [:button.move-chip
         {:type "button"
          :class (when (= selected-piece-id (:id piece)) "is-selected")
          :aria-pressed (= selected-piece-id (:id piece))
          :on-click #(rf/dispatch [::select-move-piece (:id piece)])}
         (piece-choice-label board piece)])]
     [:p.move-step__empty "No pieces available."])])

(defn- orientation-choices [options selected-orientation]
  [:div.move-step
   [:div.move-step__header
    [:span "Orientation"]
    [:strong (pieces/orientation-label selected-orientation)]]
   [:div.move-choice-list.is-compact
    (for [{:keys [id label]} options]
      ^{:key id}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-orientation id) "is-selected")
        :aria-pressed (= selected-orientation id)
        :on-click #(rf/dispatch [::set-move-orientation id])}
       label])]])

(defn- draw-count-choices [options selected-count]
  [:div.move-step
   [:div.move-step__header
    [:span "Draw count"]
    [:strong (or selected-count "None")]]
   [:div.move-choice-list.is-compact
    (for [draw-count options]
      ^{:key draw-count}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-count draw-count) "is-selected")
        :aria-pressed (= selected-count draw-count)
        :on-click #(rf/dispatch [::set-move-draw-count draw-count])}
       draw-count])]])

(defn- move-active-controls [selection]
  (let [{:keys [source params]} selection
        board @(rf/subscribe [::board])
        piece-options @(rf/subscribe [::move-piece-options])
        hand-options @(rf/subscribe [::move-hand-card-options])
        source-board-options @(rf/subscribe [::move-source-board-options])
        target-board-options @(rf/subscribe [::move-target-board-options])
        orientation-options @(rf/subscribe [::move-orientation-options])
        draw-options @(rf/subscribe [::draw-count-options])]
    (case source
      :activate-territory
      [:<>
       [board-choice-grid "Source territory" source-board-options (:source-board-index params)]
       (when (:source-board-index params)
         [piece-choices board piece-options (:piece-id params)])
       (when (:piece-id params)
         [board-choice-grid "Target territory" target-board-options (:target-board-index params)])]

      :play-hand-card
      [:<>
       [hand-card-choices hand-options (:hand-card-id params)]
       (when (:hand-card-id params)
         [piece-choices board piece-options (:piece-id params)])
       (when (:piece-id params)
         [board-choice-grid "Target territory" target-board-options (:target-board-index params)])]

      :draw-cards
      [draw-count-choices draw-options (:draw-count params)]

      :orient-piece
      [:<>
       [piece-choices board piece-options (:piece-id params)]
       (when (:piece-id params)
         [orientation-choices orientation-options (:orientation params)])]

      :place-initial-small
      [:<>
       [board-choice-grid "Target territory" target-board-options (:target-board-index params)]
       (when (:target-board-index params)
         [orientation-choices orientation-options (:orientation params)])]

      nil)))

(defn move-panel []
  (let [current-player @(rf/subscribe [::current-player])
        selection @(rf/subscribe [::move-selection])
        source-options @(rf/subscribe [::move-source-options])
        prompt @(rf/subscribe [::move-prompt])
        ready? @(rf/subscribe [::move-ready?])
        {:keys [source error]} selection]
    [:section.move-panel
     [:div.move-panel__heading
      [:p.eyebrow "Move"]
      [:h2 (if current-player
             (:name current-player)
             "No player")]]
     [move-source-picker source-options source]
     [:p.move-panel__prompt prompt]
     (when source
       [move-active-controls selection])
     (when error
       [:p.move-error
        {:role "alert"}
        (:message error)])
     (when source
       [:div.move-actions
        [:button.move-action
         {:type "button"
          :on-click #(rf/dispatch [::cancel-move])}
         "Cancel"]
        [:button.move-action.is-primary
         {:type "button"
          :disabled (not ready?)
          :on-click #(rf/dispatch [::confirm-move])}
         "Confirm"]])]))

(defn app-header []
  (let [current-player @(rf/subscribe [::current-player])]
    [:header.app-header
     [:div.brand
      [:span.brand__mark "G"]
      [:span.brand__name "Gnostica"]]
     (when current-player
       [:div.app-status
        [:span "Current player"]
        [:strong (:name current-player)]])]))

(defn setup-error-panel [error]
  [:main.app-shell.is-setup-error
   [:section.setup-error
    {:role "alert"}
    [:p.eyebrow "Setup error"]
    [:h1 "Game setup failed"]
    [:p.setup-error__message (:message error)]
    (when (seq (:data error))
      [:pre.setup-error__data (pr-str (:data error))])]])

(defn app []
  (let [setup-error @(rf/subscribe [::setup-error])]
    [:<>
     [app-header]
     (if setup-error
       [setup-error-panel setup-error]
       [:main.app-shell
        [board-stage]
        [:div.side-stack
         [move-panel]
         [territory-panel]]])]))

(defn mount! []
  (rdom/render [app] (.getElementById js/document "app")))

(defn reload! []
  (mount!))

(defn init []
  (rf/dispatch-sync [::initialize])
  (mount!))
