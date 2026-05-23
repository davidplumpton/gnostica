(ns gnostica.app
  (:require [clojure.string :as str]
            [gnostica.app-state :as app-state]
            [gnostica.board-layout :as layout]
            [gnostica.cards :as cards]
            [gnostica.icon-layout :as icon-layout]
            [gnostica.icon-view :as icon-view]
            [gnostica.icons :as icons]
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
 ::select-move-wasteland-target
 (fn [db [_ row col]]
   (app-state/select-move-wasteland-target db row col)))

(rf/reg-event-db
 ::select-move-one-point-card
 (fn [db [_ card-id]]
   (app-state/select-move-one-point-card db card-id)))

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
 ::toggle-card-icon-mode
 (fn [db _]
   (app-state/toggle-card-icon-mode db)))

(rf/reg-event-db
 ::open-hotkey-help
 (fn [db _]
   (app-state/open-hotkey-help db)))

(rf/reg-event-db
 ::close-hotkey-help
 (fn [db _]
   (app-state/close-hotkey-help db)))

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
 ::card-zones
 (fn [db _]
   (app-state/card-zones db)))

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
 ::move-target-wasteland-options
 (fn [db _]
   (app-state/move-target-wasteland-options db)))

(rf/reg-sub
 ::move-one-point-card-options
 (fn [db _]
   (app-state/move-one-point-card-options db)))

(rf/reg-sub
 ::move-orientation-options
 (fn [db _]
   (app-state/move-orientation-options db)))

(rf/reg-sub
 ::draw-count-options
 (fn [db _]
   (app-state/draw-count-options db)))

(rf/reg-sub
 ::card-icon-mode
 (fn [db _]
   (app-state/card-icon-mode db)))

(rf/reg-sub
 ::hotkey-help-open?
 (fn [db _]
   (app-state/hotkey-help-open? db)))

(def hotkey-commands
  [{:keys ["?"]
    :command "Show keyboard commands"}
   {:keys ["I"]
    :command "Toggle card icon overlays"}
   {:keys ["Esc"]
    :command "Close keyboard commands"}])

(defn orientation-label [orientation]
  (case orientation
    :portrait "Portrait"
    :landscape "Landscape"
    "Unknown"))

(defn- card-icon-stack [card]
  (when-let [icon-ids (seq (icons/present-icon-ids (:gnostica-icons card)))]
    [:span.gnostica-icon-stack
     {:aria-hidden "true"
      :style (icon-layout/dom-icon-stack-style)
      :data-icon-scale icon-layout/card-icon-scale
      :data-icon-count (count icon-ids)
      :data-icon-ids (str/join "," (map name icon-ids))
      :title (icons/icon-stack-label icon-ids)}
     (for [[position icon-id] (map-indexed vector icon-ids)]
       ^{:key (str (:id card) "-" position "-" (name icon-id))}
       [icon-view/gnostica-icon icon-id])]))

(defn- card-icon-summary [card]
  (when-let [icon-ids (seq (icons/present-icon-ids (:gnostica-icons card)))]
    (icons/icon-stack-label icon-ids)))

(defn- card-aria-label [card alt-text]
  (str alt-text
       (when-let [summary (card-icon-summary card)]
         (str ", special moves: " summary))))

(defn- class-names [& classes]
  (->> classes
       (remove str/blank?)
       (str/join " ")))

(defn- card-face
  ([card class-name card-icon-mode]
   (card-face card class-name (:title card) card-icon-mode {}))
  ([card class-name alt-text card-icon-mode]
   (card-face card class-name alt-text card-icon-mode {}))
  ([card class-name alt-text card-icon-mode {:keys [focusable?]}]
   (let [has-icons? (boolean (seq (icons/present-icon-ids (:gnostica-icons card))))
         popup-mode? (= :popup card-icon-mode)]
     [:span.card-face
      {:class (class-names class-name
                           (when has-icons? "has-gnostica-icons"))
       :data-icon-mode (name card-icon-mode)
       :aria-label (when (and focusable? has-icons?)
                     (card-aria-label card alt-text))
       :tabIndex (when (and focusable? has-icons?)
                   0)}
      [:img.card-face__image
       {:src (:image card)
        :alt alt-text
        :draggable "false"}]
      (if popup-mode?
        [icon-view/card-icon-popover card]
        [card-icon-stack card])])))

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

(defn board-card [bounds {:keys [index row col orientation card] :as cell} selected? board-pieces card-icon-mode]
  (let [{:keys [title]} card]
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
                       (when-let [summary (card-icon-summary card)]
                         (str ", special moves: " summary))
                       (when (seq board-pieces)
                         (str ", pieces: "
                              (apply str (interpose "; " (map piece-summary board-pieces))))))
      :on-click #(rf/dispatch [::select-board-card index])}
     [card-face card "board-card__face" card-icon-mode]
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
        card-icon-mode @(rf/subscribe [::card-icon-mode])
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
        card-icon-mode
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
            (get pieces-by-space (:index cell))
            card-icon-mode])]])]))

(defn- card-count-label [n]
  (str n " card" (when (not= 1 n) "s")))

(defn- hand-card [card card-icon-mode]
  ^{:key (:id card)}
  [:article.hand-card
   {:class (when (card-icon-summary card) "has-gnostica-icons")}
   [card-face card "hand-card__face" (:title card) card-icon-mode {:focusable? true}]
   [:h3.hand-card__title (:title card)]])

(defn- draw-deck-zone [draw-count]
  [:article.card-pile-zone
   {:aria-label (str "Draw deck, " (card-count-label draw-count) " remaining")}
   [:div.card-pile-zone__preview.is-deck
    {:aria-hidden "true"}
    [:span]]
   [:div.card-pile-zone__body
    [:h3.card-pile-zone__title "Draw deck"]
    [:p.card-pile-zone__detail (str (card-count-label draw-count) " remaining")]]])

(defn- discard-pile-zone [discard-count top-card card-icon-mode]
  [:article.card-pile-zone
   {:aria-label (str "Discard pile, " (card-count-label discard-count))}
   (if top-card
     [card-face
      top-card
      "card-pile-zone__preview"
      (str "Top discard: " (:title top-card))
      card-icon-mode
      {:focusable? true}]
     [:div.card-pile-zone__preview.is-empty
      {:aria-hidden "true"}])
   [:div.card-pile-zone__body
    [:h3.card-pile-zone__title "Discard pile"]
    [:p.card-pile-zone__detail
     (if top-card
       (str (card-count-label discard-count) ", top card " (:title top-card))
       "No cards discarded")]]])

(defn card-zones []
  (let [current-player @(rf/subscribe [::current-player])
        card-icon-mode @(rf/subscribe [::card-icon-mode])
        {:keys [hand draw-count discard-count discard-top-card]} @(rf/subscribe [::card-zones])]
    [:section.card-zones
     {:data-hand-count (count hand)
      :data-draw-count draw-count
      :data-discard-count discard-count}
     [:div.card-zones__header
      [:div
       [:p.eyebrow "Cards"]
       [:h2.card-zones__title
        (if current-player
          (str (:name current-player) " hand")
          "Current hand")]]
      [:span.card-zones__count (card-count-label (count hand))]]
     [:div.hand-card-grid
      (for [card hand]
        ^{:key (:id card)}
        [hand-card card card-icon-mode])]
     [:div.card-pile-grid
      [draw-deck-zone draw-count]
      [discard-pile-zone discard-count discard-top-card card-icon-mode]]]))

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

(defn- wasteland-label [{:keys [row col]}]
  (str "Wasteland row "
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

(defn- same-wasteland? [selected-space space]
  (and selected-space
       (= (:row selected-space) (:row space))
       (= (:col selected-space) (:col space))))

(defn- target-choice-grid [cells wastelands selected-index selected-wasteland]
  [:div.move-step
   [:div.move-step__header
    [:span "Target"]
    [:strong
     (if-let [selected-cell (some #(when (= selected-index (:index %)) %) cells)]
       (:title (:card selected-cell))
       (if selected-wasteland
         (wasteland-label selected-wasteland)
         "None"))]]
   [:div.move-board-choice-grid
    (for [cell cells]
      ^{:key (str "territory-" (:index cell))}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-index (:index cell)) "is-selected")
        :aria-pressed (= selected-index (:index cell))
        :on-click #(rf/dispatch [::select-board-card (:index cell)])}
       (str "Territory: " (board-cell-label cell))])
    (for [space wastelands]
      ^{:key (:id space)}
      [:button.move-chip
       {:type "button"
        :class (when (same-wasteland? selected-wasteland space) "is-selected")
        :aria-pressed (same-wasteland? selected-wasteland space)
        :on-click #(rf/dispatch [::select-move-wasteland-target (:row space) (:col space)])}
       (wasteland-label space)])]])

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

(defn- one-point-card-choices [cards selected-card-id]
  [:div.move-step
   [:div.move-step__header
    [:span "One-point card"]
    [:strong
     (or (:title (some #(when (= selected-card-id (:id %)) %) cards))
         "None")]]
   (if (seq cards)
     [:div.move-choice-list
      (for [card cards]
        ^{:key (:id card)}
        [:button.move-chip
         {:type "button"
          :class (when (= selected-card-id (:id card)) "is-selected")
          :aria-pressed (= selected-card-id (:id card))
          :on-click #(rf/dispatch [::select-move-one-point-card (:id card)])}
         (:title card)])]
     [:p.move-step__empty "No one-point cards available."])])

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
        target-wasteland-options @(rf/subscribe [::move-target-wasteland-options])
        one-point-card-options @(rf/subscribe [::move-one-point-card-options])
        orientation-options @(rf/subscribe [::move-orientation-options])
        draw-options @(rf/subscribe [::draw-count-options])]
    (case source
      :activate-territory
      [:<>
       [board-choice-grid "Source territory" source-board-options (:source-board-index params)]
       (when (:source-board-index params)
         [piece-choices board piece-options (:piece-id params)])
       (when (:piece-id params)
         [target-choice-grid target-board-options
          target-wasteland-options
          (:target-board-index params)
          (:target-wasteland params)])
       (when (:target-board-index params)
         [orientation-choices orientation-options (:orientation params)])
       (when (:target-wasteland params)
         [one-point-card-choices one-point-card-options (:one-point-card-id params)])]

      :play-hand-card
      [:<>
       [hand-card-choices hand-options (:hand-card-id params)]
       (when (:hand-card-id params)
         [piece-choices board piece-options (:piece-id params)])
       (when (:piece-id params)
         [target-choice-grid target-board-options
          target-wasteland-options
          (:target-board-index params)
          (:target-wasteland params)])
       (when (:target-board-index params)
         [orientation-choices orientation-options (:orientation params)])
       (when (:target-wasteland params)
         [one-point-card-choices one-point-card-options (:one-point-card-id params)])]

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

(defn- card-icon-mode-toggle [card-icon-mode]
  [:button.card-icon-mode-toggle
   {:type "button"
    :aria-label (if (= :always card-icon-mode)
                  "Hide card icon overlays"
                  "Show card icon overlays")
    :aria-pressed (= :always card-icon-mode)
    :data-card-icon-mode (name card-icon-mode)
    :title "Toggle card icon overlays"
    :on-click #(rf/dispatch [::toggle-card-icon-mode])}
   [:span.card-icon-mode-toggle__mark
    {:aria-hidden "true"}
    "i"]
   [:span.card-icon-mode-toggle__label "Icons"]])

(defn- hotkey-help-toggle []
  [:button.hotkey-help-toggle
   {:type "button"
    :aria-label "Show keyboard commands"
    :title "Keyboard commands (?)"
    :on-click #(rf/dispatch [::open-hotkey-help])}
   [:span.hotkey-help-toggle__mark
    {:aria-hidden "true"}
    "?"]])

(defn app-header []
  (let [current-player @(rf/subscribe [::current-player])
        card-icon-mode @(rf/subscribe [::card-icon-mode])]
    [:header.app-header
     [:div.brand
      [:span.brand__mark "G"]
      [:span.brand__name "Gnostica"]]
     [:div.app-header__actions
      [hotkey-help-toggle]
      [card-icon-mode-toggle card-icon-mode]
      (when current-player
        [:div.app-status
         [:span "Current player"]
         [:strong (:name current-player)]])]]))

(defn- hotkey-help-dialog []
  (when @(rf/subscribe [::hotkey-help-open?])
    [:div.hotkey-help-overlay
     {:role "presentation"
      :on-click #(rf/dispatch [::close-hotkey-help])}
     [:section.hotkey-help-dialog
      {:role "dialog"
       :aria-modal "true"
       :aria-labelledby "hotkey-help-title"
       :on-click #(.stopPropagation %)}
      [:div.hotkey-help-dialog__header
       [:h2#hotkey-help-title "Keyboard Commands"]
       [:button.hotkey-help-dialog__close
        {:type "button"
         :aria-label "Close keyboard commands"
         :on-click #(rf/dispatch [::close-hotkey-help])}
        "Close"]]
      [:dl.hotkey-command-list
       (for [{:keys [keys command]} hotkey-commands]
         ^{:key command}
         [:div.hotkey-command
          [:dt
           (for [key-label keys]
             ^{:key key-label}
             [:kbd key-label])]
          [:dd command]])]]]))

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
  (let [setup-error @(rf/subscribe [::setup-error])
        card-icon-mode @(rf/subscribe [::card-icon-mode])]
    [:<>
     [app-header]
     (if setup-error
       [setup-error-panel setup-error]
       [:main.app-shell
        {:data-card-icon-mode (name card-icon-mode)}
        [:div.play-stack
         [board-stage]
         [card-zones]]
        [:div.side-stack
         [move-panel]
         [territory-panel]]])
     [hotkey-help-dialog]]))

(defonce keyboard-shortcut-listener
  (atom nil))

(defn- editable-target? [target]
  (let [tag-name (some-> target .-tagName str/lower-case)]
    (or (and target (.-isContentEditable target))
        (#{"input" "select" "textarea"} tag-name))))

(defn- modified-shortcut? [event]
  (or (.-altKey event)
      (.-ctrlKey event)
      (.-metaKey event)))

(defn- question-mark-key? [event key]
  (or (= "?" key)
      (and (= "Slash" (.-code event))
           (.-shiftKey event))))

(defn- install-keyboard-shortcuts! []
  (when-let [listener @keyboard-shortcut-listener]
    (.removeEventListener js/window "keydown" listener))
  (let [listener (fn [event]
                   (let [key (.-key event)
                         lower-key (str/lower-case key)]
                     (when (and (not (modified-shortcut? event))
                                (not (editable-target? (.-target event))))
                       (cond
                         (question-mark-key? event key)
                         (do
                           (.preventDefault event)
                           (rf/dispatch [::open-hotkey-help]))

                         (= "escape" lower-key)
                         (do
                           (.preventDefault event)
                           (rf/dispatch [::close-hotkey-help]))

                         (= "i" lower-key)
                         (do
                           (.preventDefault event)
                           (rf/dispatch [::toggle-card-icon-mode]))))))]
    (reset! keyboard-shortcut-listener listener)
    (.addEventListener js/window "keydown" listener)))

(defn mount! []
  (install-keyboard-shortcuts!)
  (rdom/render [app] (.getElementById js/document "app")))

(defn reload! []
  (mount!))

(defn- smoke-major-icon-deck-order []
  (let [major-hand-card (cards/card-by-id "magician")
        major-board-cards [(cards/card-by-id "chariot")
                           (cards/card-by-id "devil")]
        minor-cards (filter #(= :minor (:arcana %)) cards/deck)
        current-hand-minors (take 5 minor-cards)
        other-hand-cards (take 30 (drop 5 minor-cards))
        board-minors (take 7 (drop 35 minor-cards))
        used-card-ids (set (map :id (concat [major-hand-card]
                                            major-board-cards
                                            current-hand-minors
                                            other-hand-cards
                                            board-minors)))
        remaining-cards (remove #(contains? used-card-ids (:id %)) cards/deck)]
    (vec (concat [major-hand-card]
                 current-hand-minors
                 other-hand-cards
                 major-board-cards
                 board-minors
                 remaining-cards))))

(defn- init-options []
  (let [params (js/URLSearchParams. (.. js/window -location -search))]
    (when (= "major-icons" (.get params "gnostica-smoke"))
      {:game-options {:deck-order (smoke-major-icon-deck-order)}})))

(defn init []
  (rf/dispatch-sync [::initialize (init-options)])
  (mount!))
