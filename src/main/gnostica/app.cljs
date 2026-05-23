(ns gnostica.app
  (:require [gnostica.app-state :as app-state]
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

(defn orientation-label [orientation]
  (case orientation
    :portrait "Portrait"
    :landscape "Landscape"
    "Unknown"))

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

(defn board-card [{:keys [index row col orientation card]} selected? board-pieces]
  (let [{:keys [image title]} card]
    [:button.board-card
     {:type "button"
      :class (str "is-" (name orientation)
                  " is-row-" row
                  " is-col-" col
                  (when selected? " is-selected"))
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
          :aria-label "Gnostica board"}
         (for [cell cells]
           ^{:key (:index cell)}
           [board-card
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
        [territory-panel]])]))

(defn mount! []
  (rdom/render [app] (.getElementById js/document "app")))

(defn reload! []
  (mount!))

(defn init []
  (rf/dispatch-sync [::initialize])
  (mount!))
