(ns gnostica.app
  (:require [gnostica.board :as board]
            [gnostica.cards :as cards]
            [re-frame.core :as rf]
            [reagent.dom :as rdom]))

(rf/reg-event-db
 ::initialize
 (fn [_ _]
   {:board (board/initial-board cards/deck)
    :selected-board-index 0}))

(rf/reg-event-db
 ::select-board-card
 (fn [db [_ index]]
   (assoc db :selected-board-index index)))

(rf/reg-sub
 ::board
 (fn [db _]
   (:board db)))

(rf/reg-sub
 ::selected-board-index
 (fn [db _]
   (:selected-board-index db)))

(rf/reg-sub
 ::selected-board-cell
 :<- [::board]
 :<- [::selected-board-index]
 (fn [[board selected-index] _]
   (get board selected-index)))

(defn orientation-label [orientation]
  (case orientation
    :portrait "Portrait"
    :landscape "Landscape"
    "Unknown"))

(defn three-runtime []
  (when (exists? js/THREE)
    js/THREE))

(defn three-revision []
  (some-> (three-runtime) (.-REVISION)))

(defn board-card [{:keys [index row col orientation card]} selected?]
  (let [{:keys [image title]} card]
    [:button.board-card
     {:type "button"
      :class (str "is-" (name orientation)
                  " is-row-" row
                  " is-col-" col
                  (when selected? " is-selected"))
      :aria-label (str title ", " (orientation-label orientation) ", row " (inc row) ", column " (inc col))
      :on-click #(rf/dispatch [::select-board-card index])}
     [:img {:src image
            :alt title
            :draggable "false"}]]))

(defn board-stage []
  (let [cells @(rf/subscribe [::board])
        selected-index @(rf/subscribe [::selected-board-index])]
    [:section.board-area
     {:data-three-revision (or (three-revision) "unavailable")}
     [:div.board-stage
      {:role "group"
       :aria-label "Gnostica board"}
      (for [cell cells]
        ^{:key (:index cell)}
        [board-card cell (= selected-index (:index cell))])]]))

(defn territory-panel []
  (let [{:keys [row col orientation card]} @(rf/subscribe [::selected-board-cell])
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
       [:dd (str "Row " (inc row) ", Column " (inc col))]]]]))

(defn app []
  [:<>
   [:header.app-header
    [:div.brand
     [:span.brand__mark "G"]
     [:span.brand__name "Gnostica"]]]
   [:main.app-shell
    [board-stage]
    [territory-panel]]])

(defn mount! []
  (rdom/render [app] (.getElementById js/document "app")))

(defn reload! []
  (mount!))

(defn init []
  (rf/dispatch-sync [::initialize])
  (mount!))
