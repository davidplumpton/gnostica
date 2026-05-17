(ns gnostica.app
  (:require [clojure.string :as str]
            [gnostica.cards :as cards]
            [re-frame.core :as rf]
            [reagent.dom :as rdom]))

(rf/reg-event-db
 ::initialize
 (fn [_ _]
   {:deck cards/deck
    :query ""
    :selected-id (:id (first cards/deck))
    :drawn-ids []}))

(rf/reg-event-db
 ::set-query
 (fn [db [_ query]]
   (assoc db :query query)))

(rf/reg-event-db
 ::select-card
 (fn [db [_ card-id]]
   (assoc db :selected-id card-id)))

(rf/reg-event-db
 ::draw-card
 (fn [db _]
   (let [card (rand-nth (:deck db))]
     (-> db
         (assoc :selected-id (:id card))
         (update :drawn-ids #(vec (take 9 (cons (:id card) %))))))))

(rf/reg-event-db
 ::clear-draws
 (fn [db _]
   (assoc db :drawn-ids [])))

(rf/reg-sub
 ::deck
 (fn [db _]
   (:deck db)))

(rf/reg-sub
 ::query
 (fn [db _]
   (:query db)))

(rf/reg-sub
 ::selected-card
 (fn [db _]
   (cards/card-by-id (:selected-id db))))

(rf/reg-sub
 ::filtered-deck
 :<- [::deck]
 :<- [::query]
 (fn [[deck query] _]
   (let [needle (str/lower-case (str/trim query))]
     (if (str/blank? needle)
       deck
       (filterv
        (fn [{:keys [title group suit rank]}]
          (str/includes?
           (str/lower-case (str title " " group " " suit " " rank))
           needle))
        deck)))))

(rf/reg-sub
 ::drawn-cards
 (fn [db _]
   (keep cards/card-by-id (:drawn-ids db))))

(defn card-tile [{:keys [id image title group]} selected?]
  [:button.card-tile
   {:type "button"
    :class (when selected? "is-selected")
    :on-click #(rf/dispatch [::select-card id])}
   [:img {:src image :alt title :loading "lazy"}]
   [:span.card-tile__title title]
   [:span.card-tile__meta group]])

(defn selected-card-panel []
  (let [card @(rf/subscribe [::selected-card])]
    [:section.focus
     [:div.focus__image
      [:img {:src (:image card) :alt (:title card)}]]
     [:div.focus__copy
      [:p.eyebrow (:group card)]
      [:h1 (:title card)]
      (when-let [rank (:rank card)]
        [:p.focus__rank rank])
      [:div.focus__actions
       [:button.primary-action
        {:type "button" :on-click #(rf/dispatch [::draw-card])}
        "Draw"]
       [:button.secondary-action
        {:type "button" :on-click #(rf/dispatch [::clear-draws])}
        "Clear"]]]]))

(defn recent-draws []
  (let [drawn @(rf/subscribe [::drawn-cards])]
    [:aside.draws
     [:div.draws__header
      [:h2 "Spread"]
      [:span (count drawn)]]
     (if (seq drawn)
       [:div.draws__list
        (for [{:keys [id image title group]} drawn]
          ^{:key id}
          [:button.draws__item
           {:type "button" :on-click #(rf/dispatch [::select-card id])}
           [:img {:src image :alt "" :loading "lazy"}]
           [:span
            [:strong title]
            [:small group]]])]
       [:div.draws__empty "No cards drawn"])]))

(defn deck-browser []
  (let [query @(rf/subscribe [::query])
        selected-id (:id @(rf/subscribe [::selected-card]))
        filtered @(rf/subscribe [::filtered-deck])]
    [:section.deck-browser
     [:div.deck-browser__bar
      [:label.search
       [:span "Search"]
       [:input
        {:type "search"
         :value query
         :placeholder "Card, suit, or arcana"
         :on-change #(rf/dispatch [::set-query (.. % -target -value)])}]]
      [:span.deck-count (str (count filtered) " cards")]]
     [:div.card-grid
      (for [card filtered]
        ^{:key (:id card)}
        [card-tile card (= selected-id (:id card))])]]))

(defn app []
  [:<>
   [:header.app-header
    [:div.brand
     [:span.brand__mark "G"]
     [:span.brand__name "Gnostica"]]
    [:button.header-draw
     {:type "button" :on-click #(rf/dispatch [::draw-card])}
     "Draw"]]
   [:main.app-shell
    [selected-card-panel]
    [recent-draws]
    [deck-browser]]])

(defn mount! []
  (rdom/render [app] (.getElementById js/document "app")))

(defn reload! []
  (mount!))

(defn init []
  (rf/dispatch-sync [::initialize])
  (mount!))
