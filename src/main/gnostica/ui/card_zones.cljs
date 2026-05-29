(ns gnostica.ui.card-zones
  (:require [gnostica.app.events :as events]
            [gnostica.ui.card :as card-ui]
            [gnostica.ui.common :as ui]
            [re-frame.core :as rf]))

(defn- target-status-class [{:keys [active? status]}]
  (when active?
    (case status
      :legal " is-legal-target"
      :disabled " is-disabled-target"
      "")))

(defn- target-reason [descriptor]
  (or (:reason descriptor)
      (get-in descriptor [:error :message])))

(defn- descriptors-by-card-id [descriptors]
  (into {}
        (map (fn [descriptor]
               [(:card-id descriptor) descriptor]))
        (filter :active? descriptors)))

(defn- descriptor-for-card [descriptors card]
  (get (descriptors-by-card-id descriptors) (:id card)))

(defn- hand-card [card card-icon-mode descriptor]
  ^{:key (:id card)}
  [:article.hand-card
   {:class (str (when (card-ui/card-icon-summary card) "has-gnostica-icons")
                (target-status-class descriptor))
    :title (target-reason descriptor)}
   [card-ui/card-face card "hand-card__face" (:title card) card-icon-mode {:focusable? true}]
   [:h3.hand-card__title (:title card)]])

(defn- draw-deck-zone [draw-count descriptor]
  [:article.card-pile-zone
   {:class (target-status-class descriptor)
    :title (target-reason descriptor)
    :aria-label (str "Draw deck, " (ui/card-count-label draw-count) " remaining")}
   [:div.card-pile-zone__preview.is-deck
    {:aria-hidden "true"}
    [:span]]
   [:div.card-pile-zone__body
    [:h3.card-pile-zone__title "Draw deck"]
    [:p.card-pile-zone__detail (str (ui/card-count-label draw-count) " remaining")]]])

(defn- discard-pile-zone [discard-count top-card card-icon-mode descriptor]
  [:article.card-pile-zone
   {:class (target-status-class descriptor)
    :title (target-reason descriptor)
    :aria-label (str "Discard pile, " (ui/card-count-label discard-count))}
   (if top-card
     [card-ui/card-face
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
       (str (ui/card-count-label discard-count) ", top card " (:title top-card))
       "No cards discarded")]]])

(defn card-zones []
  (let [{:keys [current-player card-icon-mode zones legal-targets]}
        @(rf/subscribe [events/card-zones-view])
        {:keys [hand draw-count discard-count discard-top-card]} zones
        hand-targets (:hand-cards legal-targets)
        discard-targets (:discard-cards legal-targets)]
    [:section.card-zones
     {:id "cards-panel"
      :data-hand-count (count hand)
      :data-draw-count draw-count
      :data-discard-count discard-count}
     [:div.card-zones__header
      [:div
       [:p.eyebrow "Cards"]
       [:h2.card-zones__title
        (if current-player
          (str (:name current-player) " hand")
          "Current hand")]]
      [:div.panel-heading-actions
       [:span.card-zones__count (ui/card-count-label (count hand))]
       [:button.panel-close
        {:type "button"
         :aria-label "Close cards panel"
         :on-click #(rf/dispatch [events/set-panel-open :cards false])}
        "Close"]]]
     [:div.hand-card-grid
      (for [card hand]
        ^{:key (:id card)}
        [hand-card card card-icon-mode (descriptor-for-card hand-targets card)])]
     [:div.card-pile-grid
      [draw-deck-zone draw-count (:draw-pile legal-targets)]
      [discard-pile-zone
       discard-count
       discard-top-card
       card-icon-mode
       (descriptor-for-card discard-targets discard-top-card)]]]))
