(ns gnostica.ui.card-zones
  (:require [gnostica.app.events :as events]
            [gnostica.gesture-input :as gesture-input]
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

(defn- card-action-event [card descriptor]
  (case (:role descriptor)
    :discard [events/toggle-move-discard-card (:id card)]
    :territory-card [events/select-move-one-point-card (:id card)]
    :replacement-card [events/select-move-replacement-card (:id card)]
    [events/start-gesture-intent (gesture-input/hand-card-source-input card)]))

(defn- discard-card-action-event [card descriptor]
  (case (:role descriptor)
    :judgement-card [events/toggle-move-judgement-card (:id card)]
    :replacement-card [events/select-move-replacement-card (:id card)]
    nil))

(defn- hand-card-drag-input [card descriptor]
  (gesture-input/hand-card-drag-input card descriptor))

(defn- discard-card-drag-input [card descriptor]
  (gesture-input/discard-card-drag-input card descriptor))

(defn- activation-key? [event]
  (contains? #{"Enter" " "} (.-key event)))

(defn- dispatch-on-activation-key [event dispatch-value]
  (when (activation-key? event)
    (.preventDefault event)
    (rf/dispatch dispatch-value)))

(defn- hand-card [card card-icon-mode descriptor drag-enabled?]
  (let [drag-input (when drag-enabled?
                     (hand-card-drag-input card descriptor))]
    ^{:key (:id card)}
    [:article.hand-card
     {:class (str (when (card-ui/card-icon-summary card) "has-gnostica-icons")
                  (target-status-class descriptor))
      :role "button"
      :tabIndex 0
      :title (target-reason descriptor)
      :aria-pressed (true? (:selected? descriptor))
      :draggable (if drag-input "true" "false")
      :on-click #(rf/dispatch (card-action-event card descriptor))
      :on-key-down #(dispatch-on-activation-key %
                                                (card-action-event card descriptor))
      :on-drag-start (fn [event]
                       (when drag-input
                         (gesture-input/set-gesture-data! (.-dataTransfer event)
                                                          drag-input)
                         (when-not (:preserve-selection? drag-input)
                           (rf/dispatch [events/start-gesture-intent drag-input]))))
      :on-drag-end #(gesture-input/clear-active-gesture-input!)}
     [card-ui/card-face card "hand-card__face" (:title card) card-icon-mode {:focusable? true}]
     [:h3.hand-card__title (:title card)]]))

(defn- draw-deck-zone [draw-count descriptor]
  [:button.card-pile-zone
   {:class (target-status-class descriptor)
    :type "button"
    :title (target-reason descriptor)
    :aria-label (str "Draw deck, " (ui/card-count-label draw-count) " remaining")
    :on-click #(rf/dispatch [events/start-gesture-intent
                             (gesture-input/draw-pile-source-input)])}
   [:div.card-pile-zone__preview.is-deck
    {:aria-hidden "true"}
    [:span]]
   [:div.card-pile-zone__body
    [:h3.card-pile-zone__title "Draw deck"]
    [:p.card-pile-zone__detail (str (ui/card-count-label draw-count) " remaining")]]])

(defn- discard-pile-zone [discard-count top-card card-icon-mode descriptor
                           drag-enabled?]
  (let [drag-input (when (and top-card drag-enabled?)
                     (discard-card-drag-input top-card descriptor))]
    [:button.card-pile-zone
     {:class (target-status-class descriptor)
      :type "button"
      :title (target-reason descriptor)
      :aria-label (str "Discard pile, " (ui/card-count-label discard-count))
      :disabled (nil? (and top-card
                           (discard-card-action-event top-card descriptor)))
      :draggable (if drag-input "true" "false")
      :on-click #(when-let [event (and top-card
                                        (discard-card-action-event top-card descriptor))]
                   (rf/dispatch event))
      :on-drag-start (fn [event]
                       (when drag-input
                         (gesture-input/set-gesture-data! (.-dataTransfer event)
                                                          drag-input)))
      :on-drag-end #(gesture-input/clear-active-gesture-input!)}
     (if top-card
       [card-ui/card-face
        top-card
        "card-pile-zone__preview"
        (str "Top discard: " (:title top-card))
        card-icon-mode]
       [:div.card-pile-zone__preview.is-empty
        {:aria-hidden "true"}])
     [:div.card-pile-zone__body
      [:h3.card-pile-zone__title "Discard pile"]
      [:p.card-pile-zone__detail
       (if top-card
         (str (ui/card-count-label discard-count) ", top card " (:title top-card))
         "No cards discarded")]]]))

(defn card-zones []
  (let [{:keys [current-player card-icon-mode zones legal-targets
                direct-manipulation]}
        @(rf/subscribe [events/card-zones-view])
        {:keys [hand draw-count discard-count discard-top-card]} zones
        hand-targets (:hand-cards legal-targets)
        discard-targets (:discard-cards legal-targets)
        drag-enabled? (true? (:pointer-drag-enabled? direct-manipulation))]
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
        [hand-card card card-icon-mode (descriptor-for-card hand-targets card)
         drag-enabled?])]
     [:div.card-pile-grid
      [draw-deck-zone draw-count (:draw-pile legal-targets)]
      [discard-pile-zone
       discard-count
       discard-top-card
       card-icon-mode
       (descriptor-for-card discard-targets discard-top-card)
       drag-enabled?]]]))
