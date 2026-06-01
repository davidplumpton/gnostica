(ns gnostica.ui.header
  (:require [gnostica.app.events :as events]
            [gnostica.icon-view :as icon-view]
            [re-frame.core :as rf]))

(defn- card-icon-mode-toggle [card-icon-mode]
  [:button.card-icon-mode-toggle
   {:type "button"
    :aria-label (if (= :always card-icon-mode)
                  "Hide card icon overlays"
                  "Show card icon overlays")
    :aria-pressed (= :always card-icon-mode)
    :data-card-icon-mode (name card-icon-mode)
    :title "Toggle card icon overlays"
    :on-click #(rf/dispatch [events/toggle-card-icon-mode])}
   [:span.card-icon-mode-toggle__mark
    {:aria-hidden "true"}
    "i"]
   [:span.card-icon-mode-toggle__label "Icons"]])

(defn- hotkey-help-toggle []
  [:button.hotkey-help-toggle
   {:type "button"
    :aria-label "Show keyboard commands"
    :title "Keyboard commands (?)"
    :on-click #(rf/dispatch [events/open-hotkey-help])}
   [:span.hotkey-help-toggle__mark
    {:aria-hidden "true"}
    "?"]])

(defn- icon-help-toggle []
  [:button.icon-help-toggle
   {:type "button"
    :aria-label "Show special move icon guide"
    :title "Special move icons (G)"
    :on-click #(rf/dispatch [events/open-icon-help])}
   [:span.icon-help-toggle__mark
    {:aria-hidden "true"}
    [icon-view/gnostica-icon :world]]])

(defn- score-pill [{:keys [id score eliminated? challenging?] player-name :name}]
  [:span.app-score
   {:class (cond
             eliminated? "is-eliminated"
             challenging? "is-challenging")
    :title (str player-name " score")
    :data-player-id (name id)}
   [:span.app-score__name player-name]
   [:strong score]])

(defn- score-summary [{:keys [players target-score winner finished?]}]
  (when (seq players)
    [:div.app-scores
     (if (and finished? winner)
       [:span.app-scores__target
        [:strong (:score winner)]
        [:span (str "/" (:target-score winner) " won")]]
       [:span.app-scores__target
        [:strong target-score]
        [:span "target"]])
     (for [player players]
       ^{:key (:id player)}
       [score-pill player])]))

(defn- turn-actions [show-turn-actions? can-end-turn? can-announce-challenge?]
  (when show-turn-actions?
    [:div.turn-actions
     [:button.turn-action
      {:type "button"
       :disabled (not can-announce-challenge?)
       :aria-disabled (not can-announce-challenge?)
       :data-short-label "C"
       :on-click #(rf/dispatch [events/announce-challenge])}
      "Challenge"]
     [:button.turn-action.is-primary
      {:type "button"
       :disabled (not can-end-turn?)
       :aria-disabled (not can-end-turn?)
       :data-short-label "End"
       :on-click #(rf/dispatch [events/end-turn])}
      "End turn"]]))

(def panel-toggle-labels
  {:move "Move"
   :territory "Territory"
   :cards "Cards"})

(defn- panel-toggle [open-panels panel-id]
  (let [open? (contains? open-panels panel-id)
        label (get panel-toggle-labels panel-id)]
    [:button.panel-toggle
     {:type "button"
      :class (when open? "is-active")
      :aria-controls (str (name panel-id) "-panel")
      :aria-expanded open?
      :data-short-label (subs label 0 1)
      :title (str label " panel")
      :on-click #(rf/dispatch [events/toggle-panel panel-id])}
     label]))

(defn app-header []
  (let [{:keys [current-player card-icon-mode open-panels lobby? game-status
                show-turn-actions? can-end-turn? can-announce-challenge?]}
        @(rf/subscribe [events/header-view])]
    [:header.app-header
     [:div.brand
      [:span.brand__mark "G"]
      [:span.brand__name "Gnostica"]]
     [:div.app-header__actions
      (when-not lobby?
        [:div.panel-toggles
         [panel-toggle open-panels :move]
         [panel-toggle open-panels :territory]
         [panel-toggle open-panels :cards]])
      (when-not lobby?
        [score-summary game-status])
      [turn-actions show-turn-actions? can-end-turn? can-announce-challenge?]
      [hotkey-help-toggle]
      [icon-help-toggle]
      [card-icon-mode-toggle card-icon-mode]
      (when (or lobby? current-player)
        [:div.app-status
         (if lobby?
           [:strong "Lobby"]
           [:<>
            [:span "Current player"]
            [:strong (:name current-player)]])])]]))
