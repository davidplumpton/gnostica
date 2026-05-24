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

(defn app-header []
  (let [{:keys [current-player card-icon-mode]} @(rf/subscribe [events/header-view])]
    [:header.app-header
     [:div.brand
      [:span.brand__mark "G"]
      [:span.brand__name "Gnostica"]]
     [:div.app-header__actions
      [hotkey-help-toggle]
      [icon-help-toggle]
      [card-icon-mode-toggle card-icon-mode]
      (when current-player
        [:div.app-status
         [:span "Current player"]
         [:strong (:name current-player)]])]]))
