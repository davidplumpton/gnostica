(ns gnostica.ui.move-panel.shell
  (:require [gnostica.app.events :as events]
            [gnostica.ui.move-panel.controls :as controls]
            [gnostica.ui.move-panel.ribbon :as ribbon]
            [gnostica.ui.move-panel.source :as source]
            [re-frame.core :as rf]))

(defn move-panel []
  (let [{:keys [current-player selection source-options prompt ready? controls
                control-groups action-ribbon direct-manipulation]}
        @(rf/subscribe [events/move-panel-view])
        {:keys [source error]} selection]
    [:section.move-panel
     {:id "move-panel"
      :class (if source "is-active" "is-idle")}
     [:div.move-panel__heading
      [:p.eyebrow "Move"]
      [:div.panel-heading-actions
       [:h2 (if current-player
              (:name current-player)
              "No player")]
       [source/detailed-entry-default-toggle direct-manipulation]
       [:button.panel-close
        {:type "button"
         :aria-label "Close move panel"
         :on-click #(rf/dispatch [events/set-panel-open :move false])}
        "Close"]]]
     [source/move-source-picker source-options source current-player direct-manipulation]
     [:p.move-panel__prompt prompt]
     [ribbon/action-ribbon-view action-ribbon {:actions? true}]
     (when source
       [controls/active-controls selection controls control-groups])
     (when error
       [:p.move-error
        {:role "alert"}
        (:message error)])
     (when source
       [:div.move-actions
        [:button.move-action
         {:type "button"
          :on-click #(rf/dispatch [events/cancel-move])}
         "Cancel"]
        [:button.move-action.is-primary
         {:type "button"
          :disabled (not ready?)
          :on-click #(rf/dispatch [events/confirm-move])}
         "Confirm"]])]))
