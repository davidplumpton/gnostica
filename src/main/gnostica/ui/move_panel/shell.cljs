(ns gnostica.ui.move-panel.shell
  (:require [gnostica.app.events :as events]
            [gnostica.ui.move-panel.controls :as controls]
            [gnostica.ui.move-panel.ribbon :as ribbon]
            [gnostica.ui.move-panel.source :as source]
            [re-frame.core :as rf]))

(defn move-panel []
  (let [{:keys [current-player selection source-options prompt controls
                control-groups action-ribbon direct-manipulation
                actions]}
        @(rf/subscribe [events/move-panel-view])
        {:keys [source error]} selection
        {:keys [can-cancel? can-confirm?]} actions]
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
     [ribbon/action-ribbon-view action-ribbon]
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
          :disabled (not can-cancel?)
          :on-click #(rf/dispatch [events/cancel-move])}
         "Cancel"]
        [:button.move-action.is-primary
         {:type "button"
          :disabled (not can-confirm?)
          :on-click #(rf/dispatch [events/confirm-move])}
         "Confirm"]])]))
