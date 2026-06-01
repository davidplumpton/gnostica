(ns gnostica.ui.move-panel.pending-tray
  (:require [gnostica.app.events :as events]
            [gnostica.ui.move-panel.ribbon :as ribbon]
            [re-frame.core :as rf]))

(defn- pending-missing-fields [alternatives]
  (when (seq alternatives)
    [:ul.pending-move-tray__missing
     (for [{:keys [field prompt]} alternatives]
       ^{:key (name field)}
       [:li prompt])]))

(defn pending-move-tray []
  (let [{:keys [active? summary alternatives error ready? can-confirm?
                action-ribbon
                can-cancel? detailed-entry-label detailed-open?
                detailed-entry-available?]}
        @(rf/subscribe [events/pending-move-tray-view])]
    (when active?
      [:section.pending-move-tray
       {:aria-live "polite"}
       [:div.pending-move-tray__heading
        [:p.eyebrow "Pending move"]
        [:span.pending-move-tray__status
         (if ready? "Ready" "Needs choice")]]
       [:p.pending-move-tray__summary summary]
       [ribbon/action-ribbon-view action-ribbon]
       (when (and (not ready?) (seq alternatives))
         [pending-missing-fields alternatives])
       (when error
         [:p.move-error
          {:role "alert"}
          (:message error)])
       [:div.move-actions
        [:button.move-action
         {:type "button"
          :disabled (not can-cancel?)
          :on-click #(rf/dispatch [events/cancel-gesture-intent])}
         "Cancel"]
        (when detailed-entry-available?
          [:button.move-action
           {:type "button"
            :aria-pressed detailed-open?
            :on-click #(rf/dispatch [events/open-gesture-detailed-entry])}
           detailed-entry-label])
        [:button.move-action.is-primary
         {:type "button"
          :disabled (not can-confirm?)
          :on-click #(rf/dispatch [events/confirm-move])}
         "Confirm"]]])))
