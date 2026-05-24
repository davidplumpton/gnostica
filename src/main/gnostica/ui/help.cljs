(ns gnostica.ui.help
  (:require [clojure.string :as str]
            [gnostica.app.events :as events]
            [gnostica.cards :as cards]
            [gnostica.icon-view :as icon-view]
            [gnostica.icons :as icons]
            [re-frame.core :as rf]))

(def hotkey-commands
  [{:keys ["?"]
    :command "Show keyboard commands"}
   {:keys ["G"]
    :command "Show special move icon guide"}
   {:keys ["I"]
    :command "Toggle card icon overlays"}
   {:keys ["W/A/S/D" "Arrow keys"]
    :command "Move the 3D board view when the board is focused"}
   {:keys ["Esc"]
    :command "Close open help dialogs"}])

(defn hotkey-help-dialog [open?]
  (when open?
    [:div.hotkey-help-overlay
     {:role "presentation"
      :on-click #(rf/dispatch [events/close-hotkey-help])}
     [:section.hotkey-help-dialog
      {:role "dialog"
       :aria-modal "true"
       :aria-labelledby "hotkey-help-title"
       :on-click #(.stopPropagation %)}
      [:div.hotkey-help-dialog__header
       [:h2#hotkey-help-title "Keyboard Commands"]
       [:button.hotkey-help-dialog__close
        {:type "button"
         :aria-label "Close keyboard commands"
         :on-click #(rf/dispatch [events/close-hotkey-help])}
        "Close"]]
      [:dl.hotkey-command-list
       (for [{:keys [keys command]} hotkey-commands]
         ^{:key command}
         [:div.hotkey-command
          [:dt
           (for [key-label keys]
             ^{:key key-label}
             [:kbd key-label])]
          [:dd command]])]]]))

(defn- card-titles-for-icon [icon-id]
  (->> cards/deck
       (filter #(some #{icon-id} (:gnostica-icons %)))
       (map :title)
       vec))

(defn icon-help-dialog [open?]
  (when open?
    [:div.icon-help-overlay
     {:role "presentation"
      :on-click #(rf/dispatch [events/close-icon-help])}
     [:section.icon-help-dialog
      {:role "dialog"
       :aria-modal "true"
       :aria-labelledby "icon-help-title"
       :on-click #(.stopPropagation %)}
      [:div.icon-help-dialog__header
       [:h2#icon-help-title "Special Move Icons"]
       [:button.icon-help-dialog__close
        {:type "button"
         :aria-label "Close special move icon guide"
         :on-click #(rf/dispatch [events/close-icon-help])}
        "Close"]]
      [:div.icon-help-list
       (for [{:keys [id label description]} (icons/icon-glossary-items)]
         (let [card-titles (card-titles-for-icon id)]
           ^{:key (name id)}
           [:div.icon-help-item
            [:span.icon-help-item__icon
             [icon-view/gnostica-icon id]]
            [:div.icon-help-item__body
             [:h3 label]
             [:p.icon-help-item__description description]
             (when (seq card-titles)
               [:p.icon-help-item__cards
                [:span "Cards"]
                (str " " (str/join ", " card-titles))])]]))]]]))

(defn help-dialogs []
  (let [{:keys [hotkey-help-open? icon-help-open?]} @(rf/subscribe [events/help-dialogs-view])]
    [:<>
     [hotkey-help-dialog hotkey-help-open?]
     [icon-help-dialog icon-help-open?]]))
