(ns gnostica.ui.lobby
  (:require [gnostica.app.events :as events]
            [re-frame.core :as rf]))

(defn- event-value [event]
  (.. event -target -value))

(defn- colour-swatch [colour]
  [:span.lobby-colour-swatch
   {:style {"--player-color" (:css-color colour)}
    :aria-hidden "true"}])

(defn- colour-select [slot-id player]
  [:label.lobby-field
   [:span.lobby-field__label "Colour"]
   [:span.lobby-colour-select
    [colour-swatch (:colour player)]
    [:select
     {:value (name (:id player))
      :on-change #(rf/dispatch [events/set-lobby-player-colour
                                 slot-id
                                 (event-value %)])}
     (for [{:keys [id name disabled?]} (:colour-options player)]
       ^{:key id}
       [:option
        {:value (name id)
         :disabled disabled?}
        name])]]])

(defn- player-row [{:keys [slot-id name] :as player}]
  [:article.lobby-player
   [:div.lobby-player__badge
    [colour-swatch (:colour player)]]
   [:label.lobby-field
    [:span.lobby-field__label "Name"]
    [:input
     {:type "text"
      :value name
      :aria-label "Player display name"
      :on-change #(rf/dispatch [events/set-lobby-player-name
                                 slot-id
                                 (event-value %)])}]]
   [colour-select slot-id player]
   [:button.lobby-remove
    {:type "button"
     :aria-label (str "Remove " (or name "player"))
     :on-click #(rf/dispatch [events/remove-lobby-player slot-id])}
    "Remove"]])

(defn local-lobby []
  (let [{:keys [players player-count min-players max-players can-add? can-start? error]}
        @(rf/subscribe [events/lobby-view])]
    [:main.app-shell.is-lobby
     [:section.local-lobby
      {:data-player-count player-count}
      [:div.local-lobby__header
       [:div
        [:p.eyebrow "Local lobby"]
        [:h1 "Players"]]
       [:div.local-lobby__count
        [:strong player-count]
        [:span (str "/" max-players)]]]
      [:div.local-lobby__players
       (for [player players]
         ^{:key (:slot-id player)}
         [player-row player])]
      (when error
        [:p.local-lobby__error
         {:role "alert"}
         (:message error)])
      [:div.local-lobby__actions
       [:button.lobby-action
        {:type "button"
         :disabled (not can-add?)
         :on-click #(rf/dispatch [events/add-lobby-player])}
        "Add player"]
       [:button.lobby-action.is-primary
        {:type "button"
         :disabled (not can-start?)
         :aria-disabled (not can-start?)
         :on-click #(rf/dispatch [events/start-lobby-game])}
        (if (< player-count min-players)
          "Need players"
          "Start game")]]]]))
