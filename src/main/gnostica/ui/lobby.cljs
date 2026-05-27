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
     (for [{:keys [id disabled?] option-name :name} (:colour-options player)]
       ^{:key id}
       [:option
        {:value (name id)
         :disabled disabled?}
        option-name])]]])

(defn- control-field [{:keys [controller-name]}]
  (when controller-name
    [:div.lobby-field
     [:span.lobby-field__label "Control"]
     [:output.lobby-control
      {:aria-label "Seat controller"}
      controller-name]]))

(defn- target-score-field [target-score options]
  [:label.lobby-field.local-lobby__target-score
   [:span.lobby-field__label "Target"]
   [:select
    {:value (str target-score)
     :aria-label "Target score"
     :on-change #(rf/dispatch [events/set-lobby-target-score
                                (event-value %)])}
    (for [score options]
      ^{:key score}
      [:option
       {:value (str score)}
       (str score " points")])]])

(defn- player-row [{:keys [slot-id name controller-name] :as player}]
  [:article
   {:class (cond-> "lobby-player"
             controller-name
             (str " has-control"))}
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
   [control-field player]
   [:button.lobby-remove
    {:type "button"
     :aria-label (str "Remove " (or name "player"))
     :on-click #(rf/dispatch [events/remove-lobby-player slot-id])}
    "Remove"]])

(defn local-lobby []
  (let [{:keys [players player-count min-players max-players can-add? can-start? error
                target-score target-score-options]}
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
      [target-score-field target-score target-score-options]
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
