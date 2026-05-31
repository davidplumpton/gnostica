(ns gnostica.ui.lobby
  (:require [gnostica.app.events :as events]
            [re-frame.core :as rf]))

(defn- event-value [event]
  (.. event -target -value))

(defn- colour-swatch [colour]
  [:span.lobby-colour-swatch
   {:style {"--player-color" (:css-color colour)}
    :aria-hidden "true"}])

(defn- colour-select [slot-id player locked?]
  [:label.lobby-field
   [:span.lobby-field__label "Colour"]
   [:span.lobby-colour-select
    [colour-swatch (:colour player)]
    [:select
     {:value (name (:id player))
      :disabled locked?
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

(defn- target-score-field [target-score options locked?]
  [:label.lobby-field.local-lobby__target-score
   [:span.lobby-field__label "Target"]
   [:select
    {:value (str target-score)
     :disabled locked?
     :aria-label "Target score"
     :on-change #(rf/dispatch [events/set-lobby-target-score
                                (event-value %)])}
    (for [score options]
      ^{:key score}
      [:option
       {:value (str score)}
       (str score " points")])]])

(defn- player-row [{:keys [slot-id name controller-name] :as player} locked?]
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
      :disabled locked?
      :aria-label "Player display name"
      :on-change #(rf/dispatch [events/set-lobby-player-name
                                 slot-id
                                 (event-value %)])}]]
   [colour-select slot-id player locked?]
   [control-field player]
   [:button.lobby-remove
    {:type "button"
     :disabled locked?
     :aria-label (str "Remove " (or name "player"))
     :on-click #(rf/dispatch [events/remove-lobby-player slot-id])}
    "Remove"]])

(defn- bid-card-select [{:keys [id name bid-card-id bid-card-selected?
                                bid-card-options]}]
  (if bid-card-selected?
    [:div.lobby-field.starting-bid__field
     [:span.lobby-field__label name]
     [:div.starting-bid__masked-choice
      [:output.lobby-control
       {:aria-label (str name " bid card")}
       "Bid selected"]
      [:button.lobby-remove
       {:type "button"
        :aria-label (str "Clear " name " bid card")
        :on-click #(rf/dispatch [events/select-lobby-bid-card id ""])}
       "Clear"]]]
    [:label.lobby-field.starting-bid__field
     [:span.lobby-field__label name]
     [:select
      {:value (or bid-card-id "")
       :aria-label (str name " bid card")
       :on-change #(rf/dispatch [events/select-lobby-bid-card
                                  id
                                  (event-value %)])}
      [:option {:value ""} "Choose bid"]
      (for [{card-id :id title :title} bid-card-options]
        ^{:key card-id}
        [:option {:value card-id} title])]]))

(defn- bid-history-row [{:keys [round bids winner-name tied-players]}]
  [:li.starting-bid__history-row
   [:span.starting-bid__history-round (str "Round " round)]
   [:span.starting-bid__history-cards
    (apply str
           (interpose
            " | "
            (map (fn [{:keys [player-name card]}]
                   (str player-name ": " (:title card)))
                 bids)))]
   [:strong.starting-bid__history-result
    (if winner-name
      (str "Winner: " winner-name)
      (str "Tie: " (apply str
                           (interpose ", " (map :player-name
                                                 tied-players)))))]])

(defn- redraw-order-row [{:keys [player-id player-name cards active? complete?
                                 selected-count needed-count can-clear?]}]
  [:li
   {:class (cond-> "starting-bid__redraw-row"
             active?
             (str " is-active")

             complete?
             (str " is-complete"))}
   [:span.starting-bid__redraw-player player-name]
   [:span.starting-bid__redraw-cards
    (if (seq cards)
      (apply str (interpose ", " (map :title cards)))
      "")
    (when (and can-clear? (seq cards))
      " ")
    (when can-clear?
      [:button.lobby-remove
       {:type "button"
        :aria-label (str "Clear " player-name " redraw cards")
        :on-click #(rf/dispatch [events/select-lobby-redraw-card
                                  player-id
                                  ""])}
       "Clear"])]
   [:span.starting-bid__redraw-count
    (str selected-count "/" needed-count)]])

(defn- redraw-panel [{:keys [active? active-player-id active-player-name
                             selected-count needed-count card-options order
                             complete?]}]
  [:div.starting-bid__redraw
   [:div.starting-bid__redraw-heading
    [:strong (if complete?
               "Redraw complete"
               (str active-player-name " redraw"))]
    (when active?
      [:span (str selected-count "/" needed-count)])]
   (when active?
     [:div.starting-bid__redraw-options
      (for [{card-id :id title :title} card-options]
        ^{:key card-id}
        [:button.starting-bid__redraw-card
         {:type "button"
          :on-click #(rf/dispatch [events/select-lobby-redraw-card
                                    active-player-id
                                    card-id])}
         title])])
   (when (seq order)
     [:ol.starting-bid__redraw-order
      (for [entry order]
        ^{:key (:player-id entry)}
        [redraw-order-row entry])])])

(defn- starting-bid-panel [players {:keys [stage round-number can-reveal?
                                           can-confirm? winner-name history
                                           redraw]}]
  [:section.starting-bid
   [:div.starting-bid__header
    [:div
     [:p.eyebrow "First player"]
     [:h2 (case stage
            :redrawing "Bid redraw"
            :resolved "Bid resolved"
            "Starting bid")]]
    [:span.starting-bid__round (str "Round " round-number)]]
   (when (= :choosing stage)
     [:div.starting-bid__grid
      (for [player players]
        ^{:key (:id player)}
        [bid-card-select player])])
   (when (seq history)
     [:ol.starting-bid__history
      (for [round history]
        ^{:key (:round round)}
        [bid-history-row round])])
   (when winner-name
     [:p.starting-bid__winner
      (str winner-name " starts.")])
   (when redraw
     [redraw-panel redraw])
   [:div.starting-bid__actions
    [:button.lobby-action
     {:type "button"
      :on-click #(rf/dispatch [events/cancel-lobby-bidding])}
     "Cancel bidding"]
    (cond
      can-confirm?
      [:button.lobby-action.is-primary
       {:type "button"
        :on-click #(rf/dispatch [events/confirm-lobby-bidding])}
       "Start game"]

      (= :redrawing stage)
      [:button.lobby-action.is-primary
       {:type "button"
        :disabled true
        :aria-disabled true}
       "Start game"]

      :else
      [:button.lobby-action.is-primary
       {:type "button"
        :disabled (not can-reveal?)
        :aria-disabled (not can-reveal?)
        :on-click #(rf/dispatch [events/reveal-lobby-bids])}
       "Reveal bids"])]])

(defn local-lobby []
  (let [{:keys [players player-count min-players max-players can-add? can-start? error
                target-score target-score-options starting-bid]}
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
         [player-row player (:active? starting-bid)])]
      [target-score-field target-score target-score-options (:active? starting-bid)]
      (when (:active? starting-bid)
        [starting-bid-panel players starting-bid])
      (when error
        [:p.local-lobby__error
         {:role "alert"}
         (:message error)])
      (when-not (:active? starting-bid)
        [:div.local-lobby__actions
         [:button.lobby-action
          {:type "button"
           :disabled (not can-add?)
           :on-click #(rf/dispatch [events/add-lobby-player])}
          "Add player"]
         [:button.lobby-action
          {:type "button"
           :disabled (not can-start?)
           :aria-disabled (not can-start?)
           :on-click #(rf/dispatch [events/start-lobby-bidding])}
          "Bid for first"]
         [:button.lobby-action.is-primary
          {:type "button"
           :disabled (not can-start?)
           :aria-disabled (not can-start?)
           :on-click #(rf/dispatch [events/start-lobby-game])}
          (if (< player-count min-players)
            "Need players"
            "Start with first seat")]])]]))
