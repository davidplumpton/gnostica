(ns gnostica.app-state.lobby-view-models
  (:require [gnostica.app-state.db :as db]
            [gnostica.app-state.lobby-bidding :as bidding]
            [gnostica.app-state.lobby-setup :as setup]
            [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as pieces]))

(defn- lobby-colour-option [used-player-ids current-player-id colour]
  (let [player-id (:id colour)
        selected? (= current-player-id player-id)]
    (assoc (select-keys colour [:id :name :css-color])
           :selected? selected?
           :disabled? (and (not selected?)
                           (contains? used-player-ids player-id)))))

(defn- lobby-player-name [players player-id]
  (or (some #(when (= player-id (:id %)) (:name %)) players)
      (:name (setup/player-colour player-id))
      (name player-id)))

(defn- bid-card-summary [card]
  (when card
    (assoc (select-keys card [:id :title :image :arcana :group :rank :suit])
           :bid-rank (cards/bid-rank card))))

(defn- bid-card-option [selected-card-id card]
  (assoc (bid-card-summary card)
         :selected? (= selected-card-id (:id card))))

(defn- redraw-player-view [players starting-bid player-id active-player-id]
  (let [state (:current-game starting-bid)
        card-ids (get-in starting-bid [:redraws player-id] [])
        needed-count (bidding/starting-bid-redraw-needed-count state player-id)]
    {:player-id player-id
     :player-name (lobby-player-name players player-id)
     :card-ids card-ids
     :cards (mapv #(bid-card-summary (cards/card-by-id %)) card-ids)
     :needed-count needed-count
     :selected-count (count card-ids)
     :active? (= player-id active-player-id)
     :complete? (= needed-count (count card-ids))
     :can-clear? (boolean
                  (and (contains? #{:redrawing :resolved} (:stage starting-bid))
                       (seq card-ids)))}))

(defn- starting-bid-redraw-view [players starting-bid]
  (when (contains? #{:redrawing :resolved} (:stage starting-bid))
    (let [active-player-id (bidding/starting-bid-active-redraw-player-id
                            starting-bid)
          state (:current-game starting-bid)
          selected-count (count (get-in starting-bid
                                        [:redraws active-player-id]
                                        []))
          needed-count (when active-player-id
                         (bidding/starting-bid-redraw-needed-count
                          state
                          active-player-id))]
      {:active? (= :redrawing (:stage starting-bid))
       :complete? (bidding/starting-bid-redraw-complete? starting-bid)
       :active-player-id active-player-id
       :active-player-name (when active-player-id
                             (lobby-player-name players active-player-id))
       :needed-count needed-count
       :selected-count selected-count
       :card-options (if active-player-id
                       (mapv (partial bid-card-option nil)
                             (bidding/starting-bid-remaining-redraw-cards
                              starting-bid))
                       [])
       :order (mapv #(redraw-player-view players
                                         starting-bid
                                         %
                                         active-player-id)
                    (:redraw-order starting-bid))})))

(defn- bid-history-entry-view [players round]
  (let [card-summary (fn [card-id]
                       (bid-card-summary (cards/card-by-id card-id)))]
    (-> round
        (update :bids
                (fn [bids]
                  (mapv (fn [{:keys [id]}]
                          {:player-id id
                           :player-name (lobby-player-name players id)
                           :card (card-summary (get bids id))})
                        players)))
        (assoc :tied-players
               (mapv (fn [player-id]
                       {:player-id player-id
                        :player-name (lobby-player-name players player-id)})
                     (:tied-player-ids round)))
        (cond-> (:winner-id round)
          (assoc :winner-name (lobby-player-name players (:winner-id round))
                 :winning-card (card-summary (:winning-card-id round)))))))

(defn- starting-bid-player-view [starting-bid player]
  (if-not starting-bid
    player
    (let [player-id (:id player)
          selected-card-id (get-in starting-bid [:current-bids player-id])
          hand (get-in starting-bid [:current-game :players-by-id player-id :hand])]
      (assoc player
             :bid-card-id nil
             :bid-card-selected? (some? selected-card-id)
             :bid-card-options (if selected-card-id
                                 []
                                 (mapv (partial bid-card-option nil)
                                       hand))
             :bid-ready? (some? selected-card-id)))))

(defn- starting-bid-view [players starting-bid]
  (when starting-bid
    (let [player-ids (mapv :id players)
          current-bids (select-keys (:current-bids starting-bid) player-ids)
          complete? (every? #(contains? current-bids %) player-ids)
          winner-id (:winner-id starting-bid)
          redraw-view (starting-bid-redraw-view players starting-bid)]
      {:active? true
       :stage (:stage starting-bid)
       :round-number (if (= :resolved (:stage starting-bid))
                       (count (:rounds starting-bid))
                       (inc (count (:rounds starting-bid))))
       :complete? complete?
       :can-reveal? (and (= :choosing (:stage starting-bid))
                         complete?)
       :can-confirm? (and (= :resolved (:stage starting-bid))
                          (bidding/starting-bid-redraw-complete? starting-bid))
       :winner-id winner-id
       :winner-name (when winner-id
                      (lobby-player-name players winner-id))
       :redraw redraw-view
       :redraw-order (:order redraw-view)
       :history (mapv (partial bid-history-entry-view players)
                      (:bid-history starting-bid))})))

(defn lobby-view-model [{:keys [lobby]}]
  (let [players (:players lobby)
        starting-bid (:starting-bid lobby)
        local-controller (:local-controller lobby)
        target-score (or (get-in lobby [:start-options :game-options :target-score])
                         game-state/default-target-score)
        used-player-ids (set (map :id players))
        validation-error (when lobby
                           (setup/lobby-validation-error lobby))
        error (or (:error lobby)
                  validation-error)]
    {:active? (some? lobby)
     :players (mapv (fn [player]
                      (let [colour (setup/player-colour (:id player))]
                        (starting-bid-player-view
                         starting-bid
                         (assoc player
                                :colour (select-keys colour [:id :name :css-color])
                                :colour-options (mapv #(lobby-colour-option
                                                        used-player-ids
                                                        (:id player)
                                                        %)
                                                      pieces/players)))))
                    players)
     :local-controller local-controller
     :local-control? (some? local-controller)
     :starting-bid (starting-bid-view players starting-bid)
     :target-score target-score
     :target-score-options db/target-score-options
     :available-colours (mapv #(assoc (select-keys % [:id :name :css-color])
                                      :available?
                                      (not (contains? used-player-ids (:id %))))
                              pieces/players)
     :player-count (count players)
     :min-players game-state/min-players
     :max-players game-state/max-players
     :can-add? (boolean
                (and (some? lobby)
                     (< (count players) game-state/max-players)
                     (some #(not (contains? used-player-ids (:id %)))
                           pieces/players)))
     :can-start? (boolean
                  (and (some? lobby)
                       (nil? starting-bid)
                       (nil? validation-error)))
     :error error}))

(defn lobby-view [app-db]
  (lobby-view-model {:lobby (setup/lobby app-db)}))
