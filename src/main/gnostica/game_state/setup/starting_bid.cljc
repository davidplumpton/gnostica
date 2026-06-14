(ns gnostica.game-state.setup.starting-bid
  (:require [gnostica.cards :as cards]
            [gnostica.game-state.hands :as hands]
            [gnostica.game-state.players :as players]
            [gnostica.game-state.result :as result]
            [gnostica.game-state.setup.redraw :as redraw]))

(defn- state-player-ids [state]
  (mapv :id (:players state)))

(defn- bid-round-bids [round]
  (if (and (map? round)
           (contains? round :bids))
    (:bids round)
    round))

(defn- bid-round-shape-error [round-number round]
  (when-not (map? (bid-round-bids round))
    (result/failure
     :invalid-bid-round
     "Each bid round must provide a map from player id to bid card id."
     {:round round-number
      :bid-round round})))

(defn- bid-round-player-error [state round-number bids]
  (let [expected-player-ids (set (state-player-ids state))
        actual-player-ids (set (keys bids))
        missing-player-ids (vec (sort-by str (remove actual-player-ids
                                                     expected-player-ids)))
        unknown-player-ids (vec (sort-by str (remove expected-player-ids
                                                     actual-player-ids)))]
    (cond
      (seq missing-player-ids)
      (result/failure
       :incomplete-bid-round
       "Every player must bid one card in each starting bid round."
       {:round round-number
        :missing-player-ids missing-player-ids})

      (seq unknown-player-ids)
      (result/failure :unknown-bid-players
                      "Bid rounds can only include players in the game."
                      {:round round-number
                       :unknown-player-ids unknown-player-ids
                       :player-ids (state-player-ids state)}))))

(defn- ranked-bid [state round-number player-id card-id]
  (let [card (hands/player-hand-card state player-id card-id)]
    (cond
      (nil? card)
      (result/failure
       :invalid-bid-card
       "Players can only bid cards from their current hand."
       {:round round-number
        :player-id player-id
        :card-id card-id
        :hand-card-ids (mapv :id (get-in state [:players-by-id player-id :hand]))})

      (nil? (cards/bid-rank card))
      (result/failure :unranked-bid-card
                      "Bid cards must be rankable tarot cards."
                      {:round round-number
                       :player-id player-id
                       :card-id card-id})

      :else
      {:ok? true
       :player-id player-id
       :card-id card-id
       :card card
       :rank (cards/bid-rank card)})))

(defn- bid-round-ranks [state round-number bids]
  (reduce (fn [result player-id]
            (if (false? (:ok? result))
              result
              (let [ranked (ranked-bid state round-number player-id (get bids player-id))]
                (if (:ok? ranked)
                  (update result :ranked-bids conj ranked)
                  ranked))))
          {:ok? true
           :ranked-bids []}
          (state-player-ids state)))

(defn- best-bid-group [ranked-bids]
  (let [major-bids (filterv #(= :major (get-in % [:rank :arcana])) ranked-bids)
        candidates (if (seq major-bids)
                     major-bids
                     ranked-bids)
        best-rank (apply max (map #(get-in % [:rank :rank]) candidates))]
    {:arcana (get-in (first candidates) [:rank :arcana])
     :rank best-rank
     :bids (filterv #(= best-rank (get-in % [:rank :rank])) candidates)}))

(defn- bid-round-result [round-number ranked-bids]
  (let [{:keys [arcana rank bids]} (best-bid-group ranked-bids)
        winner (when (= 1 (count bids))
                 (first bids))]
    (cond-> {:round round-number
             :bids (into {}
                         (map (fn [{:keys [player-id card-id]}]
                                [player-id card-id]))
                         ranked-bids)
             :considered-arcana arcana
             :winning-rank rank
             :tied-player-ids (mapv :player-id bids)
             :tied-card-ids (mapv :card-id bids)}
      winner
      (assoc :winner-id (:player-id winner)
             :winning-card-id (:card-id winner)))))

(defn- remove-bid-cards [state ranked-bids]
  (reduce (fn [next-state {:keys [player-id card-id]}]
            (hands/remove-card-from-hand next-state player-id card-id))
          state
          ranked-bids))

(defn- resolve-bid-round [state round-number round]
  (if-let [error (bid-round-shape-error round-number round)]
    error
    (let [bids (bid-round-bids round)]
      (if-let [error (bid-round-player-error state round-number bids)]
        error
        (let [{:keys [ok? ranked-bids] :as ranks-result}
              (bid-round-ranks state round-number bids)]
          (if-not ok?
            ranks-result
            {:ok? true
             :state (remove-bid-cards state ranked-bids)
             :bid-cards (mapv :card ranked-bids)
             :round-result (bid-round-result round-number ranked-bids)}))))))

(defn- resolve-bid-rounds
  ([state bid-rounds]
   (resolve-bid-rounds state bid-rounds false))
  ([state bid-rounds allow-unresolved?]
   (cond
     (not (sequential? bid-rounds))
     (result/failure :invalid-bid-rounds
                     "Starting bids require a sequential collection of bid rounds."
                     {:bid-rounds bid-rounds})

     (empty? bid-rounds)
     (result/failure :missing-bid-rounds
                     "Starting bids require at least one bid round."
                     {})

     :else
     (loop [next-state state
            rounds (seq bid-rounds)
            round-number 1
            bid-history []
            bid-cards []]
       (let [{round-ok? :ok?
              round-state :state
              round-result :round-result
              round-bid-cards :bid-cards
              :as round-resolution}
             (resolve-bid-round next-state round-number (first rounds))]
         (if-not round-ok?
           round-resolution
           (let [next-bid-history (conj bid-history round-result)
                 next-bid-cards (into bid-cards round-bid-cards)]
             (if-let [winner-id (:winner-id round-result)]
               {:ok? true
                :resolved? true
                :state round-state
                :winner-id winner-id
                :bid-history next-bid-history
                :bid-cards next-bid-cards}
               (if-let [remaining-rounds (next rounds)]
                 (recur round-state
                        remaining-rounds
                        (inc round-number)
                        next-bid-history
                        next-bid-cards)
                 (if allow-unresolved?
                   {:ok? true
                    :resolved? false
                    :state round-state
                    :bid-history next-bid-history
                    :bid-cards next-bid-cards}
                   (result/failure
                    :unresolved-bid-tie
                    "Starting bids ended in a tie and require another bid round."
                    {:bid-history next-bid-history})))))))))))

(defn- starting-bids-already-resolved-error [state]
  (when (some? (get-in state [:setup :starting-player-id]))
    (result/failure
     :starting-bids-already-resolved
     "The starting bid has already been resolved."
     {:starting-player-id (get-in state [:setup :starting-player-id])})))

(defn resolve-starting-bid-rounds
  [state {:keys [rounds bid-rounds] :as _command}]
  (let [rounds (or rounds bid-rounds)
        original-players (:players state)]
    (if-let [error (starting-bids-already-resolved-error state)]
      error
      (let [{:keys [ok? winner-id] :as bid-result}
            (resolve-bid-rounds state rounds true)]
        (if-not ok?
          bid-result
          (cond-> bid-result
            winner-id
            (assoc :redraw-order
                   (redraw/counterclockwise-redraw-order original-players
                                                         winner-id))))))))

(defn apply-starting-bids
  [state {:keys [rounds bid-rounds redraws] :as _command}]
  (let [rounds (or rounds bid-rounds)
        original-players (:players state)]
    (if-let [error (starting-bids-already-resolved-error state)]
      error
      (let [{bid-ok? :ok?
             bid-state :state
             winner-id :winner-id
             bid-history :bid-history
             bid-cards :bid-cards
             :as bid-result}
            (resolve-bid-rounds state rounds)]
        (if-not bid-ok?
          bid-result
          (let [redraw-order (redraw/counterclockwise-redraw-order original-players
                                                                   winner-id)
                {redraw-ok? :ok?
                 redrawn-state :state
                 redraw-history :redraw-history
                 :as redraw-result}
                (redraw/apply-bid-redraws bid-state bid-cards redraw-order redraws)]
            (if-not redraw-ok?
              redraw-result
              (let [event {:type :setup/starting-player-determined
                           :player-id winner-id
                           :bid-round-count (count bid-history)
                           :bid-card-ids (mapv :id bid-cards)
                           :redraw-order redraw-order}
                    next-state (-> redrawn-state
                                   (assoc-in [:setup :bids] {})
                                   (assoc-in [:setup :bid-history] bid-history)
                                   (assoc-in [:setup :bid-redraw-order] redraw-order)
                                   (assoc-in [:setup :bid-redraws] redraw-history)
                                   (assoc-in [:setup :starting-player-id] winner-id)
                                   (redraw/rotate-players-to-starting-player winner-id)
                                   (players/append-history event))]
                (result/success next-state [event])))))))))
