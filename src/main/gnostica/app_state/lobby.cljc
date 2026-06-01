(ns gnostica.app-state.lobby
  (:require [clojure.string :as str]
            [gnostica.app-state.db :as db]
            [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as pieces]))

(defn- player-colour [player-id]
  (get pieces/players-by-id player-id))

(defn- known-player-id? [player-id]
  (contains? pieces/players-by-id player-id))

(defn- normalize-player-id [player-id]
  (if (string? player-id)
    (keyword player-id)
    player-id))

(defn- trim-name [name]
  (str/trim (str name)))

(defn- normalize-controller-id [controller-id]
  (let [controller-id (trim-name (if (keyword? controller-id)
                                   (name controller-id)
                                   controller-id))]
    (when-not (str/blank? controller-id)
      controller-id)))

(defn- normalize-local-controller [controller]
  (when-let [controller-id (normalize-controller-id (:id controller))]
    {:id controller-id
     :name (let [controller-name (trim-name (:name controller))]
             (if (str/blank? controller-name)
               "Local browser"
               controller-name))}))

(defn- normalize-target-score [target-score]
  (let [score (cond
                (int? target-score)
                target-score

                (string? target-score)
                (some #(when (= target-score (str %)) %)
                      db/target-score-options))]
    (when (contains? game-state/allowed-target-scores score)
      score)))

(defn- lobby-error [code message data]
  {:code code
   :message message
   :data data})

(def ^:private starting-bid-choice-error-codes
  #{:invalid-starting-bid-card
    :incomplete-starting-bid-round
    :starting-bid-unresolved})

(def ^:private starting-bid-redraw-error-codes
  #{:inactive-starting-bid-redraw-player
    :invalid-bid-redraw-card
    :starting-bid-redraw-incomplete})

(defn- clear-lobby-error-codes [app-db error-codes]
  (if (contains? error-codes (get-in app-db [:lobby :error :code]))
    (update app-db :lobby dissoc :error)
    app-db))

(defn- duplicate-lobby-player-ids [players]
  (->> players
       (map :id)
       frequencies
       (keep (fn [[player-id n]]
               (when (< 1 n)
                 player-id)))
       vec))

(defn- first-available-player-id [players]
  (let [used (set (map :id players))]
    (some #(when-not (contains? used (:id %))
             (:id %))
          pieces/players)))

(defn- normalize-player-controller [player-spec local-controller]
  (or local-controller
      (normalize-local-controller {:id (:controller-id player-spec)
                                   :name (:controller-name player-spec)})))

(defn- with-player-controller [player controller]
  (cond-> player
    controller
    (assoc :controller-id (:id controller)
           :controller-name (:name controller))))

(defn- normalize-lobby-player [local-controller slot-id player-spec]
  (let [requested-id (:id player-spec)
        player-id (if (known-player-id? requested-id)
                    requested-id
                    (:id (first pieces/players)))
        colour (player-colour player-id)
        name (trim-name (or (:name player-spec)
                            (:name colour)))
        controller (normalize-player-controller player-spec local-controller)]
    (with-player-controller {:slot-id slot-id
                             :id player-id
                             :name name}
      controller)))

(defn- normalize-lobby-players [player-specs local-controller]
  (mapv (partial normalize-lobby-player local-controller)
        (range 1 (inc (count player-specs)))
        player-specs))

(defn- next-lobby-slot-id [players]
  (inc (reduce max 0 (map :slot-id players))))

(defn- lobby-player-specs [lobby]
  (mapv (fn [{:keys [id name]}]
          {:id id
           :name (trim-name name)})
        (:players lobby)))

(defn lobby-validation-error [lobby]
  (let [players (:players lobby)
        player-specs (lobby-player-specs lobby)
        duplicate-ids (duplicate-lobby-player-ids players)
        unknown-ids (->> players
                         (map :id)
                         (remove known-player-id?)
                         vec)
        blank-name-slot-ids (->> players
                                 (filter #(str/blank? (trim-name (:name %))))
                                 (mapv :slot-id))]
    (cond
      (< (count players) game-state/min-players)
      (lobby-error :too-few-players
                   "Add at least two players before starting."
                   {:count (count players)
                    :minimum game-state/min-players})

      (< game-state/max-players (count players))
      (lobby-error :too-many-players
                   "Gnostica supports no more than six players."
                   {:count (count players)
                    :maximum game-state/max-players})

      (seq unknown-ids)
      (lobby-error :unknown-player-colours
                   "Every player must use a known player colour."
                   {:player-ids unknown-ids
                    :known-ids (mapv :id pieces/players)})

      (seq duplicate-ids)
      (lobby-error :duplicate-player-colours
                   "Each player colour can only be used once."
                   {:player-ids duplicate-ids})

      (seq blank-name-slot-ids)
      (lobby-error :blank-player-names
                   "Every player needs a display name."
                   {:slot-ids blank-name-slot-ids})

      (not (game-state/valid-player-count? player-specs))
      (lobby-error :invalid-player-count
                   "Gnostica requires between two and six players."
                   {:count (count player-specs)
                    :minimum game-state/min-players
                    :maximum game-state/max-players}))))

(defn lobby-valid? [lobby]
  (nil? (lobby-validation-error lobby)))

(defn create-lobby [{:keys [player-specs lobby-player-specs game-options demo-board-pieces
                            local-controller]
                     :as opts
                     :or {player-specs db/default-lobby-player-specs
                          game-options {}}}]
  (let [local-controller (normalize-local-controller local-controller)
        players (normalize-lobby-players (or lobby-player-specs player-specs)
                                         local-controller)]
    (cond-> {:players players
             :next-slot-id (next-lobby-slot-id players)
             :start-options (cond-> {:game-options (or game-options {})}
                              (contains? opts :demo-board-pieces)
                              (assoc :demo-board-pieces demo-board-pieces))}
      local-controller
      (assoc :local-controller local-controller)

      (lobby-validation-error {:players players})
      (assoc :error (lobby-validation-error {:players players})))))

(defn lobby [app-db]
  (:lobby app-db))

(defn lobby-active? [app-db]
  (some? (lobby app-db)))

(defn lobby-players [app-db]
  (get-in app-db [:lobby :players] []))

(defn- clear-lobby-starting-bid [app-db]
  (if (lobby-active? app-db)
    (update app-db :lobby dissoc :starting-bid)
    app-db))

(defn- refresh-lobby-error [app-db]
  (if-let [lobby (lobby app-db)]
    (let [validation-error (lobby-validation-error lobby)]
      (if validation-error
        (assoc-in app-db [:lobby :error] validation-error)
        (update app-db :lobby dissoc :error)))
    app-db))

(defn add-lobby-player [app-db]
  (let [app-db (clear-lobby-starting-bid app-db)
        players (lobby-players app-db)]
    (cond
      (not (lobby-active? app-db))
      app-db

      (<= game-state/max-players (count players))
      (assoc-in app-db [:lobby :error]
                (lobby-error :too-many-players
                             "Gnostica supports no more than six players."
                             {:count (count players)
                              :maximum game-state/max-players}))

      :else
      (if-let [player-id (first-available-player-id players)]
        (let [slot-id (get-in app-db [:lobby :next-slot-id]
                              (next-lobby-slot-id players))
              colour (player-colour player-id)
              player (with-player-controller {:slot-id slot-id
                                              :id player-id
                                              :name (:name colour)}
                       (get-in app-db [:lobby :local-controller]))]
          (-> app-db
              (update-in [:lobby :players] conj player)
              (assoc-in [:lobby :next-slot-id] (inc slot-id))
              refresh-lobby-error))
        (assoc-in app-db [:lobby :error]
                  (lobby-error :no-player-colours-available
                               "No unused player colours are available."
                               {:known-ids (mapv :id pieces/players)}))))))

(defn remove-lobby-player [app-db slot-id]
  (if-not (lobby-active? app-db)
    app-db
    (-> app-db
        clear-lobby-starting-bid
        (update-in [:lobby :players]
                   (fn [players]
                     (vec (remove #(= slot-id (:slot-id %)) players))))
        refresh-lobby-error)))

(defn set-lobby-player-name [app-db slot-id name]
  (if-not (lobby-active? app-db)
    app-db
    (-> app-db
        clear-lobby-starting-bid
        (update-in [:lobby :players]
                   (fn [players]
                     (mapv (fn [player]
                             (if (= slot-id (:slot-id player))
                               (assoc player :name (str name))
                               player))
                           players)))
        refresh-lobby-error)))

(defn set-lobby-player-colour [app-db slot-id player-id]
  (let [player-id (normalize-player-id player-id)
        app-db (clear-lobby-starting-bid app-db)
        players (lobby-players app-db)
        existing-player (some #(when (= slot-id (:slot-id %)) %) players)
        duplicate? (some #(and (not= slot-id (:slot-id %))
                               (= player-id (:id %)))
                         players)]
    (cond
      (not (lobby-active? app-db))
      app-db

      (nil? existing-player)
      (assoc-in app-db [:lobby :error]
                (lobby-error :unknown-lobby-player
                             "The selected lobby player no longer exists."
                             {:slot-id slot-id}))

      (not (known-player-id? player-id))
      (assoc-in app-db [:lobby :error]
                (lobby-error :unknown-player-colour
                             "Choose one of the known player colours."
                             {:player-id player-id
                              :known-ids (mapv :id pieces/players)}))

      duplicate?
      (assoc-in app-db [:lobby :error]
                (lobby-error :duplicate-player-colour
                             "Each player colour can only be used once."
                             {:player-id player-id}))

      :else
      (let [old-default-name (:name (player-colour (:id existing-player)))
            new-default-name (:name (player-colour player-id))]
        (-> app-db
            (update-in [:lobby :players]
                       (fn [players]
                         (mapv (fn [player]
                                 (if (= slot-id (:slot-id player))
                                   (cond-> (assoc player :id player-id)
                                     (= old-default-name (:name player))
                                     (assoc :name new-default-name))
                                   player))
                               players)))
            refresh-lobby-error)))))

(defn set-lobby-target-score [app-db target-score]
  (cond
    (not (lobby-active? app-db))
    app-db

    :else
    (if-let [target-score (normalize-target-score target-score)]
      (-> app-db
          clear-lobby-starting-bid
          (assoc-in [:lobby :start-options :game-options :target-score]
                    target-score)
          refresh-lobby-error)
      (assoc-in app-db [:lobby :error]
                (lobby-error :invalid-target-score
                             "Choose an 8, 9, or 10 point game."
                             {:target-score target-score
                              :allowed-target-scores db/target-score-options})))))

(defn- game-options-with-transition-shuffle [game-options transition-options]
  (let [game-options (or game-options {})
        shuffle-fn (:shuffle-fn transition-options)]
    (cond-> game-options
      (and shuffle-fn
           (not (contains? game-options :deck-order))
           (not (contains? game-options :shuffle-fn)))
      (assoc :shuffle-fn shuffle-fn))))

(defn- initial-starting-bid [state]
  {:stage :choosing
   :initial-game state
   :current-game state
   :rounds []
   :current-bids {}
   :bid-history []
   :bid-cards []})

(defn- starting-bid-player-ids [starting-bid]
  (mapv :id (get-in starting-bid [:initial-game :players])))

(defn- starting-bid-card [state player-id card-id]
  (some #(when (= card-id (:id %)) %)
        (get-in state [:players-by-id player-id :hand])))

(defn- starting-bid-redraw-needed-count [state player-id]
  (- game-state/starting-hand-size
     (count (get-in state [:players-by-id player-id :hand]))))

(defn- starting-bid-redraw-card-ids [starting-bid]
  (vec (mapcat #(get-in starting-bid [:redraws %] [])
               (:redraw-order starting-bid))))

(defn- starting-bid-remaining-redraw-cards [starting-bid]
  (let [selected-card-ids (set (starting-bid-redraw-card-ids starting-bid))]
    (vec (remove #(contains? selected-card-ids (:id %))
                 (:bid-cards starting-bid)))))

(defn- starting-bid-active-redraw-player-id [starting-bid]
  (let [state (:current-game starting-bid)]
    (some (fn [player-id]
            (let [needed-count (starting-bid-redraw-needed-count state player-id)
                  selected-count (count (get-in starting-bid
                                                [:redraws player-id]
                                                []))]
              (when (< selected-count needed-count)
                player-id)))
          (:redraw-order starting-bid))))

(defn- starting-bid-redraw-complete? [starting-bid]
  (let [state (:current-game starting-bid)
        selected-card-ids (starting-bid-redraw-card-ids starting-bid)]
    (and (seq (:redraw-order starting-bid))
         (= (count selected-card-ids)
            (count (set selected-card-ids)))
         (empty? (starting-bid-remaining-redraw-cards starting-bid))
         (every? (fn [player-id]
                   (= (starting-bid-redraw-needed-count state player-id)
                      (count (get-in starting-bid
                                     [:redraws player-id]
                                     []))))
                 (:redraw-order starting-bid)))))

(defn- with-starting-bid-redraw-stage [starting-bid]
  (assoc starting-bid
         :stage (if (starting-bid-redraw-complete? starting-bid)
                  :resolved
                  :redrawing)))

(defn start-lobby-bidding
  ([app-db] (start-lobby-bidding app-db {}))
  ([app-db transition-options]
   (if-not (lobby-active? app-db)
     app-db
     (let [app-db (-> app-db clear-lobby-starting-bid refresh-lobby-error)
           lobby (lobby app-db)
           validation-error (lobby-validation-error lobby)]
       (if validation-error
         (assoc-in app-db [:lobby :error] validation-error)
         (let [start-options (:start-options lobby)
               game-options (dissoc
                             (game-options-with-transition-shuffle
                              (:game-options start-options)
                              transition-options)
                             :starting-bids)
               result (game-state/create-game (lobby-player-specs lobby)
                                              game-options)]
           (if (:ok? result)
             (-> app-db
                 (assoc :move-selection (db/empty-move-selection))
                 (assoc-in [:lobby :starting-bid]
                           (initial-starting-bid (:state result)))
                 (update :lobby dissoc :error)
                 (dissoc :setup-error :game :turn-action))
             (assoc-in app-db [:lobby :error] (:error result)))))))))

(defn select-lobby-bid-card [app-db player-id card-id]
  (let [player-id (normalize-player-id player-id)
        card-id (str card-id)
        starting-bid (get-in app-db [:lobby :starting-bid])
        current-game (:current-game starting-bid)]
    (cond
      (nil? starting-bid)
      app-db

      (not= :choosing (:stage starting-bid))
      app-db

      (str/blank? card-id)
      (-> app-db
          (update-in [:lobby :starting-bid :current-bids] dissoc player-id)
          (clear-lobby-error-codes starting-bid-choice-error-codes))

      (nil? (starting-bid-card current-game player-id card-id))
      (assoc-in app-db [:lobby :error]
                (lobby-error :invalid-starting-bid-card
                             "Choose a card from that player's current bid hand."
                             {:player-id player-id
                              :card-id card-id
                              :hand-card-ids (mapv :id
                                                   (get-in current-game
                                                           [:players-by-id
                                                            player-id
                                                            :hand]))}))

      :else
      (-> app-db
          (assoc-in [:lobby :starting-bid :current-bids player-id] card-id)
          (clear-lobby-error-codes starting-bid-choice-error-codes)))))

(defn reveal-lobby-bids [app-db]
  (let [starting-bid (get-in app-db [:lobby :starting-bid])
        player-ids (starting-bid-player-ids starting-bid)
        current-bids (select-keys (:current-bids starting-bid) player-ids)
        missing-player-ids (vec (remove #(contains? current-bids %) player-ids))]
    (cond
      (nil? starting-bid)
      app-db

      (not= :choosing (:stage starting-bid))
      app-db

      (seq missing-player-ids)
      (assoc-in app-db [:lobby :error]
                (lobby-error :incomplete-starting-bid-round
                             "Every player must choose a bid card."
                             {:missing-player-ids missing-player-ids}))

      :else
      (let [rounds (conj (:rounds starting-bid) current-bids)
            {:keys [ok? state resolved? winner-id bid-history bid-cards
                    redraw-order error]}
            (game-state/resolve-starting-bid-rounds
             (:initial-game starting-bid)
             {:rounds rounds})]
        (if-not ok?
          (assoc-in app-db [:lobby :error] error)
          (let [next-starting-bid
                (cond-> (assoc starting-bid
                               :current-game state
                               :rounds rounds
                               :current-bids {}
                               :bid-history bid-history
                               :bid-cards bid-cards)
                  resolved?
                  (assoc :stage :redrawing
                         :winner-id winner-id
                         :redraw-order redraw-order
                         :redraws {})

                  (not resolved?)
                  (assoc :stage :choosing
                         :winner-id nil
                         :redraw-order nil
                         :redraws nil))]
            (-> app-db
                (assoc-in [:lobby :starting-bid] next-starting-bid)
                (update :lobby dissoc :error))))))))

(defn select-lobby-redraw-card [app-db player-id card-id]
  (let [player-id (normalize-player-id player-id)
        card-id (str card-id)
        starting-bid (get-in app-db [:lobby :starting-bid])
        active-player-id (starting-bid-active-redraw-player-id starting-bid)
        remaining-cards (starting-bid-remaining-redraw-cards starting-bid)
        remaining-card-ids (set (map :id remaining-cards))]
    (cond
      (nil? starting-bid)
      app-db

      (not (contains? #{:redrawing :resolved} (:stage starting-bid)))
      app-db

      (str/blank? card-id)
      (-> app-db
          (update-in [:lobby :starting-bid :redraws] dissoc player-id)
          (update-in [:lobby :starting-bid]
                     with-starting-bid-redraw-stage)
          (clear-lobby-error-codes starting-bid-redraw-error-codes))

      (not= :redrawing (:stage starting-bid))
      app-db

      (not= player-id active-player-id)
      (assoc-in app-db [:lobby :error]
                (lobby-error :inactive-starting-bid-redraw-player
                             "Choose bid redraw cards for the active redraw player."
                             {:player-id player-id
                              :active-player-id active-player-id}))

      (not (contains? remaining-card-ids card-id))
      (assoc-in app-db [:lobby :error]
                (lobby-error :invalid-bid-redraw-card
                             "Players can only redraw from bid cards that are still available."
                             {:player-id player-id
                              :card-id card-id
                              :available-card-ids (mapv :id remaining-cards)}))

      :else
      (-> app-db
          (update-in [:lobby :starting-bid :redraws player-id]
                     (fnil conj [])
                     card-id)
          (update-in [:lobby :starting-bid]
                     with-starting-bid-redraw-stage)
          (update :lobby dissoc :error)))))

(defn confirm-lobby-bidding [app-db]
  (let [starting-bid (get-in app-db [:lobby :starting-bid])]
    (cond
      (nil? starting-bid)
      app-db

      (= :redrawing (:stage starting-bid))
      (assoc-in app-db [:lobby :error]
                (lobby-error :starting-bid-redraw-incomplete
                             "Finish bid-card redraws before starting the game."
                             {:active-player-id
                              (starting-bid-active-redraw-player-id starting-bid)
                              :redraw-order (:redraw-order starting-bid)}))

      (not= :resolved (:stage starting-bid))
      (assoc-in app-db [:lobby :error]
                (lobby-error :starting-bid-unresolved
                             "Reveal a winning bid before starting the game."
                             {:stage (:stage starting-bid)}))

      :else
      (let [result (game-state/apply-starting-bids
                    (:initial-game starting-bid)
                    {:rounds (:rounds starting-bid)
                     :redraws (:redraws starting-bid)})]
        (if (:ok? result)
          (-> app-db
              (assoc :game (db/state-with-demo-board-pieces
                            (:state result)
                            (get-in app-db [:lobby :start-options])))
              (assoc :move-selection (db/empty-move-selection))
              (dissoc :turn-action)
              (dissoc :setup-error :lobby))
          (assoc-in app-db [:lobby :error] (:error result)))))))

(defn cancel-lobby-bidding [app-db]
  (-> app-db
      clear-lobby-starting-bid
      refresh-lobby-error))

(defn start-lobby-game
  ([app-db] (start-lobby-game app-db {}))
  ([app-db transition-options]
   (if-not (lobby-active? app-db)
     app-db
     (let [starting-bid (get-in app-db [:lobby :starting-bid])
           app-db (refresh-lobby-error app-db)
           lobby (lobby app-db)
           validation-error (lobby-validation-error lobby)]
       (cond
         starting-bid
         (assoc-in app-db [:lobby :error]
                   (lobby-error :starting-bid-active
                                "Finish or cancel bidding before starting a casual game."
                                {:stage (:stage starting-bid)}))

         validation-error
         (assoc-in app-db [:lobby :error] validation-error)

         :else
         (let [start-options (:start-options lobby)
               game-options (game-options-with-transition-shuffle
                             (:game-options start-options)
                             transition-options)]
           (db/initialize-game-db
            (assoc app-db :move-selection (db/empty-move-selection))
            start-options
            (lobby-player-specs lobby)
            game-options)))))))

(defn- lobby-colour-option [used-player-ids current-player-id colour]
  (let [player-id (:id colour)
        selected? (= current-player-id player-id)]
    (assoc (select-keys colour [:id :name :css-color])
           :selected? selected?
           :disabled? (and (not selected?)
                           (contains? used-player-ids player-id)))))

(defn- lobby-player-name [players player-id]
  (or (some #(when (= player-id (:id %)) (:name %)) players)
      (:name (player-colour player-id))
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
        needed-count (starting-bid-redraw-needed-count state player-id)]
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
    (let [active-player-id (starting-bid-active-redraw-player-id starting-bid)
          state (:current-game starting-bid)
          selected-count (count (get-in starting-bid
                                        [:redraws active-player-id]
                                        []))
          needed-count (when active-player-id
                         (starting-bid-redraw-needed-count state
                                                           active-player-id))]
      {:active? (= :redrawing (:stage starting-bid))
       :complete? (starting-bid-redraw-complete? starting-bid)
       :active-player-id active-player-id
       :active-player-name (when active-player-id
                             (lobby-player-name players active-player-id))
       :needed-count needed-count
       :selected-count selected-count
       :card-options (if active-player-id
                       (mapv (partial bid-card-option nil)
                             (starting-bid-remaining-redraw-cards starting-bid))
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
                          (starting-bid-redraw-complete? starting-bid))
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
                           (lobby-validation-error lobby))
        error (or (:error lobby)
                  validation-error)]
    {:active? (some? lobby)
     :players (mapv (fn [player]
                      (let [colour (player-colour (:id player))]
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
  (lobby-view-model {:lobby (lobby app-db)}))
