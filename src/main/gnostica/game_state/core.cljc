(ns gnostica.game-state.core
  (:require [clojure.string :as str]
            [gnostica.board :as board]
            [gnostica.board-layout :as board-layout]
            [gnostica.cards :as cards]
            [gnostica.pieces :as pieces]))

(def min-players 2)

(def max-players 6)

(def starting-hand-size 6)

(def pieces-per-size-in-stash 5)

(def initial-phase :setup)

(def finished-phase :finished)

(def default-target-score 9)

(def allowed-target-scores #{8 9 10})

(def required-player-fields [:id :name :color :css-color])

(def required-card-fields [:id :title :image])

(declare with-current-scores)

(defn success
  ([state]
   (success state []))
  ([state events]
   {:ok? true
    :state (with-current-scores state)
    :events (vec events)}))

(defn failure [code message data]
  {:ok? false
   :error {:code code
           :message message
           :data data}})

(defn valid-player-count? [player-specs]
  (<= min-players (count player-specs) max-players))

(defn- player-id [player-spec]
  (:id player-spec))

(defn- duplicate-player-ids [player-specs]
  (->> player-specs
       (map player-id)
       frequencies
       (filter (fn [[_ occurrences]]
                 (> occurrences 1)))
       (map first)
       vec))

(defn- missing-player-id-indexes [player-specs]
  (->> player-specs
       (map-indexed vector)
       (keep (fn [[index player-spec]]
               (when (nil? (player-id player-spec))
                 index)))
       vec))

(defn- unknown-player-ids [player-specs]
  (->> player-specs
       (map player-id)
       (remove #(contains? pieces/players-by-id %))
       distinct
       vec))

(defn- valid-css-color? [value]
  (and (string? value)
       (some? (re-matches #"#[0-9a-fA-F]{6}" value))))

(defn- valid-three-color? [value]
  (and (integer? value)
       (<= 0 value 0xffffff)))

(defn- missing-or-invalid-player-fields [player]
  (cond-> []
    (nil? (:id player))
    (conj :id)

    (not (and (string? (:name player))
              (not (str/blank? (:name player)))))
    (conj :name)

    (not (valid-three-color? (:color player)))
    (conj :color)

    (not (valid-css-color? (:css-color player)))
    (conj :css-color)))

(defn- invalid-player-metadata [player-specs]
  (->> player-specs
       (map-indexed
        (fn [index player-spec]
          (let [player (when (map? player-spec)
                         (merge (get pieces/players-by-id (player-id player-spec))
                                player-spec))
                invalid-fields (missing-or-invalid-player-fields player)]
            (when (seq invalid-fields)
              {:index index
               :id (player-id player-spec)
               :invalid-fields invalid-fields}))))
       (remove nil?)
       vec))

(defn- validate-player-specs [player-specs]
  (if-not (sequential? player-specs)
    (failure :invalid-player-specs
             "Player specs must be a sequential collection."
             {:player-specs player-specs})
    (let [missing-id-indexes (missing-player-id-indexes player-specs)
          duplicate-ids (duplicate-player-ids player-specs)
          unknown-ids (unknown-player-ids player-specs)
          invalid-players (invalid-player-metadata player-specs)]
      (cond
        (not (valid-player-count? player-specs))
        (failure :invalid-player-count
                 "Gnostica requires between two and six players."
                 {:count (count player-specs)
                  :minimum min-players
                  :maximum max-players})

        (seq missing-id-indexes)
        (failure :invalid-player-specs
                 "Each player spec must include an :id."
                 {:missing-id-indexes missing-id-indexes})

        (seq duplicate-ids)
        (failure :duplicate-player-ids
                 "Player ids must be unique."
                 {:duplicate-ids duplicate-ids})

        (seq unknown-ids)
        (failure :unknown-player-ids
                 "Player ids must reference known player metadata."
                 {:unknown-ids unknown-ids
                  :known-ids (mapv :id pieces/players)})

        (seq invalid-players)
        (failure :invalid-player-metadata
                 "Player metadata must include a name, Three.js color, and CSS color."
                 {:players invalid-players
                  :required-fields required-player-fields})))))

(defn- initial-stash []
  (into {}
        (map (fn [size]
               [size pieces-per-size-in-stash]))
        (keys pieces/piece-sizes)))

(defn- normalize-player [index player-spec]
  (let [id (player-id player-spec)
        piece-player (get pieces/players-by-id id)]
    (merge
     (select-keys piece-player [:id :name :color :css-color])
     player-spec
     {:order-index index
      :hand []
      :score 0
      :challenge nil
      :eliminated? false
      :stash (initial-stash)
      :bid nil})))

(defn- deck-source [{:keys [deck deck-order]
                     :or {deck cards/deck}}]
  (if deck-order
    deck-order
    deck))

(defn- ordered-deck [{:keys [deck deck-order shuffle-fn]
                      :or {deck cards/deck
                           shuffle-fn shuffle}}]
  (let [ordered (if deck-order
                  deck-order
                  (shuffle-fn deck))]
    (if (sequential? ordered)
      (vec ordered)
      ordered)))

(defn- invalid-card-fields [card]
  (if-not (map? card)
    required-card-fields
    (cond-> []
      (not (and (string? (:id card))
                (not (str/blank? (:id card)))))
      (conj :id)

      (not (and (string? (:title card))
                (not (str/blank? (:title card)))))
      (conj :title)

      (not (and (string? (:image card))
                (not (str/blank? (:image card)))))
      (conj :image))))

(defn- invalid-cards [deck]
  (->> deck
       (map-indexed
        (fn [index card]
          (let [invalid-fields (invalid-card-fields card)]
            (when (seq invalid-fields)
              {:index index
               :card-id (:id card)
               :invalid-fields invalid-fields}))))
       (remove nil?)
       vec))

(defn- duplicate-card-ids [deck]
  (->> deck
       (map :id)
       frequencies
       (filter (fn [[_ occurrences]]
                 (> occurrences 1)))
       (map first)
       vec))

(defn- required-starting-card-count [player-count]
  (+ board/board-card-count
     (* starting-hand-size player-count)))

(defn- validate-deck
  ([deck]
   (validate-deck deck board/board-card-count))
  ([deck minimum-card-count]
   (if-not (sequential? deck)
     (failure :invalid-deck
              "The deck must be an ordered sequential collection of card maps."
              {:deck deck})
     (let [invalid-deck-cards (invalid-cards deck)
           duplicate-ids (duplicate-card-ids deck)]
       (cond
         (< (count deck) minimum-card-count)
         (failure :insufficient-deck
                  "The deck must contain enough cards to deal player hands and build the territory board."
                  {:count (count deck)
                   :minimum minimum-card-count
                   :starting-hand-size starting-hand-size
                   :board-card-count board/board-card-count})

         (seq invalid-deck-cards)
         (failure :invalid-deck-cards
                  "Every deck card must include an id, title, and image path."
                  {:invalid-cards invalid-deck-cards
                   :required-fields required-card-fields})

         (seq duplicate-ids)
         (failure :duplicate-card-ids
                  "Deck card ids must be unique."
                  {:duplicate-ids duplicate-ids}))))))

(defn- target-score-option [opts]
  (if (contains? opts :target-score)
    (:target-score opts)
    default-target-score))

(defn- validate-target-score [target-score]
  (when-not (contains? allowed-target-scores target-score)
    (failure :invalid-target-score
             "Gnostica target score must be 8, 9, or 10."
             {:target-score target-score
              :allowed-target-scores (vec (sort allowed-target-scores))})))

(defn- initial-stashes [players]
  (into {}
        (map (fn [player]
               [(:id player) (:stash player)]))
        players))

(defn- initial-turn [players]
  (let [order (mapv :id players)]
    {:order order
     :current-player-index 0
     :current-player-id (first order)
     :round 1}))

(defn- rotate-vector-from-index [values index]
  (vec (concat (drop index values)
               (take index values))))

(defn- deal-starting-hands [players deck]
  (let [hand-card-count (* starting-hand-size (count players))
        hand-cards (take hand-card-count deck)
        hands (partition starting-hand-size hand-cards)
        board-deck (vec (drop hand-card-count deck))]
    {:players (mapv (fn [player hand]
                      (assoc player :hand (vec hand)))
                    players
                    hands)
     :board-deck board-deck}))

(defn current-player [state]
  (get-in state [:players-by-id (get-in state [:turn :current-player-id])]))

(defn append-history [state event]
  (update state :history conj event))

(defn- rebuild-players-by-id [players]
  (into {} (map (juxt :id identity)) players))

(defn update-player [state player-id f & args]
  (let [players (mapv (fn [player]
                        (if (= player-id (:id player))
                          (apply f player args)
                          player))
                      (:players state))]
    (assoc state
           :players players
           :players-by-id (rebuild-players-by-id players))))

(defn- board-piece-counts [board-pieces]
  (reduce (fn [counts {:keys [player-id size]}]
            (if (and player-id size)
              (update-in counts [player-id size] (fnil inc 0))
              counts))
          {}
          board-pieces))

(defn- stash-after-board-pieces [piece-counts player-id]
  (into {}
        (map (fn [size]
               [size (- pieces-per-size-in-stash
                        (get-in piece-counts [player-id size] 0))]))
        (keys pieces/piece-sizes)))

(defn with-board-pieces
  "Replace active pieces and rebuild both stash mirrors from the starting stash size."
  [state board-pieces]
  (let [board-pieces (vec board-pieces)
        piece-counts (board-piece-counts board-pieces)
        players (mapv (fn [player]
                        (assoc player
                               :stash (stash-after-board-pieces piece-counts
                                                                 (:id player))))
                      (:players state))]
    (assoc state
           :players players
           :players-by-id (rebuild-players-by-id players)
           :pieces (assoc (:pieces state)
                          :on-board board-pieces
                          :stashes (initial-stashes players)))))

(defn board-cell-by-index [state board-index]
  (some (fn [cell]
          (when (= board-index (:index cell))
            cell))
        (:board state)))

(defn board-cell-at [state row col]
  (some (fn [cell]
          (when (and (= row (:row cell))
                     (= col (:col cell)))
            cell))
        (:board state)))

(defn piece-by-id [state piece-id]
  (some (fn [piece]
          (when (= piece-id (:id piece))
            piece))
        (get-in state [:pieces :on-board])))

(defn pieces-at-board-index [state board-index]
  (filterv #(= board-index (:space-index %))
           (get-in state [:pieces :on-board])))

(defn piece-coordinate [state piece]
  (if-let [{:keys [row col]} (:space piece)]
    [row col]
    (when-let [{:keys [row col]} (board-cell-by-index state (:space-index piece))]
      [row col])))

(defn player-hand-card [state player-id card-id]
  (some (fn [card]
          (when (= card-id (:id card))
            card))
        (get-in state [:players-by-id player-id :hand])))

(defn remove-card-from-hand [state player-id card-id]
  (update-player state player-id update :hand
                 (fn [hand]
                   (vec (remove #(= card-id (:id %)) hand)))))

(defn remove-cards-from-hand [state player-id card-ids]
  (let [card-id-set (set card-ids)]
    (update-player state player-id update :hand
                   (fn [hand]
                     (vec (remove #(contains? card-id-set (:id %)) hand))))))

(defn discard-card [state card]
  (update state :discard-pile conj card))

(defn discard-cards [state cards]
  (update state :discard-pile into (vec cards)))

(defn refresh-draw-pile [state shuffle-fn]
  (cond
    (not (ifn? shuffle-fn))
    (failure :invalid-shuffle-fn
             "Draw-pile refresh requires a callable shuffle function."
             {:shuffle-fn shuffle-fn})

    (and (empty? (:draw-pile state))
         (seq (:discard-pile state)))
    (let [shuffled-cards (shuffle-fn (:discard-pile state))]
      (if (sequential? shuffled-cards)
        {:ok? true
         :state (-> state
                    (assoc :draw-pile (vec shuffled-cards))
                    (assoc :discard-pile []))
         :reshuffled? true}
        (failure :invalid-shuffle-result
                 "The draw-pile shuffle function must return a sequential collection of cards."
                 {:result shuffled-cards})))

    :else
    {:ok? true
     :state state
     :reshuffled? false}))

(defn append-cards-to-hand [state player-id cards]
  (update-player state player-id update :hand
                 (fn [hand]
                   (vec (concat hand cards)))))

(defn- state-player-ids [state]
  (mapv :id (:players state)))

(defn- bid-round-bids [round]
  (if (and (map? round)
           (contains? round :bids))
    (:bids round)
    round))

(defn- bid-round-shape-error [round-number round]
  (when-not (map? (bid-round-bids round))
    (failure :invalid-bid-round
             "Each bid round must provide a map from player id to bid card id."
             {:round round-number
              :bid-round round})))

(defn- bid-round-player-error [state round-number bids]
  (let [expected-player-ids (set (state-player-ids state))
        actual-player-ids (set (keys bids))
        missing-player-ids (vec (sort-by str (remove actual-player-ids expected-player-ids)))
        unknown-player-ids (vec (sort-by str (remove expected-player-ids actual-player-ids)))]
    (cond
      (seq missing-player-ids)
      (failure :incomplete-bid-round
               "Every player must bid one card in each starting bid round."
               {:round round-number
                :missing-player-ids missing-player-ids})

      (seq unknown-player-ids)
      (failure :unknown-bid-players
               "Bid rounds can only include players in the game."
               {:round round-number
                :unknown-player-ids unknown-player-ids
                :player-ids (state-player-ids state)}))))

(defn- ranked-bid [state round-number player-id card-id]
  (let [card (player-hand-card state player-id card-id)]
    (cond
      (nil? card)
      (failure :invalid-bid-card
               "Players can only bid cards from their current hand."
               {:round round-number
                :player-id player-id
                :card-id card-id
                :hand-card-ids (mapv :id (get-in state [:players-by-id player-id :hand]))})

      (nil? (cards/bid-rank card))
      (failure :unranked-bid-card
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
            (remove-card-from-hand next-state player-id card-id))
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
     (failure :invalid-bid-rounds
              "Starting bids require a sequential collection of bid rounds."
              {:bid-rounds bid-rounds})

     (empty? bid-rounds)
     (failure :missing-bid-rounds
              "Starting bids require at least one bid round."
              {})

     :else
     (loop [next-state state
            rounds (seq bid-rounds)
            round-number 1
            bid-history []
            bid-cards []]
       (let [{:keys [ok? state round-result] round-bid-cards :bid-cards
              :as round-resolution}
             (resolve-bid-round next-state round-number (first rounds))]
         (if-not ok?
           round-resolution
           (let [bid-history (conj bid-history round-result)
                 bid-cards (into bid-cards round-bid-cards)]
             (if-let [winner-id (:winner-id round-result)]
               {:ok? true
                :resolved? true
                :state state
                :winner-id winner-id
                :bid-history bid-history
                :bid-cards bid-cards}
               (if-let [remaining-rounds (next rounds)]
                 (recur state
                        remaining-rounds
                        (inc round-number)
                        bid-history
                        bid-cards)
                 (if allow-unresolved?
                   {:ok? true
                    :resolved? false
                    :state state
                    :bid-history bid-history
                    :bid-cards bid-cards}
                   (failure :unresolved-bid-tie
                            "Starting bids ended in a tie and require another bid round."
                            {:bid-history bid-history})))))))))))

(defn- player-index-in [players player-id]
  (first
   (keep-indexed (fn [index player]
                   (when (= player-id (:id player))
                     index))
                 players)))

(defn- counterclockwise-redraw-order [players winner-id]
  (let [player-ids (mapv :id players)
        winner-index (player-index-in players winner-id)
        player-count (count player-ids)]
    (mapv (fn [offset]
            (get player-ids (mod (- winner-index offset) player-count)))
          (range 1 (inc player-count)))))

(defn resolve-starting-bid-rounds
  [state {:keys [rounds bid-rounds] :as _command}]
  (let [rounds (or rounds bid-rounds)
        original-players (:players state)]
    (cond
      (some? (get-in state [:setup :starting-player-id]))
      (failure :starting-bids-already-resolved
               "The starting bid has already been resolved."
               {:starting-player-id (get-in state [:setup :starting-player-id])})

      :else
      (let [{:keys [ok? winner-id] :as bid-result}
            (resolve-bid-rounds state rounds true)]
        (if-not ok?
          bid-result
          (cond-> bid-result
            winner-id
            (assoc :redraw-order
                   (counterclockwise-redraw-order original-players winner-id))))))))

(defn- redraw-card-ids-for [redraws player-id]
  (vec (get redraws player-id [])))

(defn- redraw-shape-error [state redraws]
  (let [player-id-set (set (state-player-ids state))]
    (cond
      (not (map? redraws))
      (failure :invalid-bid-redraws
               "Bid redraws must provide a map from player id to selected bid card ids."
               {:redraws redraws})

      (seq (remove player-id-set (keys redraws)))
      (failure :unknown-bid-redraw-players
               "Bid redraws can only include players in the game."
               {:unknown-player-ids (vec (sort-by str (remove player-id-set (keys redraws))))
                :player-ids (state-player-ids state)}))))

(defn- apply-redraw-card [state available-cards player-id card-id]
  (if-let [card (get available-cards card-id)]
    {:ok? true
     :state (append-cards-to-hand state player-id [card])
     :available-cards (dissoc available-cards card-id)
     :card card}
    (failure :invalid-bid-redraw-card
             "Players can only redraw from bid cards that are still available."
             {:player-id player-id
              :card-id card-id
              :available-card-ids (vec (sort (keys available-cards)))})))

(defn- apply-player-redraws [state available-cards player-id card-ids]
  (loop [next-state state
         next-available-cards available-cards
         remaining-card-ids (seq card-ids)
         selected-cards []]
    (if-let [card-id (first remaining-card-ids)]
      (let [{:keys [ok? state available-cards] :as result}
            (apply-redraw-card next-state next-available-cards player-id card-id)]
        (if ok?
          (recur state
                 available-cards
                 (next remaining-card-ids)
                 (conj selected-cards (:card result)))
          result))
      {:ok? true
       :state next-state
       :available-cards next-available-cards
       :selected-card-ids (mapv :id selected-cards)})))

(defn- apply-bid-redraws [state bid-cards redraw-order redraws]
  (if-let [error (redraw-shape-error state redraws)]
    error
    (loop [next-state state
           available-cards (into {} (map (juxt :id identity)) bid-cards)
           remaining-player-ids (seq redraw-order)
           redraw-history []]
      (if-let [player-id (first remaining-player-ids)]
        (let [needed-count (- starting-hand-size
                              (count (get-in next-state [:players-by-id player-id :hand])))
              selected-card-ids (redraw-card-ids-for redraws player-id)]
          (if (not= needed-count (count selected-card-ids))
            (failure :invalid-bid-redraw-count
                     "Each player must redraw until their hand has six cards again."
                     {:player-id player-id
                      :expected-count needed-count
                      :actual-count (count selected-card-ids)
                      :selected-card-ids selected-card-ids})
            (let [{:keys [ok? state available-cards selected-card-ids] :as redraw-result}
                  (apply-player-redraws next-state
                                        available-cards
                                        player-id
                                        selected-card-ids)]
              (if ok?
                (recur state
                       available-cards
                       (next remaining-player-ids)
                       (conj redraw-history
                             {:player-id player-id
                              :card-ids selected-card-ids}))
                redraw-result))))
        (if (seq available-cards)
          (failure :unused-bid-redraw-cards
                   "All bid cards must be redrawn before the game starts."
                   {:remaining-card-ids (vec (sort (keys available-cards)))})
          {:ok? true
           :state next-state
           :redraw-history redraw-history})))))

(defn- rotate-players-to-starting-player [state winner-id]
  (let [players (:players state)
        winner-index (player-index-in players winner-id)
        rotated-players (rotate-vector-from-index players winner-index)]
    (assoc state
           :players rotated-players
           :players-by-id (rebuild-players-by-id rotated-players)
           :turn (initial-turn rotated-players))))

(defn apply-starting-bids
  [state {:keys [rounds bid-rounds redraws] :as _command}]
  (let [rounds (or rounds bid-rounds)
        original-players (:players state)]
    (cond
      (some? (get-in state [:setup :starting-player-id]))
      (failure :starting-bids-already-resolved
               "The starting bid has already been resolved."
               {:starting-player-id (get-in state [:setup :starting-player-id])})

      :else
      (let [{:keys [ok? state winner-id bid-history bid-cards] :as bid-result}
            (resolve-bid-rounds state rounds)]
        (if-not ok?
          bid-result
          (let [redraw-order (counterclockwise-redraw-order original-players winner-id)
                {:keys [ok? state redraw-history] :as redraw-result}
                (apply-bid-redraws state bid-cards redraw-order redraws)]
            (if-not ok?
              redraw-result
              (let [event {:type :setup/starting-player-determined
                           :player-id winner-id
                           :bid-round-count (count bid-history)
                           :bid-card-ids (mapv :id bid-cards)
                           :redraw-order redraw-order}
                    next-state (-> state
                                   (assoc-in [:setup :bids] {})
                                   (assoc-in [:setup :bid-history] bid-history)
                                   (assoc-in [:setup :bid-redraw-order] redraw-order)
                                   (assoc-in [:setup :bid-redraws] redraw-history)
                                   (assoc-in [:setup :starting-player-id] winner-id)
                                   (rotate-players-to-starting-player winner-id)
                                   (append-history event))]
                (success next-state [event])))))))))

(defn duplicate-values [values]
  (->> values
       frequencies
       (filter (fn [[_ occurrences]]
                 (> occurrences 1)))
       (map first)
       vec))

(defn stash-count [state player-id size]
  (or (get-in state [:players-by-id player-id :stash size])
      (get-in state [:pieces :stashes player-id size])
      0))

(defn update-stash-count [state player-id size f]
  (-> state
      (update-player player-id update-in [:stash size] f)
      (update-in [:pieces :stashes player-id size] f)))

(defn increment-stash [state player-id size]
  (update-stash-count state player-id size inc))

(defn decrement-stash [state player-id size]
  (update-stash-count state player-id size dec))

(defn small-stash-count [state player-id]
  (stash-count state player-id :small))

(defn decrement-small-stash [state player-id]
  (decrement-stash state player-id :small))

(defn next-piece-id [state player-id size]
  (let [prefix (str (name player-id) "-" (name size) "-")
        used-ids (set (map :id (get-in state [:pieces :on-board])))]
    (->> (iterate inc 1)
         (map #(keyword (str prefix %)))
         (remove used-ids)
         first)))

(defn source-summary [source]
  (select-keys source [:kind :board-index :card-id :piece-id]))

(defn current-player-id? [state player-id]
  (= player-id (get-in state [:turn :current-player-id])))

(defn apply-source-cost [state player-id {:keys [source-card discard-source-card?]}]
  (if discard-source-card?
    (-> state
        (remove-card-from-hand player-id (:id source-card))
        (discard-card source-card))
    state))

(defn wasteland-target [target]
  (when (and (map? target)
             (= :wasteland (:kind target))
             (int? (:row target))
             (int? (:col target)))
    (select-keys target [:kind :row :col])))

(defn wasteland-target? [state target]
  (boolean
   (some (fn [space]
           (and (= (:row target) (:row space))
                (= (:col target) (:col space))))
         (board-layout/wasteland-spaces (:board state)))))

(defn legal-piece-coordinate? [state [row col]]
  (or (some? (board-cell-at state row col))
      (wasteland-target? state {:kind :wasteland
                                :row row
                                :col col})))

(defn void-pieces [state]
  (->> (get-in state [:pieces :on-board])
       (filterv (fn [piece]
                  (let [coordinate (piece-coordinate state piece)]
                    (or (nil? coordinate)
                        (not (legal-piece-coordinate? state coordinate))))))))

(defn enemy-pieces-at-coordinate [state player-id row col]
  (->> (get-in state [:pieces :on-board])
       (filter (fn [piece]
                 (and (not= player-id (:player-id piece))
                      (= [row col] (piece-coordinate state piece)))))
       vec))

(defn move-wasteland-pieces-to-board-index [state row col board-index]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (mapv (fn [piece]
                       (if (= [row col] (piece-coordinate state piece))
                         (-> piece
                             (dissoc :space)
                             (assoc :space-index board-index))
                         piece))
                     board-pieces))))

(defn move-board-index-pieces-to-wasteland [state board-index row col]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (mapv (fn [piece]
                       (if (= board-index (:space-index piece))
                         (-> piece
                             (dissoc :space-index)
                             (assoc :space {:kind :wasteland
                                            :row row
                                            :col col}))
                         piece))
                     board-pieces))))

(defn pieces-at-coordinate [state row col]
  (filterv #(= [row col] (piece-coordinate state %))
           (get-in state [:pieces :on-board])))

(defn target-piece-territory-cell [state piece]
  (if-let [space-index (:space-index piece)]
    (board-cell-by-index state space-index)
    (when-let [{:keys [row col]} (:space piece)]
      (board-cell-at state row col))))

(defn next-board-index [state]
  (inc (apply max -1 (map :index (:board state)))))

(defn remove-piece-by-id [state piece-id]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (vec (remove #(= piece-id (:id %)) board-pieces)))))

(defn replace-piece-by-id [state piece-id piece]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (mapv (fn [board-piece]
                       (if (= piece-id (:id board-piece))
                         piece
                         board-piece))
                     board-pieces))))

(defn replace-piece [state piece]
  (replace-piece-by-id state (:id piece) piece))

(defn player-pieces [state player-id]
  (filterv #(= player-id (:player-id %))
           (get-in state [:pieces :on-board])))

(defn move-territory-cell [state board-index row col]
  (update state :board
          (fn [cells]
            (mapv (fn [cell]
                    (if (= board-index (:index cell))
                      (assoc cell
                             :row row
                             :col col
                             :orientation (board/orientation-for row col))
                      cell))
                  cells))))

(defn- scoreable-state? [state]
  (and (map? state)
       (sequential? (:players state))
       (sequential? (:board state))
       (map? (:pieces state))))

(defn- controlled-territory-player-id [pieces]
  (let [player-ids (set (map :player-id pieces))]
    (when (and (seq pieces)
               (= 1 (count player-ids)))
      (first player-ids))))

(defn player-score [state player-id]
  (reduce
   (fn [score cell]
     (let [pieces (pieces-at-board-index state (:index cell))]
       (if (= player-id (controlled-territory-player-id pieces))
         (+ score (or (cards/card-point-value (:card cell)) 0))
         score)))
   0
   (:board state)))

(defn scores [state]
  (into {}
        (map (fn [{:keys [id eliminated?]}]
               [id (if eliminated?
                     0
                     (player-score state id))]))
        (:players state)))

(defn with-current-scores [state]
  (if-not (scoreable-state? state)
    state
    (let [scores-by-player-id (scores state)
          players (mapv (fn [player]
                          (assoc player
                                 :score (get scores-by-player-id
                                             (:id player)
                                             0)))
                        (:players state))]
      (assoc state
             :players players
             :players-by-id (rebuild-players-by-id players)))))

(defn target-score [state]
  (get-in state [:setup :target-score] default-target-score))

(defn finished? [state]
  (= finished-phase (:phase state)))

(defn player-eliminated? [state player-id]
  (true? (get-in state [:players-by-id player-id :eliminated?])))

(defn- initial-placement-required-result [player-id action]
  (failure :initial-placement-required
           (case action
             :draw-cards
             "A player with no pieces must place their initial small piece instead of drawing cards."

             :end-turn
             "A player with no pieces must place their initial small piece before ending the turn."

             :announce-challenge
             "A player with no pieces must place their initial small piece before announcing a challenge."

             "A player with no pieces must place their initial small piece before taking another turn action.")
           {:player-id player-id
            :action action}))

(defn- initial-placement-required? [state player-id action]
  (and (not= :place-initial-small action)
       (empty? (player-pieces state player-id))))

(defn turn-action-unavailable-result [state player-id action]
  (cond
    (nil? (get-in state [:players-by-id player-id]))
    (failure :unknown-player
             "Turn actions require a participating player."
             {:player-id player-id})

    (not (current-player-id? state player-id))
    (failure :not-current-player
             "Only the current player can take a turn action."
             {:player-id player-id
              :current-player-id (get-in state [:turn :current-player-id])
              :action action})

    (finished? state)
    (failure :game-finished
             "The game is already finished."
             {:winner (:winner state)
              :action action})

    (player-eliminated? state player-id)
    (failure :player-eliminated
             "Eliminated players cannot take turn actions."
             {:player-id player-id
              :action action})

    (initial-placement-required? state player-id action)
    (initial-placement-required-result player-id action)))

(defn- unresolved-challenge? [challenge]
  (= :announced (:status challenge)))

(defn active-challenge-player-id [state]
  (some (fn [{:keys [id challenge eliminated?]}]
          (when (and (not eliminated?)
                     (unresolved-challenge? challenge))
            id))
        (:players state)))

(defn can-announce-challenge? [state player-id]
  (boolean
   (and (some? (get-in state [:players-by-id player-id]))
        (current-player-id? state player-id)
        (not (finished? state))
        (not (player-eliminated? state player-id))
        (seq (player-pieces state player-id))
        (nil? (active-challenge-player-id state)))))

(defn challenge-unavailable-reason [state player-id]
  (let [active-challenger-id (active-challenge-player-id state)]
    (cond
      (nil? (get-in state [:players-by-id player-id]))
      "Unknown player."

      (not (current-player-id? state player-id))
      "Only the current player can announce a challenge."

      (finished? state)
      "The game is already finished."

      (player-eliminated? state player-id)
      "Eliminated players cannot announce a challenge."

      (initial-placement-required? state player-id :announce-challenge)
      "A player with no pieces must place their initial small piece before announcing a challenge."

      (= active-challenger-id player-id)
      "This player already has an unresolved challenge."

      active-challenger-id
      "Another player has an unresolved challenge.")))

(defn can-end-turn? [state player-id]
  (nil? (turn-action-unavailable-result state player-id :end-turn)))

(declare end-turn)

(defn- record-challenge [state {:keys [player-id]}]
  (let [state (with-current-scores state)]
    (if-let [reason (challenge-unavailable-reason state player-id)]
      (failure :challenge-unavailable
               reason
               {:player-id player-id
                :active-challenge-player-id (active-challenge-player-id state)})
      (let [challenge {:status :announced
                       :target-score (target-score state)
                       :score-at-announcement (get-in state [:players-by-id player-id :score])
                       :announced-round (get-in state [:turn :round])
                       :announced-turn-index (get-in state [:turn :current-player-index])}
            event {:type :challenge/announced
                   :player-id player-id
                   :score (:score-at-announcement challenge)
                   :target-score (:target-score challenge)
                   :round (:announced-round challenge)}
            next-state (-> state
                           (update-player player-id assoc :challenge challenge)
                           (append-history event))]
        (success next-state [event])))))

(defn announce-challenge [state command]
  (end-turn state (assoc command :announce-challenge? true)))

(defn- active-players [state]
  (filterv (comp not :eliminated?) (:players state)))

(defn- single-active-player-id [state]
  (let [active (active-players state)]
    (when (= 1 (count active))
      (:id (first active)))))

(defn- current-player-index-for [state player-id]
  (let [order (get-in state [:turn :order])]
    (first
     (keep-indexed (fn [index id]
                     (when (= player-id id)
                       index))
                   order))))

(defn- set-current-player [state player-id]
  (if-let [index (current-player-index-for state player-id)]
    (assoc-in (assoc-in state [:turn :current-player-index] index)
              [:turn :current-player-id]
              player-id)
    state))

(defn- finish-game [state winner-id reason score target-score]
  (let [event {:type :game/won
               :player-id winner-id
               :reason reason
               :score score
               :target-score target-score}
        next-state (-> state
                       (set-current-player winner-id)
                       (assoc :phase finished-phase
                              :winner {:player-id winner-id
                                       :reason reason
                                       :score score
                                       :target-score target-score})
                       (append-history event))]
    {:state next-state
     :event event}))

(defn return-pieces-to-stash [state pieces]
  (reduce (fn [next-state {:keys [id player-id size]}]
            (-> next-state
                (increment-stash player-id size)
                (remove-piece-by-id id)))
          state
          pieces))

(defn return-void-pieces-to-stash [state]
  (return-pieces-to-stash state (void-pieces state)))

(defn- remove-player-pieces [state player-id]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (vec (remove #(= player-id (:player-id %)) board-pieces)))))

(defn- eliminate-player [state player-id score target-score]
  (let [removed-pieces (player-pieces state player-id)
        discarded-hand (vec (get-in state [:players-by-id player-id :hand]))
        event {:type :challenge/failed
               :player-id player-id
               :score score
               :target-score target-score
               :discarded-card-ids (mapv :id discarded-hand)
               :removed-piece-ids (mapv :id removed-pieces)}
        next-state (-> state
                       (return-pieces-to-stash removed-pieces)
                       (remove-player-pieces player-id)
                       (update-player player-id assoc
                                      :hand []
                                      :score 0
                                      :challenge nil
                                      :eliminated? true)
                       (discard-cards discarded-hand)
                       (with-current-scores)
                       (append-history event))]
    {:state next-state
     :event event}))

(defn- next-active-index [state order current-player-index]
  (let [player-active? (fn [player-id]
                         (not (player-eliminated? state player-id)))
        player-count (count order)]
    (some (fn [offset]
            (let [index (mod (+ current-player-index offset) player-count)]
              (when (player-active? (get order index))
                index)))
          (range 1 (inc player-count)))))

(defn create-game
  ([player-specs]
   (create-game player-specs {}))
  ([player-specs opts]
   (if-let [error (validate-player-specs player-specs)]
     error
     (let [target-score (target-score-option opts)
           minimum-card-count (required-starting-card-count (count player-specs))
           source-deck (deck-source opts)]
       (if-let [error (or (validate-target-score target-score)
                          (validate-deck source-deck minimum-card-count))]
         error
         (let [deck (ordered-deck opts)]
           (if-let [error (validate-deck deck minimum-card-count)]
             error
             (let [base-players (mapv normalize-player (range) player-specs)
                   {:keys [players board-deck]} (deal-starting-hands base-players deck)
                   board-cells (board/initial-board board-deck identity)
                   event {:type :game/created
                          :phase initial-phase
                          :player-ids (mapv :id players)
                          :starting-hand-size starting-hand-size
                          :board-card-count (count board-cells)}
                   state {:phase initial-phase
                          :players players
                          :players-by-id (into {} (map (juxt :id identity) players))
                          :turn (initial-turn players)
                          :board board-cells
                          :pieces {:on-board []
                                   :stashes (initial-stashes players)}
                          :draw-pile (vec (drop board/board-card-count board-deck))
                          :discard-pile []
                          :setup {:bids {}
                                  :bid-history []
                                  :starting-player-id nil
                                  :target-score target-score}
                          :winner nil
                          :history [event]}]
               (if-let [starting-bids (:starting-bids opts)]
                 (let [{:keys [ok? state events error]} (apply-starting-bids state starting-bids)]
                   (if ok?
                     (success state (into [event] events))
                     (failure (:code error)
                              (:message error)
                              (:data error))))
                 (success state [event]))))))))))

(defn advance-turn [state]
  (let [state (with-current-scores state)
        {:keys [order current-player-index round]} (:turn state)]
    (cond
      (finished? state)
      (failure :game-finished
               "Cannot advance turn after the game is finished."
               {:winner (:winner state)})

      (not (seq order))
      (failure :missing-turn-order
               "Cannot advance turn without a player order."
               {:turn (:turn state)})

      :else
      (if-let [next-index (next-active-index state order current-player-index)]
        (let [next-round (if (<= next-index current-player-index)
                           (inc round)
                           round)
              next-player-id (get order next-index)
              event {:type :turn/advanced
                     :player-id next-player-id
                     :round next-round}
              next-state (-> state
                             (assoc :turn {:order order
                                           :current-player-index next-index
                                           :current-player-id next-player-id
                                           :round next-round})
                             (append-history event))]
          (success next-state [event]))
        (failure :no-active-players
                 "Cannot advance turn without an active player."
                 {:turn (:turn state)})))))

(defn- resolve-challenge [state player-id]
  (let [state (with-current-scores state)
        score (get-in state [:players-by-id player-id :score] 0)
        target-score (target-score state)]
    (if (<= target-score score)
      (let [challenge-event {:type :challenge/won
                             :player-id player-id
                             :score score
                             :target-score target-score}
            winner-result (finish-game
                           (-> state
                               (update-player player-id assoc-in
                                              [:challenge :status]
                                              :won)
                               (append-history challenge-event))
                           player-id
                           :challenge
                           score
                           target-score)]
        (success (:state winner-result)
                 [challenge-event (:event winner-result)]))
      (let [{eliminated-state :state
             eliminated-event :event} (eliminate-player state
                                                        player-id
                                                        score
                                                        target-score)]
        (if-let [winner-id (single-active-player-id eliminated-state)]
          (let [winner-score (get-in eliminated-state [:players-by-id winner-id :score] 0)
                winner-result (finish-game eliminated-state
                                           winner-id
                                           :last-active-player
                                           winner-score
                                           target-score)]
            (success (:state winner-result)
                     [eliminated-event (:event winner-result)]))
          (let [{:keys [ok? state events error]} (advance-turn eliminated-state)]
            (if ok?
              (success state (into [eliminated-event] events))
              (failure (:code error)
                       (:message error)
                       (:data error)))))))))

(defn end-turn [state {:keys [player-id announce-challenge? challenge?]}]
  (let [announce-challenge? (or announce-challenge? challenge?)
        state (with-current-scores state)
        player (get-in state [:players-by-id player-id])]
    (cond
      (nil? player)
      (failure :unknown-player
               "Cannot end a turn for an unknown player."
               {:player-id player-id})

      (not (current-player-id? state player-id))
      (failure :not-current-player
               "Only the current player can end their turn."
               {:player-id player-id
                :current-player-id (get-in state [:turn :current-player-id])})

      (finished? state)
      (failure :game-finished
               "The game is already finished."
               {:winner (:winner state)})

      (player-eliminated? state player-id)
      (failure :player-eliminated
               "Eliminated players cannot take turns."
               {:player-id player-id})

      (initial-placement-required? state player-id :end-turn)
      (initial-placement-required-result player-id :end-turn)

      (unresolved-challenge? (:challenge player))
      (resolve-challenge state player-id)

      announce-challenge?
      (let [{:keys [ok? state events error]} (record-challenge state
                                                                {:player-id player-id})]
        (if ok?
          (let [{advanced-ok? :ok?
                 advanced-state :state
                 advanced-events :events
                 advanced-error :error} (advance-turn state)]
            (if advanced-ok?
              (success advanced-state (into events advanced-events))
              (failure (:code advanced-error)
                       (:message advanced-error)
                       (:data advanced-error))))
          (failure (:code error)
                   (:message error)
                   (:data error))))

      :else
      (advance-turn state))))
