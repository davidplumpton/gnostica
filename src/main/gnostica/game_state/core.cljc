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

(def default-target-score 9)

(def required-player-fields [:id :name :color :css-color])

(def required-card-fields [:id :title :image])

(defn success
  ([state]
   (success state []))
  ([state events]
   {:ok? true
    :state state
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

(defn append-cards-to-hand [state player-id cards]
  (update-player state player-id update :hand
                 (fn [hand]
                   (vec (concat hand cards)))))

(defn duplicate-values [values]
  (->> values
       frequencies
       (filter (fn [[_ occurrences]]
                 (> occurrences 1)))
       (map first)
       vec))

(defn small-stash-count [state player-id]
  (or (get-in state [:players-by-id player-id :stash :small])
      (get-in state [:pieces :stashes player-id :small])
      0))

(defn decrement-small-stash [state player-id]
  (-> state
      (update-player player-id update-in [:stash :small] dec)
      (update-in [:pieces :stashes player-id :small] dec)))

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

(defn replace-piece [state piece]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (mapv (fn [board-piece]
                       (if (= (:id piece) (:id board-piece))
                         piece
                         board-piece))
                     board-pieces))))

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

(defn create-game
  ([player-specs]
   (create-game player-specs {}))
  ([player-specs opts]
   (if-let [error (validate-player-specs player-specs)]
     error
     (let [minimum-card-count (required-starting-card-count (count player-specs))
           source-deck (deck-source opts)]
       (if-let [error (validate-deck source-deck minimum-card-count)]
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
                                  :target-score default-target-score}
                          :history [event]}]
               (success state [event])))))))))

(defn advance-turn [state]
  (let [{:keys [order current-player-index round]} (:turn state)]
    (if (seq order)
      (let [next-index (mod (inc current-player-index) (count order))
            next-round (if (zero? next-index) (inc round) round)
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
      (failure :missing-turn-order
               "Cannot advance turn without a player order."
               {:turn (:turn state)}))))
