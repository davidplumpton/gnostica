(ns gnostica.game-state
  (:require [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.pieces :as pieces]))

(def min-players 2)
(def max-players 6)
(def pieces-per-size-in-stash 5)
(def initial-phase :setup)
(def default-target-score 9)

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

(defn- validate-player-specs [player-specs]
  (cond
    (not (sequential? player-specs))
    (failure :invalid-player-specs
             "Player specs must be a sequential collection."
             {:player-specs player-specs})

    (not (valid-player-count? player-specs))
    (failure :invalid-player-count
             "Gnostica requires between two and six players."
             {:count (count player-specs)
              :minimum min-players
              :maximum max-players})

    (seq (missing-player-id-indexes player-specs))
    (failure :invalid-player-specs
             "Each player spec must include an :id."
             {:missing-id-indexes (missing-player-id-indexes player-specs)})

    (seq (duplicate-player-ids player-specs))
    (failure :duplicate-player-ids
             "Player ids must be unique."
             {:duplicate-ids (duplicate-player-ids player-specs)})))

(defn- normalize-player [index player-spec]
  (let [id (player-id player-spec)
        piece-player (get pieces/players-by-id id)]
    (merge
     (select-keys piece-player [:id :name :color :css-color])
     player-spec
     {:order-index index
      :hand []
      :bid nil})))

(defn- ordered-deck [{:keys [deck deck-order shuffle-fn]
                      :or {deck cards/deck
                           shuffle-fn shuffle}}]
  (vec (if deck-order
         deck-order
         (shuffle-fn deck))))

(defn- validate-deck [deck]
  (when (< (count deck) board/board-card-count)
    (failure :insufficient-deck
             "The deck must contain enough cards to build the territory board."
             {:count (count deck)
              :minimum board/board-card-count})))

(defn- initial-stash []
  (into {}
        (map (fn [size]
               [size pieces-per-size-in-stash]))
        (keys pieces/piece-sizes)))

(defn- initial-stashes [players]
  (into {}
        (map (fn [player]
               [(:id player) (initial-stash)]))
        players))

(defn- initial-turn [players]
  (let [order (mapv :id players)]
    {:order order
     :current-player-index 0
     :current-player-id (first order)
     :round 1}))

(defn current-player [state]
  (get-in state [:players-by-id (get-in state [:turn :current-player-id])]))

(defn append-history [state event]
  (update state :history conj event))

(defn create-game
  ([player-specs]
   (create-game player-specs {}))
  ([player-specs opts]
   (if-let [error (validate-player-specs player-specs)]
     error
     (let [deck (ordered-deck opts)]
       (if-let [error (validate-deck deck)]
         error
         (let [players (mapv normalize-player (range) player-specs)
               board-cells (board/initial-board deck identity)
               event {:type :game/created
                      :phase initial-phase
                      :player-ids (mapv :id players)
                      :board-card-count (count board-cells)}
               state {:phase initial-phase
                      :players players
                      :players-by-id (into {} (map (juxt :id identity) players))
                      :turn (initial-turn players)
                      :board board-cells
                      :pieces {:on-board []
                               :stashes (initial-stashes players)}
                      :draw-pile (vec (drop board/board-card-count deck))
                      :discard-pile []
                      :setup {:bids {}
                              :bid-history []
                              :starting-player-id nil
                              :target-score default-target-score}
                      :history [event]}]
           (success state [event])))))))

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
