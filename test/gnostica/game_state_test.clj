(ns gnostica.game-state-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.game-schema :as game-schema]
            [gnostica.game-state :as game-state]
            [gnostica.game-state.cup :as game-state-cup]
            [gnostica.game-state.disc :as game-state-disc]
            [gnostica.game-state.draw :as game-state-draw]
            [gnostica.game-state.manipulation :as game-state-manipulation]
            [gnostica.game-state.placement :as game-state-placement]
            [gnostica.game-state.rod :as game-state-rod]
            [gnostica.game-state.sword :as game-state-sword]
            [gnostica.game-state.world :as game-state-world]
            [gnostica.pieces :as pieces]))

(def player-specs
  [{:id :rose
    :name "Rose"}
   {:id :indigo
    :name "Indigo"}])

(def three-player-specs
  [{:id :rose
    :name "Rose"}
   {:id :indigo
    :name "Indigo"}
   {:id :gold
    :name "Gold"}])

(defn- card-ids [cards]
  (mapv :id cards))

(defn- board-card-ids [state]
  (mapv (comp :id :card) (:board state)))

(defn- hand-card-count [player-count]
  (* game-state/starting-hand-size player-count))

(defn- board-deck-position [board-index]
  (+ (hand-card-count (count player-specs))
     board-index))

(defn- all-card-ids [state]
  (vec
   (concat
    (map :id (mapcat :hand (:players state)))
    (map (comp :id :card) (:board state))
    (map :id (:draw-pile state))
    (map :id (:discard-pile state)))))

(defn- deterministic-game []
  (:state (game-state/create-game player-specs {:shuffle-fn identity})))

(defn- deck-starting-with [card-ids]
  (let [front-ids (set card-ids)]
    (vec
     (concat
      (map cards/card-by-id card-ids)
      (remove #(contains? front-ids (:id %)) cards/deck)))))

(defn- deck-with-board-card [board-index card-id]
  (let [card (cards/card-by-id card-id)
        other-cards (vec (remove #(= card-id (:id %)) cards/deck))
        deck-index (+ (hand-card-count (count player-specs))
                      board-index)]
    (vec
     (concat
      (take deck-index other-cards)
      [card]
      (drop deck-index other-cards)))))

(defn- deck-with-cards-at [position->card-id]
  (let [placed-card-ids (set (vals position->card-id))]
    (loop [index 0
           filler-cards (remove #(contains? placed-card-ids (:id %)) cards/deck)
           deck []]
      (if (= (count cards/deck) (count deck))
        (vec deck)
        (if-let [card-id (get position->card-id index)]
          (recur (inc index)
                 filler-cards
                 (conj deck (cards/card-by-id card-id)))
          (recur (inc index)
                 (rest filler-cards)
                 (conj deck (first filler-cards))))))))

(defn- state-with-pieces [pieces]
  (game-state/with-board-pieces (deterministic-game) pieces))

(defn- state-with-board-cards [board-index->card-id]
  (:state (game-state/create-game
           player-specs
           {:deck-order
            (deck-with-cards-at
             (into {}
                   (map (fn [[board-index card-id]]
                          [(board-deck-position board-index) card-id]))
                   board-index->card-id))})))

(defn- state-with-board-card [state board-index card-id]
  (update state :board
          (fn [cells]
            (mapv (fn [cell]
                    (if (= board-index (:index cell))
                      (assoc cell :card (cards/card-by-id card-id))
                      cell))
                  cells))))

(def rose-cup-minion
  {:id :rose-cup-minion
   :player-id :rose
   :space-index 3
   :size :small
   :orientation :east})

(def rose-cup-wasteland-minion
  (assoc rose-cup-minion :orientation :west))

(def rose-rod-minion
  {:id :rose-rod-minion
   :player-id :rose
   :space-index 3
   :size :medium
   :orientation :east})

(def rose-disc-minion
  {:id :rose-disc-minion
   :player-id :rose
   :space-index 3
   :size :medium
   :orientation :east})

(def rose-sword-minion
  {:id :rose-sword-minion
   :player-id :rose
   :space-index 3
   :size :medium
   :orientation :east})

(def rose-target-minion
  {:id :rose-target-minion
   :player-id :rose
   :space-index 4
   :size :medium
   :orientation :up})

(def indigo-cup-target
  {:id :indigo-cup-target
   :player-id :indigo
   :space-index 4
   :size :medium
   :orientation :west})

(defn- piece-by-id [state piece-id]
  (some #(when (= piece-id (:id %)) %)
        (get-in state [:pieces :on-board])))

(defn- board-cell-by-index [state board-index]
  (some #(when (= board-index (:index %)) %)
        (:board state)))

(defn- board-cell-at [state row col]
  (some #(when (and (= row (:row %))
                    (= col (:col %)))
           %)
        (:board state)))

(defn- with-board-cells-at [state index-coordinates]
  (assoc state
         :board
         (mapv (fn [[board-index {:keys [row col]}]]
                 (assoc (board-cell-by-index state board-index)
                        :row row
                        :col col
                        :orientation (board/orientation-for row col)))
               index-coordinates)))

(defn- player-hand-ids [state player-id]
  (mapv :id (get-in state [:players-by-id player-id :hand])))

(defn- replace-player-hand [state player-id hand]
  (let [players (mapv (fn [player]
                        (if (= player-id (:id player))
                          (assoc player :hand (vec hand))
                          player))
                      (:players state))]
    (assoc state
           :players players
           :players-by-id (into {} (map (juxt :id identity) players)))))

(defn- set-player-eliminated [state player-id eliminated?]
  (let [players (mapv (fn [player]
                        (if (= player-id (:id player))
                          (assoc player :eliminated? eliminated?)
                          player))
                      (:players state))]
    (assoc state
           :players players
           :players-by-id (into {} (map (juxt :id identity) players)))))

(defn- remove-card-id [cards card-id]
  (vec (remove #(= card-id (:id %)) cards)))

(defn- move-card-to-discard [state card-id]
  (let [card (cards/card-by-id card-id)]
    (-> (reduce (fn [next-state player-id]
                  (replace-player-hand next-state
                                       player-id
                                       (remove-card-id
                                        (get-in next-state
                                                [:players-by-id player-id :hand])
                                        card-id)))
                state
                (map :id (:players state)))
        (update :draw-pile remove-card-id card-id)
        (update :discard-pile conj card))))

(deftest creates-deterministic-initial-state
  (let [hand-count (hand-card-count (count player-specs))
        board-deck (drop hand-count cards/deck)
        {:keys [ok? state]} (game-state/create-game player-specs {:shuffle-fn identity})]
    (is ok?)
    (is (= (board/initial-board board-deck identity)
           (:board state)))
    (is (= (mapv :id (drop (+ hand-count board/board-card-count) cards/deck))
           (mapv :id (:draw-pile state))))
    (is (empty? (:discard-pile state)))
    (is (= #{:rose :indigo}
           (set (keys (get-in state [:pieces :stashes])))))))

(deftest records-phase-and-current-player-shape
  (let [{:keys [state]} (game-state/create-game player-specs {:shuffle-fn identity})]
    (is (= :setup (:phase state)))
    (is (= {:order [:rose :indigo]
            :current-player-index 0
            :current-player-id :rose
            :round 1}
           (:turn state)))
    (is (= :rose (:id (game-state/current-player state))))
    (is (= {:small 5
            :medium 5
            :large 5}
           (get-in state [:pieces :stashes :rose])))))

(deftest explicit-deck-order-controls-hands-board-and-draw-pile
  (let [deck-order (vec (reverse cards/deck))
        {:keys [state]} (game-state/create-game player-specs {:deck-order deck-order})
        hand-count (hand-card-count (count player-specs))]
    (is (= (card-ids (take game-state/starting-hand-size deck-order))
           (card-ids (get-in state [:players 0 :hand]))))
    (is (= (card-ids (take game-state/starting-hand-size
                           (drop game-state/starting-hand-size deck-order)))
           (card-ids (get-in state [:players 1 :hand]))))
    (is (= (card-ids (take board/board-card-count
                           (drop hand-count deck-order)))
           (board-card-ids state)))
    (is (= (card-ids (drop (+ hand-count board/board-card-count) deck-order))
           (mapv :id (:draw-pile state))))))

(deftest rejects-invalid-player-counts
  (doseq [player-specs [[] [{:id :solo}] (mapv (fn [index]
                                                 {:id (keyword (str "player-" index))})
                                               (range 7))]]
    (let [{:keys [ok? error]} (game-state/create-game player-specs {:shuffle-fn identity})]
      (is (false? ok?))
      (is (= :invalid-player-count (:code error))))))

(deftest creates-two-through-six-player-games-with-complete-metadata
  (doseq [player-count (range game-state/min-players (inc game-state/max-players))
          :let [player-specs (mapv #(select-keys % [:id])
                                   (take player-count pieces/players))
                {:keys [ok? state]} (game-state/create-game player-specs
                                                             {:shuffle-fn identity})
                player-ids (mapv :id (:players state))]]
    (is ok?)
    (is (= player-count (count (:players state))))
    (is (= player-ids (get-in state [:turn :order])))
    (is (= (set player-ids)
           (set (keys (get-in state [:pieces :stashes])))))
    (is (every?
         (fn [player]
           (and (string? (:name player))
                (integer? (:color player))
                (string? (:css-color player))
                (vector? (:hand player))
                (zero? (:score player))
                (contains? player :challenge)
                (false? (:eliminated? player))
                (= {:small 5
                    :medium 5
                    :large 5}
                   (:stash player))))
         (:players state)))))

(deftest deals-six-card-hands-for-two-through-six-players
  (doseq [player-count (range game-state/min-players (inc game-state/max-players))
          :let [player-specs (mapv #(select-keys % [:id])
                                   (take player-count pieces/players))
                {:keys [ok? state]} (game-state/create-game player-specs
                                                             {:shuffle-fn identity})]]
    (is ok?)
    (is (every? #(= game-state/starting-hand-size (count (:hand %)))
                (:players state)))
    (is (= (count cards/deck)
           (count (all-card-ids state))))
    (is (= (count cards/deck)
           (count (set (all-card-ids state)))))
    (is (= (set (map :id cards/deck))
           (set (all-card-ids state))))))

(deftest rejects-unknown-player-ids
  (let [{:keys [ok? error]} (game-state/create-game [{:id :rose}
                                                     {:id :obsidian}]
                                                    {:shuffle-fn identity})]
    (is (false? ok?))
    (is (= :unknown-player-ids (:code error)))
    (is (= [:obsidian] (get-in error [:data :unknown-ids])))))

(deftest rejects-player-specs-that-remove-required-metadata
  (let [{:keys [ok? error]} (game-state/create-game [{:id :rose
                                                      :css-color nil}
                                                     {:id :indigo}]
                                                    {:shuffle-fn identity})]
    (is (false? ok?))
    (is (= :invalid-player-metadata (:code error)))
    (is (= [{:index 0
             :id :rose
             :invalid-fields [:css-color]}]
           (get-in error [:data :players])))))

(deftest rejects-duplicate-deck-card-ids
  (let [duplicated-deck (assoc cards/deck 1 (first cards/deck))
        duplicate-id (:id (first cards/deck))
        {:keys [ok? error]} (game-state/create-game player-specs
                                                    {:deck-order duplicated-deck})]
    (is (false? ok?))
    (is (= :duplicate-card-ids (:code error)))
    (is (= [duplicate-id] (get-in error [:data :duplicate-ids])))))

(deftest rejects-malformed-deck-cards
  (let [malformed-deck (assoc cards/deck 0 (dissoc (first cards/deck) :image))
        {:keys [ok? error]} (game-state/create-game player-specs
                                                    {:deck-order malformed-deck})]
    (is (false? ok?))
    (is (= :invalid-deck-cards (:code error)))
    (is (= [{:index 0
             :card-id (:id (first cards/deck))
             :invalid-fields [:image]}]
           (get-in error [:data :invalid-cards])))))

(deftest advances-turn-with-success-result
  (let [{:keys [state]} (game-state/create-game player-specs {:shuffle-fn identity})
        {:keys [ok? state events]} (game-state/advance-turn state)]
    (is ok?)
    (is (= :indigo (get-in state [:turn :current-player-id])))
    (is (= :indigo (:id (game-state/current-player state))))
    (is (= [{:type :turn/advanced
             :player-id :indigo
             :round 1}]
           events))))

(deftest target-score-option-accepts-official-short-and-long-games
  (let [{:keys [ok? state]} (game-state/create-game player-specs
                                                     {:shuffle-fn identity
                                                      :target-score 10})
        invalid-result (game-state/create-game player-specs
                                               {:shuffle-fn identity
                                                :target-score 7})]
    (is ok?)
    (is (= 10 (game-state/target-score state)))
    (is (false? (:ok? invalid-result)))
    (is (= :invalid-target-score
           (get-in invalid-result [:error :code])))))

(deftest official-starting-bid-can-make_a_major_bidder_start
  (let [deck-order (deck-with-cards-at {0 "cupsking"
                                        game-state/starting-hand-size "fool"})
        {:keys [ok? state events]} (game-state/create-game
                                    player-specs
                                    {:deck-order deck-order
                                     :starting-bids
                                     {:rounds [{:rose "cupsking"
                                                :indigo "fool"}]
                                      :redraws {:rose ["fool"]
                                                :indigo ["cupsking"]}}})]
    (is ok?)
    (is (= [:game/created :setup/starting-player-determined]
           (mapv :type events)))
    (is (= :indigo (get-in state [:setup :starting-player-id])))
    (is (= [:indigo :rose] (get-in state [:turn :order])))
    (is (= :indigo (get-in state [:turn :current-player-id])))
    (is (= [:indigo :rose] (mapv :id (:players state))))
    (is (= ["fool"]
           (take-last 1 (player-hand-ids state :rose))))
    (is (= ["cupsking"]
           (take-last 1 (player-hand-ids state :indigo))))
    (is (= [{:round 1
             :bids {:rose "cupsking"
                    :indigo "fool"}
             :considered-arcana :major
             :winning-rank 0
             :tied-player-ids [:indigo]
             :tied-card-ids ["fool"]
             :winner-id :indigo
             :winning-card-id "fool"}]
           (get-in state [:setup :bid-history])))
    (is (= [:rose :indigo]
           (get-in state [:setup :bid-redraw-order])))
    (is (= [{:player-id :rose
             :card-ids ["fool"]}
            {:player-id :indigo
             :card-ids ["cupsking"]}]
           (get-in state [:setup :bid-redraws])))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest official-starting-bid-rebids_after_tied_minors_and_redraws_counterclockwise
  (let [deck-order (deck-with-cards-at {0 "cupsking"
                                        1 "coins2"
                                        6 "swordsking"
                                        7 "cups3"
                                        12 "wandsqueen"
                                        13 "world"})
        {:keys [ok? state events]} (game-state/create-game
                                    three-player-specs
                                    {:deck-order deck-order
                                     :starting-bids
                                     {:rounds [{:rose "cupsking"
                                                :indigo "swordsking"
                                                :gold "wandsqueen"}
                                               {:rose "coins2"
                                                :indigo "cups3"
                                                :gold "world"}]
                                      :redraws {:indigo ["world" "cupsking"]
                                                :rose ["swordsking" "wandsqueen"]
                                                :gold ["coins2" "cups3"]}}})]
    (is ok?)
    (is (= [:game/created :setup/starting-player-determined]
           (mapv :type events)))
    (is (= :gold (get-in state [:setup :starting-player-id])))
    (is (= [:gold :rose :indigo] (get-in state [:turn :order])))
    (is (= :gold (get-in state [:turn :current-player-id])))
    (is (= [:gold :rose :indigo] (mapv :id (:players state))))
    (is (= [:indigo :rose :gold]
           (get-in state [:setup :bid-redraw-order])))
    (is (= [{:round 1
             :bids {:rose "cupsking"
                    :indigo "swordsking"
                    :gold "wandsqueen"}
             :considered-arcana :minor
             :winning-rank 14
             :tied-player-ids [:rose :indigo]
             :tied-card-ids ["cupsking" "swordsking"]}
            {:round 2
             :bids {:rose "coins2"
                    :indigo "cups3"
                    :gold "world"}
             :considered-arcana :major
             :winning-rank 21
             :tied-player-ids [:gold]
             :tied-card-ids ["world"]
             :winner-id :gold
             :winning-card-id "world"}]
           (get-in state [:setup :bid-history])))
    (is (= [{:player-id :indigo
             :card-ids ["world" "cupsking"]}
            {:player-id :rose
             :card-ids ["swordsking" "wandsqueen"]}
            {:player-id :gold
             :card-ids ["coins2" "cups3"]}]
           (get-in state [:setup :bid-redraws])))
    (is (every? #(= game-state/starting-hand-size (count (:hand %)))
                (:players state)))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest official-starting-bid-rejects_unresolved_ties_and_invalid_redraws
  (let [tie-deck (deck-with-cards-at {0 "cupsking"
                                      6 "swordsking"})
        tied-result (game-state/create-game
                     player-specs
                     {:deck-order tie-deck
                      :starting-bids
                      {:rounds [{:rose "cupsking"
                                 :indigo "swordsking"}]
                       :redraws {:rose ["swordsking"]
                                 :indigo ["cupsking"]}}})
        redraw-result (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-cards-at {0 "cupsking"
                                                         6 "fool"})
                        :starting-bids
                        {:rounds [{:rose "cupsking"
                                   :indigo "fool"}]
                         :redraws {:rose ["coins2"]
                                   :indigo ["cupsking"]}}})]
    (is (false? (:ok? tied-result)))
    (is (= :unresolved-bid-tie
           (get-in tied-result [:error :code])))
    (is (false? (:ok? redraw-result)))
    (is (= :invalid-bid-redraw-card
           (get-in redraw-result [:error :code])))))

(deftest scores-count-only-territories-controlled-by-one-player
  (let [state (-> (state-with-board-cards {0 "cups2"
                                           1 "cupsking"
                                           2 "sun"})
                  (game-state/with-board-pieces
                   [{:id :rose-mixed
                     :player-id :rose
                     :space-index 0
                     :size :small
                     :orientation :north}
                    {:id :indigo-mixed
                     :player-id :indigo
                     :space-index 0
                     :size :small
                     :orientation :north}
                    {:id :rose-royalty
                     :player-id :rose
                     :space-index 1
                     :size :small
                     :orientation :north}
                    {:id :rose-major
                     :player-id :rose
                     :space-index 2
                     :size :small
                     :orientation :north}])
                  game-state/with-current-scores)]
    (is (= {:rose 5
            :indigo 0}
           (game-state/scores state)))
    (is (= 5 (get-in state [:players-by-id :rose :score])))
    (is (game-schema/valid-game? state))))

(deftest end-turn-can-announce-and-resolve-a-winning-challenge
  (let [state (-> (state-with-board-cards {0 "cups2"
                                           1 "cupsking"
                                           2 "sun"
                                           3 "magician"})
                  (game-state/with-board-pieces
                   [{:id :rose-spot
                     :player-id :rose
                     :space-index 0
                     :size :small
                     :orientation :north}
                    {:id :rose-royalty
                     :player-id :rose
                     :space-index 1
                     :size :small
                     :orientation :north}
                    {:id :rose-major-a
                     :player-id :rose
                     :space-index 2
                     :size :small
                     :orientation :north}
                    {:id :rose-major-b
                     :player-id :rose
                     :space-index 3
                     :size :small
                     :orientation :north}
                    {:id :indigo-piece
                     :player-id :indigo
                     :space-index 4
                     :size :small
                     :orientation :north}]))
        announced (game-state/end-turn state {:player-id :rose
                                              :announce-challenge? true})
        indigo-blocked (game-state/end-turn (:state announced)
                                            {:player-id :indigo
                                             :announce-challenge? true})
        indigo-ended (game-state/end-turn (:state announced) {:player-id :indigo})
        resolved (game-state/end-turn (:state indigo-ended) {:player-id :rose})]
    (is (:ok? announced))
    (is (= [:challenge/announced :turn/advanced]
           (mapv :type (:events announced))))
    (is (= :rose (game-state/active-challenge-player-id (:state announced))))
    (is (= :challenge-unavailable
           (get-in indigo-blocked [:error :code])))
    (is (:ok? resolved))
    (is (= game-state/finished-phase (:phase (:state resolved))))
    (is (= {:player-id :rose
            :reason :challenge
            :score 9
            :target-score 9}
           (:winner (:state resolved))))
    (is (game-schema/valid-game? (:state resolved)))))

(deftest failed-challenge-eliminates-player-and-discards-their-hand
  (let [state (-> (state-with-board-cards {0 "sun"
                                           4 "magician"})
                  (game-state/with-board-pieces
                   [{:id :rose-major
                     :player-id :rose
                     :space-index 0
                     :size :small
                     :orientation :north}
                    {:id :indigo-piece
                     :player-id :indigo
                     :space-index 4
                     :size :small
                     :orientation :north}]))
        rose-hand-ids (player-hand-ids state :rose)
        announced (game-state/end-turn state {:player-id :rose
                                              :announce-challenge? true})
        indigo-ended (game-state/end-turn (:state announced) {:player-id :indigo})
        resolved (game-state/end-turn (:state indigo-ended) {:player-id :rose})
        resolved-state (:state resolved)]
    (is (:ok? resolved))
    (is (= [:challenge/failed :game/won]
           (mapv :type (:events resolved))))
    (is (true? (get-in resolved-state [:players-by-id :rose :eliminated?])))
    (is (empty? (get-in resolved-state [:players-by-id :rose :hand])))
    (is (empty? (filter #(= :rose (:player-id %))
                        (get-in resolved-state [:pieces :on-board]))))
    (is (= rose-hand-ids
           (mapv :id (take-last (count rose-hand-ids)
                                (:discard-pile resolved-state)))))
    (is (= {:player-id :indigo
            :reason :last-active-player
            :score 3
            :target-score 9}
           (:winner resolved-state)))
    (is (game-schema/valid-game? resolved-state))))

(deftest focused-transition-namespaces-match-public-facade
  (let [draw-state (state-with-pieces [rose-target-minion])
        draw-card (first (get-in draw-state [:players-by-id :rose :hand]))
        draw-command {:player-id :rose
                      :discard-card-ids [(:id draw-card)]
                      :draw-count 1
                      :shuffle-fn identity}
        orient-state (state-with-pieces [rose-target-minion])
        orient-command {:player-id :rose
                        :piece-id :rose-target-minion
                        :orientation :west}
        cup-state (-> (state-with-pieces [rose-cup-minion])
                      (state-with-board-card 3 "cups2"))
        cup-command {:player-id :rose
                     :source {:kind :territory
                              :board-index 3
                              :piece-id :rose-cup-minion}
                     :target {:kind :territory
                              :board-index 4}
                     :orientation :east}
        rod-state (-> (state-with-pieces [rose-rod-minion])
                      (state-with-board-card 3 "wands2"))
        rod-command {:player-id :rose
                     :source {:kind :territory
                              :board-index 3
                              :piece-id :rose-rod-minion}
                     :mode :move-minion
                     :distance 1
                     :orientation :south}
        disc-state (-> (state-with-pieces [rose-disc-minion])
                       (state-with-board-card 3 "coins2"))
        disc-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-disc-minion}
                      :target {:kind :piece
                               :piece-id :rose-disc-minion}
                      :orientation :south}
        sword-state (-> (state-with-pieces [rose-sword-minion])
                        (state-with-board-card 3 "swords2"))
        sword-command {:player-id :rose
                       :source {:kind :territory
                                :board-index 3
                                :piece-id :rose-sword-minion}
                       :target {:kind :piece
                                :piece-id :rose-sword-minion}
                       :damage 1
                       :orientation :south}
        draw-major-state (-> (:state (game-state/create-game
                                       player-specs
                                       {:deck-order
                                        (deck-starting-with
                                         ["fool" "high-priestess" "judgement"])}))
                             (game-state/with-board-pieces [rose-target-minion]))
        fool-command {:player-id :rose
                      :source {:kind :hand-card
                               :card-id "fool"}
                      :reveals []}
        high-priestess-command {:player-id :rose
                                :source {:kind :hand-card
                                         :card-id "high-priestess"}
                                :redraws []}
        judgement-command {:player-id :rose
                           :source {:kind :hand-card
                                    :card-id "judgement"}
                           :piece-id :rose-target-minion
                           :card-ids []}
        manipulation-state (-> (:state (game-state/create-game
                                         player-specs
                                         {:deck-order
                                          (deck-starting-with
                                           ["hierophant" "hermit" "devil"])}))
                               (game-state/with-board-pieces
                                [rose-target-minion
                                 indigo-cup-target]))
        hierophant-command {:player-id :rose
                            :source {:kind :hand-card
                                     :card-id "hierophant"
                                     :piece-id :rose-target-minion}
                            :target {:kind :piece
                                     :piece-id :indigo-cup-target}
                            :orientation :south}
        hermit-command {:player-id :rose
                        :source {:kind :hand-card
                                 :card-id "hermit"
                                 :piece-id :rose-target-minion}
                        :target {:kind :piece
                                 :piece-id :indigo-cup-target}
                        :destination {:kind :territory
                                      :board-index 0}}
        devil-command {:player-id :rose
                       :source {:kind :hand-card
                                :card-id "devil"
                                :piece-id :rose-target-minion}
                       :target {:kind :piece
                                :piece-id :indigo-cup-target}
                       :orientation :south}
        world-state (-> (:state (game-state/create-game
                                  player-specs
                                  {:deck-order
                                   (deck-with-cards-at
                                    {0 "world"
                                     (board-deck-position 3) "magician"})}))
                        (game-state/with-board-pieces [rose-cup-minion]))
        world-command {:player-id :rose
                       :source {:kind :hand-card
                                :card-id "world"
                                :piece-id :rose-cup-minion}
                       :copied-board-index 3
                       :power :cup
                       :target {:kind :territory
                                :board-index 4}
                       :orientation :east}]
    (is (= (game-state/apply-draw-move draw-state draw-command)
           (game-state-draw/apply-draw-move draw-state draw-command)))
    (is (= (game-state/apply-fool-move draw-major-state fool-command)
           (game-state-draw/apply-fool-move draw-major-state fool-command)))
    (is (= (game-state/apply-high-priestess-move draw-major-state
                                                  high-priestess-command)
           (game-state-draw/apply-high-priestess-move draw-major-state
                                                       high-priestess-command)))
    (is (= (game-state/apply-judgement-move draw-major-state judgement-command)
           (game-state-draw/apply-judgement-move draw-major-state judgement-command)))
    (is (= (game-state/apply-orient-move orient-state orient-command)
           (game-state-placement/apply-orient-move orient-state orient-command)))
    (is (= (game-state/apply-cup-move cup-state cup-command)
           (game-state-cup/apply-cup-move cup-state cup-command)))
    (is (= (game-state/resolve-rod-command rod-state rod-command)
           (game-state-rod/resolve-rod-command rod-state rod-command)))
    (is (= (game-state/apply-rod-move rod-state rod-command)
           (game-state-rod/apply-rod-move rod-state rod-command)))
	    (is (= (game-state/resolve-disc-command disc-state disc-command)
	           (game-state-disc/resolve-disc-command disc-state disc-command)))
    (is (= (game-state/apply-disc-move disc-state disc-command)
           (game-state-disc/apply-disc-move disc-state disc-command)))
    (is (= (game-state/resolve-sword-command sword-state sword-command)
           (game-state-sword/resolve-sword-command sword-state sword-command)))
    (is (= (game-state/apply-sword-move sword-state sword-command)
           (game-state-sword/apply-sword-move sword-state sword-command)))
    (is (= (game-state/apply-hierophant-move manipulation-state hierophant-command)
           (game-state-manipulation/apply-hierophant-move manipulation-state
                                                            hierophant-command)))
    (is (= (game-state/apply-hermit-move manipulation-state hermit-command)
           (game-state-manipulation/apply-hermit-move manipulation-state
                                                       hermit-command)))
    (is (= (game-state/apply-devil-move manipulation-state devil-command)
           (game-state-manipulation/apply-devil-move manipulation-state
                                                      devil-command)))
    (is (= (game-state/world-major-territories world-state)
           (game-state-world/world-major-territories world-state)))
    (is (= (game-state/apply-world-move world-state world-command)
           (game-state-world/apply-world-move world-state world-command)))))

(deftest draw-move-discards-selected-cards-and-draws-to-hand
  (let [initial-state (state-with-pieces [rose-target-minion])
        original-hand (get-in initial-state [:players-by-id :rose :hand])
        discarded-cards (take 2 original-hand)
        drawn-cards (take 2 (:draw-pile initial-state))
        command {:player-id :rose
                 :discard-card-ids (mapv :id discarded-cards)
                 :draw-count 2
                 :shuffle-fn identity}
        {:keys [ok? state events]} (game-state/apply-draw-move initial-state command)]
    (is ok?)
    (is (= (mapv :id (concat (drop 2 original-hand) drawn-cards))
           (player-hand-ids state :rose)))
    (is (= (mapv :id discarded-cards)
           (mapv :id (:discard-pile state))))
    (is (= (mapv :id (drop 2 (:draw-pile initial-state)))
           (mapv :id (:draw-pile state))))
    (is (= [{:type :draw/cards-drawn
             :player-id :rose
             :discarded-card-ids (mapv :id discarded-cards)
             :draw-count 2
             :drawn-card-ids (mapv :id drawn-cards)
             :reshuffled-discard? false}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest draw-move-reshuffles-discard-pile-when-draw-pile-is-exhausted
  (let [base-state (state-with-pieces [rose-target-minion])
        original-hand (get-in base-state [:players-by-id :rose :hand])
        discarded-hand-cards (take 2 original-hand)
        shortened-hand (vec (drop 2 original-hand))
        first-draw-card (first (:draw-pile base-state))
        prepared-discard (vec (concat discarded-hand-cards
                                      (rest (:draw-pile base-state))))
        state (-> base-state
                  (replace-player-hand :rose shortened-hand)
                  (assoc :draw-pile [first-draw-card]
                         :discard-pile prepared-discard))
        {:keys [ok? state events]} (game-state/apply-draw-move
                                    state
                                    {:player-id :rose
                                     :draw-count 2
                                     :shuffle-fn identity})
        expected-drawn [first-draw-card (first discarded-hand-cards)]]
    (is ok?)
    (is (= (mapv :id (concat shortened-hand expected-drawn))
           (player-hand-ids state :rose)))
    (is (empty? (:discard-pile state)))
    (is (= (mapv :id (concat (rest discarded-hand-cards)
                             (rest (:draw-pile base-state))))
           (mapv :id (:draw-pile state))))
    (is (= {:type :draw/cards-drawn
            :player-id :rose
            :discarded-card-ids []
            :draw-count 2
            :drawn-card-ids (mapv :id expected-drawn)
            :reshuffled-discard? true}
           (first events)))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest draw-move-rejects-invalid-counts-and-discard-cards
  (let [state (state-with-pieces [rose-target-minion])
        first-card-id (first (player-hand-ids state :rose))
        too-many-result (game-state/apply-draw-move
                         state
                         {:player-id :rose
                          :draw-count 1})
        duplicate-result (game-state/apply-draw-move
                          state
                          {:player-id :rose
                           :discard-card-ids [first-card-id first-card-id]
                           :draw-count 1})
        missing-result (game-state/apply-draw-move
                        state
                        {:player-id :rose
                         :discard-card-ids ["not-in-hand"]
                         :draw-count 1})]
    (is (= :invalid-draw-count
           (get-in too-many-result [:error :code])))
    (is (= 0
           (get-in too-many-result [:error :data :maximum])))
    (is (= :duplicate-discard-cards
           (get-in duplicate-result [:error :code])))
    (is (= :invalid-discard-cards
           (get-in missing-result [:error :code])))
    (is (false? (:ok? too-many-result)))
    (is (not (contains? too-many-result :state)))))

(deftest turn-action-facades-reject-illegal-turn-state
  (let [active-state (state-with-pieces [rose-target-minion])
        finished-result (game-state/apply-orient-move
                         (assoc active-state :phase game-state/finished-phase)
                         {:player-id :rose
                          :piece-id :rose-target-minion
                          :orientation :west})
        eliminated-result (game-state/apply-orient-move
                           (set-player-eliminated active-state :rose true)
                           {:player-id :rose
                            :piece-id :rose-target-minion
                            :orientation :west})
        non-current-result (game-state/apply-orient-move
                            active-state
                            {:player-id :indigo
                             :piece-id :rose-target-minion
                             :orientation :west})
        no-piece-draw-result (game-state/apply-draw-move
                              (deterministic-game)
                              {:player-id :rose
                               :draw-count 0
                               :shuffle-fn identity})]
    (is (= :game-finished
           (get-in finished-result [:error :code])))
    (is (= :player-eliminated
           (get-in eliminated-result [:error :code])))
    (is (= :not-current-player
           (get-in non-current-result [:error :code])))
    (is (= :initial-placement-required
           (get-in no-piece-draw-result [:error :code])))
    (is (false? (:ok? finished-result)))
    (is (false? (:ok? eliminated-result)))
    (is (false? (:ok? non-current-result)))
    (is (false? (:ok? no-piece-draw-result)))))

(deftest high-priestess-applies-two-redraw-passes-after-paying-hand-source
  (let [initial-state (-> (:state (game-state/create-game
                                   player-specs
                                   {:deck-order
                                    (deck-starting-with
                                     ["high-priestess" "cups2" "wands2"
                                      "coins2" "swords2" "cups3"])}))
                          (game-state/with-board-pieces [rose-target-minion]))
        first-drawn-card (first (:draw-pile initial-state))
        second-drawn-card (second (:draw-pile initial-state))
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "high-priestess"}
                 :redraws [{:discard-card-ids ["cups2"]
                            :draw-count 1}
                           {:discard-card-ids [(:id first-drawn-card)]
                            :draw-count 1}]
                 :shuffle-fn identity}
        {:keys [ok? state events]} (game-state/apply-high-priestess-move
                                    initial-state
                                    command)]
    (is ok?)
    (is (= ["wands2" "coins2" "swords2" "cups3" (:id second-drawn-card)]
           (player-hand-ids state :rose)))
    (is (= ["high-priestess" "cups2" (:id first-drawn-card)]
           (mapv :id (:discard-pile state))))
    (is (= (mapv :id (drop 2 (:draw-pile initial-state)))
           (mapv :id (:draw-pile state))))
    (is (= [:high-priestess/redrawn :high-priestess/redrawn]
           (mapv :type events)))
    (is (= [1 2] (mapv :pass-index events)))
    (is (= [["cups2"] [(:id first-drawn-card)]]
           (mapv :discarded-card-ids events)))
    (is (= [[(:id first-drawn-card)] [(:id second-drawn-card)]]
           (mapv :drawn-card-ids events)))
    (is (= events (take-last 2 (:history state))))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest judgement-draws-selected-discard-cards-from_anywhere_up_to_pips_and_hand_limit
  (let [base-state (state-with-board-cards {4 "judgement"})
        original-hand (get-in base-state [:players-by-id :rose :hand])
        shortened-hand (vec (take 4 original-hand))
        hand-discard-cards (vec (drop 4 original-hand))
        draw-discard-cards (vec (take 3 (:draw-pile base-state)))
        discard-cards (vec (concat hand-discard-cards draw-discard-cards))
        selected-cards [(second draw-discard-cards) (first hand-discard-cards)]
        state (-> base-state
                  (replace-player-hand :rose shortened-hand)
                  (assoc :draw-pile (vec (drop 3 (:draw-pile base-state)))
                         :discard-pile discard-cards)
                  (game-state/with-board-pieces [rose-target-minion]))
        {:keys [ok? state events]} (game-state/apply-judgement-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :territory
                                              :board-index 4}
                                     :piece-id :rose-target-minion
                                     :card-ids (mapv :id selected-cards)})]
    (is ok?)
    (is (= (mapv :id (concat shortened-hand selected-cards))
           (player-hand-ids state :rose)))
    (is (= (vec (remove (set (mapv :id selected-cards))
                        (mapv :id discard-cards)))
           (mapv :id (:discard-pile state))))
    (is (= [{:type :judgement/cards-drawn
             :player-id :rose
             :source {:kind :territory
                      :board-index 4}
             :piece-id :rose-target-minion
             :card-ids (mapv :id selected-cards)
             :draw-count 2
             :maximum 2}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest hand-source-judgement-can-draw_itself_after_source_cost
  (let [initial-state (-> (:state (game-state/create-game
                                   player-specs
                                   {:deck-order
                                    (deck-starting-with
                                     ["judgement" "cups2" "wands2"
                                      "coins2" "swords2" "cups3"])}))
                          (game-state/with-board-pieces [rose-target-minion]))
        {:keys [ok? state events]} (game-state/apply-judgement-move
                                    initial-state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "judgement"}
                                     :piece-id :rose-target-minion
                                     :card-ids ["judgement"]})]
    (is ok?)
    (is (= ["cups2" "wands2" "coins2" "swords2" "cups3" "judgement"]
           (player-hand-ids state :rose)))
    (is (empty? (:discard-pile state)))
    (is (= :judgement/cards-drawn (get-in events [0 :type])))
    (is (game-schema/valid-game? state))))

(deftest judgement-rejects-draws_over_pip_or_hand_limit
  (let [initial-state (-> (:state (game-state/create-game
                                   player-specs
                                   {:deck-order
                                    (deck-starting-with
                                     ["judgement" "cups2" "wands2"
                                      "coins2" "swords2" "cups3"])}))
                          (game-state/with-board-pieces [rose-target-minion]))
        extra-discard (first (:draw-pile initial-state))
        state (-> initial-state
                  (update :draw-pile remove-card-id (:id extra-discard))
                  (assoc :discard-pile [extra-discard]))
        result (game-state/apply-judgement-move
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "judgement"}
                 :piece-id :rose-target-minion
                 :card-ids ["judgement" (:id extra-discard)]})]
    (is (= :invalid-judgement-card-count
           (get-in result [:error :code])))
    (is (= 1 (get-in result [:error :data :maximum])))
    (is (false? (:ok? result)))
    (is (not (contains? result :state)))))

(deftest fool-reveals_draw_pile_cards_and_can_play_an_implemented_suit_power
  (let [initial-state (-> (:state (game-state/create-game
                                   player-specs
                                   {:deck-order
                                    (deck-with-cards-at
                                     {0 "fool"
                                      (+ (hand-card-count (count player-specs))
                                         board/board-card-count)
                                      "cups2"
                                      (inc (+ (hand-card-count (count player-specs))
                                              board/board-card-count))
                                      "wands2"})}))
                          (game-state/with-board-pieces [rose-cup-minion]))
        {:keys [ok? state events]} (game-state/apply-fool-move
                                    initial-state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "fool"}
                                     :reveals [{:power :cup
                                                :play-command
                                                {:source {:piece-id :rose-cup-minion}
                                                 :target {:kind :territory
                                                          :board-index 4}
                                                 :orientation :east}}
                                               {}]
                                     :shuffle-fn identity})
        created-piece (some #(when (and (= :rose (:player-id %))
                                        (= :small (:size %))
                                        (= 4 (:space-index %)))
                               %)
                            (get-in state [:pieces :on-board]))]
    (is ok?)
    (is (= ["fool" "cups2" "wands2"]
           (mapv :id (:discard-pile state))))
    (is (= [:fool/card-revealed
            :cup/small-piece-created
            :fool/card-revealed]
           (mapv :type events)))
    (is (= [true false]
           (mapv :played? (filter #(= :fool/card-revealed (:type %)) events))))
    (is (= :east (:orientation created-piece)))
    (is (= 3 (get-in state [:players-by-id :rose :stash :small])))
    (is (= (mapv :id (drop 2 (:draw-pile initial-state)))
           (mapv :id (:draw-pile state))))
    (is (not (some #{"fool"} (player-hand-ids state :rose))))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest hierophant-replaces_target_piece_with_current_players_same_size_piece
  (let [state (-> (:state (game-state/create-game
                           player-specs
                           {:deck-order (deck-starting-with ["hierophant"])}))
                  (game-state/with-board-pieces [rose-target-minion
                                                 indigo-cup-target]))
        {:keys [ok? state events]} (game-state/apply-hierophant-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "hierophant"
                                              :piece-id :rose-target-minion}
                                     :target {:kind :piece
                                              :piece-id :indigo-cup-target}
                                     :orientation :south})
        replacement (piece-by-id state :rose-medium-1)]
    (is ok?)
    (is (= {:id :rose-medium-1
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :south}
           replacement))
    (is (nil? (piece-by-id state :indigo-cup-target)))
    (is (= 3 (get-in state [:players-by-id :rose :stash :medium])))
    (is (= 5 (get-in state [:players-by-id :indigo :stash :medium])))
    (is (= ["hierophant"] (mapv :id (:discard-pile state))))
    (is (= :hierophant/piece-replaced (get-in events [0 :type])))
    (is (game-schema/valid-game? state))))

(deftest hierophant-rejects_missing_same_size_stash_piece
  (let [rose-mediums [{:id :rose-medium-a
                       :player-id :rose
                       :space-index 0
                       :size :medium
                       :orientation :east}
                      {:id :rose-medium-b
                       :player-id :rose
                       :space-index 1
                       :size :medium
                       :orientation :east}
                      {:id :rose-medium-c
                       :player-id :rose
                       :space-index 2
                       :size :medium
                       :orientation :east}
                      {:id :rose-medium-d
                       :player-id :rose
                       :space-index 3
                       :size :medium
                       :orientation :east}
                      {:id :rose-medium-e
                       :player-id :rose
                       :space-index 4
                       :size :medium
                       :orientation :east}]
        target {:id :indigo-medium-target
                :player-id :indigo
                :space-index 5
                :size :medium
                :orientation :north}
        state (-> (:state (game-state/create-game
                           player-specs
                           {:deck-order (deck-starting-with ["hierophant"])}))
                  (game-state/with-board-pieces (conj rose-mediums target)))
        result (game-state/apply-hierophant-move
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "hierophant"
                          :piece-id :rose-medium-e}
                 :target {:kind :piece
                          :piece-id :indigo-medium-target}
                 :orientation :south})]
    (is (= :no-same-size-piece-available
           (get-in result [:error :code])))
    (is (false? (:ok? result)))
    (is (not (contains? result :state)))))

(deftest hermit-moves_target_piece_to_empty_board_space
  (let [state (-> (:state (game-state/create-game
                           player-specs
                           {:deck-order (deck-starting-with ["hermit"])}))
                  (game-state/with-board-pieces [rose-target-minion
                                                 indigo-cup-target]))
        {:keys [ok? state events]} (game-state/apply-hermit-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "hermit"
                                              :piece-id :rose-target-minion}
                                     :target {:kind :piece
                                              :piece-id :indigo-cup-target}
                                     :destination {:kind :territory
                                                   :board-index 0}})
        moved-piece (piece-by-id state :indigo-cup-target)]
    (is ok?)
    (is (= :west (:orientation moved-piece)))
    (is (= 0 (:space-index moved-piece)))
    (is (= ["hermit"] (mapv :id (:discard-pile state))))
    (is (= :hermit/piece-moved (get-in events [0 :type])))
    (is (game-schema/valid-game? state))))

(deftest hermit-moves_target_territory_to_eligible_wasteland
  (let [initial-state (:state (game-state/create-game
                               player-specs
                               {:deck-order (deck-starting-with ["hermit"])}))
        target-card (get-in initial-state [:board 4 :card])
        passenger {:id :rose-passenger
                   :player-id :rose
                   :space-index 4
                   :size :small
                   :orientation :north}
        state (-> initial-state
                  (game-state/with-board-pieces [(assoc rose-target-minion
                                                        :space-index 3
                                                        :orientation :east)
                                                 passenger]))
        {:keys [ok? state events]} (game-state/apply-hermit-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "hermit"
                                              :piece-id :rose-target-minion}
                                     :target {:kind :territory
                                              :board-index 4}
                                     :destination {:kind :wasteland
                                                   :row 1
                                                   :col 3}})
        moved-cell (board-cell-by-index state 4)
        passenger-after (piece-by-id state :rose-passenger)]
    (is ok?)
    (is (= {:index 4
            :row 1
            :col 3
            :orientation :portrait
            :face :up
            :card target-card}
           moved-cell))
    (is (= {:kind :wasteland
            :row 1
            :col 1}
           (:space passenger-after)))
    (is (= :hermit/territory-moved (get-in events [0 :type])))
    (is (game-schema/valid-game? state))))

(deftest hermit-move-returns-pieces-left-in-void-by-territory-relocation
  (let [initial-state (:state (game-state/create-game
                               player-specs
                               {:deck-order (deck-starting-with ["hermit"])}))
        state (with-board-cells-at initial-state
                [[0 {:row 0 :col 0}]
                 [8 {:row 0 :col 3}]])
        target-card (get-in state [:board 0 :card])
        hermit-minion {:id :rose-hermit-minion
                       :player-id :rose
                       :space-index 0
                       :size :medium
                       :orientation :up}
        passenger {:id :rose-hermit-passenger
                   :player-id :rose
                   :space-index 0
                   :size :small
                   :orientation :north}
        state (game-state/with-board-pieces state [hermit-minion
                                                   passenger])
        {:keys [ok? state events]} (game-state/apply-hermit-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "hermit"
                                              :piece-id :rose-hermit-minion}
                                     :target {:kind :territory
                                              :board-index 0}
                                     :destination {:kind :wasteland
                                                   :row 0
                                                   :col 2}})
        moved-cell (board-cell-by-index state 0)]
    (is ok?)
    (is (= {:index 0
            :row 0
            :col 2
            :orientation :portrait
            :face :up
            :card target-card}
           moved-cell))
    (is (nil? (board-cell-at state 0 0)))
    (is (nil? (piece-by-id state :rose-hermit-minion)))
    (is (nil? (piece-by-id state :rose-hermit-passenger)))
    (is (= 5 (get-in state [:pieces :stashes :rose :small])))
    (is (= 5 (get-in state [:pieces :stashes :rose :medium])))
    (is (= ["hermit"] (mapv :id (:discard-pile state))))
    (is (= :hermit/territory-moved (get-in events [0 :type])))
    (is (game-schema/valid-game? state))))

(deftest devil-can-retarget_after_orienting_the_acting_minion
  (let [devil-minion {:id :rose-devil-minion
                      :player-id :rose
                      :space-index 4
                      :size :medium
                      :orientation :up}
        enemy-target {:id :indigo-devil-target
                      :player-id :indigo
                      :space-index 5
                      :size :small
                      :orientation :north}
        state (-> (:state (game-state/create-game
                           player-specs
                           {:deck-order (deck-with-board-card 4 "devil")}))
                  (game-state/with-board-pieces [devil-minion enemy-target]))
        {:keys [ok? state events]} (game-state/apply-devil-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :territory
                                              :board-index 4}
                                     :actions [{:power :orient-target
                                                :piece-id :rose-devil-minion
                                                :target {:kind :piece
                                                         :piece-id :rose-devil-minion}
                                                :orientation :east}
                                               {:power :orient-target
                                                :piece-id :rose-devil-minion
                                                :target {:kind :piece
                                                         :piece-id :indigo-devil-target}
                                                :orientation :south}]})
        minion-after (piece-by-id state :rose-devil-minion)
        target-after (piece-by-id state :indigo-devil-target)]
    (is ok?)
    (is (= :east (:orientation minion-after)))
    (is (= :south (:orientation target-after)))
    (is (= [:devil/piece-oriented :devil/piece-oriented]
           (mapv :type events)))
    (is (empty? (:discard-pile state)))
    (is (game-schema/valid-game? state))))

(deftest orient-move-updates-current-players-piece
  (let [state (state-with-pieces [rose-target-minion])
        {:keys [ok? state events]} (game-state/apply-orient-move
                                    state
                                    {:player-id :rose
                                     :piece-id :rose-target-minion
                                     :orientation :west})
        oriented-piece (piece-by-id state :rose-target-minion)]
    (is ok?)
    (is (= {:id :rose-target-minion
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :west}
           oriented-piece))
    (is (= [{:type :piece/oriented
             :player-id :rose
             :piece-id :rose-target-minion
             :from-orientation :up
             :to-orientation :west
             :piece oriented-piece}]
           events))
    (is (= events [(peek (:history state))]))
    (is (game-schema/valid-game? state))))

(deftest orient-move-rejects-enemy-pieces-without-mutation
  (let [enemy-piece {:id :indigo-minion
                     :player-id :indigo
                     :space-index 4
                     :size :small
                     :orientation :north}
        state (state-with-pieces [enemy-piece])
        result (game-state/apply-orient-move
                state
                {:player-id :rose
                 :piece-id :indigo-minion
                 :orientation :west})]
    (is (= :invalid-piece
           (get-in result [:error :code])))
    (is (false? (:ok? result)))
    (is (not (contains? result :state)))
    (is (= [enemy-piece]
           (get-in state [:pieces :on-board])))))

(deftest initial-placement-can-place-small_piece_on_empty_territory_or_wasteland
  (let [territory-state (deterministic-game)
        territory-result (game-state/apply-initial-placement
                          territory-state
                          {:player-id :rose
                           :target {:kind :territory
                                    :board-index 0}
                           :orientation :east})
        territory-piece (piece-by-id (:state territory-result) :rose-small-1)
        wasteland-state (deterministic-game)
        wasteland-result (game-state/apply-initial-placement
                          wasteland-state
                          {:player-id :rose
                           :target {:kind :wasteland
                                    :row 0
                                    :col 3}
                           :orientation :north})
        wasteland-piece (piece-by-id (:state wasteland-result) :rose-small-1)]
    (is (:ok? territory-result))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 0
            :size :small
            :orientation :east}
           territory-piece))
    (is (= 4 (get-in territory-result [:state :players-by-id :rose :stash :small])))
    (is (= 4 (get-in territory-result [:state :pieces :stashes :rose :small])))
    (is (= :initial-placement/small-piece-placed
           (get-in territory-result [:events 0 :type])))
    (is (game-schema/valid-game? (:state territory-result)))
    (is (:ok? wasteland-result))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space {:kind :wasteland
                    :row 0
                    :col 3}
            :size :small
            :orientation :north}
           wasteland-piece))
    (is (= 4 (get-in wasteland-result [:state :players-by-id :rose :stash :small])))
    (is (game-schema/valid-game? (:state wasteland-result)))))

(deftest initial-placement-rejects-occupied_targets_and_players_with_pieces
  (let [occupied-piece {:id :indigo-blocker
                        :player-id :indigo
                        :space-index 0
                        :size :small
                        :orientation :up}
        occupied-state (state-with-pieces [occupied-piece])
        occupied-result (game-state/apply-initial-placement
                         occupied-state
                         {:player-id :rose
                          :target {:kind :territory
                                   :board-index 0}
                          :orientation :east})
        owned-state (state-with-pieces [rose-target-minion])
        owned-result (game-state/apply-initial-placement
                      owned-state
                      {:player-id :rose
                       :target {:kind :territory
                                :board-index 0}
                       :orientation :east})]
    (is (= :target-space-occupied
           (get-in occupied-result [:error :code])))
    (is (= [:indigo-blocker]
           (get-in occupied-result [:error :data :piece-ids])))
    (is (= :initial-placement-has-pieces
           (get-in owned-result [:error :code])))
    (is (not (contains? occupied-result :state)))
    (is (not (contains? owned-result :state)))))

(deftest cup-move-adds-current-players-small-piece-to-target-territory
  (let [state (state-with-pieces [rose-cup-minion])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-cup-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :orientation :east}
        {:keys [ok? state events]} (game-state/apply-cup-move state command)
        created-piece (piece-by-id state :rose-small-1)]
    (is ok?)
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 4
            :size :small
            :orientation :east}
           created-piece))
    (is (= 3 (get-in state [:players-by-id :rose :stash :small])))
    (is (= 3 (get-in state [:pieces :stashes :rose :small])))
	    (is (= [{:type :cup/small-piece-created
	             :player-id :rose
	             :cup-variant :cup
	             :source {:kind :territory
	                      :board-index 3
	                      :piece-id :rose-cup-minion}
             :target {:kind :territory
                      :board-index 4}
             :piece created-piece}]
           events))
    (is (= events [(peek (:history state))]))
    (is (game-schema/valid-game? state))))

(deftest cup-move-can-create-an-enemy-small-piece-by-targeting-an-enemy-piece
  (let [state (state-with-pieces [rose-cup-minion indigo-cup-target])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-cup-minion}
                 :target {:kind :piece
                          :piece-id :indigo-cup-target}}
        {:keys [ok? state events]} (game-state/apply-cup-move state command)
        created-piece (piece-by-id state :indigo-small-1)]
    (is ok?)
    (is (= {:id :indigo-small-1
            :player-id :indigo
            :space-index 4
            :size :small
            :orientation :west}
           created-piece))
    (is (= 4 (get-in state [:players-by-id :rose :stash :small])))
    (is (= 4 (get-in state [:players-by-id :indigo :stash :small])))
    (is (= 4 (get-in state [:pieces :stashes :indigo :small])))
	    (is (= [{:type :cup/enemy-small-piece-created
	             :player-id :rose
	             :cup-variant :cup
	             :source {:kind :territory
	                      :board-index 3
	                      :piece-id :rose-cup-minion}
             :target {:kind :piece
                      :piece-id :indigo-cup-target
                      :board-index 4}
             :target-piece indigo-cup-target
             :piece created-piece}]
           events))
    (is (= events [(peek (:history state))]))
    (is (game-schema/valid-game? state))))

(deftest cup-move-can-use-cup-card-from-hand-as-source
  (let [deck-order (deck-starting-with ["cups2" "coins2"])
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-cup-minion])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "cups2"
                          :piece-id :rose-cup-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :orientation :west}
        {:keys [ok? state]} (game-state/apply-cup-move state command)]
    (is ok?)
    (is (= ["cups2"] (mapv :id (:discard-pile state))))
    (is (not (some #{"cups2"} (player-hand-ids state :rose))))
    (is (= :rose-small-1 (:id (last (get-in state [:pieces :on-board])))))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest cup-move-creates-territory-from-one-point-card-in-targeted-wasteland
  (let [state (state-with-pieces [rose-cup-wasteland-minion])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-cup-minion}
                 :target {:kind :wasteland
                          :row 1
                          :col -1}
                 :one-point-card-id "coins2"}
        {:keys [ok? state events]} (game-state/apply-cup-move state command)
        created-cell (board-cell-by-index state 9)]
    (is ok?)
    (is (= {:index 9
            :row 1
            :col -1
            :orientation :portrait
            :face :up
            :card (cards/card-by-id "coins2")}
           created-cell))
    (is (not (some #{"coins2"} (player-hand-ids state :rose))))
	    (is (= [{:type :cup/territory-created
	             :player-id :rose
	             :cup-variant :cup
	             :source {:kind :territory
	                      :board-index 3
	                      :piece-id :rose-cup-minion}
             :target {:kind :wasteland
                      :row 1
	                      :col -1}
	             :board-index 9
	             :card-id "coins2"
	             :territory-card-source :hand}]
	           events))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest cup-move-rejects-untargeted-wasteland-territory-creation
  (let [state (state-with-pieces [rose-cup-wasteland-minion])
        result (game-state/apply-cup-move
                state
                {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-cup-minion}
                 :target {:kind :wasteland
                          :row 0
                          :col 3}
                 :one-point-card-id "coins2"})]
    (is (= :cup-target-out-of-range
           (get-in result [:error :code])))
    (is (= {:row 1 :col -1}
           (get-in result [:error :data :expected-coordinate])))
    (is (= {:row 0 :col 3}
           (get-in result [:error :data :target-coordinate])))))

(deftest cup-move-rejects-out-of-range-territory-and-enemy-piece-targets
  (let [north-minion (assoc rose-cup-minion :orientation :north)
        state (state-with-pieces [north-minion indigo-cup-target])
        territory-result (game-state/apply-cup-move
                          state
                          {:player-id :rose
                           :source {:kind :territory
                                    :board-index 3
                                    :piece-id :rose-cup-minion}
                           :target {:kind :territory
                                    :board-index 4}
                           :orientation :east})
        enemy-result (game-state/apply-cup-move
                      state
                      {:player-id :rose
                       :source {:kind :territory
                                :board-index 3
                                :piece-id :rose-cup-minion}
                       :target {:kind :piece
                                :piece-id :indigo-cup-target}})]
    (is (= :cup-target-out-of-range
           (get-in territory-result [:error :code])))
    (is (= :cup-target-out-of-range
           (get-in enemy-result [:error :code])))
    (is (= {:row 0 :col 0}
           (get-in territory-result [:error :data :expected-coordinate])))
    (is (= {:row 1 :col 1}
           (get-in enemy-result [:error :data :target-coordinate])))
    (is (not (contains? territory-result :state)))
    (is (not (contains? enemy-result :state)))))

(deftest cup-move-rejects-invalid-command-shapes-and_sources
  (let [non-cup-source (assoc rose-cup-minion
                              :id :rose-non-cup-minion
                              :space-index 0)
        state (state-with-pieces [non-cup-source])
        missing-orientation {:player-id :rose
                             :source {:kind :territory
                                      :board-index 0
                                      :piece-id :rose-non-cup-minion}
                             :target {:kind :territory
                                      :board-index 4}}
        non-cup-result (game-state/apply-cup-move state
                                                  (assoc-in missing-orientation
                                                            [:target :board-index]
                                                            4))]
    (is (= :source-card-not-cup
           (get-in non-cup-result [:error :code])))
    (let [state (state-with-pieces [rose-cup-minion])
          result (game-state/apply-cup-move state
                                            {:player-id :rose
                                             :source {:kind :territory
                                                      :board-index 3
                                                      :piece-id :rose-cup-minion}
                                             :target {:kind :territory
                                                      :board-index 4}})]
      (is (= :invalid-orientation
             (get-in result [:error :code]))))))

(deftest cup-move-rejects-full-target-territories
  (let [state (state-with-pieces [rose-cup-minion
                                  rose-target-minion
                                  {:id :indigo-target-minion
                                   :player-id :indigo
                                   :space-index 4
                                   :size :small
                                   :orientation :north}
                                  {:id :indigo-target-guard
                                   :player-id :indigo
                                   :space-index 4
                                   :size :large
                                   :orientation :south}])
        result (game-state/apply-cup-move state
                                          {:player-id :rose
                                           :source {:kind :territory
                                                    :board-index 3
                                                    :piece-id :rose-cup-minion}
                                           :target {:kind :territory
                                                    :board-index 4}
                                           :orientation :up})]
    (is (= :target-territory-full
           (get-in result [:error :code])))))

(deftest cup-unbounded-variant-ignores-full-target-territory-limit
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 3 "empress")}))
        state (game-state/with-board-pieces
               state
               [rose-cup-minion
                rose-target-minion
                indigo-cup-target
                {:id :rose-target-small
                 :player-id :rose
                 :space-index 4
                 :size :small
                 :orientation :east}])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-cup-minion}
                 :cup-variant :cup-unbounded
                 :target {:kind :territory
                          :board-index 4}
                 :orientation :up}
        {:keys [ok? state events]} (game-state/apply-cup-move state command)
        target-piece-ids (->> (get-in state [:pieces :on-board])
                              (filter #(= 4 (:space-index %)))
                              (mapv :id))]
    (is ok?)
    (is (= [:rose-target-minion
            :indigo-cup-target
            :rose-target-small
            :rose-small-1]
           target-piece-ids))
    (is (= :cup-unbounded (:cup-variant (first events))))
    (is (game-schema/valid-game? state))))

(deftest cup-move-rejects-unavailable-variants
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 3 "empress")}))
        state (game-state/with-board-pieces state [rose-cup-minion])
        result (game-state/apply-cup-move
                state
                {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-cup-minion}
                 :cup-variant :cup
                 :target {:kind :territory
                          :board-index 4}
                 :orientation :up})]
    (is (= :cup-variant-unavailable
           (get-in result [:error :code])))
    (is (= [:cup-unbounded]
           (get-in result [:error :data :available-variants])))))

(deftest cup-move-rejects-invalid-wasteland-territory-cards
  (let [state (state-with-pieces [rose-cup-wasteland-minion])
        result (game-state/apply-cup-move state
                                          {:player-id :rose
                                           :source {:kind :territory
                                                    :board-index 3
                                                    :piece-id :rose-cup-minion}
                                           :target {:kind :wasteland
                                                    :row 1
                                                    :col -1}
                                           :one-point-card-id "chariot"})]
    (is (= :invalid-one-point-card
           (get-in result [:error :code])))))

(deftest wheel-cup-can-create-wasteland-territory-from-draw-pile
  (let [initial-state (:state (game-state/create-game
                               player-specs
                               {:deck-order (deck-with-board-card 3 "wheeloffortune")}))
        draw-pile-card (first (:draw-pile initial-state))
        state (game-state/with-board-pieces initial-state [rose-cup-wasteland-minion])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-cup-minion}
                 :cup-variant :wheel-cup
                 :target {:kind :wasteland
                          :row 1
                          :col -1}
                 :territory-card-source :draw-pile-top}
        {:keys [ok? state events]} (game-state/apply-cup-move state command)
        created-cell (board-cell-by-index state 9)]
    (is ok?)
    (is (= draw-pile-card (:card created-cell)))
    (is (= (mapv :id (rest (:draw-pile initial-state)))
           (mapv :id (:draw-pile state))))
    (is (= [{:type :cup/territory-created
             :player-id :rose
             :cup-variant :wheel-cup
             :source {:kind :territory
                      :board-index 3
                      :piece-id :rose-cup-minion}
             :target {:kind :wasteland
                      :row 1
                      :col -1}
             :board-index 9
             :card-id (:id draw-pile-card)
             :territory-card-source :draw-pile-top}]
           events))
    (is (game-schema/valid-game? state))))

(deftest wheel-cup-reshuffles-discard-pile-for-draw-pile-territory
  (let [draw-start (+ (hand-card-count (count player-specs)) board/board-card-count)
        initial-state (:state (game-state/create-game
                               player-specs
                               {:deck-order (deck-with-cards-at
                                             {0 "wheeloffortune"
                                              draw-start "world"})}))
        wheel-card (cards/card-by-id "wheeloffortune")
        prepared-discard (:draw-pile initial-state)
        state (-> initial-state
                  (game-state/with-board-pieces [rose-cup-wasteland-minion])
                  (assoc :draw-pile []
                         :discard-pile prepared-discard))
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "wheeloffortune"
                          :piece-id :rose-cup-minion}
                 :cup-variant :wheel-cup
                 :target {:kind :wasteland
                          :row 1
                          :col -1}
                 :territory-card-source :draw-pile-top
                 :shuffle-fn identity}
        {:keys [ok? state events]} (game-state/apply-cup-move state command)
        created-cell (board-cell-by-index state 9)
        expected-refreshed-draw-pile (conj prepared-discard wheel-card)]
    (is ok?)
    (is (= "world" (get-in created-cell [:card :id])))
    (is (= (mapv :id (rest expected-refreshed-draw-pile))
           (mapv :id (:draw-pile state))))
    (is (empty? (:discard-pile state)))
    (is (not (some #{"wheeloffortune"} (player-hand-ids state :rose))))
    (is (= [{:type :cup/territory-created
             :player-id :rose
             :cup-variant :wheel-cup
             :source {:kind :hand-card
                      :card-id "wheeloffortune"
                      :piece-id :rose-cup-minion}
             :target {:kind :wasteland
                      :row 1
                      :col -1}
             :board-index 9
             :card-id "world"
             :territory-card-source :draw-pile-top
             :reshuffled-discard? true}]
           events))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest non-wheel-cups-reject-draw-pile-territory-source
  (let [state (state-with-pieces [rose-cup-wasteland-minion])
        result (game-state/apply-cup-move
                state
                {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-cup-minion}
                 :cup-variant :cup
                 :target {:kind :wasteland
                          :row 1
                          :col -1}
                 :territory-card-source :draw-pile-top})]
    (is (= :cup-variant-option-unavailable
           (get-in result [:error :code])))))

(deftest cup-move-rejects-wastelands-occupied-by-enemy-pieces
  (let [state (state-with-pieces [rose-cup-wasteland-minion
                                  {:id :indigo-wasteland-minion
                                   :player-id :indigo
                                   :space {:kind :wasteland
                                           :row 1
                                           :col -1}
                                   :size :small
                                   :orientation :up}])
        result (game-state/apply-cup-move state
                                          {:player-id :rose
                                           :source {:kind :territory
                                                   :board-index 3
                                                   :piece-id :rose-cup-minion}
                                           :target {:kind :wasteland
                                                    :row 1
                                                    :col -1}
                                           :one-point-card-id "coins2"})]
    (is (= :wasteland-occupied-by-enemy
           (get-in result [:error :code])))
    (is (= [:indigo-wasteland-minion]
           (get-in result [:error :data :enemy-piece-ids])))))

(deftest cup-move-rejects-invalid-enemy-piece-targets
  (let [own-target-state (state-with-pieces [rose-cup-minion rose-target-minion])
        own-target-result (game-state/apply-cup-move
                           own-target-state
                           {:player-id :rose
                            :source {:kind :territory
                                     :board-index 3
                                     :piece-id :rose-cup-minion}
                            :target {:kind :piece
                                     :piece-id :rose-target-minion}})
        no-small-state (state-with-pieces
                        [rose-cup-minion
                         indigo-cup-target
                         {:id :indigo-small-a
                          :player-id :indigo
                          :space-index 0
                          :size :small
                          :orientation :north}
                         {:id :indigo-small-b
                          :player-id :indigo
                          :space-index 1
                          :size :small
                          :orientation :east}
                         {:id :indigo-small-c
                          :player-id :indigo
                          :space-index 2
                          :size :small
                          :orientation :south}
                         {:id :indigo-small-d
                          :player-id :indigo
                          :space-index 5
                          :size :small
                          :orientation :west}
                         {:id :indigo-small-e
                          :player-id :indigo
                          :space-index 6
                          :size :small
                          :orientation :up}])
        no-small-result (game-state/apply-cup-move
                         no-small-state
                         {:player-id :rose
                          :source {:kind :territory
                                   :board-index 3
                                   :piece-id :rose-cup-minion}
                          :target {:kind :piece
                                   :piece-id :indigo-cup-target}})]
    (is (= :target-piece-not-enemy
           (get-in own-target-result [:error :code])))
    (is (= :no-small-piece-available
           (get-in no-small-result [:error :code])))
    (is (= :indigo
           (get-in no-small-result [:error :data :player-id])))
    (is (not (contains? own-target-result :state)))
    (is (not (contains? no-small-result :state)))))

(deftest disc-command-normalizes-territory-source-and-piece-target
  (let [target-piece {:id :indigo-disc-target
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
        state (-> (state-with-pieces [rose-disc-minion target-piece])
                  (state-with-board-card 3 "coins2"))
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-disc-minion}
                 :target {:kind :piece
                          :piece-id :indigo-disc-target}}
        result (game-state/resolve-disc-command state command)]
    (is (:ok? result))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 3
                     :piece-id :rose-disc-minion}
            :disc-variant :disc
            :target {:kind :piece
                     :piece-id :indigo-disc-target
                     :player-id :indigo
                     :board-index 4
                     :row 1
                     :col 1}}
           (:command result)))
    (is (= "coins2" (get-in result [:source-card :id])))
    (is (= rose-disc-minion (:piece result)))
    (is (= target-piece (:target-piece result)))))

(deftest disc-command-normalizes-hand-card-source-and-territory-target_options
  (let [deck-order (deck-starting-with ["coins2" "cupsking"])
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [(assoc rose-disc-minion
                                                          :orientation :up)])
        result (game-state/resolve-disc-command
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "coins2"
                          :piece-id :rose-disc-minion}
                 :target {:kind :territory
                          :board-index 3}
                 :replacement-card-id "cupsking"})]
    (is (:ok? result))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "coins2"
                     :piece-id :rose-disc-minion}
            :disc-variant :disc
            :target {:kind :territory
                     :board-index 3
                     :row 1
                     :col 0}
            :replacement-card-source :hand
            :replacement-card-id "cupsking"}
           (:command result)))
    (is (= "coins2" (get-in result [:source-card :id])))
    (is (= 3 (get-in result [:target-cell :index])))))

(deftest disc-command-allows-upright_current_space_and_minion_self_targeting
  (let [upright-state (-> (state-with-pieces [(assoc rose-disc-minion
                                                     :orientation :up)])
                          (state-with-board-card 3 "coins2"))
        territory-result (game-state/resolve-disc-command
                          upright-state
                          {:player-id :rose
                           :source {:kind :territory
                                    :board-index 3
                                    :piece-id :rose-disc-minion}
                           :target {:kind :territory
                                    :board-index 3}})
        self-state (-> (state-with-pieces [rose-disc-minion])
                       (state-with-board-card 3 "coins2"))
        self-result (game-state/resolve-disc-command
                     self-state
                     {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-disc-minion}
                      :target {:kind :piece
                               :piece-id :rose-disc-minion}
                      :orientation :south})]
    (is (:ok? territory-result))
    (is (= {:kind :territory
            :board-index 3
            :row 1
            :col 0}
           (get-in territory-result [:command :target])))
    (is (:ok? self-result))
    (is (= {:kind :piece
            :piece-id :rose-disc-minion
            :player-id :rose
            :board-index 3
            :row 1
            :col 0
            :orientation :south}
           (get-in self-result [:command :target])))
    (is (= :south (get-in self-result [:command :orientation])))))

(deftest disc-command-carries-source-variants
  (let [base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-disc-minion}
                      :target {:kind :piece
                               :piece-id :rose-disc-minion}}
        strength-result (game-state/resolve-disc-command
                         (-> (state-with-pieces [rose-disc-minion])
                             (state-with-board-card 3 "strength"))
                         base-command)
        star-result (game-state/resolve-disc-command
                     (-> (state-with-pieces [rose-disc-minion])
                         (state-with-board-card 3 "star"))
                     base-command)
        sun-result (game-state/resolve-disc-command
                    (-> (state-with-pieces [rose-disc-minion])
                        (state-with-board-card 3 "sun"))
                    base-command)
        magician-result (game-state/resolve-disc-command
                         (-> (state-with-pieces [rose-disc-minion])
                             (state-with-board-card 3 "magician"))
                         base-command)]
    (is (:ok? strength-result))
    (is (:ok? star-result))
    (is (:ok? sun-result))
    (is (:ok? magician-result))
    (is (= :disc (get-in strength-result [:command :disc-variant])))
    (is (= :disc-from-discard (get-in star-result [:command :disc-variant])))
    (is (= :disc (get-in sun-result [:command :disc-variant])))
    (is (= :wild-suits (get-in magician-result [:command :disc-variant])))))

(deftest disc-command-rejects-unavailable_and_invalid_variants
  (let [state (-> (state-with-pieces [rose-disc-minion])
                  (state-with-board-card 3 "coins2"))
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-disc-minion}
                      :target {:kind :piece
                               :piece-id :rose-disc-minion}}
        unavailable-result (game-state/resolve-disc-command
                            state
                            (assoc base-command :disc-variant :disc-from-discard))
        invalid-result (game-state/resolve-disc-command
                        state
                        (assoc base-command :disc-variant :rod))
        discard-source-result (game-state/resolve-disc-command
                               state
                               (assoc base-command
                                      :target {:kind :territory
                                               :board-index 4}
                                      :replacement-card-source :discard-pile
                                      :replacement-card-id "cupsking"))
        non-disc-result (game-state/resolve-disc-command
                         (-> (state-with-pieces [rose-disc-minion])
                             (state-with-board-card 3 "wands2"))
                         base-command)]
    (is (= :disc-variant-unavailable
           (get-in unavailable-result [:error :code])))
    (is (= :invalid-disc-variant
           (get-in invalid-result [:error :code])))
    (is (= :disc-variant-option-unavailable
           (get-in discard-source-result [:error :code])))
    (is (= :source-card-not-disc
           (get-in non-disc-result [:error :code])))))

(deftest disc-command-rejects_invalid_targets_and_options_without_mutation
  (let [off-axis-target {:id :indigo-off-axis-disc-target
                         :player-id :indigo
                         :space-index 0
                         :size :small
                         :orientation :north}
        enemy-target {:id :indigo-disc-target
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
        state (-> (state-with-pieces [rose-disc-minion off-axis-target enemy-target])
                  (state-with-board-card 3 "coins2"))
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-disc-minion}}
        off-axis-result (game-state/resolve-disc-command
                         state
                         (assoc base-command
                                :target {:kind :piece
                                         :piece-id :indigo-off-axis-disc-target}))
        enemy-orientation-result (game-state/resolve-disc-command
                                  state
                                  (assoc base-command
                                         :target {:kind :piece
                                                  :piece-id :indigo-disc-target}
                                         :orientation :west))
        enemy-territory-result (game-state/resolve-disc-command
                                state
                                (assoc base-command
                                       :target {:kind :territory
                                                :board-index 4}))
        piece-replacement-result (game-state/resolve-disc-command
                                  state
                                  (assoc base-command
                                         :target {:kind :piece
                                                  :piece-id :rose-disc-minion}
                                         :replacement-card-id "cupsking"))]
    (is (= :invalid-disc-target
           (get-in off-axis-result [:error :code])))
    (is (= :invalid-orientation
           (get-in enemy-orientation-result [:error :code])))
    (is (= :target-territory-occupied-by-enemy
           (get-in enemy-territory-result [:error :code])))
    (is (= :invalid-disc-replacement
           (get-in piece-replacement-result [:error :code])))
    (is (false? (:ok? off-axis-result)))
    (is (not (contains? off-axis-result :state)))
    (is (= [rose-disc-minion off-axis-target enemy-target]
           (get-in state [:pieces :on-board])))))

(deftest star-disc-command-allows_discard_pile_replacement_source
  (let [state (-> (state-with-pieces [rose-disc-minion])
                  (state-with-board-card 3 "star"))
        result (game-state/resolve-disc-command
                state
                {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-disc-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :replacement-card-source :discard-pile
                 :replacement-card-id "cupsking"})]
    (is (:ok? result))
    (is (= :disc-from-discard
           (get-in result [:command :disc-variant])))
    (is (= :discard-pile
           (get-in result [:command :replacement-card-source])))))

(deftest sword-command-normalizes-territory-source-and-piece-target
  (let [target-piece {:id :indigo-sword-target
                      :player-id :indigo
                      :space-index 4
                      :size :medium
                      :orientation :north}
        state (-> (state-with-pieces [rose-sword-minion target-piece])
                  (state-with-board-card 3 "swords2"))
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-sword-minion}
                 :target {:kind :piece
                          :piece-id :indigo-sword-target}
                 :damage 1}
        result (game-state/resolve-sword-command state command)]
    (is (:ok? result))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 3
                     :piece-id :rose-sword-minion}
            :sword-variant :sword
            :target {:kind :piece
                     :piece-id :indigo-sword-target
                     :player-id :indigo
                     :board-index 4
                     :row 1
                     :col 1}
            :damage 1}
           (:command result)))
    (is (= "swords2" (get-in result [:source-card :id])))
    (is (= rose-sword-minion (:piece result)))
    (is (= target-piece (:target-piece result)))
    (is (false? (:destroyed? result)))))

(deftest sword-command-normalizes-hand-card-source-and-territory-target_options
  (let [deck-order (deck-starting-with ["swords2" "cups2"])
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (-> state
                  (game-state/with-board-pieces [rose-sword-minion])
                  (state-with-board-card 4 "cupsking"))
        result (game-state/resolve-sword-command
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "swords2"
                          :piece-id :rose-sword-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :damage 1
                 :replacement-card-id "cups2"})]
    (is (:ok? result))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "swords2"
                     :piece-id :rose-sword-minion}
            :sword-variant :sword
            :target {:kind :territory
                     :board-index 4
                     :row 1
                     :col 1}
            :damage 1
            :replacement-card-source :hand
            :replacement-card-id "cups2"}
           (:command result)))
    (is (= "swords2" (get-in result [:source-card :id])))
    (is (= 4 (get-in result [:target-cell :index])))))

(deftest sword-command-allows-upright_current_space_and_minion_self_targeting
  (let [upright-state (-> (state-with-pieces [(assoc rose-sword-minion
                                                      :orientation :up)])
                          (state-with-board-card 3 "swords2"))
        territory-result (game-state/resolve-sword-command
                          upright-state
                          {:player-id :rose
                           :source {:kind :territory
                                    :board-index 3
                                    :piece-id :rose-sword-minion}
                           :target {:kind :territory
                                    :board-index 3}
                           :damage 1})
        self-state (-> (state-with-pieces [rose-sword-minion])
                       (state-with-board-card 3 "swords2"))
        self-result (game-state/resolve-sword-command
                     self-state
                     {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-sword-minion}
                      :target {:kind :piece
                               :piece-id :rose-sword-minion}
                      :damage 1
                      :orientation :south})]
    (is (:ok? territory-result))
    (is (= {:kind :territory
            :board-index 3
            :row 1
            :col 0}
           (get-in territory-result [:command :target])))
    (is (:ok? self-result))
    (is (= {:kind :piece
            :piece-id :rose-sword-minion
            :player-id :rose
            :board-index 3
            :row 1
            :col 0
            :orientation :south}
           (get-in self-result [:command :target])))
    (is (= :south (get-in self-result [:command :orientation])))))

(deftest sword-command-carries-source-variants
  (let [base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-sword-minion}
                      :target {:kind :piece
                               :piece-id :rose-sword-minion}
                      :damage 1}
        justice-result (game-state/resolve-sword-command
                        (-> (state-with-pieces [rose-sword-minion])
                            (state-with-board-card 3 "justice"))
                        base-command)
        death-result (game-state/resolve-sword-command
                      (-> (state-with-pieces [rose-sword-minion])
                          (state-with-board-card 3 "death"))
                      base-command)
        tower-result (game-state/resolve-sword-command
                      (-> (state-with-pieces [rose-sword-minion])
                          (state-with-board-card 3 "tower"))
                      base-command)
        moon-result (game-state/resolve-sword-command
                     (-> (state-with-pieces [rose-sword-minion])
                         (state-with-board-card 3 "moon"))
                     base-command)
        magician-result (game-state/resolve-sword-command
                         (-> (state-with-pieces [rose-sword-minion])
                             (state-with-board-card 3 "magician"))
                         base-command)]
    (is (:ok? justice-result))
    (is (:ok? death-result))
    (is (:ok? tower-result))
    (is (:ok? moon-result))
    (is (:ok? magician-result))
    (is (= :sword (get-in justice-result [:command :sword-variant])))
    (is (= :sword (get-in death-result [:command :sword-variant])))
    (is (= :sword-from-discard (get-in tower-result [:command :sword-variant])))
    (is (= :sword (get-in moon-result [:command :sword-variant])))
    (is (= :wild-suits (get-in magician-result [:command :sword-variant])))))

(deftest sword-command-rejects-unavailable_and_invalid_variants
  (let [state (-> (state-with-pieces [rose-sword-minion])
                  (state-with-board-card 3 "swords2")
                  (state-with-board-card 4 "cupsking"))
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-sword-minion}
                      :target {:kind :piece
                               :piece-id :rose-sword-minion}
                      :damage 1}
        unavailable-result (game-state/resolve-sword-command
                            state
                            (assoc base-command :sword-variant :sword-from-discard))
        invalid-result (game-state/resolve-sword-command
                        state
                        (assoc base-command :sword-variant :disc))
        discard-source-result (game-state/resolve-sword-command
                               state
                               (assoc base-command
                                      :target {:kind :territory
                                               :board-index 4}
                                      :replacement-card-source :discard-pile
                                      :replacement-card-id "cups2"))
        non-sword-result (game-state/resolve-sword-command
                          (-> (state-with-pieces [rose-sword-minion])
                              (state-with-board-card 3 "coins2"))
                          base-command)]
    (is (= :sword-variant-unavailable
           (get-in unavailable-result [:error :code])))
    (is (= :invalid-sword-variant
           (get-in invalid-result [:error :code])))
    (is (= :sword-variant-option-unavailable
           (get-in discard-source-result [:error :code])))
    (is (= :source-card-not-sword
           (get-in non-sword-result [:error :code])))))

(deftest sword-command-rejects_invalid_targets_damage_and_options_without_mutation
  (let [off-axis-target {:id :indigo-off-axis-sword-target
                         :player-id :indigo
                         :space-index 0
                         :size :small
                         :orientation :north}
        enemy-target {:id :indigo-sword-target
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
        state (-> (state-with-pieces [rose-sword-minion
                                      off-axis-target
                                      enemy-target])
                  (state-with-board-card 3 "swords2")
                  (state-with-board-card 4 "cupsking"))
        over-target-state (-> (state-with-pieces [(assoc rose-sword-minion
                                                          :size :large)])
                              (state-with-board-card 3 "swords2")
                              (state-with-board-card 4 "cupsking"))
        invalid-source-state (-> (state-with-pieces [(assoc rose-sword-minion
                                                            :size :tiny)])
                                 (state-with-board-card 3 "swords2"))
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-sword-minion}}
        invalid-source-result (game-state/resolve-sword-command
                               invalid-source-state
                               (assoc base-command
                                      :target {:kind :piece
                                               :piece-id :rose-sword-minion}
                                      :damage 1))
        off-axis-result (game-state/resolve-sword-command
                         state
                         (assoc base-command
                                :target {:kind :piece
                                         :piece-id :indigo-off-axis-sword-target}
                                :damage 1))
        zero-damage-result (game-state/resolve-sword-command
                            state
                            (assoc base-command
                                   :target {:kind :piece
                                            :piece-id :indigo-sword-target}
                                   :damage 0))
        over-minion-result (game-state/resolve-sword-command
                            state
                            (assoc base-command
                                   :target {:kind :piece
                                            :piece-id :indigo-sword-target}
                                   :damage 3))
        over-target-result (game-state/resolve-sword-command
                            over-target-state
                            (assoc base-command
                                   :target {:kind :territory
                                            :board-index 4}
                                   :damage 3))
        enemy-orientation-result (game-state/resolve-sword-command
                                  state
                                  (assoc base-command
                                         :target {:kind :piece
                                                  :piece-id :indigo-sword-target}
                                         :damage 1
                                         :orientation :west))
        enemy-territory-result (game-state/resolve-sword-command
                                state
                                (assoc base-command
                                       :target {:kind :territory
                                                :board-index 4}
                                       :damage 1
                                       :replacement-card-id "cups2"))
        piece-replacement-result (game-state/resolve-sword-command
                                  state
                                  (assoc base-command
                                         :target {:kind :piece
                                                  :piece-id :rose-sword-minion}
                                         :damage 1
                                         :replacement-card-id "cups2"))
        territory-orientation-result (game-state/resolve-sword-command
                                      state
                                      (assoc base-command
                                             :target {:kind :territory
                                                      :board-index 4}
                                             :damage 1
                                             :orientation :west))]
    (is (= :invalid-piece-size
           (get-in invalid-source-result [:error :code])))
    (is (= :invalid-sword-target
           (get-in off-axis-result [:error :code])))
    (is (= :invalid-sword-damage
           (get-in zero-damage-result [:error :code])))
    (is (= :invalid-sword-damage
           (get-in over-minion-result [:error :code])))
    (is (= :invalid-sword-damage
           (get-in over-target-result [:error :code])))
    (is (= :invalid-orientation
           (get-in enemy-orientation-result [:error :code])))
    (is (= :target-territory-occupied-by-enemy
           (get-in enemy-territory-result [:error :code])))
    (is (= :invalid-sword-replacement
           (get-in piece-replacement-result [:error :code])))
    (is (= :invalid-orientation
           (get-in territory-orientation-result [:error :code])))
    (is (false? (:ok? off-axis-result)))
    (is (not (contains? off-axis-result :state)))
    (is (= [rose-sword-minion off-axis-target enemy-target]
           (get-in state [:pieces :on-board])))))

(deftest tower-sword-command-allows_discard_pile_replacement_source
  (let [state (-> (state-with-pieces [rose-sword-minion])
                  (state-with-board-card 3 "tower")
                  (state-with-board-card 4 "cupsking"))
        result (game-state/resolve-sword-command
                state
                {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-sword-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :damage 1
                 :replacement-card-source :discard-pile
                 :replacement-card-id "cups2"})]
    (is (:ok? result))
    (is (= :sword-from-discard
           (get-in result [:command :sword-variant])))
    (is (= :discard-pile
           (get-in result [:command :replacement-card-source])))))

(deftest plain-sword-major-hand-sources-use-single_attack_contract
  (doseq [card-id ["justice" "death" "moon"]]
    (let [enemy-piece {:id :indigo-sword-target
                       :player-id :indigo
                       :space-index 4
                       :size :large
                       :orientation :north}
          state (:state (game-state/create-game
                         player-specs
                         {:deck-order (deck-starting-with [card-id])}))
          state (game-state/with-board-pieces state [rose-sword-minion enemy-piece])
          command {:player-id :rose
                   :source {:kind :hand-card
                            :card-id card-id
                            :piece-id :rose-sword-minion}
                   :target {:kind :piece
                            :piece-id :indigo-sword-target}
                   :damage 1}
          {:keys [ok? state events]} (game-state/apply-sword-move state command)
          shrunk-piece (piece-by-id state :indigo-medium-1)]
      (is ok? card-id)
      (is (= :sword/piece-shrunk (get-in events [0 :type])) card-id)
      (is (= :sword (get-in events [0 :sword-variant])) card-id)
      (is (= {:kind :hand-card
              :card-id card-id
              :piece-id :rose-sword-minion}
             (get-in events [0 :source]))
          card-id)
      (is (= {:id :indigo-medium-1
              :player-id :indigo
              :space-index 4
              :size :medium
              :orientation :north}
             shrunk-piece)
          card-id)
      (is (= [card-id] (mapv :id (:discard-pile state))) card-id)
      (is (not (some #{card-id} (player-hand-ids state :rose))) card-id)
      (is (= events [(peek (:history state))]) card-id)
      (is (= (count cards/deck) (count (all-card-ids state))) card-id)
      (is (= (count cards/deck) (count (set (all-card-ids state)))) card-id)
      (is (game-schema/valid-game? state) card-id))))

(deftest magician-wild_suit_sword_variant_is_carried_through_attack_application
  (let [enemy-piece {:id :indigo-sword-target
                     :player-id :indigo
                     :space-index 4
                     :size :medium
                     :orientation :north}
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["magician"])}))
        state (game-state/with-board-pieces state [rose-sword-minion enemy-piece])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "magician"
                          :piece-id :rose-sword-minion}
                 :target {:kind :piece
                          :piece-id :indigo-sword-target}
                 :damage 1}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)]
    (is (= [:wild-suits] (cards/sword-variants (cards/card-by-id "magician"))))
    (is ok?)
    (is (= :sword/piece-shrunk (get-in events [0 :type])))
    (is (= :wild-suits (get-in events [0 :sword-variant])))
    (is (= ["magician"] (mapv :id (:discard-pile state))))
    (is (not (some #{"magician"} (player-hand-ids state :rose))))
    (is (= events [(peek (:history state))]))
    (is (game-schema/valid-game? state))))

(deftest tower-sword-discard_pile_replacement_is_only_for_surviving_territories
  (let [state (-> (state-with-pieces [rose-sword-minion])
                  (state-with-board-card 3 "tower")
                  (state-with-board-card 4 "cups2")
                  (move-card-to-discard "cupsking"))
        piece-result (game-state/resolve-sword-command
                      state
                      {:player-id :rose
                       :source {:kind :territory
                                :board-index 3
                                :piece-id :rose-sword-minion}
                       :target {:kind :piece
                                :piece-id :rose-sword-minion}
                       :damage 1
                       :replacement-card-source :discard-pile
                       :replacement-card-id "cupsking"})
        destroyed-territory-result (game-state/resolve-sword-command
                                    state
                                    {:player-id :rose
                                     :source {:kind :territory
                                              :board-index 3
                                              :piece-id :rose-sword-minion}
                                     :target {:kind :territory
                                              :board-index 4}
                                     :damage 1
                                     :replacement-card-source :discard-pile
                                     :replacement-card-id "cupsking"})]
    (is (= :invalid-sword-replacement
           (get-in piece-result [:error :code])))
    (is (= :invalid-sword-replacement
           (get-in destroyed-territory-result [:error :code])))
    (is (not (contains? piece-result :state)))
    (is (not (contains? destroyed-territory-result :state)))))

(deftest sword-move-shrinks-current-player-piece-and-may-reorient-it
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 3 "swords2")}))
        state (game-state/with-board-pieces state [rose-sword-minion])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-sword-minion}
                 :target {:kind :piece
                          :piece-id :rose-sword-minion}
                 :damage 1
                 :orientation :south}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)
        shrunk-piece (piece-by-id state :rose-small-1)]
    (is ok?)
    (is (nil? (piece-by-id state :rose-sword-minion)))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 3
            :size :small
            :orientation :south}
           shrunk-piece))
    (is (= 4 (get-in state [:players-by-id :rose :stash :small])))
    (is (= 5 (get-in state [:players-by-id :rose :stash :medium])))
    (is (= 4 (get-in state [:pieces :stashes :rose :small])))
    (is (= 5 (get-in state [:pieces :stashes :rose :medium])))
    (is (= [{:type :sword/piece-shrunk
             :player-id :rose
             :source {:kind :territory
                      :board-index 3
                      :piece-id :rose-sword-minion}
             :sword-variant :sword
             :target {:kind :piece
                      :piece-id :rose-sword-minion
                      :player-id :rose
                      :board-index 3
                      :row 1
                      :col 0}
             :damage 1
             :from-size :medium
             :to-size :small
             :replaced-piece rose-sword-minion
             :piece shrunk-piece}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest sword-move-shrinks-enemy-piece-and-discards-hand-source
  (let [enemy-piece {:id :indigo-sword-target
                     :player-id :indigo
                     :space-index 4
                     :size :large
                     :orientation :north}
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["swords2"])}))
        state (game-state/with-board-pieces state [rose-sword-minion enemy-piece])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "swords2"
                          :piece-id :rose-sword-minion}
                 :target {:kind :piece
                          :piece-id :indigo-sword-target}
                 :damage 1}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)
        shrunk-piece (piece-by-id state :indigo-medium-1)]
    (is ok?)
    (is (nil? (piece-by-id state :indigo-sword-target)))
    (is (= {:id :indigo-medium-1
            :player-id :indigo
            :space-index 4
            :size :medium
            :orientation :north}
           shrunk-piece))
    (is (= ["swords2"] (mapv :id (:discard-pile state))))
    (is (not (some #{"swords2"} (player-hand-ids state :rose))))
    (is (= 4 (get-in state [:players-by-id :indigo :stash :medium])))
    (is (= 5 (get-in state [:players-by-id :indigo :stash :large])))
    (is (= :sword/piece-shrunk (get-in events [0 :type])))
    (is (= :indigo (get-in events [0 :target :player-id])))
    (is (= enemy-piece (get-in events [0 :replaced-piece])))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest sword-move-destroys-small-piece
  (let [enemy-piece {:id :indigo-sword-target
                     :player-id :indigo
                     :space-index 4
                     :size :small
                     :orientation :north}
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["swords2"])}))
        state (game-state/with-board-pieces state [rose-sword-minion enemy-piece])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "swords2"
                          :piece-id :rose-sword-minion}
                 :target {:kind :piece
                          :piece-id :indigo-sword-target}
                 :damage 1}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)]
    (is ok?)
    (is (= [rose-sword-minion]
           (get-in state [:pieces :on-board])))
    (is (= 5 (get-in state [:players-by-id :indigo :stash :small])))
    (is (= 5 (get-in state [:pieces :stashes :indigo :small])))
    (is (= ["swords2"] (mapv :id (:discard-pile state))))
    (is (= [{:type :sword/piece-destroyed
             :player-id :rose
             :source {:kind :hand-card
                      :card-id "swords2"
                      :piece-id :rose-sword-minion}
             :sword-variant :sword
             :target {:kind :piece
                      :piece-id :indigo-sword-target
                      :player-id :indigo
                      :board-index 4
                      :row 1
                      :col 1}
             :damage 1
             :from-size :small
             :destroyed-piece enemy-piece}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest sword-move-rejects-missing-smaller-piece-and-overdamage_without_mutation
  (let [target-piece {:id :indigo-sword-target
                      :player-id :indigo
                      :space-index 4
                      :size :medium
                      :orientation :north}
        small-pieces [{:id :indigo-small-a
                       :player-id :indigo
                       :space-index 0
                       :size :small
                       :orientation :north}
                      {:id :indigo-small-b
                       :player-id :indigo
                       :space-index 1
                       :size :small
                       :orientation :east}
                      {:id :indigo-small-c
                       :player-id :indigo
                       :space-index 2
                       :size :small
                       :orientation :south}
                      {:id :indigo-small-d
                       :player-id :indigo
                       :space-index 5
                       :size :small
                       :orientation :west}
                      {:id :indigo-small-e
                       :player-id :indigo
                       :space-index 6
                       :size :small
                       :orientation :up}]
        no-small-pieces (vec (concat [rose-sword-minion target-piece]
                                     small-pieces))
        no-small-state (:state (game-state/create-game
                                player-specs
                                {:deck-order (deck-with-board-card 3 "swords2")}))
        no-small-state (game-state/with-board-pieces no-small-state
                                                     no-small-pieces)
        no-small-result (game-state/apply-sword-move
                         no-small-state
                         {:player-id :rose
                          :source {:kind :territory
                                   :board-index 3
                                   :piece-id :rose-sword-minion}
                          :target {:kind :piece
                                   :piece-id :indigo-sword-target}
                          :damage 1})
        small-target {:id :indigo-small-target
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
        overdamage-state (:state (game-state/create-game
                                  player-specs
                                  {:deck-order (deck-with-board-card 3 "swords2")}))
        overdamage-state (game-state/with-board-pieces
                          overdamage-state
                          [rose-sword-minion small-target])
        overdamage-result (game-state/apply-sword-move
                           overdamage-state
                           {:player-id :rose
                            :source {:kind :territory
                                     :board-index 3
                                     :piece-id :rose-sword-minion}
                            :target {:kind :piece
                                     :piece-id :indigo-small-target}
                            :damage 2})]
    (is (= :no-smaller-piece-available
           (get-in no-small-result [:error :code])))
    (is (= :invalid-sword-damage
           (get-in overdamage-result [:error :code])))
    (is (not (contains? no-small-result :state)))
    (is (not (contains? overdamage-result :state)))
    (is (= no-small-pieces
           (get-in no-small-state [:pieces :on-board])))
    (is (= [rose-sword-minion small-target]
           (get-in overdamage-state [:pieces :on-board])))
    (is (empty? (:discard-pile no-small-state)))
    (is (empty? (:discard-pile overdamage-state)))))

(deftest sword-move-shrinks-royalty-territory-with-hand-replacement
  (let [deck-order (deck-with-cards-at {0 "cups2"
                                        (board-deck-position 3) "swords2"
                                        (board-deck-position 4) "cupsking"})
        target-piece (assoc rose-target-minion
                            :space-index 4
                            :orientation :south)
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-sword-minion target-piece])
        original-cell (board-cell-by-index state 4)
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-sword-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :damage 1
                 :replacement-card-source :hand
                 :replacement-card-id "cups2"}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)
        shrunk-cell (board-cell-by-index state 4)]
    (is ok?)
    (is (= "cups2" (get-in shrunk-cell [:card :id])))
    (is (= (select-keys original-cell [:index :row :col :orientation :face])
           (select-keys shrunk-cell [:index :row :col :orientation :face])))
    (is (= target-piece (piece-by-id state :rose-target-minion)))
    (is (= ["cupsking"] (mapv :id (:discard-pile state))))
    (is (not (some #{"cups2"} (player-hand-ids state :rose))))
    (is (= [{:type :sword/territory-shrunk
             :player-id :rose
             :source {:kind :territory
                      :board-index 3
                      :piece-id :rose-sword-minion}
             :sword-variant :sword
             :target {:kind :territory
                      :board-index 4
                      :row 1
                      :col 1}
             :damage 1
             :replacement-card-source :hand
             :original-card-id "cupsking"
             :replacement-card-id "cups2"
             :from-value 2
             :to-value 1
             :territory shrunk-cell}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest sword-move-rejects-missing_reused_and_invalid_territory_replacements_without_mutation
  (let [deck-order (deck-with-cards-at {0 "swords2"
                                        1 "swordsking"
                                        (board-deck-position 4) "cupsking"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-sword-minion])
        base-command {:player-id :rose
                      :source {:kind :hand-card
                               :card-id "swords2"
                               :piece-id :rose-sword-minion}
                      :target {:kind :territory
                               :board-index 4}
                      :damage 1}
        missing-result (game-state/apply-sword-move state base-command)
        reused-result (game-state/apply-sword-move
                       state
                       (assoc base-command :replacement-card-id "swords2"))
        invalid-result (game-state/apply-sword-move
                        state
                        (assoc base-command :replacement-card-id "swordsking"))]
    (is (= :invalid-sword-replacement
           (get-in missing-result [:error :code])))
    (is (= :card-already-used
           (get-in reused-result [:error :code])))
    (is (= :invalid-sword-replacement-card
           (get-in invalid-result [:error :code])))
    (is (not (contains? missing-result :state)))
    (is (not (contains? reused-result :state)))
    (is (not (contains? invalid-result :state)))
    (is (= "cupsking" (get-in (board-cell-by-index state 4) [:card :id])))
    (is (= ["swords2" "swordsking"]
           (filterv #{"swords2" "swordsking"} (player-hand-ids state :rose))))
    (is (empty? (:discard-pile state)))))

(deftest tower-sword-move-can-shrink-major-territory-from-discard_source
  (let [deck-order (deck-with-cards-at {0 "cupsking"
                                        (board-deck-position 3) "tower"
                                        (board-deck-position 4) "star"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (-> state
                  (move-card-to-discard "cupsking")
                  (game-state/with-board-pieces [rose-sword-minion]))
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-sword-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :damage 1
                 :replacement-card-source :discard-pile
                 :replacement-card-id "cupsking"}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)]
    (is ok?)
    (is (= "cupsking" (get-in (board-cell-by-index state 4) [:card :id])))
    (is (= ["star"] (mapv :id (:discard-pile state))))
    (is (= :sword/territory-shrunk (get-in events [0 :type])))
    (is (= :sword-from-discard (get-in events [0 :sword-variant])))
    (is (= :discard-pile (get-in events [0 :replacement-card-source])))
    (is (= 3 (get-in events [0 :from-value])))
    (is (= 2 (get-in events [0 :to-value])))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest justice-trades-hands-before-applying-sword
  (let [enemy-piece {:id :indigo-justice-target
                     :player-id :indigo
                     :space-index 4
                     :size :medium
                     :orientation :north}
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["justice"])}))
        state (game-state/with-board-pieces state [rose-sword-minion enemy-piece])
        rose-hand-before (player-hand-ids state :rose)
        indigo-hand-before (player-hand-ids state :indigo)
        {:keys [ok? state events]} (game-state/apply-sword-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "justice"
                                              :piece-id :rose-sword-minion}
                                     :hand-trade-target {:kind :piece
                                                         :piece-id :indigo-justice-target}
                                     :target {:kind :piece
                                              :piece-id :indigo-justice-target}
                                     :damage 1})
        shrunk-piece (piece-by-id state :indigo-small-1)]
    (is ok?)
    (is (= [:justice/hands-traded :sword/piece-shrunk]
           (mapv :type events)))
    (is (= indigo-hand-before (player-hand-ids state :rose)))
    (is (= (vec (remove #{"justice"} rose-hand-before))
           (player-hand-ids state :indigo)))
    (is (= ["justice"] (mapv :id (:discard-pile state))))
    (is (= {:id :indigo-small-1
            :player-id :indigo
            :space-index 4
            :size :small
            :orientation :north}
           shrunk-piece))
    (is (game-schema/valid-game? state))))

(deftest tower-orients-minion-before-applying-sword
  (let [tower-minion (assoc rose-sword-minion :orientation :north)
        enemy-piece {:id :indigo-tower-target
                     :player-id :indigo
                     :space-index 4
                     :size :medium
                     :orientation :north}
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["tower"])}))
        state (game-state/with-board-pieces state [tower-minion enemy-piece])
        {:keys [ok? state events]} (game-state/apply-sword-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "tower"
                                              :piece-id :rose-sword-minion}
                                     :minion-orientation :east
                                     :target {:kind :piece
                                              :piece-id :indigo-tower-target}
                                     :damage 1})
        shrunk-piece (piece-by-id state :indigo-small-1)]
    (is ok?)
    (is (= [:piece/oriented :sword/piece-shrunk]
           (mapv :type events)))
    (is (= :east (:orientation (piece-by-id state :rose-sword-minion))))
    (is (= :north (:orientation shrunk-piece)))
    (is (= ["tower"] (mapv :id (:discard-pile state))))
    (is (game-schema/valid-game? state))))

(deftest death-shortcut-can-skip-missing-intermediate-piece
  (let [enemy-piece {:id :indigo-death-target
                     :player-id :indigo
                     :space-index 4
                     :size :medium
                     :orientation :north}
        indigo-small-pieces [{:id :indigo-small-a
                              :player-id :indigo
                              :space-index 0
                              :size :small
                              :orientation :north}
                             {:id :indigo-small-b
                              :player-id :indigo
                              :space-index 1
                              :size :small
                              :orientation :east}
                             {:id :indigo-small-c
                              :player-id :indigo
                              :space-index 2
                              :size :small
                              :orientation :south}
                             {:id :indigo-small-d
                              :player-id :indigo
                              :space-index 5
                              :size :small
                              :orientation :west}
                             {:id :indigo-small-e
                              :player-id :indigo
                              :space-index 6
                              :size :small
                              :orientation :up}]
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["death"])}))
        state (game-state/with-board-pieces
               state
               (vec (concat [rose-sword-minion enemy-piece]
                            indigo-small-pieces)))
        {:keys [ok? state events]} (game-state/apply-sword-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "death"}
                                     :sword-actions [{:piece-id :rose-sword-minion
                                                      :target {:kind :piece
                                                               :piece-id :indigo-death-target}
                                                      :damage 1}
                                                     {:piece-id :rose-sword-minion
                                                      :target {:kind :piece
                                                               :piece-id :indigo-death-target}
                                                      :damage 1}]})]
    (is ok?)
    (is (nil? (piece-by-id state :indigo-death-target)))
    (is (= :sword/piece-destroyed (get-in events [0 :type])))
    (is (= 2 (get-in events [0 :action-count])))
    (is (true? (get-in events [0 :shortcut?])))
    (is (= ["death"] (mapv :id (:discard-pile state))))
    (is (= 0 (get-in state [:pieces :stashes :indigo :small])))
    (is (= 5 (get-in state [:pieces :stashes :indigo :medium])))
    (is (game-schema/valid-game? state))))

(deftest moon-can-enter-full-territory-if-sword-restores_piece_limit
  (let [full-space-pieces [{:id :indigo-moon-target
                            :player-id :indigo
                            :space-index 4
                            :size :small
                            :orientation :north}
                           {:id :indigo-moon-bystander
                            :player-id :indigo
                            :space-index 4
                            :size :medium
                            :orientation :west}
                           {:id :rose-moon-bystander
                            :player-id :rose
                            :space-index 4
                            :size :small
                            :orientation :south}]
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["moon"])}))
        state (game-state/with-board-pieces
               state
               (vec (cons rose-sword-minion full-space-pieces)))
        {:keys [ok? state events]} (game-state/apply-moon-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "moon"}
                                     :rod {:piece-id :rose-sword-minion
                                           :mode :move-minion
                                           :distance 1
                                           :orientation :up}
                                     :sword {:piece-id :rose-sword-minion
                                             :target {:kind :piece
                                                      :piece-id :indigo-moon-target}
                                             :damage 1}})]
    (is ok?)
    (is (= [:rod/minion-moved :sword/piece-destroyed]
           (mapv :type events)))
    (is (= {:id :rose-sword-minion
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :up}
           (piece-by-id state :rose-sword-minion)))
    (is (nil? (piece-by-id state :indigo-moon-target)))
    (is (= 3 (count (filter #(= 4 (:space-index %))
                            (get-in state [:pieces :on-board])))))
    (is (= ["moon"] (mapv :id (:discard-pile state))))
    (is (game-schema/valid-game? state))))

(deftest moon-rejects-full-territory_entry_unless_sword_restores_piece_limit
  (let [full-space-pieces [{:id :indigo-moon-target
                            :player-id :indigo
                            :space-index 4
                            :size :medium
                            :orientation :north}
                           {:id :indigo-moon-bystander
                            :player-id :indigo
                            :space-index 4
                            :size :small
                            :orientation :west}
                           {:id :rose-moon-bystander
                            :player-id :rose
                            :space-index 4
                            :size :small
                            :orientation :south}]
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["moon"])}))
        state (game-state/with-board-pieces
               state
               (vec (cons rose-sword-minion full-space-pieces)))
        result (game-state/apply-moon-move
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "moon"}
                 :rod {:piece-id :rose-sword-minion
                       :mode :move-minion
                       :distance 1
                       :orientation :up}
                 :sword {:piece-id :rose-sword-minion
                         :target {:kind :piece
                                  :piece-id :indigo-moon-target}
                         :damage 1}})]
    (is (= :moon-full-territory-unresolved
           (get-in result [:error :code])))
    (is (not (contains? result :state)))
    (is (= ["moon"] (take 1 (player-hand-ids state :rose))))
    (is (empty? (:discard-pile state)))))

(deftest empress-orients-minion-before-unbounded-cup
  (let [full-target-pieces [rose-target-minion
                            indigo-cup-target
                            {:id :rose-target-small
                             :player-id :rose
                             :space-index 4
                             :size :small
                             :orientation :south}]
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["empress"])}))
        state (game-state/with-board-pieces
               state
               (vec (cons (assoc rose-cup-minion :orientation :north)
                          full-target-pieces)))
        {:keys [ok? state events]} (game-state/apply-empress-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "empress"}
                                     :actions [{:power :orient-minion
                                                :piece-id :rose-cup-minion
                                                :orientation :east}
                                               {:power :cup
                                                :piece-id :rose-cup-minion
                                                :target {:kind :territory
                                                         :board-index 4}
                                                :orientation :up}]})
        target-piece-ids (->> (get-in state [:pieces :on-board])
                              (filter #(= 4 (:space-index %)))
                              (mapv :id))]
    (is ok?)
    (is (= [:piece/oriented :cup/small-piece-created]
           (mapv :type events)))
    (is (= :east (:orientation (piece-by-id state :rose-cup-minion))))
    (is (= [:rose-target-minion
            :indigo-cup-target
            :rose-target-small
            :rose-small-1]
           target-piece-ids))
    (is (= :cup-unbounded (get-in events [1 :cup-variant])))
    (is (= ["empress"] (mapv :id (:discard-pile state))))
    (is (game-schema/valid-game? state))))

(deftest emperor-orients-minion-before-unbounded-rod
  (let [full-target-pieces [rose-target-minion
                            indigo-cup-target
                            {:id :rose-target-small
                             :player-id :rose
                             :space-index 4
                             :size :small
                             :orientation :south}]
        emperor-minion (assoc rose-rod-minion :orientation :north)
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["emperor"])}))
        state (game-state/with-board-pieces
               state
               (vec (cons emperor-minion full-target-pieces)))
        {:keys [ok? state events]} (game-state/apply-emperor-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "emperor"}
                                     :actions [{:power :orient-minion
                                                :piece-id :rose-rod-minion
                                                :orientation :east}
                                               {:power :rod
                                                :piece-id :rose-rod-minion
                                                :mode :move-minion
                                                :distance 1
                                                :orientation :up}]})
        moved-piece (piece-by-id state :rose-rod-minion)]
    (is ok?)
    (is (= [:piece/oriented :rod/minion-moved]
           (mapv :type events)))
    (is (= {:id :rose-rod-minion
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :up}
           moved-piece))
    (is (= 4 (count (filter #(= 4 (:space-index %))
                            (get-in state [:pieces :on-board])))))
    (is (= :rod-unbounded (get-in events [1 :rod-variant])))
    (is (= ["emperor"] (mapv :id (:discard-pile state))))
    (is (game-schema/valid-game? state))))

(deftest lovers-promotes-rod-moved-piece-for-cup-action
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 3 "lovers")}))
        state (game-state/with-board-pieces
               state
               [(assoc rose-rod-minion :orientation :east)])
        {:keys [ok? state events]} (game-state/apply-lovers-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :territory
                                              :board-index 3}
                                     :actions [{:power :rod
                                                :piece-id :rose-rod-minion
                                                :mode :move-minion
                                                :distance 1
                                                :orientation :east}
                                               {:power :cup
                                                :piece-id :rose-rod-minion
                                                :target {:kind :territory
                                                         :board-index 5}
                                                :orientation :up}]})
        moved-piece (piece-by-id state :rose-rod-minion)
        created-piece (piece-by-id state :rose-small-1)]
    (is ok?)
    (is (= [:rod/minion-moved :cup/small-piece-created]
           (mapv :type events)))
    (is (= 4 (:space-index moved-piece)))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 5
            :size :small
            :orientation :up}
           created-piece))
    (is (empty? (:discard-pile state)))
    (is (game-schema/valid-game? state))))

(deftest chariot-shortcut-can-pass-through-full-territory
  (let [chariot-minion {:id :rose-chariot-minion
                        :player-id :rose
                        :space-index 3
                        :size :small
                        :orientation :east}
        full-target-pieces [rose-target-minion
                            indigo-cup-target
                            {:id :rose-target-small
                             :player-id :rose
                             :space-index 4
                             :size :small
                             :orientation :south}]
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["chariot"])}))
        state (game-state/with-board-pieces
               state
               (vec (cons chariot-minion full-target-pieces)))
        {:keys [ok? state events]} (game-state/apply-chariot-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "chariot"}
                                     :rod-actions [{:piece-id :rose-chariot-minion
                                                    :mode :move-minion
                                                    :distance 1}
                                                   {:piece-id :rose-chariot-minion
                                                    :mode :move-minion
                                                    :distance 1
                                                    :orientation :up}]})
        moved-piece (piece-by-id state :rose-chariot-minion)]
    (is ok?)
    (is (= [:chariot/rod-shortcut]
           (mapv :type events)))
    (is (= {:id :rose-chariot-minion
            :player-id :rose
            :space-index 5
            :size :small
            :orientation :up}
           moved-piece))
    (is (= {:row 1 :col 1}
           (get-in events [0 :intermediate])))
    (is (= :territory (get-in events [0 :destination :kind])))
    (is (= 3 (count (filter #(= 4 (:space-index %))
                            (get-in state [:pieces :on-board])))))
    (is (= ["chariot"] (mapv :id (:discard-pile state))))
    (is (game-schema/valid-game? state))))

(deftest hanged-man-applies-rod-before-targeted-hand-trade
  (let [enemy-piece {:id :indigo-hanged-target
                     :player-id :indigo
                     :space-index 5
                     :size :medium
                     :orientation :north}
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["hangedman"])}))
        state (game-state/with-board-pieces
               state
               [(assoc rose-rod-minion :orientation :east)
                enemy-piece])
        rose-hand-before (player-hand-ids state :rose)
        indigo-hand-before (player-hand-ids state :indigo)
        {:keys [ok? state events]} (game-state/apply-hanged-man-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "hangedman"}
                                     :rod {:piece-id :rose-rod-minion
                                           :mode :move-minion
                                           :distance 1}
                                     :hand-trade-target-piece-id :indigo-hanged-target})]
    (is ok?)
    (is (= [:rod/minion-moved :hanged-man/hands-traded]
           (mapv :type events)))
    (is (= 4 (:space-index (piece-by-id state :rose-rod-minion))))
    (is (= indigo-hand-before (player-hand-ids state :rose)))
    (is (= (vec (remove #{"hangedman"} rose-hand-before))
           (player-hand-ids state :indigo)))
    (is (= ["hangedman"] (mapv :id (:discard-pile state))))
    (is (game-schema/valid-game? state))))

(deftest temperance-can-apply-two-cup-actions-with-one-source-cost
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["temperance"])}))
        state (game-state/with-board-pieces state [rose-cup-minion])
        {:keys [ok? state events]} (game-state/apply-temperance-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "temperance"}
                                     :cup-actions [{:piece-id :rose-cup-minion
                                                    :target {:kind :territory
                                                             :board-index 4}
                                                    :orientation :north}
                                                   {:piece-id :rose-cup-minion
                                                    :target {:kind :territory
                                                             :board-index 4}
                                                    :orientation :east}]})
        first-piece (piece-by-id state :rose-small-1)
        second-piece (piece-by-id state :rose-small-2)]
    (is ok?)
    (is (= [:cup/small-piece-created :cup/small-piece-created]
           (mapv :type events)))
    (is (= 1 (count (:discard-pile state))))
    (is (= ["temperance"] (mapv :id (:discard-pile state))))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 4
            :size :small
            :orientation :north}
           first-piece))
    (is (= {:id :rose-small-2
            :player-id :rose
            :space-index 4
            :size :small
            :orientation :east}
           second-piece))
    (is (game-schema/valid-game? state))))

(deftest world-major-territories-enumerates-copyable-board-majors
  (let [state (state-with-board-cards {0 "world"
                                       1 "empress"
                                       2 "cups2"
                                       3 "moon"})]
    (is (= [{:board-index 1
             :card-id "empress"
             :powers [:orient-minion :cup-unbounded]}
            {:board-index 3
             :card-id "moon"
             :powers [:rod :sword]}]
           (mapv #(select-keys % [:board-index :card-id :powers])
                 (game-state/world-major-territories state))))))

(deftest world-hand-source-copies-composite-major-through-world-source
  (let [full-target-pieces [rose-target-minion
                            indigo-cup-target
                            {:id :rose-target-small
                             :player-id :rose
                             :space-index 4
                             :size :small
                             :orientation :south}]
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order
                        (deck-with-cards-at
                         {0 "world"
                          (board-deck-position 3) "empress"})}))
        state (game-state/with-board-pieces
               state
               (vec (cons rose-cup-minion full-target-pieces)))
        {:keys [ok? state events]} (game-state/apply-world-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "world"}
                                     :copied-board-index 3
                                     :actions [{:power :cup
                                                :piece-id :rose-cup-minion
                                                :target {:kind :territory
                                                         :board-index 4}
                                                :orientation :up}]})
        target-piece-ids (->> (get-in state [:pieces :on-board])
                              (filter #(= 4 (:space-index %)))
                              (mapv :id))]
    (is ok?)
    (is (= [:cup/small-piece-created] (mapv :type events)))
    (is (= :cup-unbounded (get-in events [0 :cup-variant])))
    (is (= {:kind :hand-card
            :card-id "world"
            :piece-id :rose-cup-minion}
           (get-in events [0 :source])))
    (is (= [:rose-target-minion
            :indigo-cup-target
            :rose-target-small
            :rose-small-1]
           target-piece-ids))
    (is (= ["world"] (mapv :id (:discard-pile state))))
    (is (= "empress" (get-in (board-cell-by-index state 3) [:card :id])))
    (is (game-schema/valid-game? state))))

(deftest world-territory-source-copies-magician-wild-suit
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order
                        (deck-with-cards-at
                         {(board-deck-position 3) "world"
                          (board-deck-position 5) "magician"})}))
        state (game-state/with-board-pieces state [rose-cup-minion])
        {:keys [ok? state events]} (game-state/apply-world-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :territory
                                              :board-index 3
                                              :piece-id :rose-cup-minion}
                                     :copied-board-index 5
                                     :power :cup
                                     :target {:kind :territory
                                              :board-index 4}
                                     :orientation :east})
        created-piece (piece-by-id state :rose-small-1)]
    (is ok?)
    (is (= [:cup/small-piece-created] (mapv :type events)))
    (is (= :wild-suits (get-in events [0 :cup-variant])))
    (is (= {:kind :territory
            :board-index 3
            :piece-id :rose-cup-minion}
           (get-in events [0 :source])))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 4
            :size :small
            :orientation :east}
           created-piece))
    (is (empty? (:discard-pile state)))
    (is (game-schema/valid-game? state))))

(deftest world-rejects-non-major-and-world-copy-targets
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order
                        (deck-with-cards-at
                         {(board-deck-position 3) "world"
                          (board-deck-position 4) "cups2"})}))
        state (game-state/with-board-pieces state [rose-cup-minion])
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-cup-minion}
                      :power :cup
                      :target {:kind :territory
                               :board-index 5}
                      :orientation :east}
        minor-result (game-state/apply-world-move
                      state
                      (assoc base-command :copied-board-index 4))
        self-result (game-state/apply-world-move
                     state
                     (assoc base-command :copied-board-index 3))]
    (is (= :invalid-world-copy (get-in minor-result [:error :code])))
    (is (= :invalid-world-copy (get-in self-result [:error :code])))
    (is (not (contains? minor-result :state)))
    (is (not (contains? self-result :state)))))

(deftest sword-move-destroys-spot-territory-and-voided-pieces
  (let [deck-order (deck-with-cards-at {(board-deck-position 0) "cups2"
                                        (board-deck-position 1) "swords2"})
        sword-minion (assoc rose-sword-minion
                            :space-index 1
                            :size :small
                            :orientation :west)
        target-piece {:id :rose-target-territory-minion
                      :player-id :rose
                      :space-index 0
                      :size :medium
                      :orientation :south}
        voided-piece {:id :rose-outboard-minion
                      :player-id :rose
                      :space {:kind :wasteland
                              :row 0
                              :col -1}
                      :size :small
                      :orientation :north}
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [sword-minion
                                                   target-piece
                                                   voided-piece])
        original-cell (board-cell-by-index state 0)
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 1
                          :piece-id :rose-sword-minion}
                 :target {:kind :territory
                          :board-index 0}
                 :damage 1}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)
        surviving-target-piece (piece-by-id state :rose-target-territory-minion)]
    (is ok?)
    (is (nil? (board-cell-by-index state 0)))
    (is (= "cups2" (get-in original-cell [:card :id])))
    (is (= ["cups2"] (mapv :id (:discard-pile state))))
    (is (= sword-minion (piece-by-id state :rose-sword-minion)))
    (is (= {:id :rose-target-territory-minion
            :player-id :rose
            :space {:kind :wasteland
                    :row 0
                    :col 0}
            :size :medium
            :orientation :south}
           surviving-target-piece))
    (is (nil? (piece-by-id state :rose-outboard-minion)))
    (is (= 4 (get-in state [:players-by-id :rose :stash :small])))
    (is (= 4 (get-in state [:players-by-id :rose :stash :medium])))
    (is (= [{:type :sword/territory-destroyed
             :player-id :rose
             :source {:kind :territory
                      :board-index 1
                      :piece-id :rose-sword-minion}
             :sword-variant :sword
             :target {:kind :territory
                      :board-index 0
                      :row 0
                      :col 0}
             :damage 1
             :original-card-id "cups2"
             :from-value 1
             :destroyed-territory original-cell
             :destroyed-pieces [voided-piece]}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest disc-move-grows-spot-territory-with-hand-replacement
  (let [deck-order (deck-with-cards-at {0 "cupsking"
                                        (board-deck-position 3) "coins2"
                                        (board-deck-position 4) "cups2"})
        target-piece (assoc rose-target-minion
                            :space-index 4
                            :orientation :south)
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-disc-minion target-piece])
        original-cell (board-cell-by-index state 4)
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-disc-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :replacement-card-source :hand
                 :replacement-card-id "cupsking"}
        {:keys [ok? state events]} (game-state/apply-disc-move state command)
        grown-cell (board-cell-by-index state 4)]
    (is ok?)
    (is (= "cupsking" (get-in grown-cell [:card :id])))
    (is (= (select-keys original-cell [:index :row :col :orientation :face])
           (select-keys grown-cell [:index :row :col :orientation :face])))
    (is (= target-piece (piece-by-id state :rose-target-minion)))
    (is (= ["cups2"] (mapv :id (:discard-pile state))))
    (is (not (some #{"cupsking"} (player-hand-ids state :rose))))
    (is (= [{:type :disc/territory-grown
             :player-id :rose
             :source {:kind :territory
                      :board-index 3
                      :piece-id :rose-disc-minion}
             :disc-variant :disc
             :target {:kind :territory
                      :board-index 4
                      :row 1
                      :col 1}
             :replacement-card-source :hand
             :original-card-id "cups2"
             :replacement-card-id "cupsking"
             :from-value 1
             :to-value 2
             :territory grown-cell}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest disc-move-hand-source-grows-territory-without-duplicating-cards
  (let [deck-order (deck-with-cards-at {0 "coins2"
                                        1 "cupsking"
                                        (board-deck-position 4) "cups2"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-disc-minion])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "coins2"
                          :piece-id :rose-disc-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :replacement-card-source :hand
                 :replacement-card-id "cupsking"}
        {:keys [ok? state events]} (game-state/apply-disc-move state command)]
    (is ok?)
    (is (= "cupsking" (get-in (board-cell-by-index state 4) [:card :id])))
    (is (= ["coins2" "cups2"] (mapv :id (:discard-pile state))))
    (is (not (some #{"coins2"} (player-hand-ids state :rose))))
    (is (not (some #{"cupsking"} (player-hand-ids state :rose))))
    (is (= :disc/territory-grown (get-in events [0 :type])))
    (is (= :hand-card (get-in events [0 :source :kind])))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest star-disc-move-can-grow-royalty-territory-from-discard_source
  (let [deck-order (deck-with-cards-at {0 "star"
                                        (board-deck-position 4) "cupsking"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-disc-minion])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "star"
                          :piece-id :rose-disc-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :replacement-card-source :discard-pile
                 :replacement-card-id "star"}
        {:keys [ok? state events]} (game-state/apply-disc-move state command)]
    (is ok?)
    (is (= "star" (get-in (board-cell-by-index state 4) [:card :id])))
    (is (= ["cupsking"] (mapv :id (:discard-pile state))))
    (is (not (some #{"star"} (player-hand-ids state :rose))))
    (is (= :disc-from-discard (get-in events [0 :disc-variant])))
    (is (= :discard-pile (get-in events [0 :replacement-card-source])))
    (is (= 2 (get-in events [0 :from-value])))
    (is (= 3 (get-in events [0 :to-value])))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest star-disc-orients-minion-before-targeting
  (let [deck-order (deck-with-cards-at {0 "star"
                                        (board-deck-position 4) "cupsking"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces
               state
               [(assoc rose-disc-minion :orientation :north)])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "star"
                          :piece-id :rose-disc-minion}
                 :minion-orientation :east
                 :target {:kind :territory
                          :board-index 4}
                 :replacement-card-source :discard-pile
                 :replacement-card-id "star"}
        {:keys [ok? state events]} (game-state/apply-disc-move state command)]
    (is ok?)
    (is (= [:piece/oriented :disc/territory-grown]
           (mapv :type events)))
    (is (= :east (:orientation (piece-by-id state :rose-disc-minion))))
    (is (= "star" (get-in (board-cell-by-index state 4) [:card :id])))
    (is (= ["cupsking"] (mapv :id (:discard-pile state))))
    (is (game-schema/valid-game? state))))

(deftest strength-disc-can_skip_intermediate_piece_size
  (let [small-minion (assoc rose-disc-minion :size :small)
        medium-pieces [{:id :rose-medium-a
                        :player-id :rose
                        :space-index 0
                        :size :medium
                        :orientation :north}
                       {:id :rose-medium-b
                        :player-id :rose
                        :space-index 1
                        :size :medium
                        :orientation :east}
                       {:id :rose-medium-c
                        :player-id :rose
                        :space-index 2
                        :size :medium
                        :orientation :south}
                       {:id :rose-medium-d
                        :player-id :rose
                        :space-index 4
                        :size :medium
                        :orientation :west}
                       {:id :rose-medium-e
                        :player-id :rose
                        :space-index 5
                        :size :medium
                        :orientation :up}]
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["strength"])}))
        state (game-state/with-board-pieces
               state
               (vec (cons small-minion medium-pieces)))
        {:keys [ok? state events]} (game-state/apply-disc-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "strength"
                                              :piece-id :rose-disc-minion}
                                     :disc-actions [{:target {:kind :piece
                                                              :piece-id :rose-disc-minion}}
                                                    {:target {:kind :piece
                                                              :piece-id :rose-disc-minion}}]})
        grown-piece (piece-by-id state :rose-large-1)]
    (is ok?)
    (is (= {:id :rose-large-1
            :player-id :rose
            :space-index 3
            :size :large
            :orientation :east}
           grown-piece))
    (is (= ["strength"] (mapv :id (:discard-pile state))))
    (is (= :small (get-in events [0 :from-size])))
    (is (= :large (get-in events [0 :to-size])))
    (is (= 2 (get-in events [0 :action-count])))
    (is (true? (get-in events [0 :shortcut?])))
    (is (= 0 (get-in state [:pieces :stashes :rose :medium])))
    (is (game-schema/valid-game? state))))

(deftest strength-disc-can_skip_intermediate_territory_value
  (let [deck-order (deck-with-cards-at {0 "strength"
                                        1 "star"
                                        (board-deck-position 4) "cups2"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-disc-minion])
        {:keys [ok? state events]} (game-state/apply-disc-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "strength"
                                              :piece-id :rose-disc-minion}
                                     :disc-actions [{:target {:kind :territory
                                                              :board-index 4}}
                                                    {:target {:kind :territory
                                                              :board-index 4}
                                                     :replacement-card-id "star"}]})
        grown-cell (board-cell-by-index state 4)]
    (is ok?)
    (is (= "star" (get-in grown-cell [:card :id])))
    (is (= ["strength" "cups2"] (mapv :id (:discard-pile state))))
    (is (= 1 (get-in events [0 :from-value])))
    (is (= 3 (get-in events [0 :to-value])))
    (is (= 2 (get-in events [0 :action-count])))
    (is (true? (get-in events [0 :shortcut?])))
    (is (not (some #{"strength" "star"} (player-hand-ids state :rose))))
    (is (game-schema/valid-game? state))))

(deftest sun-move-applies_cup_then_disc_to_created_piece
  (let [small-pieces [{:id :rose-small-a
                       :player-id :rose
                       :space-index 0
                       :size :small
                       :orientation :north}
                      {:id :rose-small-b
                       :player-id :rose
                       :space-index 1
                       :size :small
                       :orientation :east}
                      {:id :rose-small-c
                       :player-id :rose
                       :space-index 2
                       :size :small
                       :orientation :south}
                      {:id :rose-small-d
                       :player-id :rose
                       :space-index 5
                       :size :small
                       :orientation :west}
                      {:id :rose-small-e
                       :player-id :rose
                       :space-index 6
                       :size :small
                       :orientation :up}]
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["sun"])}))
        state (game-state/with-board-pieces
               state
               (vec (cons rose-disc-minion small-pieces)))
        {:keys [ok? state events]} (game-state/apply-sun-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "sun"
                                              :piece-id :rose-disc-minion}
                                     :cup {:target {:kind :territory
                                                    :board-index 4}
                                           :orientation :north}
                                     :disc {:target {:kind :created-piece}
                                            :orientation :south}})
        grown-piece (piece-by-id state :rose-medium-1)]
    (is ok?)
    (is (= [:sun/piece-created-and-grown]
           (mapv :type events)))
    (is (= {:id :rose-medium-1
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :south}
           grown-piece))
    (is (= ["sun"] (mapv :id (:discard-pile state))))
    (is (not (some #{"sun"} (player-hand-ids state :rose))))
    (is (= 0 (get-in state [:pieces :stashes :rose :small])))
    (is (= 3 (get-in state [:pieces :stashes :rose :medium])))
    (is (game-schema/valid-game? state))))

(deftest sun-created-piece-shortcut-requires_cup_target_space
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["sun"])}))
        state (game-state/with-board-pieces
               state
               [(assoc rose-disc-minion
                       :space-index 0
                       :orientation :east)])
        result (game-state/apply-sun-move
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "sun"
                          :piece-id :rose-disc-minion}
                 :cup {:target {:kind :territory
                                :board-index 8}
                       :orientation :north}
                 :disc {:target {:kind :created-piece}
                        :orientation :south}})]
    (is (false? (:ok? result)))
    (is (= :cup-target-out-of-range
           (get-in result [:error :code])))
    (is (= {:row 2
            :col 2}
           (get-in result [:error :data :target-coordinate])))
    (is (= {:row 0
            :col 1}
           (get-in result [:error :data :expected-coordinate])))
    (is (not (contains? result :state)))
    (is (= ["sun"] (take 1 (player-hand-ids state :rose))))
    (is (empty? (:discard-pile state)))))

(deftest sun-move-can_skip_one_point_territory_creation
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["sun" "cupsking"])}))
        state (game-state/with-board-pieces
               state
               [(assoc rose-disc-minion :orientation :west)])
        {:keys [ok? state events]} (game-state/apply-sun-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "sun"
                                              :piece-id :rose-disc-minion}
                                     :cup {:target {:kind :wasteland
                                                    :row 1
                                                    :col -1}}
                                     :disc {:target {:kind :created-territory}
                                            :replacement-card-id "cupsking"}})
        created-cell (last (:board state))]
    (is ok?)
    (is (= [:sun/territory-created-and-grown]
           (mapv :type events)))
    (is (= {:row 1
            :col -1}
           (select-keys created-cell [:row :col])))
    (is (= "cupsking" (get-in created-cell [:card :id])))
    (is (= ["sun"] (mapv :id (:discard-pile state))))
    (is (not (some #{"sun" "cupsking"} (player-hand-ids state :rose))))
    (is (game-schema/valid-game? state))))

(deftest sun-created-territory-shortcut-requires_cup_target_space
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["sun" "cupsking"])}))
        state (game-state/with-board-pieces
               state
               [(assoc rose-disc-minion
                       :space-index 0
                       :orientation :east)])
        result (game-state/apply-sun-move
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "sun"
                          :piece-id :rose-disc-minion}
                 :cup {:target {:kind :wasteland
                                :row 3
                                :col 2}}
                 :disc {:target {:kind :created-territory}
                        :replacement-card-id "cupsking"}})]
    (is (false? (:ok? result)))
    (is (= :cup-target-out-of-range
           (get-in result [:error :code])))
    (is (= {:row 3
            :col 2}
           (get-in result [:error :data :target-coordinate])))
    (is (= {:row 0
            :col 1}
           (get-in result [:error :data :expected-coordinate])))
    (is (not (contains? result :state)))
    (is (every? (set (player-hand-ids state :rose))
                ["sun" "cupsking"]))
    (is (empty? (:discard-pile state)))))

(deftest disc-move-rejects-missing-or-invalid-territory_replacements_without_mutation
  (let [deck-order (deck-with-cards-at {0 "sun"
                                        (board-deck-position 3) "coins2"
                                        (board-deck-position 4) "cups2"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-disc-minion])
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-disc-minion}
                      :target {:kind :territory
                               :board-index 4}}
        missing-result (game-state/apply-disc-move state base-command)
        invalid-result (game-state/apply-disc-move
                        state
                        (assoc base-command
                               :replacement-card-id "sun"))]
    (is (= :invalid-disc-replacement
           (get-in missing-result [:error :code])))
    (is (= :invalid-disc-replacement-card
           (get-in invalid-result [:error :code])))
    (is (not (contains? missing-result :state)))
    (is (not (contains? invalid-result :state)))
    (is (= "cups2" (get-in (board-cell-by-index state 4) [:card :id])))
    (is (= ["sun"] (take 1 (player-hand-ids state :rose))))
    (is (empty? (:discard-pile state)))))

(deftest disc-move-grows-current-player-piece-and-may-reorient-it
  (let [small-minion (assoc rose-disc-minion :size :small)
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 3 "coins2")}))
        state (game-state/with-board-pieces state [small-minion])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-disc-minion}
                 :target {:kind :piece
                          :piece-id :rose-disc-minion}
                 :orientation :south}
        {:keys [ok? state events]} (game-state/apply-disc-move state command)
        grown-piece (piece-by-id state :rose-medium-1)]
    (is ok?)
    (is (nil? (piece-by-id state :rose-disc-minion)))
    (is (= {:id :rose-medium-1
            :player-id :rose
            :space-index 3
            :size :medium
            :orientation :south}
           grown-piece))
    (is (= 5 (get-in state [:players-by-id :rose :stash :small])))
    (is (= 4 (get-in state [:players-by-id :rose :stash :medium])))
    (is (= 5 (get-in state [:pieces :stashes :rose :small])))
    (is (= 4 (get-in state [:pieces :stashes :rose :medium])))
    (is (= [{:type :disc/piece-grown
             :player-id :rose
             :source {:kind :territory
                      :board-index 3
                      :piece-id :rose-disc-minion}
             :disc-variant :disc
             :target {:kind :piece
                      :piece-id :rose-disc-minion
                      :player-id :rose
                      :board-index 3
                      :row 1
                      :col 0}
             :from-size :small
             :to-size :medium
             :replaced-piece small-minion
             :piece grown-piece}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest disc-move-grows-medium-pieces-to-large
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 3 "coins2")}))
        state (game-state/with-board-pieces state [rose-disc-minion])
        {:keys [ok? state events]} (game-state/apply-disc-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :territory
                                              :board-index 3
                                              :piece-id :rose-disc-minion}
                                     :target {:kind :piece
                                              :piece-id :rose-disc-minion}})
        grown-piece (piece-by-id state :rose-large-1)]
    (is ok?)
    (is (= {:id :rose-large-1
            :player-id :rose
            :space-index 3
            :size :large
            :orientation :east}
           grown-piece))
    (is (= :medium (get-in events [0 :from-size])))
    (is (= :large (get-in events [0 :to-size])))
    (is (= 5 (get-in state [:players-by-id :rose :stash :medium])))
    (is (= 4 (get-in state [:players-by-id :rose :stash :large])))
    (is (game-schema/valid-game? state))))

(deftest disc-move-grows-enemy-piece-and-discards-hand-source
  (let [enemy-piece {:id :indigo-disc-target
                     :player-id :indigo
                     :space-index 4
                     :size :small
                     :orientation :north}
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["coins2"])}))
        state (game-state/with-board-pieces state [rose-disc-minion enemy-piece])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "coins2"
                          :piece-id :rose-disc-minion}
                 :target {:kind :piece
                          :piece-id :indigo-disc-target}}
        {:keys [ok? state events]} (game-state/apply-disc-move state command)
        grown-piece (piece-by-id state :indigo-medium-1)]
    (is ok?)
    (is (= {:id :indigo-medium-1
            :player-id :indigo
            :space-index 4
            :size :medium
            :orientation :north}
           grown-piece))
    (is (= ["coins2"] (mapv :id (:discard-pile state))))
    (is (not (some #{"coins2"} (player-hand-ids state :rose))))
    (is (= 5 (get-in state [:players-by-id :indigo :stash :small])))
    (is (= 4 (get-in state [:players-by-id :indigo :stash :medium])))
    (is (= :disc/piece-grown (get-in events [0 :type])))
    (is (= :indigo (get-in events [0 :target :player-id])))
    (is (= enemy-piece (get-in events [0 :replaced-piece])))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest disc-move-rejects-large-pieces-and-missing-replacements_without_mutation
  (let [large-piece (assoc rose-disc-minion :size :large)
        large-state (-> (:state (game-state/create-game
                                 player-specs
                                 {:deck-order (deck-with-board-card 3 "coins2")}))
                        (game-state/with-board-pieces [large-piece]))
        large-result (game-state/apply-disc-move
                      large-state
                      {:player-id :rose
                       :source {:kind :territory
                                :board-index 3
                                :piece-id :rose-disc-minion}
                       :target {:kind :piece
                                :piece-id :rose-disc-minion}})
        small-piece (assoc rose-disc-minion :size :small)
        medium-pieces [{:id :rose-medium-a
                        :player-id :rose
                        :space-index 0
                        :size :medium
                        :orientation :north}
                       {:id :rose-medium-b
                        :player-id :rose
                        :space-index 1
                        :size :medium
                        :orientation :east}
                       {:id :rose-medium-c
                        :player-id :rose
                        :space-index 2
                        :size :medium
                        :orientation :south}
                       {:id :rose-medium-d
                        :player-id :rose
                        :space-index 4
                        :size :medium
                        :orientation :west}
                       {:id :rose-medium-e
                        :player-id :rose
                        :space-index 5
                        :size :medium
                        :orientation :up}]
        no-medium-pieces (vec (cons small-piece medium-pieces))
        no-medium-state (-> (:state (game-state/create-game
                                     player-specs
                                     {:deck-order (deck-with-board-card 3 "coins2")}))
                            (game-state/with-board-pieces no-medium-pieces))
        no-medium-result (game-state/apply-disc-move
                          no-medium-state
                          {:player-id :rose
                           :source {:kind :territory
                                    :board-index 3
                                    :piece-id :rose-disc-minion}
                           :target {:kind :piece
                                    :piece-id :rose-disc-minion}})]
    (is (= :target-piece-max-size
           (get-in large-result [:error :code])))
    (is (= :no-larger-piece-available
           (get-in no-medium-result [:error :code])))
    (is (not (contains? large-result :state)))
    (is (not (contains? no-medium-result :state)))
    (is (= [large-piece]
           (get-in large-state [:pieces :on-board])))
    (is (= no-medium-pieces
           (get-in no-medium-state [:pieces :on-board])))))

(deftest rod-command-normalizes-territory-source
  (let [state (-> (state-with-pieces [rose-rod-minion])
                  (state-with-board-card 3 "wands2"))
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-rod-minion}
                 :mode :move-minion
                 :distance 2
                 :orientation :south}
        result (game-state/resolve-rod-command state command)]
    (is (:ok? result))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 3
                     :piece-id :rose-rod-minion}
            :rod-variant :rod
            :mode :move-minion
            :target {:kind :piece
                     :piece-id :rose-rod-minion
                     :player-id :rose
                     :row 1
                     :col 0
                     :destination {:row 1
                                   :col 2}
                     :orientation :south}
            :distance 2
            :direction :east
            :orientation :south}
           (:command result)))
    (is (= "wands2" (get-in result [:source-card :id])))
    (is (= rose-rod-minion (:piece result)))))

(deftest rod-command-normalizes-hand-card-source-and-piece-target
  (let [deck-order (deck-starting-with ["wands2"])
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces
               state
               [rose-rod-minion
                {:id :indigo-rod-target
                 :player-id :indigo
                 :space-index 4
                 :size :small
                 :orientation :north}])
        result (game-state/resolve-rod-command
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "wands2"
                          :piece-id :rose-rod-minion}
                 :mode :push-piece
                 :target {:kind :piece
                          :piece-id :indigo-rod-target}
                 :distance 1})]
    (is (:ok? result))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "wands2"
                     :piece-id :rose-rod-minion}
            :rod-variant :rod
            :mode :push-piece
            :target {:kind :piece
                     :piece-id :indigo-rod-target
                     :player-id :indigo
                     :row 1
                     :col 1
                     :destination {:row 1
                                   :col 2}}
            :distance 1
            :direction :east}
           (:command result)))
    (is (= "wands2" (get-in result [:source-card :id])))
    (is (= :indigo-rod-target (get-in result [:target-piece :id])))))

(deftest rod-command-rejects-enemy-piece-reorientation
  (let [deck-order (deck-starting-with ["wands2"])
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces
               state
               [rose-rod-minion
                {:id :indigo-rod-target
                 :player-id :indigo
                 :space-index 4
                 :size :small
                 :orientation :north}])
        result (game-state/resolve-rod-command
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "wands2"
                          :piece-id :rose-rod-minion}
                 :mode :push-piece
                 :target {:kind :piece
                          :piece-id :indigo-rod-target}
                 :distance 1
                 :orientation :west})]
    (is (= :invalid-orientation
           (get-in result [:error :code])))
    (is (= {:piece-id :indigo-rod-target
            :piece-player-id :indigo
            :orientation :west}
           (get-in result [:error :data])))
    (is (false? (:ok? result)))
    (is (not (contains? result :state)))))

(deftest rod-command-normalizes-territory-target-coordinates
  (let [state (-> (state-with-pieces [rose-rod-minion])
                  (state-with-board-card 3 "wands2"))
        result (game-state/resolve-rod-command
                state
                {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-rod-minion}
                 :mode :push-territory
                 :target {:kind :territory
                          :row 1
                          :col 1}
                 :distance 1})]
    (is (:ok? result))
    (is (= {:kind :territory
            :board-index 4
            :row 1
            :col 1
            :destination {:row 1
                          :col 2}}
           (get-in result [:command :target])))
    (is (= :rod (get-in result [:command :rod-variant])))
    (is (= 4 (get-in result [:target-cell :index])))))

(deftest rod-command-carries-source-variant
  (let [emperor-state (-> (state-with-pieces [rose-rod-minion])
                          (state-with-board-card 3 "emperor"))
        emperor-result (game-state/resolve-rod-command
                        emperor-state
                        {:player-id :rose
                         :source {:kind :territory
                                  :board-index 3
                                  :piece-id :rose-rod-minion}
                         :mode :move-minion
                         :distance 1})
        magician-state (-> (state-with-pieces [rose-rod-minion])
                           (state-with-board-card 3 "magician"))
        magician-result (game-state/resolve-rod-command
                         magician-state
                         {:player-id :rose
                          :source {:kind :territory
                                   :board-index 3
                                   :piece-id :rose-rod-minion}
                          :mode :move-minion
                          :distance 1})]
    (is (:ok? emperor-result))
    (is (:ok? magician-result))
    (is (= :rod-unbounded
           (get-in emperor-result [:command :rod-variant])))
    (is (= :wild-suits
           (get-in magician-result [:command :rod-variant])))))

(deftest rod-command-rejects-unavailable-variants
  (let [state (-> (state-with-pieces [rose-rod-minion])
                  (state-with-board-card 3 "wands2"))
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-rod-minion}
                      :mode :move-minion
                      :distance 1}
        unavailable-result (game-state/resolve-rod-command
                            state
                            (assoc base-command :rod-variant :rod-unbounded))
        invalid-result (game-state/resolve-rod-command
                        state
                        (assoc base-command :rod-variant :wheel-cup))]
    (is (= :rod-variant-unavailable
           (get-in unavailable-result [:error :code])))
    (is (= :invalid-rod-variant
           (get-in invalid-result [:error :code])))))

(deftest rod-command-rejects-invalid-sources-and-upright-minions
  (let [state (-> (state-with-pieces [rose-rod-minion])
                  (state-with-board-card 3 "cups2"))
        non-rod-result (game-state/resolve-rod-command
                        state
                        {:player-id :rose
                         :source {:kind :territory
                                  :board-index 3
                                  :piece-id :rose-rod-minion}
                         :mode :move-minion
                         :distance 1})
        upright-state (-> (state-with-pieces [(assoc rose-rod-minion
                                                     :orientation :up)])
                          (state-with-board-card 3 "wands2"))
        upright-result (game-state/resolve-rod-command
                        upright-state
                        {:player-id :rose
                         :source {:kind :territory
                                  :board-index 3
                                  :piece-id :rose-rod-minion}
                         :mode :move-minion
                         :distance 1})]
    (is (= :source-card-not-rod
           (get-in non-rod-result [:error :code])))
    (is (= :rod-minion-upright
           (get-in upright-result [:error :code])))))

(deftest rod-command-rejects-invalid-direction-distance-and_targets_without_mutation
  (let [state (-> (state-with-pieces [rose-rod-minion
                                      {:id :indigo-off-axis-target
                                       :player-id :indigo
                                       :space-index 0
                                       :size :small
                                       :orientation :north}])
                  (state-with-board-card 3 "wands2"))
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-rod-minion}
                      :mode :move-minion
                      :distance 1}
        zero-result (game-state/resolve-rod-command state
                                                    (assoc base-command
                                                           :distance 0))
        too-far-result (game-state/resolve-rod-command state
                                                       (assoc base-command
                                                              :distance 3))
        direction-result (game-state/resolve-rod-command state
                                                         (assoc base-command
                                                                :direction :north))
        target-result (game-state/resolve-rod-command
                       state
                       (assoc base-command
                              :mode :push-piece
                              :target {:kind :piece
                                       :piece-id :indigo-off-axis-target}))]
    (is (= :invalid-rod-distance
           (get-in zero-result [:error :code])))
    (is (= :invalid-rod-distance
           (get-in too-far-result [:error :code])))
    (is (= 2 (get-in too-far-result [:error :data :maximum])))
    (is (= :invalid-rod-direction
           (get-in direction-result [:error :code])))
    (is (= :invalid-rod-target
           (get-in target-result [:error :code])))
    (is (false? (:ok? zero-result)))
    (is (not (contains? zero-result :state)))
    (is (= [rose-rod-minion
            {:id :indigo-off-axis-target
             :player-id :indigo
             :space-index 0
             :size :small
             :orientation :north}]
           (get-in state [:pieces :on-board])))))

(deftest rod-move-moves-minion-to-territory-and-may-reorient-owned-piece
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 3 "wands2")}))
        state (game-state/with-board-pieces state [rose-rod-minion])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-rod-minion}
                 :mode :move-minion
                 :distance 2
                 :orientation :south}
        {:keys [ok? state events]} (game-state/apply-rod-move state command)
        moved-piece (piece-by-id state :rose-rod-minion)]
    (is ok?)
    (is (= {:id :rose-rod-minion
            :player-id :rose
            :space-index 5
            :size :medium
            :orientation :south}
           moved-piece))
    (is (= [{:type :rod/minion-moved
             :player-id :rose
             :source {:kind :territory
                      :board-index 3
                      :piece-id :rose-rod-minion}
             :rod-variant :rod
             :target {:kind :piece
                      :piece-id :rose-rod-minion
                      :player-id :rose
                      :row 1
                      :col 0}
             :destination {:kind :territory
                           :board-index 5
                           :row 1
                           :col 2}
             :distance 2
             :direction :east
             :piece moved-piece}]
           events))
    (is (= events [(peek (:history state))]))
    (is (game-schema/valid-game? state))))

(deftest rod-move-pushes-enemy-piece-and-discards-hand-source
  (let [deck-order (deck-starting-with ["wands2"])
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces
               state
               [rose-rod-minion
                {:id :indigo-rod-target
                 :player-id :indigo
                 :space-index 4
                 :size :small
                 :orientation :north}])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "wands2"
                          :piece-id :rose-rod-minion}
                 :mode :push-piece
                 :target {:kind :piece
                          :piece-id :indigo-rod-target}
                 :distance 1}
        {:keys [ok? state events]} (game-state/apply-rod-move state command)
        pushed-piece (piece-by-id state :indigo-rod-target)]
    (is ok?)
    (is (= {:id :indigo-rod-target
            :player-id :indigo
            :space-index 5
            :size :small
            :orientation :north}
           pushed-piece))
    (is (= ["wands2"] (mapv :id (:discard-pile state))))
    (is (not (some #{"wands2"} (player-hand-ids state :rose))))
    (is (= [{:type :rod/piece-pushed
             :player-id :rose
             :source {:kind :hand-card
                      :card-id "wands2"
                      :piece-id :rose-rod-minion}
             :rod-variant :rod
             :target {:kind :piece
                      :piece-id :indigo-rod-target
                      :player-id :indigo
                      :row 1
                      :col 1}
             :destination {:kind :territory
                           :board-index 5
                           :row 1
                           :col 2}
             :distance 1
             :direction :east
             :piece pushed-piece}]
           events))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest rod-move-represents-wasteland-destinations-as-wasteland-spaces
  (let [rod-minion (assoc rose-rod-minion
                          :space-index 2
                          :orientation :east)
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 2 "wands2")}))
        state (game-state/with-board-pieces state [rod-minion])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 2
                          :piece-id :rose-rod-minion}
                 :mode :move-minion
                 :distance 1}
        {:keys [ok? state events]} (game-state/apply-rod-move state command)
        moved-piece (piece-by-id state :rose-rod-minion)]
    (is ok?)
    (is (= {:id :rose-rod-minion
            :player-id :rose
            :space {:kind :wasteland
                    :row 0
                    :col 3}
            :size :medium
            :orientation :east}
           moved-piece))
    (is (= {:kind :wasteland
            :row 0
            :col 3}
           (get-in events [0 :destination])))
    (is (not (contains? moved-piece :space-index)))
    (is (game-schema/valid-game? state))))

(deftest rod-move-pushes-territory-into-wasteland-without-moving-pieces
  (let [deck-order (deck-starting-with ["wands2"])
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        target-card (get-in state [:board 5 :card])
        rod-minion (assoc rose-rod-minion :space-index 4)
        state (game-state/with-board-pieces
               state
               [rod-minion
                (assoc rose-target-minion :space-index 5)
                {:id :rose-landing-minion
                 :player-id :rose
                 :space {:kind :wasteland
                         :row 1
                         :col 3}
                 :size :small
                 :orientation :west}])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "wands2"
                          :piece-id :rose-rod-minion}
                 :mode :push-territory
                 :target {:kind :territory
                          :board-index 5}
                 :distance 1}
        {:keys [ok? state events]} (game-state/apply-rod-move state command)
        moved-cell (board-cell-by-index state 5)
        old-target-piece (piece-by-id state :rose-target-minion)
        landing-piece (piece-by-id state :rose-landing-minion)]
    (is ok?)
    (is (= {:index 5
            :row 1
            :col 3
            :orientation :portrait
            :face :up
            :card target-card}
           moved-cell))
    (is (nil? (board-cell-at state 1 2)))
    (is (= {:id :rose-target-minion
            :player-id :rose
            :space {:kind :wasteland
                    :row 1
                    :col 2}
            :size :medium
            :orientation :up}
           old-target-piece))
    (is (= {:id :rose-landing-minion
            :player-id :rose
            :space-index 5
            :size :small
            :orientation :west}
           landing-piece))
    (is (= ["wands2"] (mapv :id (:discard-pile state))))
    (is (not (some #{"wands2"} (player-hand-ids state :rose))))
    (is (= [{:type :rod/territory-pushed
             :player-id :rose
             :source {:kind :hand-card
                      :card-id "wands2"
                      :piece-id :rose-rod-minion}
             :rod-variant :rod
             :target {:kind :territory
                      :board-index 5
                      :row 1
                      :col 2}
             :destination {:kind :wasteland
                           :row 1
                           :col 3}
             :distance 1
             :direction :east
             :territory moved-cell}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

(deftest rod-move-returns-pieces-left-in-void-by-territory-relocation
  (let [initial-state (:state (game-state/create-game
                               player-specs
                               {:deck-order (deck-starting-with ["wands2"])}))
        state (with-board-cells-at initial-state
                [[0 {:row 0 :col 0}]
                 [8 {:row 0 :col 3}]])
        target-card (get-in state [:board 0 :card])
        rod-minion {:id :rose-rod-wasteland-minion
                    :player-id :rose
                    :space {:kind :wasteland
                            :row 0
                            :col -1}
                    :size :medium
                    :orientation :east}
        passenger {:id :rose-rod-passenger
                   :player-id :rose
                   :space-index 0
                   :size :small
                   :orientation :north}
        state (game-state/with-board-pieces state [rod-minion
                                                   passenger])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "wands2"
                          :piece-id :rose-rod-wasteland-minion}
                 :mode :push-territory
                 :target {:kind :territory
                          :board-index 0}
                 :distance 2}
        {:keys [ok? state events]} (game-state/apply-rod-move state command)
        moved-cell (board-cell-by-index state 0)]
    (is ok?)
    (is (= {:index 0
            :row 0
            :col 2
            :orientation :portrait
            :face :up
            :card target-card}
           moved-cell))
    (is (nil? (board-cell-at state 0 0)))
    (is (nil? (piece-by-id state :rose-rod-wasteland-minion)))
    (is (nil? (piece-by-id state :rose-rod-passenger)))
    (is (= 5 (get-in state [:pieces :stashes :rose :small])))
    (is (= 5 (get-in state [:pieces :stashes :rose :medium])))
    (is (= ["wands2"] (mapv :id (:discard-pile state))))
    (is (= :rod/territory-pushed (get-in events [0 :type])))
    (is (game-schema/valid-game? state))))

(deftest rod-move-rejects-enemy-occupied-territory-push-targets_without_mutation
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 4 "wands2")}))
        pieces [(assoc rose-rod-minion :space-index 4)
                {:id :indigo-target-minion
                 :player-id :indigo
                 :space-index 5
                 :size :small
                 :orientation :north}]
        state (game-state/with-board-pieces state pieces)
        result (game-state/apply-rod-move
                state
                {:player-id :rose
                 :source {:kind :territory
                          :board-index 4
                          :piece-id :rose-rod-minion}
                 :mode :push-territory
                 :target {:kind :territory
                          :board-index 5}
                 :distance 1})]
    (is (= :target-territory-occupied-by-enemy
           (get-in result [:error :code])))
    (is (= [:indigo-target-minion]
           (get-in result [:error :data :enemy-piece-ids])))
    (is (not (contains? result :state)))
    (is (= pieces (get-in state [:pieces :on-board])))))

(deftest rod-move-rejects-territory-pushes-to-enemy-wastelands-and-void_without_mutation
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 4 "wands2")}))
        enemy-landing-pieces [(assoc rose-rod-minion :space-index 4)
                              {:id :indigo-landing-minion
                               :player-id :indigo
                               :space {:kind :wasteland
                                       :row 1
                                       :col 3}
                               :size :small
                               :orientation :south}]
        enemy-landing-state (game-state/with-board-pieces state enemy-landing-pieces)
        enemy-landing-result (game-state/apply-rod-move
                              enemy-landing-state
                              {:player-id :rose
                               :source {:kind :territory
                                        :board-index 4
                                        :piece-id :rose-rod-minion}
                               :mode :push-territory
                               :target {:kind :territory
                                        :board-index 5}
                               :distance 1})
        void-state (game-state/with-board-pieces
                    state
                    [(assoc rose-rod-minion :space-index 4)])
        void-result (game-state/apply-rod-move
                     void-state
                     {:player-id :rose
                      :source {:kind :territory
                               :board-index 4
                               :piece-id :rose-rod-minion}
                      :mode :push-territory
                      :target {:kind :territory
                               :board-index 5}
                      :distance 2})]
    (is (= :wasteland-occupied-by-enemy
           (get-in enemy-landing-result [:error :code])))
    (is (= [:indigo-landing-minion]
           (get-in enemy-landing-result [:error :data :enemy-piece-ids])))
    (is (= :rod-destination-void
           (get-in void-result [:error :code])))
    (is (not (contains? enemy-landing-result :state)))
    (is (not (contains? void-result :state)))
    (is (= enemy-landing-pieces
           (get-in enemy-landing-state [:pieces :on-board])))
    (is (= [(assoc rose-rod-minion :space-index 4)]
           (get-in void-state [:pieces :on-board])))))

(deftest rod-move-rejects-void-and-full-territory-destinations_without_mutation
  (let [full-pieces [rose-rod-minion
                     rose-target-minion
                     {:id :indigo-target-minion
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
                     {:id :indigo-target-guard
                      :player-id :indigo
                      :space-index 4
                      :size :large
                      :orientation :south}]
        full-state (:state (game-state/create-game
                            player-specs
                            {:deck-order (deck-with-board-card 3 "wands2")}))
        full-state (game-state/with-board-pieces full-state full-pieces)
        full-result (game-state/apply-rod-move
                     full-state
                     {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-rod-minion}
                      :mode :move-minion
                      :distance 1})
        void-minion (assoc rose-rod-minion
                           :space-index 2
                           :orientation :east)
        void-state (:state (game-state/create-game
                            player-specs
                            {:deck-order (deck-with-board-card 2 "wands2")}))
        void-state (game-state/with-board-pieces void-state [void-minion])
        void-result (game-state/apply-rod-move
                     void-state
                     {:player-id :rose
                      :source {:kind :territory
                               :board-index 2
                               :piece-id :rose-rod-minion}
                      :mode :move-minion
                      :distance 2})]
    (is (= :target-territory-full
           (get-in full-result [:error :code])))
    (is (= :rod-destination-void
           (get-in void-result [:error :code])))
    (is (not (contains? full-result :state)))
    (is (not (contains? void-result :state)))
    (is (= (get-in full-state [:pieces :on-board])
           full-pieces))
    (is (= [void-minion]
           (get-in void-state [:pieces :on-board])))))

(deftest rod-unbounded-variant-ignores-full-territory-destination-limit
  (let [full-pieces [rose-rod-minion
                     rose-target-minion
                     {:id :indigo-target-minion
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
                     {:id :indigo-target-guard
                      :player-id :indigo
                      :space-index 4
                      :size :large
                      :orientation :south}]
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 3 "emperor")}))
        state (game-state/with-board-pieces state full-pieces)
        {:keys [ok? state events]} (game-state/apply-rod-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :territory
                                              :board-index 3
                                              :piece-id :rose-rod-minion}
                                     :mode :move-minion
                                     :distance 1})
        moved-piece (piece-by-id state :rose-rod-minion)
        destination-pieces (filter #(= 4 (:space-index %))
                                   (get-in state [:pieces :on-board]))]
    (is ok?)
    (is (= {:id :rose-rod-minion
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :east}
           moved-piece))
    (is (= 4 (count destination-pieces)))
    (is (= :rod-unbounded
           (get-in events [0 :rod-variant])))
    (is (game-schema/valid-game? state))))
