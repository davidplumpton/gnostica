(ns gnostica.game-state.setup-test
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
            [gnostica.pieces :as pieces]
            [gnostica.test-support.board :refer [board-card-ids
                                                 board-cell-at
                                                 board-cell-by-index
                                                 state-with-board-card
                                                 with-board-cells-at]]
            [gnostica.test-support.deck :refer [card-ids
                                                deck-starting-with
                                                deck-with-board-card
                                                deck-with-cards-at
                                                hand-card-count
                                                board-card-position]]
            [gnostica.test-support.game-state :refer [all-card-ids
                                                      deterministic-game
                                                      move-card-to-discard
                                                      player-hand-ids
                                                      player-specs
                                                      replace-player-hand
                                                      remove-card-id
                                                      set-player-eliminated
                                                      state-with-board-cards
                                                      state-with-pieces
                                                      three-player-specs]]
            [gnostica.test-support.game-state-moves :refer :all]
            [gnostica.test-support.pieces :refer [piece-by-id]]))

(defn- seeding-error [result code]
  (some #(when (= code (:code %)) %)
        (get-in result [:error :data :errors])))

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
(deftest validates-seeded-board-pieces-before-rebuilding-stashes
  (let [state (deterministic-game)
        piece {:id :rose-seeded-small
               :player-id :rose
               :space-index 0
               :size :small
               :orientation :north}
        result (game-state/with-board-pieces-result state [piece])]
    (is (nil? (game-state/validate-board-pieces state [piece])))
    (is (:ok? result))
    (is (= [piece] (get-in result [:state :pieces :on-board])))
    (is (= {:small 4
            :medium 5
            :large 5}
           (get-in result [:state :pieces :stashes :rose])))
    (is (= (get-in result [:state :players-by-id :rose :stash])
           (get-in result [:state :pieces :stashes :rose])))))
(deftest rejects-impossible-seeded-board-pieces-with-structured-errors
  (let [state (deterministic-game)
        duplicate-id :duplicate-scout
        duplicate-result (game-state/with-board-pieces-result
                           state
                           [{:id duplicate-id
                             :player-id :rose
                             :space-index 0
                             :size :small
                             :orientation :up}
                            {:id duplicate-id
                             :player-id :rose
                             :space-index 1
                             :size :small
                             :orientation :north}])
        unknown-result (game-state/with-board-pieces-result
                         state
                         [{:id :obsidian-scout
                           :player-id :obsidian
                           :space-index 0
                           :size :small
                           :orientation :north}])
        location-result (game-state/with-board-pieces-result
                          state
                          [{:id :rose-missing-space
                            :player-id :rose
                            :space-index 99
                            :size :small
                            :orientation :up}
                           {:id :rose-ambiguous-location
                            :player-id :rose
                            :space-index 0
                            :space {:kind :wasteland
                                    :row 0
                                    :col 3}
                            :size :small
                            :orientation :up}
                           {:id :rose-missing-location
                            :player-id :rose
                            :size :small
                            :orientation :up}
                           {:id :rose-lost-wasteland
                            :player-id :rose
                            :space {:kind :wasteland
                                    :row 99
                                    :col 99}
                            :size :small
                            :orientation :up}])
        overflow-pieces (mapv (fn [index]
                                {:id (keyword (str "rose-extra-small-" index))
                                 :player-id :rose
                                 :space-index (mod index board/board-card-count)
                                 :size :small
                                 :orientation :up})
                              (range 6))
        overflow-result (game-state/with-board-pieces-result state overflow-pieces)
        location-error-codes (set (map :code (get-in location-result
                                                     [:error :data :errors])))]
    (is (false? (:ok? duplicate-result)))
    (is (= {:code :duplicate-active-piece-ids
            :message "Active pieces must have unique ids."
            :data {:piece-ids [duplicate-id]}}
           (seeding-error duplicate-result :duplicate-active-piece-ids)))
    (is (= {:code :unknown-piece-player
            :message "Pieces on the board must belong to a player in the game."
            :data {:piece-id :obsidian-scout
                   :player-id :obsidian
                   :player-ids [:rose :indigo]}}
           (seeding-error unknown-result :unknown-piece-player)))
    (is (every? location-error-codes
                [:piece-space-missing
                 :ambiguous-piece-location
                 :missing-piece-location
                 :piece-wasteland-missing]))
    (is (= {:code :piece-count-exceeds-stash
            :message "Seeded board pieces cannot use more pieces than a player's starting stash."
            :data {:player-id :rose
                   :size :small
                   :active-piece-count 6
                   :available-count game-state/pieces-per-size-in-stash}}
           (seeding-error overflow-result :piece-count-exceeds-stash)))
    (try
      (game-state/with-board-pieces state overflow-pieces)
      (is false "with-board-pieces should reject invalid seeds")
      (catch clojure.lang.ExceptionInfo ex
        (is (= :invalid-board-pieces (:code (ex-data ex))))
        (is (= :piece-count-exceeds-stash
               (-> ex ex-data :data :errors first :code)))))))
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
(deftest starting-bid-round-preview-reuses-official-ranking-without_redraws
  (let [deck-order (deck-with-cards-at {0 "cupsking"
                                        1 "cupsqueen"
                                        6 "swordsking"
                                        7 "swords10"})
        state (:state (game-state/create-game player-specs
                                              {:deck-order deck-order}))
        tied-result (game-state/resolve-starting-bid-rounds
                     state
                     {:rounds [{:rose "cupsking"
                                :indigo "swordsking"}]})
        resolved-result (game-state/resolve-starting-bid-rounds
                         state
                         {:rounds [{:rose "cupsking"
                                    :indigo "swordsking"}
                                   {:rose "cupsqueen"
                                    :indigo "swords10"}]})]
    (is (:ok? tied-result))
    (is (false? (:resolved? tied-result)))
    (is (= [:rose :indigo]
           (get-in tied-result [:bid-history 0 :tied-player-ids])))
    (is (= 5 (count (get-in tied-result
                            [:state :players-by-id :rose :hand]))))
    (is (not (some #{"cupsking"}
                   (player-hand-ids (:state tied-result) :rose))))
    (is (:ok? resolved-result))
    (is (true? (:resolved? resolved-result)))
    (is (= :rose (:winner-id resolved-result)))
    (is (= [:indigo :rose] (:redraw-order resolved-result)))
    (is (= 2 (count (:bid-history resolved-result))))
    (is (= 4 (count (get-in resolved-result
                            [:state :players-by-id :rose :hand]))))))
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
