(ns gnostica.game-state-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.game-schema :as game-schema]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as pieces]))

(def player-specs
  [{:id :rose
    :name "Rose"}
   {:id :indigo
    :name "Indigo"}])

(defn- card-ids [cards]
  (mapv :id cards))

(defn- board-card-ids [state]
  (mapv (comp :id :card) (:board state)))

(defn- hand-card-count [player-count]
  (* game-state/starting-hand-size player-count))

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

(defn- state-with-pieces [pieces]
  (game-state/with-board-pieces (deterministic-game) pieces))

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
   :orientation :north})

(def rose-rod-minion
  {:id :rose-rod-minion
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

(deftest draw-move-discards-selected-cards-and-draws-to-hand
  (let [initial-state (deterministic-game)
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
  (let [base-state (deterministic-game)
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
  (let [state (deterministic-game)
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
             :source {:kind :territory
                      :board-index 3
                      :piece-id :rose-cup-minion}
             :target {:kind :territory
                      :board-index 4}
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

(deftest cup-move-creates-territory-from-one-point-card-in-wasteland
  (let [state (state-with-pieces [rose-cup-minion])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-cup-minion}
                 :target {:kind :wasteland
                          :row 0
                          :col 3}
                 :one-point-card-id "coins2"}
        {:keys [ok? state events]} (game-state/apply-cup-move state command)
        created-cell (board-cell-by-index state 9)]
    (is ok?)
    (is (= {:index 9
            :row 0
            :col 3
            :orientation :landscape
            :face :up
            :card (cards/card-by-id "coins2")}
           created-cell))
    (is (not (some #{"coins2"} (player-hand-ids state :rose))))
    (is (= [{:type :cup/territory-created
             :player-id :rose
             :source {:kind :territory
                      :board-index 3
                      :piece-id :rose-cup-minion}
             :target {:kind :wasteland
                      :row 0
                      :col 3}
             :board-index 9
             :card-id "coins2"}]
           events))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))

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

(deftest cup-move-rejects-invalid-wasteland-territory-cards
  (let [state (state-with-pieces [rose-cup-minion])
        result (game-state/apply-cup-move state
                                          {:player-id :rose
                                           :source {:kind :territory
                                                    :board-index 3
                                                    :piece-id :rose-cup-minion}
                                           :target {:kind :wasteland
                                                    :row 0
                                                    :col 3}
                                           :one-point-card-id "chariot"})]
    (is (= :invalid-one-point-card
           (get-in result [:error :code])))))

(deftest cup-move-rejects-wastelands-occupied-by-enemy-pieces
  (let [state (state-with-pieces [rose-cup-minion
                                  {:id :indigo-wasteland-minion
                                   :player-id :indigo
                                   :space {:kind :wasteland
                                           :row 0
                                           :col 3}
                                   :size :small
                                   :orientation :up}])
        result (game-state/apply-cup-move state
                                          {:player-id :rose
                                           :source {:kind :territory
                                                    :board-index 3
                                                    :piece-id :rose-cup-minion}
                                           :target {:kind :wasteland
                                                    :row 0
                                                    :col 3}
                                           :one-point-card-id "coins2"})]
    (is (= :wasteland-occupied-by-enemy
           (get-in result [:error :code])))
    (is (= [:indigo-wasteland-minion]
           (get-in result [:error :data :enemy-piece-ids])))))

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
    (is (= 4 (get-in result [:target-cell :index])))))

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
