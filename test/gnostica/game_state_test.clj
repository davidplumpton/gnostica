(ns gnostica.game-state-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.game-schema :as game-schema]
            [gnostica.game-state :as game-state]
            [gnostica.game-state.cup :as game-state-cup]
            [gnostica.game-state.disc :as game-state-disc]
            [gnostica.game-state.draw :as game-state-draw]
            [gnostica.game-state.placement :as game-state-placement]
            [gnostica.game-state.rod :as game-state-rod]
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

(def rose-disc-minion
  {:id :rose-disc-minion
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

(deftest focused-transition-namespaces-match-public-facade
  (let [draw-state (deterministic-game)
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
                      :orientation :south}]
    (is (= (game-state/apply-draw-move draw-state draw-command)
           (game-state-draw/apply-draw-move draw-state draw-command)))
    (is (= (game-state/apply-orient-move orient-state orient-command)
           (game-state-placement/apply-orient-move orient-state orient-command)))
    (is (= (game-state/apply-cup-move cup-state cup-command)
           (game-state-cup/apply-cup-move cup-state cup-command)))
    (is (= (game-state/resolve-rod-command rod-state rod-command)
           (game-state-rod/resolve-rod-command rod-state rod-command)))
    (is (= (game-state/apply-rod-move rod-state rod-command)
           (game-state-rod/apply-rod-move rod-state rod-command)))
    (is (= (game-state/resolve-disc-command disc-state disc-command)
           (game-state-disc/resolve-disc-command disc-state disc-command)))))

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
	             :cup-variant :cup
	             :source {:kind :territory
	                      :board-index 3
	                      :piece-id :rose-cup-minion}
             :target {:kind :wasteland
                      :row 0
	                      :col 3}
	             :board-index 9
	             :card-id "coins2"
	             :territory-card-source :hand}]
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

(deftest wheel-cup-can-create-wasteland-territory-from-draw-pile
  (let [initial-state (:state (game-state/create-game
                               player-specs
                               {:deck-order (deck-with-board-card 3 "wheeloffortune")}))
        draw-pile-card (first (:draw-pile initial-state))
        state (game-state/with-board-pieces initial-state [rose-cup-minion])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-cup-minion}
                 :cup-variant :wheel-cup
                 :target {:kind :wasteland
                          :row 0
                          :col 3}
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
                      :row 0
                      :col 3}
             :board-index 9
             :card-id (:id draw-pile-card)
             :territory-card-source :draw-pile-top}]
           events))
    (is (game-schema/valid-game? state))))

(deftest non-wheel-cups-reject-draw-pile-territory-source
  (let [state (state-with-pieces [rose-cup-minion])
        result (game-state/apply-cup-move
                state
                {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-cup-minion}
                 :cup-variant :cup
                 :target {:kind :wasteland
                          :row 0
                          :col 3}
                 :territory-card-source :draw-pile-top})]
    (is (= :cup-variant-option-unavailable
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
