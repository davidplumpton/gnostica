(ns gnostica.game-state.draw-test
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
(deftest fool-revealed-card_can_play_a_full_composite_major_power
  (let [draw-start (+ (hand-card-count (count player-specs))
                      board/board-card-count)
        enemy-piece {:id :indigo-fool-hanged-target
                     :player-id :indigo
                     :space-index 5
                     :size :medium
                     :orientation :north}
        initial-state (-> (:state (game-state/create-game
                                   player-specs
                                   {:deck-order
                                    (deck-with-cards-at
                                     {0 "fool"
                                      draw-start "hangedman"})}))
                          (game-state/with-board-pieces
                           [(assoc rose-rod-minion :orientation :east)
                            enemy-piece]))
        rose-hand-before (player-hand-ids initial-state :rose)
        indigo-hand-before (player-hand-ids initial-state :indigo)
        {:keys [ok? state events]} (game-state/apply-fool-move
                                    initial-state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "fool"}
                                     :reveals [{:power :hangedman
                                                :piece-id :rose-rod-minion
                                                :play-command
                                                {:rod {:piece-id :rose-rod-minion
                                                       :mode :move-minion
                                                       :distance 1}
                                                 :hand-trade-target-piece-id
                                                 :indigo-fool-hanged-target}}]
                                     :shuffle-fn identity})]
    (is ok?)
    (is (= [:fool/card-revealed
            :rod/minion-moved
            :hanged-man/hands-traded]
           (mapv :type events)))
    (is (= 4 (:space-index (piece-by-id state :rose-rod-minion))))
    (is (= indigo-hand-before (player-hand-ids state :rose)))
    (is (= (vec (remove #{"fool"} rose-hand-before))
           (player-hand-ids state :indigo)))
    (is (= ["fool" "hangedman"] (mapv :id (:discard-pile state))))
    (is (= (mapv :id (drop 1 (:draw-pile initial-state)))
           (mapv :id (:draw-pile state))))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
