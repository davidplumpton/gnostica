(ns gnostica.game-state.cup-test
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
