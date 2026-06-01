(ns gnostica.game-state.sun-test
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
