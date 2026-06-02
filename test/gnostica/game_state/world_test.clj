(ns gnostica.game-state.world-test
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
                          (board-card-position 3) "empress"})}))
        state (game-state/with-board-pieces
                state
                (vec (cons rose-cup-minion full-target-pieces)))
        {:keys [ok? state events]} (game-state/apply-world-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "world"}
                                     :copied-board-index 3
                                     :copied-power :empress
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

(deftest world-rejects-full-card-power-selector-that-copied-card-does-not-provide
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order
                        (deck-with-cards-at
                         {0 "world"
                          (board-card-position 3) "empress"})}))
        state (game-state/with-board-pieces state [rose-cup-minion])
        result (game-state/apply-world-move
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "world"}
                 :copied-board-index 3
                 :copied-power :death
                 :sword-actions [{:target {:kind :piece
                                           :piece-id :indigo-target}
                                  :damage 1
                                  :piece-id :rose-cup-minion}]})]
    (is (= :world-copied-power-unavailable
           (get-in result [:error :code])))
    (is (= [:cup :empress]
           (get-in result [:error :data :available-powers])))
    (is (not (contains? result :state)))))
(deftest world-territory-source-copies-magician-wild-suit
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order
                        (deck-with-cards-at
                         {(board-card-position 3) "world"
                          (board-card-position 5) "magician"})}))
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
                         {(board-card-position 3) "world"
                          (board-card-position 4) "cups2"})}))
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
