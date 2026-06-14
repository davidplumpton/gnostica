(ns gnostica.game-state.composite-test
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
(deftest chariot-can-apply-one-rod-action-with-one-source-cost
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["chariot"])}))
        state (game-state/with-board-pieces state [rose-rod-minion])
        {:keys [ok? state events]} (game-state/apply-chariot-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "chariot"}
                                     :actions [{:power :rod
                                                :piece-id :rose-rod-minion
                                                :mode :move-minion
                                                :distance 1
                                                :orientation :up}]})
        moved-piece (piece-by-id state :rose-rod-minion)]
    (is ok?)
    (is (= [:rod/minion-moved]
           (mapv :type events)))
    (is (= {:id :rose-rod-minion
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :up}
           moved-piece))
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
(deftest hanged-man-can-trade-hands-without-applying-rod
  (let [enemy-piece {:id :indigo-hanged-target
                     :player-id :indigo
                     :space-index 4
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
                                              :card-id "hangedman"
                                              :piece-id :rose-rod-minion}
                                     :hand-trade-target-piece-id
                                     :indigo-hanged-target})]
    (is ok?)
    (is (= [:hanged-man/hands-traded]
           (mapv :type events)))
    (is (= 3 (:space-index (piece-by-id state :rose-rod-minion))))
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
(deftest temperance-can-apply-one-cup-action-with-one-source-cost
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["temperance"])}))
        state (game-state/with-board-pieces state [rose-cup-minion])
        {:keys [ok? state events]} (game-state/apply-temperance-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "temperance"}
                                     :actions [{:power :cup
                                                :piece-id :rose-cup-minion
                                                :target {:kind :territory
                                                         :board-index 4}
                                                :orientation :north}]})
        created-piece (piece-by-id state :rose-small-1)]
    (is ok?)
    (is (= [:cup/small-piece-created]
           (mapv :type events)))
    (is (= 1 (count (:discard-pile state))))
    (is (= ["temperance"] (mapv :id (:discard-pile state))))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 4
            :size :small
            :orientation :north}
           created-piece))
    (is (nil? (piece-by-id state :rose-small-2)))
    (is (game-schema/valid-game? state))))
