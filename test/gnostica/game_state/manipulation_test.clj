(ns gnostica.game-state.manipulation-test
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

(deftest devil-can-use_one_orientation_against_enemy_piece
  (let [devil-minion {:id :rose-devil-minion
                      :player-id :rose
                      :space-index 4
                      :size :medium
                      :orientation :up}
        enemy-target {:id :indigo-devil-target
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
        state (-> (:state (game-state/create-game
                           player-specs
                           {:deck-order (deck-starting-with ["devil"])}))
                  (game-state/with-board-pieces [devil-minion enemy-target]))
        cards-before (frequencies (all-card-ids state))
        rose-hand-before (player-hand-ids state :rose)
        {:keys [ok? state events]} (game-state/apply-devil-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "devil"
                                              :piece-id :rose-devil-minion}
                                     :actions [{:power :orient-target
                                                :piece-id :rose-devil-minion
                                                :target {:kind :piece
                                                         :piece-id :indigo-devil-target}
                                                :orientation :west}]})
        target-after (piece-by-id state :indigo-devil-target)]
    (is (some #{"devil"} rose-hand-before))
    (is ok?)
    (is (= 1 (count events)))
    (is (= [:devil/piece-oriented]
           (mapv :type events)))
    (is (= [:indigo-devil-target]
           (mapv #(get-in % [:target :piece-id]) events)))
    (is (= :west (:orientation target-after)))
    (is (= :north (get-in events [0 :from-orientation])))
    (is (= :west (get-in events [0 :to-orientation])))
    (is (= :indigo (get-in events [0 :target :player-id])))
    (is (= ["devil"] (mapv :id (:discard-pile state))))
    (is (not (some #{"devil"} (player-hand-ids state :rose))))
    (is (= cards-before (frequencies (all-card-ids state))))
    (is (game-schema/valid-game? state))))

(deftest devil-can-use_three_orientations_after_retargeting_the_acting_minion
  (let [devil-minion {:id :rose-devil-minion
                      :player-id :rose
                      :space-index 4
                      :size :medium
                      :orientation :up}
        first-enemy {:id :indigo-devil-target
                     :player-id :indigo
                     :space-index 5
                     :size :small
                     :orientation :north}
        second-enemy {:id :indigo-devil-second-target
                      :player-id :indigo
                      :space-index 5
                      :size :medium
                      :orientation :up}
        state (-> (:state (game-state/create-game
                           player-specs
                           {:deck-order (deck-starting-with ["devil"])}))
                  (game-state/with-board-pieces [devil-minion
                                                 first-enemy
                                                 second-enemy]))
        cards-before (frequencies (all-card-ids state))
        {:keys [ok? state events]} (game-state/apply-devil-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "devil"
                                              :piece-id :rose-devil-minion}
                                     :actions [{:power :orient-target
                                                :piece-id :rose-devil-minion
                                                :target {:kind :piece
                                                         :piece-id :rose-devil-minion}
                                                :orientation :east}
                                               {:power :orient-target
                                                :piece-id :rose-devil-minion
                                                :target {:kind :piece
                                                         :piece-id :indigo-devil-target}
                                                :orientation :south}
                                               {:power :orient-target
                                                :piece-id :rose-devil-minion
                                                :target {:kind :piece
                                                         :piece-id :indigo-devil-second-target}
                                                :orientation :west}]})]
    (is ok?)
    (is (= 3 (count events)))
    (is (= [:devil/piece-oriented
            :devil/piece-oriented
            :devil/piece-oriented]
           (mapv :type events)))
    (is (= [:rose-devil-minion
            :indigo-devil-target
            :indigo-devil-second-target]
           (mapv #(get-in % [:target :piece-id]) events)))
    (is (= [:up :north :up]
           (mapv :from-orientation events)))
    (is (= [:east :south :west]
           (mapv :to-orientation events)))
    (is (= :east (:orientation (piece-by-id state :rose-devil-minion))))
    (is (= :south (:orientation (piece-by-id state :indigo-devil-target))))
    (is (= :west (:orientation (piece-by-id state
                                            :indigo-devil-second-target))))
    (is (= [:rose :indigo :indigo]
           (mapv #(get-in % [:target :player-id]) events)))
    (is (= ["devil"] (mapv :id (:discard-pile state))))
    (is (not (some #{"devil"} (player-hand-ids state :rose))))
    (is (= cards-before (frequencies (all-card-ids state))))
    (is (game-schema/valid-game? state))))
