(ns gnostica.game-state.sword-major-test
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
(deftest justice-can-trade-hands-without-applying-sword
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
                                     :hand-trade-target-piece-id
                                     :indigo-justice-target})]
    (is ok?)
    (is (= [:justice/hands-traded]
           (mapv :type events)))
    (is (= indigo-hand-before (player-hand-ids state :rose)))
    (is (= (vec (remove #{"justice"} rose-hand-before))
           (player-hand-ids state :indigo)))
    (is (= enemy-piece (piece-by-id state :indigo-justice-target)))
    (is (= ["justice"] (mapv :id (:discard-pile state))))
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
(deftest death-row-col-territory-targets-can-apply-sequentially
  (let [second-minion {:id :rose-death-second-minion
                       :player-id :rose
                       :space-index 6
                       :size :small
                       :orientation :east}
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-cards-at
                                     {0 "death"
                                      (board-card-position 4) "cups2"
                                      (board-card-position 7) "coins2"})}))
        state (game-state/with-board-pieces
                state
                [rose-sword-minion second-minion])
        {:keys [ok? state events]} (game-state/apply-sword-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "death"}
                                     :sword-actions [{:piece-id :rose-sword-minion
                                                      :target {:kind :territory
                                                               :row 1
                                                               :col 1}
                                                      :damage 1}
                                                     {:piece-id :rose-death-second-minion
                                                      :target {:kind :territory
                                                               :row 2
                                                               :col 1}
                                                      :damage 1}]})]
    (is ok?)
    (is (= [:sword/territory-destroyed
            :sword/territory-destroyed]
           (mapv :type events)))
    (is (every? nil? (map :shortcut? events)))
    (is (nil? (board-cell-by-index state 4)))
    (is (nil? (board-cell-by-index state 7)))
    (is (= ["death" "cups2" "coins2"]
           (mapv :id (:discard-pile state))))
    (is (game-schema/valid-game? state))))
(deftest death-row-col-territory-targets-can-use-shortcut
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-cards-at
                                     {0 "death"
                                      (board-card-position 4) "cupsking"})}))
        state (game-state/with-board-pieces state [rose-sword-minion])
        {:keys [ok? state events]} (game-state/apply-sword-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "death"}
                                     :sword-actions [{:piece-id :rose-sword-minion
                                                      :target {:kind :territory
                                                               :row 1
                                                               :col 1}
                                                      :damage 1}
                                                     {:piece-id :rose-sword-minion
                                                      :target {:kind :territory
                                                               :row 1
                                                               :col 1}
                                                      :damage 1}]})]
    (is ok?)
    (is (= [:sword/territory-destroyed]
           (mapv :type events)))
    (is (= 2 (get-in events [0 :action-count])))
    (is (true? (get-in events [0 :shortcut?])))
    (is (nil? (board-cell-by-index state 4)))
    (is (= ["death" "cupsking"]
           (mapv :id (:discard-pile state))))
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
