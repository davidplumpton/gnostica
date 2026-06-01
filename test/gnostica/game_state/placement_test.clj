(ns gnostica.game-state.placement-test
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
        state (state-with-pieces [rose-target-minion enemy-piece])
        result (game-state/apply-orient-move
                state
                {:player-id :rose
                 :piece-id :indigo-minion
                 :orientation :west})]
    (is (= :invalid-piece
           (get-in result [:error :code])))
    (is (false? (:ok? result)))
    (is (not (contains? result :state)))
    (is (= enemy-piece
           (piece-by-id state :indigo-minion)))))
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
