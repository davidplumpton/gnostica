(ns gnostica.app-state.move-selection-manipulation-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.app.handlers :as app-handlers]
            [gnostica.app.subscriptions :as app-subscriptions]
            [gnostica.app-state :as app-state]
            [gnostica.board :as board]
            [gnostica.board-layout :as layout]
            [gnostica.cards :as cards]
            [gnostica.deterministic-shuffle :as deterministic-shuffle]
            [gnostica.fixtures :as fixtures]
            [gnostica.game-schema :as game-schema]
            [gnostica.game-state :as game-state]
            [gnostica.gesture-input :as gesture-input]
            [gnostica.move-selection.registry :as move-registry]
            [gnostica.pieces :as pieces]
            [gnostica.test-support.app-db :refer [board-cell-by-index
                                                  discard-card-target
                                                  hand-card-target
                                                  mark-game-player-eliminated
                                                  piece-by-id
                                                  piece-target
                                                  remove-board-cell
                                                  replace-game-player-hand
                                                  territory-target]]
            [gnostica.test-support.app-state :refer :all]
            [gnostica.test-support.deck :refer [board-card-position
                                                deck-starting-with
                                                deck-with-card-at
                                                deck-with-cards-at]]))

(deftest hierophant-hand-card-can_replace_a_target_piece_from_move_panel
  (let [rose-major-minion {:id :rose-major-minion
                           :player-id :rose
                           :space-index 4
                           :size :medium
                           :orientation :up}
        indigo-target {:id :indigo-major-target
                       :player-id :indigo
                       :space-index 4
                       :size :medium
                       :orientation :north}
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order
                                                 (deck-starting-with ["hierophant"])}
                                  :demo-board-pieces [rose-major-minion
                                                      indigo-target]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "hierophant")
                     (app-state/select-move-piece :rose-major-minion))
        target-db (app-state/select-move-target-piece piece-db :indigo-major-target)
        oriented-db (app-state/set-move-orientation target-db :west)
        confirmed-db (app-state/confirm-move oriented-db)
        replacement (piece-by-id confirmed-db :rose-medium-1)
        zones (app-state/card-zones confirmed-db)]
    (is (= :hierophant (app-state/move-power piece-db)))
    (is (= :target-piece (:stage (app-state/move-selection piece-db))))
    (is (= [:indigo-major-target :rose-major-minion]
           (sort (mapv :id (app-state/move-target-piece-options piece-db)))))
    (is (= :orientation (:stage (app-state/move-selection target-db))))
    (is (= :confirm (:stage (app-state/move-selection oriented-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "hierophant"
                     :piece-id :rose-major-minion}
            :target {:kind :piece
                     :piece-id :indigo-major-target}
            :orientation :west}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-medium-1
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :west}
           replacement))
    (is (nil? (piece-by-id confirmed-db :indigo-major-target)))
    (is (= ["hierophant"] (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest devil-territory-source-can_stage_retargeted_orientation_actions
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
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order
                            (deck-with-card-at
                             (board-card-position test-player-specs 4)
                             "devil")}
             :demo-board-pieces [devil-minion enemy-target]})
        source-db (-> db
                      (app-state/select-board-card 4)
                      (app-state/select-move-source :activate-territory)
                      (app-state/select-move-piece :rose-devil-minion))
        action-count-db (app-state/set-move-devil-action-count source-db 2)
        first-target-db (app-state/select-move-target-piece action-count-db
                                                            :rose-devil-minion)
        first-action-db (app-state/set-move-orientation first-target-db :east)
        second-target-db (app-state/select-move-target-piece first-action-db
                                                             :indigo-devil-target)
        ready-db (app-state/set-move-orientation second-target-db :south)
        confirmed-db (app-state/confirm-move ready-db)]
    (is (= :devil (app-state/move-power source-db)))
    (is (= :devil-action-count (:stage (app-state/move-selection source-db))))
    (is (= [1 2 3]
           (get-in (app-state/move-panel-view source-db)
                   [:controls :devil-action-count-options])))
    (is (= [:rose-devil-minion]
           (mapv :id (app-state/move-target-piece-options action-count-db))))
    (is (= :orientation (:stage (app-state/move-selection first-target-db))))
    (is (= :target-piece (:stage (app-state/move-selection first-action-db))))
    (is (= [{:power :orient-target
             :piece-id :rose-devil-minion
             :target {:kind :piece
                      :piece-id :rose-devil-minion}
             :orientation :east}]
           (get-in first-action-db [:move-selection :params :major-actions])))
    (is (some #{:indigo-devil-target}
              (mapv :id (app-state/move-target-piece-options first-action-db))))
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 4
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
                       :orientation :south}]}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= :east (:orientation (piece-by-id confirmed-db :rose-devil-minion))))
    (is (= :south (:orientation (piece-by-id confirmed-db :indigo-devil-target))))
    (is (= [:devil/piece-oriented :devil/piece-oriented]
           (mapv :type (get-in confirmed-db [:move-selection :last-result :events]))))
    (is (empty? (get-in confirmed-db [:game :discard-pile])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
