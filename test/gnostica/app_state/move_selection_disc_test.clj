(ns gnostica.app-state.move-selection-disc-test
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

(deftest disc-hand-card-can-grow-a_target_piece
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["coins2"])}
                                  :demo-board-pieces [rose-rod-minion
                                                      indigo-rod-target]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "coins2")
                     (app-state/select-move-piece :rose-rod-minion))
        kind-db (app-state/select-move-disc-target-kind piece-db :piece)
        target-db (app-state/select-move-target-piece kind-db :indigo-rod-target)
        confirmed-db (app-state/confirm-move target-db)
        zones (app-state/card-zones confirmed-db)
        grown-piece (piece-by-id confirmed-db :indigo-medium-1)]
    (is (= :disc (app-state/move-power piece-db)))
    (is (= :disc-target-kind (:stage (app-state/move-selection piece-db))))
    (is (= [:rose-rod-minion :indigo-rod-target]
           (mapv :id (app-state/move-target-piece-options kind-db))))
    (is (= :confirm (:stage (app-state/move-selection target-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "coins2"
                     :piece-id :rose-rod-minion}
            :disc-variant :disc
            :target {:kind :piece
                     :piece-id :indigo-rod-target}}
           (app-state/move-command target-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :indigo-medium-1
            :player-id :indigo
            :space-index 4
            :size :medium
            :orientation :north}
           grown-piece))
    (is (= ["coins2"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"coins2"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest disc-hand-card-can-grow-a_territory_with_hand_replacement
  (let [deck-order (deck-with-cards-at {0 "coins2"
                                        1 "cupsking"
                                        (board-card-position test-player-specs 4) "cups2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion]})
        kind-db (-> db
                    (app-state/select-move-source :play-hand-card)
                    (app-state/select-move-hand-card "coins2")
                    (app-state/select-move-piece :rose-rod-minion)
                    (app-state/select-move-disc-target-kind :territory))
        target-db (app-state/select-board-card kind-db 4)
        replacement-db (app-state/select-move-replacement-card target-db "cupsking")
        confirmed-db (app-state/confirm-move replacement-db)
        zones (app-state/card-zones confirmed-db)
        grown-cell (board-cell-by-index confirmed-db 4)]
    (is (= [4] (mapv :index (app-state/move-target-board-options kind-db))))
    (is (= :replacement-card (:stage (app-state/move-selection target-db))))
    (is (= ["cupsking"]
           (mapv :id (app-state/move-replacement-card-options target-db))))
    (is (= :confirm (:stage (app-state/move-selection replacement-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "coins2"
                     :piece-id :rose-rod-minion}
            :disc-variant :disc
            :target {:kind :territory
                     :board-index 4}
            :replacement-card-source :hand
            :replacement-card-id "cupsking"}
           (app-state/move-command replacement-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= "cupsking" (get-in grown-cell [:card :id])))
    (is (= ["coins2" "cups2"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"coins2" "cupsking"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest disc-targets-surviving-territory-after-destruction-gap-by-board-index
  (let [targeting-piece {:id :rose-gap-disc-minion
                         :player-id :rose
                         :space-index 2
                         :size :medium
                         :orientation :south}
        deck-order (deck-with-cards-at {0 "coins2"
                                        1 "cupsking"
                                        (board-card-position test-player-specs 5) "cups2"
                                        (board-card-position test-player-specs 6) "swords2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [targeting-piece]})
        gapped-db (remove-board-cell db 4)
        kind-db (-> gapped-db
                    (app-state/select-move-source :play-hand-card)
                    (app-state/select-move-hand-card "coins2")
                    (app-state/select-move-piece :rose-gap-disc-minion)
                    (app-state/select-move-disc-target-kind :territory))
        target-db (app-state/select-board-card kind-db 5)]
    (is (= [5] (mapv :index (app-state/move-target-board-options kind-db))))
    (is (= :replacement-card (:stage (app-state/move-selection target-db))))
    (is (= {:hand-card-id "coins2"
            :piece-id :rose-gap-disc-minion
            :disc-target-kind :territory
            :target-board-index 5}
           (app-state/move-params target-db)))
    (is (= ["cupsking"]
           (mapv :id (app-state/move-replacement-card-options target-db))))))
(deftest star-disc-can_orient_minion_before_discard_pile_replacement
  (let [deck-order (deck-with-cards-at {0 "star"
                                        (board-card-position test-player-specs 4) "cupsking"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [(assoc rose-rod-minion
                                                             :orientation :north)]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "star")
                     (app-state/select-move-piece :rose-rod-minion))
        oriented-db (app-state/set-move-minion-orientation piece-db :east)
        kind-db (app-state/select-move-disc-target-kind oriented-db :territory)
        target-db (-> kind-db
                      (app-state/select-board-card 4))
        source-db (app-state/select-move-territory-card-source target-db :discard-pile)
        replacement-db (app-state/select-move-replacement-card source-db "star")
        confirmed-db (app-state/confirm-move replacement-db)
        zones (app-state/card-zones confirmed-db)
        grown-cell (board-cell-by-index confirmed-db 4)
        oriented-piece (piece-by-id confirmed-db :rose-rod-minion)]
    (is (= :minion-orientation
           (:stage (app-state/move-selection piece-db))))
    (is (true? (get-in (app-state/move-panel-view piece-db)
                       [:controls :disc-minion-orientation-required?])))
    (is (= :disc-target-kind
           (:stage (app-state/move-selection oriented-db))))
    (is (= [4] (mapv :index (app-state/move-target-board-options kind-db))))
    (is (= [:hand :discard-pile]
           (mapv :id (app-state/move-territory-card-source-options target-db))))
    (is (= :replacement-card-source
           (:stage (app-state/move-selection target-db))))
    (is (= ["star"]
           (mapv :id (app-state/move-replacement-card-options source-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "star"
                     :piece-id :rose-rod-minion}
            :disc-variant :disc-from-discard
            :minion-orientation :east
            :target {:kind :territory
                     :board-index 4}
            :replacement-card-source :discard-pile
            :replacement-card-id "star"}
           (app-state/move-command replacement-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= :east (:orientation oriented-piece)))
    (is (= "star" (get-in grown-cell [:card :id])))
    (is (= ["cupsking"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"star"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest strength-disc-can_stage_two_piece_growth_actions
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["strength"])}
                                  :demo-board-pieces [(assoc rose-rod-minion
                                                             :size :small)]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "strength")
                     (app-state/select-move-piece :rose-rod-minion))
        action-db (app-state/set-move-disc-action-count piece-db 2)
        kind-db (app-state/select-move-disc-target-kind action-db :piece)
        target-db (app-state/select-move-target-piece kind-db :rose-rod-minion)
        confirmed-db (app-state/confirm-move target-db)
        zones (app-state/card-zones confirmed-db)
        grown-piece (piece-by-id confirmed-db :rose-large-1)]
    (is (= :disc-action-count (:stage (app-state/move-selection piece-db))))
    (is (= [1 2]
           (get-in (app-state/move-panel-view piece-db)
                   [:controls :disc-action-count-options])))
    (is (= :disc-target-kind (:stage (app-state/move-selection action-db))))
    (is (= :confirm (:stage (app-state/move-selection target-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "strength"
                     :piece-id :rose-rod-minion}
            :disc-variant :disc
            :disc-actions [{:target {:kind :piece
                                     :piece-id :rose-rod-minion}}
                           {:target {:kind :piece
                                     :piece-id :rose-rod-minion}}]}
           (app-state/move-command target-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-large-1
            :player-id :rose
            :space-index 3
            :size :large
            :orientation :east}
           grown-piece))
    (is (= ["strength"] (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest strength-disc-can_stage_two_territory_growth_actions
  (let [deck-order (deck-with-cards-at {0 "strength"
                                        1 "star"
                                        2 "cupsking"
                                        (board-card-position test-player-specs 4) "cups2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion]})
        target-db (-> db
                      (app-state/select-move-source :play-hand-card)
                      (app-state/select-move-hand-card "strength")
                      (app-state/select-move-piece :rose-rod-minion)
                      (app-state/set-move-disc-action-count 2)
                      (app-state/select-move-disc-target-kind :territory)
                      (app-state/select-board-card 4))
        replacement-db (app-state/select-move-replacement-card target-db "star")
        confirmed-db (app-state/confirm-move replacement-db)
        zones (app-state/card-zones confirmed-db)
        grown-cell (board-cell-by-index confirmed-db 4)
        replacement-option-ids (mapv :id (app-state/move-replacement-card-options target-db))]
    (is (some #{"star"} replacement-option-ids))
    (is (not (some #{"cupsking"} replacement-option-ids)))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "strength"
                     :piece-id :rose-rod-minion}
            :disc-variant :disc
            :disc-actions [{:target {:kind :territory
                                     :board-index 4}
                            :replacement-card-source :hand
                            :replacement-card-id "star"}
                           {:target {:kind :territory
                                     :board-index 4}
                            :replacement-card-source :hand
                            :replacement-card-id "star"}]}
           (app-state/move-command replacement-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= "star" (get-in grown-cell [:card :id])))
    (is (= ["strength" "cups2"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"strength" "star"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest rejected-disc-confirmation-keeps-staged-selection
  (let [deck-order (deck-with-cards-at {0 "coins2"
                                        1 "cupsking"
                                        (board-card-position test-player-specs 4) "cups2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion]})
        replacement-db (-> db
                           (app-state/select-move-source :play-hand-card)
                           (app-state/select-move-hand-card "coins2")
                           (app-state/select-move-piece :rose-rod-minion)
                           (app-state/select-move-disc-target-kind :territory)
                           (app-state/select-board-card 4)
                           (app-state/select-move-replacement-card "cupsking"))
        stale-game (game-state/with-board-pieces
                     (app-state/game replacement-db)
                     [rose-rod-minion indigo-rod-target])
        stale-db (assoc replacement-db :game stale-game)
        confirmed-db (app-state/confirm-move stale-db)]
    (is (= :confirm (:stage (app-state/move-selection stale-db))))
    (is (= :rejected (:stage (app-state/move-selection confirmed-db))))
    (is (= :target-territory-occupied-by-enemy
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= (app-state/move-params stale-db)
           (app-state/move-params confirmed-db)))
    (is (= stale-game
           (app-state/game confirmed-db)))))
