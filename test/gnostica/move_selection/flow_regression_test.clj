(ns gnostica.move-selection.flow-regression-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.app-state :as app-state]
            [gnostica.board :as board]
            [gnostica.game-state :as game-state]
            [gnostica.test-support.app-db :refer [piece-by-id]]
            [gnostica.test-support.app-state :refer :all]
            [gnostica.test-support.deck :refer [board-card-position
                                                deck-starting-with
                                                deck-with-card-at
                                                deck-with-cards-at]]))

(deftest flow-split-regressions-preserve-basic-suit-staging
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["cups2"])}
             :demo-board-pieces [rose-hand-cup-territory-piece]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "cups2")
                     (app-state/select-move-piece :rose-striker))
        target-db (app-state/select-board-card piece-db 3)
        ready-db (app-state/set-move-orientation target-db :north)]
    (is (= :target (:stage (app-state/move-selection piece-db))))
    (is (= :orientation (:stage (app-state/move-selection target-db))))
    (is (app-state/move-ready? ready-db))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "cups2"
                     :piece-id :rose-striker}
            :target {:kind :territory
                     :board-index 3}
            :orientation :north
            :cup-variant :cup}
           (app-state/move-command ready-db)))))

(deftest flow-split-regressions-preserve-compound-advancement
  (testing "composite major advances after the first action"
    (let [deck-order (deck-with-card-at (board-card-position test-player-specs 0)
                                        "empress")
          db (app-state/initialize
              {:player-specs test-player-specs
               :game-options {:deck-order deck-order}
               :demo-board-pieces [(assoc rose-source-piece :orientation :north)]})
          orient-db (-> db
                        (app-state/select-move-source :activate-territory)
                        (app-state/select-move-piece :rose-scout)
                        (app-state/select-move-power :empress)
                        (app-state/set-move-minion-orientation :east))]
      (is (= :target (:stage (app-state/move-selection orient-db))))
      (is (= [{:power :orient-minion
               :piece-id :rose-scout
               :orientation :east}]
             (get-in orient-db [:move-selection :params :major-actions])))))
  (testing "sword major advances after its opener"
    (let [db (app-state/initialize
              {:player-specs test-player-specs
               :game-options {:deck-order (deck-starting-with ["tower"])}
               :demo-board-pieces [(assoc rose-rod-minion :orientation :north)
                                   indigo-rod-target]})
          orient-db (-> db
                        (app-state/select-move-source :play-hand-card)
                        (app-state/select-move-hand-card "tower")
                        (app-state/select-move-piece :rose-rod-minion)
                        (app-state/select-move-power :tower)
                        (app-state/set-move-minion-orientation :east))]
      (is (= :sword-target-kind (:stage (app-state/move-selection orient-db))))
      (is (= [{:power :orient-minion
               :piece-id :rose-rod-minion
               :orientation :east}]
             (get-in orient-db [:move-selection :params :major-actions]))))))

(deftest flow-split-regressions-preserve-devil-multi-orient
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
        first-action-db (-> db
                            (app-state/select-board-card 4)
                            (app-state/select-move-source :activate-territory)
                            (app-state/select-move-piece :rose-devil-minion)
                            (app-state/set-move-devil-action-count 2)
                            (app-state/select-move-target-piece :rose-devil-minion)
                            (app-state/set-move-orientation :east))
        ready-db (-> first-action-db
                     (app-state/select-move-target-piece :indigo-devil-target)
                     (app-state/set-move-orientation :south))
        confirmed-db (app-state/confirm-move ready-db)]
    (is (= :target-piece (:stage (app-state/move-selection first-action-db))))
    (is (= [{:power :orient-target
             :piece-id :rose-devil-minion
             :target {:kind :piece
                      :piece-id :rose-devil-minion}
             :orientation :east}]
           (get-in first-action-db [:move-selection :params :major-actions])))
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= :east (:orientation (piece-by-id confirmed-db :rose-devil-minion))))
    (is (= :south (:orientation (piece-by-id confirmed-db :indigo-devil-target))))))

(deftest flow-split-regressions-preserve-fool-play-advancement
  (let [draw-start (+ (* game-state/starting-hand-size (count test-player-specs))
                      board/board-card-count)
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at {0 "fool"
                                                             draw-start "cups2"})}
             :demo-board-pieces [rose-hand-cup-territory-piece]})
        ready-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "fool")
                     (app-state/select-move-piece :rose-striker)
                     (app-state/set-move-fool-reveal-count 1)
                     app-state/reveal-move-fool-card
                     app-state/play-move-fool-reveal
                     (app-state/select-board-card 3)
                     (app-state/set-move-orientation :north))]
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (nil? (get-in ready-db [:move-selection :params :fool-active-reveal])))
    (is (= [{:card-id "cups2"
             :choice :play
             :action {:power :cup
                      :piece-id :rose-striker
                      :play-command {:target {:kind :territory
                                              :board-index 3}
                                     :orientation :north
                                     :cup-variant :cup}}}]
           (get-in ready-db [:move-selection :params :fool-reveals])))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "fool"
                     :piece-id :rose-striker}
            :reveals [{:power :cup
                       :piece-id :rose-striker
                       :play-command {:target {:kind :territory
                                               :board-index 3}
                                      :orientation :north
                                      :cup-variant :cup}}]}
           (app-state/move-command ready-db)))))
