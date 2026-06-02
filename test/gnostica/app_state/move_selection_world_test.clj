(ns gnostica.app-state.move-selection-world-test
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

(deftest world-hand-card-can_stage_copied_composite_major
  (let [deck-order (deck-with-cards-at {0 "world"
                                        (board-card-position test-player-specs 3) "empress"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [(assoc rose-source-piece
                                                             :orientation :north)]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "world")
                     (app-state/select-move-piece :rose-scout))
        copy-db (app-state/select-move-world-copy piece-db 3)
        power-db (app-state/select-move-power copy-db :empress)
        orient-db (app-state/set-move-minion-orientation power-db :east)
        target-db (app-state/select-board-card orient-db 1)
        ready-db (app-state/set-move-orientation target-db :up)
        confirmed-db (app-state/confirm-move ready-db)
        zones (app-state/card-zones confirmed-db)
        source-piece (piece-by-id confirmed-db :rose-scout)
        created-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (= :world (app-state/move-power piece-db)))
    (is (= :world-copy (:stage (app-state/move-selection piece-db))))
    (is (some #(= 3 (:index %)) (app-state/move-world-copy-options piece-db)))
    (is (= :copied-power (:stage (app-state/move-selection copy-db))))
    (is (= [:empress :cup]
           (mapv :id (app-state/move-world-copied-power-options copy-db))))
    (is (= :minion-orientation (:stage (app-state/move-selection power-db))))
    (is (= :empress (app-state/move-world-copied-power power-db)))
    (is (= [{:power :orient-minion
             :piece-id :rose-scout
             :orientation :east}]
           (get-in orient-db [:move-selection :params :major-actions])))
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "world"
                     :piece-id :rose-scout}
            :copied-board-index 3
            :copied-power :empress
            :actions [{:power :orient-minion
                       :piece-id :rose-scout
                       :orientation :east}
                      {:power :cup
                       :piece-id :rose-scout
                       :target {:kind :territory
                                :board-index 1}
                       :orientation :up}]}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= :east (:orientation source-piece)))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 1
            :size :small
            :orientation :up}
           created-piece))
    (is (= ["world"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"world"} (map :id (:hand zones)))))
    (is (= "empress" (get-in (board-cell-by-index confirmed-db 3) [:card :id])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest world-hand-card-can_stage_copied_death_major
  (let [target-piece {:id :indigo-world-death-target
                      :player-id :indigo
                      :space-index 4
                      :size :medium
                      :orientation :north}
        deck-order (deck-with-cards-at {0 "world"
                                        (board-card-position test-player-specs 5) "death"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion
                                                      target-piece]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "world")
                     (app-state/select-move-piece :rose-rod-minion))
        copy-db (app-state/select-move-world-copy piece-db 5)
        power-db (app-state/select-move-power copy-db :death)
        action-count-db (app-state/set-move-sword-action-count power-db 2)
        first-db (-> action-count-db
                     (app-state/select-move-sword-target-kind :piece)
                     (app-state/select-move-target-piece :indigo-world-death-target)
                     (app-state/set-move-damage 1))
        ready-db (-> first-db
                     (app-state/select-move-sword-target-kind :piece)
                     (app-state/select-move-target-piece :indigo-world-death-target)
                     (app-state/set-move-damage 1))
        confirmed-db (app-state/confirm-move ready-db)
        zones (app-state/card-zones confirmed-db)]
    (is (= :copied-power (:stage (app-state/move-selection copy-db))))
    (is (= [:death :sword]
           (mapv :id (app-state/move-world-copied-power-options copy-db))))
    (is (= :sword-action-count (:stage (app-state/move-selection power-db))))
    (is (= :sword-target-kind (:stage (app-state/move-selection action-count-db))))
    (is (= :sword-target-kind (:stage (app-state/move-selection first-db))))
    (is (= [{:power :sword
             :target {:kind :piece
                      :piece-id :indigo-world-death-target}
             :damage 1
             :piece-id :rose-rod-minion}]
           (get-in first-db [:move-selection :params :major-actions])))
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "world"
                     :piece-id :rose-rod-minion}
            :copied-board-index 5
            :copied-power :death
            :sword-actions [{:target {:kind :piece
                                      :piece-id :indigo-world-death-target}
                             :damage 1
                             :piece-id :rose-rod-minion}
                            {:target {:kind :piece
                                      :piece-id :indigo-world-death-target}
                             :damage 1
                             :piece-id :rose-rod-minion}]}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (nil? (piece-by-id confirmed-db :indigo-world-death-target)))
    (is (= ["world"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"world"} (map :id (:hand zones)))))
    (is (= "death" (get-in (board-cell-by-index confirmed-db 5) [:card :id])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest world-territory-source-can_stage_copied_suit_power
  (let [deck-order (deck-with-cards-at {(board-card-position test-player-specs 0) "world"
                                        (board-card-position test-player-specs 3) "magician"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-source-piece]})
        piece-db (-> db
                     (app-state/select-move-source :activate-territory)
                     (app-state/select-move-piece :rose-scout))
        copy-db (app-state/select-move-world-copy piece-db 3)
        power-db (app-state/select-move-power copy-db :cup)
        target-db (app-state/select-board-card power-db 1)
        ready-db (app-state/set-move-orientation target-db :east)
        confirmed-db (app-state/confirm-move ready-db)
        created-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (= :world-copy (:stage (app-state/move-selection piece-db))))
    (is (= [:cup :rod :disc :sword]
           (mapv :id (app-state/move-world-copied-power-options copy-db))))
    (is (= :target (:stage (app-state/move-selection power-db))))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 0
                     :piece-id :rose-scout}
            :copied-board-index 3
            :copied-power :cup
            :target {:kind :territory
                     :board-index 1}
            :orientation :east
            :cup-variant :wild-suits}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 1
            :size :small
            :orientation :east}
           created-piece))
    (is (empty? (get-in confirmed-db [:game :discard-pile])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest world-copy-selection-rejects_minor_and_world_targets
  (let [deck-order (deck-with-cards-at {(board-card-position test-player-specs 0) "world"
                                        (board-card-position test-player-specs 3) "cups2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-source-piece]})
        piece-db (-> db
                     (app-state/select-move-source :activate-territory)
                     (app-state/select-move-piece :rose-scout))
        minor-db (app-state/select-move-world-copy piece-db 3)
        self-db (app-state/select-move-world-copy piece-db 0)]
    (is (= :world-copy (:stage (app-state/move-selection piece-db))))
    (is (= :invalid-world-copy
           (get-in minor-db [:move-selection :error :code])))
    (is (= :invalid-world-copy
           (get-in self-db [:move-selection :error :code])))
    (is (= (app-state/game piece-db)
           (app-state/game minor-db)))
    (is (= (app-state/game piece-db)
           (app-state/game self-db)))))
