(ns gnostica.app-state.move-selection-sun-test
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

(deftest sun-piece-cup-target-stays-distinct-from-disc-target-in-move-panel-view
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["sun"])}
             :demo-board-pieces [rose-hand-cup-enemy-piece
                                 rose-rod-target
                                 indigo-rod-target]})
        cup-target-db (-> db
                          (app-state/select-move-source :play-hand-card)
                          (app-state/select-move-hand-card "sun")
                          (app-state/select-move-piece :rose-striker)
                          (app-state/select-move-power :sun)
                          (app-state/select-move-target-piece :indigo-rod-target))
        skip-db (app-state/select-move-sun-disc-mode cup-target-db :skip)
        disc-piece-db (-> cup-target-db
                          (app-state/select-move-sun-disc-mode :piece)
                          (app-state/select-move-target-piece :rose-striker))
        skip-controls (:controls (app-state/move-panel-view skip-db))
        disc-piece-controls (:controls (app-state/move-panel-view disc-piece-db))]
    (is (= :indigo-rod-target
           (get-in skip-controls [:sun-cup-target-piece :piece-id])))
    (is (nil? (:sun-disc-target-piece skip-controls)))
    (is (= :indigo-rod-target
           (get-in disc-piece-controls [:sun-cup-target-piece :piece-id])))
    (is (= :rose-striker
           (get-in disc-piece-controls [:sun-disc-target-piece :piece-id])))
    (is (= {:target-piece-id :indigo-rod-target
            :sun-disc-mode :piece
            :sun-disc-target-piece-id :rose-striker}
           (select-keys (app-state/move-params disc-piece-db)
                        [:target-piece-id
                         :sun-disc-mode
                         :sun-disc-target-piece-id])))))
(deftest sun-hand-card-can_stage_created_piece_shortcut
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["sun"])}
                                  :demo-board-pieces [rose-hand-cup-territory-piece]})
        power-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "sun")
                     (app-state/select-move-piece :rose-striker))
        staged-db (app-state/select-move-power power-db :sun)
        target-db (app-state/select-board-card staged-db 3)
        oriented-db (app-state/set-move-orientation target-db :south)
        disc-db (app-state/select-move-sun-disc-mode oriented-db :created-piece)
        confirmed-db (app-state/confirm-move disc-db)
        zones (app-state/card-zones confirmed-db)
        created-piece (piece-by-id confirmed-db :rose-medium-1)]
    (is (= :power (:stage (app-state/move-selection power-db))))
    (is (= [:cup :disc :sun]
           (mapv :id (app-state/move-power-options power-db))))
    (is (= :sun (app-state/move-power staged-db)))
    (is (= :orientation (:stage (app-state/move-selection target-db))))
    (is (= :sun-disc-mode (:stage (app-state/move-selection oriented-db))))
    (is (= [:skip :created-piece :piece :territory]
           (mapv :id (app-state/move-sun-disc-mode-options oriented-db))))
    (is (= :confirm (:stage (app-state/move-selection disc-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "sun"
                     :piece-id :rose-striker}
            :cup {:target {:kind :territory
                           :board-index 3}
                  :orientation :south}
            :disc {:target {:kind :created-piece}
                   :orientation :south}}
           (app-state/move-command disc-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-medium-1
            :player-id :rose
            :space-index 3
            :size :medium
            :orientation :south}
           created-piece))
    (is (= ["sun"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"sun"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest sun-hand-card-can_stage_created_territory_shortcut
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with
                                                              ["sun" "cupsking"])}
                                  :demo-board-pieces [rose-hand-piece]})
        wasteland-db (-> db
                         (app-state/select-move-source :play-hand-card)
                         (app-state/select-move-hand-card "sun")
                         (app-state/select-move-piece :rose-striker)
                         (app-state/select-move-power :sun)
                         (app-state/select-move-wasteland-target 3 2))
        mode-db (app-state/select-move-sun-disc-mode wasteland-db :created-territory)
        replacement-db (app-state/select-move-replacement-card mode-db "cupsking")
        confirmed-db (app-state/confirm-move replacement-db)
        zones (app-state/card-zones confirmed-db)
        created-cell (last (app-state/board confirmed-db))]
    (is (= :sun-disc-mode (:stage (app-state/move-selection wasteland-db))))
    (is (= [:skip :created-territory :piece :territory]
           (mapv :id (app-state/move-sun-disc-mode-options wasteland-db))))
    (is (= ["cupsking"]
           (mapv :id (app-state/move-replacement-card-options mode-db))))
    (is (= :confirm (:stage (app-state/move-selection replacement-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "sun"
                     :piece-id :rose-striker}
            :cup {:target {:kind :wasteland
                           :row 3
                           :col 2}}
            :disc {:target {:kind :created-territory}
                   :replacement-card-source :hand
                   :replacement-card-id "cupsking"}}
           (app-state/move-command replacement-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:row 3
            :col 2}
           (select-keys created-cell [:row :col])))
    (is (= "cupsking" (get-in created-cell [:card :id])))
    (is (= ["sun"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"sun" "cupsking"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest sun-hand-card-can_stage_existing_piece_disc_reorientation
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["sun"])}
                                  :demo-board-pieces [rose-hand-cup-enemy-piece
                                                      rose-rod-target
                                                      indigo-rod-target]})
        cup-db (-> db
                   (app-state/select-move-source :play-hand-card)
                   (app-state/select-move-hand-card "sun")
                   (app-state/select-move-piece :rose-striker)
                   (app-state/select-move-power :sun)
                   (app-state/select-move-target-piece :indigo-rod-target))
        mode-db (app-state/select-move-sun-disc-mode cup-db :piece)
        target-db (app-state/select-move-target-piece mode-db :rose-striker)
        oriented-db (app-state/set-move-sun-disc-orientation target-db :west)
        confirmed-db (app-state/confirm-move oriented-db)
        grown-piece (piece-by-id confirmed-db :rose-large-1)]
    (is (= :sun-disc-mode (:stage (app-state/move-selection cup-db))))
    (is (= [:skip :piece :territory]
           (mapv :id (app-state/move-sun-disc-mode-options cup-db))))
    (is (true? (app-state/move-sun-disc-orientation-available? target-db)))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "sun"
                     :piece-id :rose-striker}
            :cup {:target {:kind :piece
                           :piece-id :indigo-rod-target}}
            :disc {:target {:kind :piece
                            :piece-id :rose-striker}
                   :orientation :west}}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-large-1
            :player-id :rose
            :space-index 3
            :size :large
            :orientation :west}
           grown-piece))
    (is (= ["sun"] (mapv :id (get-in confirmed-db [:game :discard-pile]))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
