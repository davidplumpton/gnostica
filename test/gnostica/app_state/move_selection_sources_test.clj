(ns gnostica.app-state.move-selection-sources-test
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

(deftest activating-a-board-territory-uses-board-and-piece-selections
  (let [deck-order (deck-with-card-at (board-card-position test-player-specs 0)
                                      "cups2")
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-source-piece]})
        source-db (app-state/select-move-source db :activate-territory)
        piece-db (app-state/select-move-piece source-db :rose-scout)
        target-db (app-state/select-board-card piece-db 1)
        oriented-db (app-state/set-move-orientation target-db :east)
        confirmed-db (app-state/confirm-move oriented-db)
        created-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (= :piece (:stage (app-state/move-selection source-db))))
    (is (= {:source-board-index 0}
           (app-state/move-params source-db)))
    (is (= :target (:stage (app-state/move-selection piece-db))))
    (is (= {:source-board-index 0
            :piece-id :rose-scout}
           (app-state/move-params piece-db)))
    (is (= :orientation (:stage (app-state/move-selection target-db))))
    (is (= {:source-board-index 0
            :piece-id :rose-scout
            :target-board-index 1}
           (app-state/move-params target-db)))
    (is (= :confirm (:stage (app-state/move-selection oriented-db))))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 1
            :size :small
            :orientation :east}
           created-piece))
    (is (= 3 (get-in confirmed-db [:game :pieces :stashes :rose :small])))
    (is (= 3 (get-in confirmed-db [:game :players-by-id :rose :stash :small])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))
    (is (= :source (:stage (app-state/move-selection confirmed-db))))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))))
(deftest playing-a-hand-card-stages-card_piece_and_target
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2"])}
                                  :demo-board-pieces [rose-hand-cup-territory-piece]})
        card-id "cups2"
        source-db (app-state/select-move-source db :play-hand-card)
        card-db (app-state/select-move-hand-card source-db card-id)
        piece-db (app-state/select-move-piece card-db :rose-striker)
        target-db (app-state/select-board-card piece-db 3)
        oriented-db (app-state/set-move-orientation target-db :north)]
    (is (= :hand-card (:stage (app-state/move-selection source-db))))
    (is (= :piece (:stage (app-state/move-selection card-db))))
    (is (= :target (:stage (app-state/move-selection piece-db))))
    (is (= :orientation (:stage (app-state/move-selection target-db))))
    (is (= :confirm (:stage (app-state/move-selection oriented-db))))
    (is (= {:hand-card-id card-id
            :piece-id :rose-striker
            :target-board-index 3
            :orientation :north}
           (app-state/move-params oriented-db)))))
(deftest selecting-new-territory-created-after-a-board-gap-uses-board-index
  (let [deck-order (deck-with-cards-at {0 "coins2"
                                        (board-card-position test-player-specs 3) "cups2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion]})
        gapped-db (remove-board-cell db 4)
        create-result (game-state/apply-cup-move
                       (app-state/game gapped-db)
                       {:player-id :rose
                        :source {:kind :territory
                                 :board-index 3
                                 :piece-id :rose-rod-minion}
                        :target {:kind :wasteland
                                 :row 1
                                 :col 1}
                        :one-point-card-id "coins2"})
        created-db (assoc gapped-db :game (:state create-result))
        selected-db (app-state/select-board-card created-db 9)
        source-db (-> created-db
                      (app-state/select-board-card 3)
                      (app-state/select-move-source :activate-territory)
                      (app-state/select-move-piece :rose-rod-minion))
        target-db (app-state/select-board-card source-db 9)]
    (is (:ok? create-result))
    (is (= 9 (get-in (app-state/selected-board-cell selected-db) [:index])))
    (is (= :orientation (:stage (app-state/move-selection target-db))))
    (is (= {:source-board-index 3
            :piece-id :rose-rod-minion
            :target-board-index 9}
           (app-state/move-params target-db)))))
