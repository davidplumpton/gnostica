(ns gnostica.app-state.move-selection-rod-test
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

(deftest rod-territory-source-can-move-the-acting-minion
  (let [deck-order (deck-with-card-at (board-card-position test-player-specs 0)
                                      "wands2")
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-source-piece]})
        source-db (app-state/select-move-source db :activate-territory)
        piece-db (app-state/select-move-piece source-db :rose-scout)
        mode-db (app-state/select-move-rod-mode piece-db :move-minion)
        distance-db (app-state/set-move-distance mode-db 1)
        oriented-db (app-state/set-move-orientation distance-db :south)
        confirmed-db (app-state/confirm-move oriented-db)
        moved-piece (piece-by-id confirmed-db :rose-scout)]
    (is (= :rod (app-state/move-power piece-db)))
    (is (= :rod-mode (:stage (app-state/move-selection piece-db))))
    (is (= :distance (:stage (app-state/move-selection mode-db))))
    (is (= :orientation (:stage (app-state/move-selection distance-db))))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 0
                     :piece-id :rose-scout}
            :rod-variant :rod
            :mode :move-minion
            :distance 1
            :orientation :south}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-scout
            :player-id :rose
            :space-index 1
            :size :small
            :orientation :south}
           moved-piece))))
(deftest rod-unbounded-territory-source-can-move-into-full-territory
  (let [deck-order (deck-with-card-at (board-card-position test-player-specs 3)
                                      "emperor")
        full-pieces [rose-rod-minion
                     {:id :rose-target-medium
                      :player-id :rose
                      :space-index 4
                      :size :medium
                      :orientation :up}
                     indigo-rod-target
                     {:id :indigo-target-large
                      :player-id :indigo
                      :space-index 4
                      :size :large
                      :orientation :south}]
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces full-pieces})
        oriented-db (-> db
                        (app-state/select-move-source :activate-territory)
                        (app-state/select-move-piece :rose-rod-minion)
                        (app-state/select-move-power :rod)
                        (app-state/select-move-rod-mode :move-minion)
                        (app-state/set-move-distance 1)
                        (app-state/set-move-orientation :south))
        confirmed-db (app-state/confirm-move oriented-db)
        moved-piece (piece-by-id confirmed-db :rose-rod-minion)
        destination-pieces (filter #(= 4 (:space-index %))
                                   (app-state/board-pieces confirmed-db))]
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 3
                     :piece-id :rose-rod-minion}
            :rod-variant :rod-unbounded
            :mode :move-minion
            :distance 1
            :orientation :south}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-rod-minion
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :south}
           moved-piece))
    (is (= 4 (count destination-pieces)))))
(deftest rod-hand-card-can-push-a-piece-and_discard_source
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["wands2"])}
                                  :demo-board-pieces [rose-rod-minion
                                                      indigo-rod-target]})
        target-db (-> db
                      (app-state/select-move-source :play-hand-card)
                      (app-state/select-move-hand-card "wands2")
                      (app-state/select-move-piece :rose-rod-minion)
                      (app-state/select-move-rod-mode :push-piece)
                      (app-state/select-move-target-piece :indigo-rod-target))
        distance-db (app-state/set-move-distance target-db 1)
        confirmed-db (app-state/confirm-move distance-db)
        zones (app-state/card-zones confirmed-db)
        pushed-piece (piece-by-id confirmed-db :indigo-rod-target)]
    (is (= :distance (:stage (app-state/move-selection target-db))))
    (is (= :confirm (:stage (app-state/move-selection distance-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "wands2"
                     :piece-id :rose-rod-minion}
            :rod-variant :rod
            :mode :push-piece
            :distance 1
            :target {:kind :piece
                     :piece-id :indigo-rod-target}}
           (app-state/move-command distance-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :indigo-rod-target
            :player-id :indigo
            :space-index 5
            :size :small
            :orientation :north}
           pushed-piece))
    (is (= ["wands2"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"wands2"} (map :id (:hand zones)))))))
(deftest rod-hand-card-can-push-a-territory_from_board_click_target
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["wands2"])}
                                  :demo-board-pieces [(assoc rose-rod-minion :space-index 4)
                                                      rose-rod-target]})
        target-card (get-in db [:game :board 5 :card])
        target-db (-> db
                      (app-state/select-move-source :play-hand-card)
                      (app-state/select-move-hand-card "wands2")
                      (app-state/select-move-piece :rose-rod-minion)
                      (app-state/select-move-rod-mode :push-territory)
                      (app-state/select-board-card 5))
        distance-db (app-state/set-move-distance target-db 1)
        confirmed-db (app-state/confirm-move distance-db)
        moved-cell (board-cell-by-index confirmed-db 5)
        old-target-piece (piece-by-id confirmed-db :rose-rod-target)]
    (is (= 5 (app-state/selected-board-index target-db)))
    (is (= :distance (:stage (app-state/move-selection target-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "wands2"
                     :piece-id :rose-rod-minion}
            :rod-variant :rod
            :mode :push-territory
            :target {:kind :territory
                     :board-index 5}
            :distance 1}
           (app-state/move-command distance-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:index 5
            :row 1
            :col 3
            :orientation :portrait
            :face :up
            :card target-card}
           moved-cell))
    (is (= {:id :rose-rod-target
            :player-id :rose
            :space {:kind :wasteland
                    :row 1
                    :col 2}
            :size :small
            :orientation :north}
           old-target-piece))))
(deftest rejected-rod-confirmation-keeps-staged-selection
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["wands2"])}
                                  :demo-board-pieces [(assoc rose-rod-minion
                                                             :orientation :up)]})
        oriented-db (-> db
                        (app-state/select-move-source :play-hand-card)
                        (app-state/select-move-hand-card "wands2")
                        (app-state/select-move-piece :rose-rod-minion)
                        (app-state/select-move-rod-mode :move-minion)
                        (app-state/set-move-distance 1)
                        (app-state/set-move-orientation :east))
        confirmed-db (app-state/confirm-move oriented-db)]
    (is (= :rejected (:stage (app-state/move-selection confirmed-db))))
    (is (= :rod-minion-upright
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= (app-state/move-params oriented-db)
           (app-state/move-params confirmed-db)))
    (is (= (app-state/game oriented-db)
           (app-state/game confirmed-db)))))
