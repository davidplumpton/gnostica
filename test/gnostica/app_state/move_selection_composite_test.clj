(ns gnostica.app-state.move-selection-composite-test
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

(deftest empress-territory-source-confirms-ordered-major-actions
  (let [deck-order (deck-with-card-at (board-card-position test-player-specs 0)
                                      "empress")
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order deck-order}
             :demo-board-pieces [(assoc rose-source-piece :orientation :north)
                                 (assoc indigo-rod-target :space-index 1)
                                 {:id :rose-target-small
                                  :player-id :rose
                                  :space-index 1
                                  :size :small
                                  :orientation :north}
                                 {:id :indigo-target-large
                                  :player-id :indigo
                                  :space-index 1
                                  :size :large
                                  :orientation :west}]})
        orient-db (-> db
                      (app-state/select-move-source :activate-territory)
                      (app-state/select-move-piece :rose-scout)
                      (app-state/select-move-power :empress)
                      (app-state/set-move-minion-orientation :east))
        target-db (app-state/select-board-card orient-db 1)
        ready-db (app-state/set-move-orientation target-db :up)
        confirmed-db (app-state/confirm-move ready-db)
        source-piece (piece-by-id confirmed-db :rose-scout)
        target-piece-ids (->> (app-state/board-pieces confirmed-db)
                              (filter #(= 1 (:space-index %)))
                              (mapv :id))]
    (is (= :target (:stage (app-state/move-selection orient-db))))
    (is (= [{:power :orient-minion
             :piece-id :rose-scout
             :orientation :east}]
           (get-in orient-db [:move-selection :params :major-actions])))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 0
                     :piece-id :rose-scout}
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
    (is (= [:indigo-rod-target
            :rose-target-small
            :indigo-target-large
            :rose-small-1]
           target-piece-ids))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest lovers-hand-card-confirms-promoted-ordered-major-actions
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["lovers"])}
             :demo-board-pieces [(assoc rose-rod-minion :orientation :east)]})
        rod-db (-> db
                   (app-state/select-move-source :play-hand-card)
                   (app-state/select-move-hand-card "lovers")
                   (app-state/select-move-piece :rose-rod-minion)
                   (app-state/select-move-power :lovers)
                   (app-state/select-move-rod-mode :move-minion)
                   (app-state/set-move-distance 1)
                   (app-state/set-move-orientation :east))
        target-db (app-state/select-board-card rod-db 5)
        ready-db (app-state/set-move-orientation target-db :up)
        confirmed-db (app-state/confirm-move ready-db)
        zones (app-state/card-zones confirmed-db)
        moved-piece (piece-by-id confirmed-db :rose-rod-minion)
        created-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (= :target (:stage (app-state/move-selection rod-db))))
    (is (= [{:power :rod
             :mode :move-minion
             :distance 1
             :orientation :east
             :piece-id :rose-rod-minion}]
           (get-in rod-db [:move-selection :params :major-actions])))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "lovers"
                     :piece-id :rose-rod-minion}
            :actions [{:power :rod
                       :mode :move-minion
                       :distance 1
                       :orientation :east
                       :piece-id :rose-rod-minion}
                      {:power :cup
                       :piece-id :rose-rod-minion
                       :target {:kind :territory
                                :board-index 5}
                       :orientation :up}]}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-rod-minion
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :east}
           moved-piece))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 5
            :size :small
            :orientation :up}
           created-piece))
    (is (= ["lovers"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"lovers"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest temperance-hand-card-filters-each-cup-action-to-the-target-space
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["temperance"])}
             :demo-board-pieces [rose-hand-cup-enemy-piece]})
        first-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "temperance")
                     (app-state/select-move-piece :rose-striker)
                     (app-state/select-move-power :temperance))
        invalid-first-db (app-state/select-board-card first-db 8)
        second-db (-> first-db
                      (app-state/select-board-card 4)
                      (app-state/set-move-orientation :north))
        invalid-second-db (app-state/select-board-card second-db 8)]
    (is (= [4]
           (mapv :index (app-state/move-target-board-options first-db))))
    (is (= :invalid-cup-target
           (get-in invalid-first-db [:move-selection :error :code])))
    (is (= [{:power :cup
             :piece-id :rose-striker
             :target {:kind :territory
                      :board-index 4}
             :orientation :north}]
           (get-in second-db [:move-selection :params :major-actions])))
    (is (= [4]
           (mapv :index (app-state/move-target-board-options second-db))))
    (is (= :invalid-cup-target
           (get-in invalid-second-db [:move-selection :error :code])))
    (is (= :target (:stage (app-state/move-selection invalid-second-db))))))
(deftest hanged-man-hand-card-can-stage-hand-trade-only
  (let [hanged-target {:id :indigo-hanged-target
                       :player-id :indigo
                       :space-index 4
                       :size :small
                       :orientation :north}
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["hangedman"])}
                                  :demo-board-pieces [rose-rod-minion
                                                      hanged-target]})
        rose-hand-before (mapv :id (get-in db [:game :players-by-id :rose :hand]))
        indigo-hand-before (mapv :id (get-in db [:game :players-by-id :indigo :hand]))
        power-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "hangedman")
                     (app-state/select-move-piece :rose-rod-minion)
                     (app-state/select-move-power :hanged-man))
        trade-only-db (app-state/set-move-major-action-count power-db 1)
        ready-db (app-state/select-move-target-piece trade-only-db
                                                     :indigo-hanged-target)
        confirmed-db (app-state/confirm-move ready-db)
        zones (app-state/card-zones confirmed-db)]
    (is (= [{:type :hand-card}
            {:type :piece}
            {:type :power}
            {:type :major-action-count
             :power :hanged-man}
            {:type :target-piece
             :power :hanged-man
             :action-power :trade-hand}]
           (move-control-group-summary trade-only-db)))
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "hangedman"
                     :piece-id :rose-rod-minion}
            :actions [{:power :trade-hand
                       :piece-id :rose-rod-minion
                       :target {:kind :piece
                                :piece-id :indigo-hanged-target}}]}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= [:hanged-man/hands-traded]
           (mapv :type (get-in confirmed-db [:move-selection :last-result :events]))))
    (is (= 3 (:space-index (piece-by-id confirmed-db :rose-rod-minion))))
    (is (= indigo-hand-before
           (mapv :id (get-in confirmed-db [:game :players-by-id :rose :hand]))))
    (is (= (vec (remove #{"hangedman"} rose-hand-before))
           (mapv :id (get-in confirmed-db [:game :players-by-id :indigo :hand]))))
    (is (= ["hangedman"] (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
