(ns gnostica.app-state.move-selection-cup-test
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

(deftest playing-a-cup-hand-card-confirms-through-game-state
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2"])}
                                  :demo-board-pieces [rose-hand-cup-territory-piece]})
        confirmed-db (-> db
                         (app-state/select-move-source :play-hand-card)
                         (app-state/select-move-hand-card "cups2")
                         (app-state/select-move-piece :rose-striker)
                         (app-state/select-board-card 3)
                         (app-state/set-move-orientation :west)
                         app-state/confirm-move)
        zones (app-state/card-zones confirmed-db)
        created-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= ["cups2"] (mapv :id (:discard-pile zones))))
    (is (= 1 (:discard-count zones)))
    (is (= "cups2" (:id (:discard-top-card zones))))
    (is (= 5 (count (:hand zones))))
    (is (not (some #{"cups2"} (map :id (:hand zones)))))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 3
            :size :small
            :orientation :west}
           created-piece))))
(deftest playing-a-cup-hand-card-can-create-an-enemy-piece
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2"])}
                                  :demo-board-pieces [rose-hand-cup-enemy-piece
                                                      indigo-rod-target]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "cups2")
                     (app-state/select-move-piece :rose-striker))
        target-db (app-state/select-move-target-piece piece-db :indigo-rod-target)
        confirmed-db (app-state/confirm-move target-db)
        zones (app-state/card-zones confirmed-db)
        created-piece (piece-by-id confirmed-db :indigo-small-1)]
    (is (= [:indigo-rod-target]
           (mapv :id (app-state/move-target-piece-options piece-db))))
    (is (= :confirm (:stage (app-state/move-selection target-db))))
    (is (= {:hand-card-id "cups2"
            :piece-id :rose-striker
            :target-piece-id :indigo-rod-target}
           (app-state/move-params target-db)))
	    (is (= {:player-id :rose
	            :source {:kind :hand-card
	                     :card-id "cups2"
	                     :piece-id :rose-striker}
	            :cup-variant :cup
	            :target {:kind :piece
	                     :piece-id :indigo-rod-target}}
	           (app-state/move-command target-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= ["cups2"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"cups2"} (map :id (:hand zones)))))
    (is (= {:id :indigo-small-1
            :player-id :indigo
            :space-index 4
            :size :small
            :orientation :north}
           created-piece))
    (is (= 3 (get-in confirmed-db [:game :pieces :stashes :indigo :small])))
    (is (= 3 (get-in confirmed-db [:game :players-by-id :indigo :stash :small])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest cup-browser-targets-are-limited-to-the-minion-target-space
  (let [far-enemy {:id :indigo-far-cup-target
                   :player-id :indigo
                   :space-index 8
                   :size :small
                   :orientation :north}
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2"])}
                                  :demo-board-pieces [rose-hand-cup-enemy-piece
                                                      indigo-rod-target
                                                      far-enemy]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "cups2")
                     (app-state/select-move-piece :rose-striker))
        invalid-board-db (app-state/select-board-card piece-db 8)
        invalid-piece-db (app-state/select-move-target-piece
                          piece-db
                          :indigo-far-cup-target)]
    (is (= [4]
           (mapv :index (app-state/move-target-board-options piece-db))))
    (is (= [:indigo-rod-target]
           (mapv :id (app-state/move-target-piece-options piece-db))))
    (is (= :invalid-cup-target
           (get-in invalid-board-db [:move-selection :error :code])))
    (is (= :invalid-target-piece
           (get-in invalid-piece-db [:move-selection :error :code])))
    (is (= :target (:stage (app-state/move-selection invalid-board-db))))
    (is (= :target (:stage (app-state/move-selection invalid-piece-db))))))
(deftest playing-a-cup-hand-card-can-create-a_wasteland_territory
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2" "coins2"])}
                                  :demo-board-pieces [rose-hand-piece]})
        source-db (app-state/select-move-source db :play-hand-card)
        card-db (app-state/select-move-hand-card source-db "cups2")
        piece-db (app-state/select-move-piece card-db :rose-striker)
        invalid-wasteland-db (app-state/select-move-wasteland-target piece-db 0 3)
        wasteland-db (app-state/select-move-wasteland-target piece-db 3 2)
        one-point-db (app-state/select-move-one-point-card wasteland-db "coins2")
        confirmed-db (app-state/confirm-move one-point-db)
        zones (app-state/card-zones confirmed-db)
        created-cell (last (app-state/board confirmed-db))]
    (is (= [{:kind :wasteland :row 3 :col 2 :id "wasteland-3-2"}]
           (mapv #(select-keys % [:kind :row :col :id])
                 (app-state/move-target-wasteland-options piece-db))))
    (is (= :invalid-wasteland-target
           (get-in invalid-wasteland-db [:move-selection :error :code])))
    (is (= :one-point-card (:stage (app-state/move-selection wasteland-db))))
    (is (= {:hand-card-id "cups2"
            :piece-id :rose-striker
            :target-wasteland {:kind :wasteland
                               :row 3
                               :col 2}}
           (app-state/move-params wasteland-db)))
    (is (some #{"coins2"}
              (mapv :id (app-state/move-one-point-card-options wasteland-db))))
    (is (not (some #{"cups2"}
                   (mapv :id (app-state/move-one-point-card-options wasteland-db)))))
	    (is (= {:player-id :rose
	            :source {:kind :hand-card
	                     :card-id "cups2"
	                     :piece-id :rose-striker}
	            :cup-variant :cup
	            :target {:kind :wasteland
	                     :row 3
	                     :col 2}
	            :territory-card-source :hand
	            :one-point-card-id "coins2"}
	           (app-state/move-command one-point-db)))
    (is (= :confirm (:stage (app-state/move-selection one-point-db))))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= ["cups2"] (mapv :id (:discard-pile zones))))
    (is (= 4 (count (:hand zones))))
    (is (not (some #{"cups2" "coins2"} (map :id (:hand zones)))))
    (is (= {:index 9
            :row 3
            :col 2
            :orientation :landscape
            :face :up
            :card (cards/card-by-id "coins2")}
           created-cell))))
(deftest cup-unbounded-board-source-can-place-into-full-territory
  (let [deck-order (deck-with-card-at (board-card-position test-player-specs 0)
                                      "empress")
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order deck-order}
             :demo-board-pieces [rose-source-piece
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
        oriented-db (-> db
                        (app-state/select-move-source :activate-territory)
                        (app-state/select-move-piece :rose-scout)
                        (app-state/select-move-power :cup)
                        (app-state/select-board-card 1)
                        (app-state/set-move-orientation :up))
        confirmed-db (app-state/confirm-move oriented-db)
        target-piece-ids (->> (app-state/board-pieces confirmed-db)
                              (filter #(= 1 (:space-index %)))
                              (mapv :id))]
    (is (= :cup (app-state/move-power oriented-db)))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 0
                     :piece-id :rose-scout}
            :cup-variant :cup-unbounded
            :target {:kind :territory
                     :board-index 1}
            :orientation :up}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= [:indigo-rod-target
            :rose-target-small
            :indigo-target-large
            :rose-small-1]
           target-piece-ids))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest wheel-cup-hand-card-can-use_top_draw_pile_for_wasteland_territory
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["wheeloffortune"])}
                                  :demo-board-pieces [rose-hand-piece]})
        draw-card (first (get-in db [:game :draw-pile]))
        wasteland-db (-> db
                         (app-state/select-move-source :play-hand-card)
                         (app-state/select-move-hand-card "wheeloffortune")
                         (app-state/select-move-piece :rose-striker)
                         (app-state/select-move-wasteland-target 3 2))
        draw-source-db (app-state/select-move-territory-card-source
                        wasteland-db
                        :draw-pile-top)
        confirmed-db (app-state/confirm-move draw-source-db)
        zones (app-state/card-zones confirmed-db)
        created-cell (last (app-state/board confirmed-db))]
    (is (= :territory-card-source
           (:stage (app-state/move-selection wasteland-db))))
    (is (= [:hand :draw-pile-top]
           (mapv :id (app-state/move-territory-card-source-options wasteland-db))))
    (is (= :confirm (:stage (app-state/move-selection draw-source-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "wheeloffortune"
                     :piece-id :rose-striker}
            :cup-variant :wheel-cup
            :target {:kind :wasteland
                     :row 3
                     :col 2}
            :territory-card-source :draw-pile-top}
           (app-state/move-command draw-source-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= draw-card (:card created-cell)))
    (is (= ["wheeloffortune"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"wheeloffortune"} (map :id (:hand zones)))))
    (is (= (dec (count (get-in db [:game :draw-pile])))
           (:draw-count zones)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest wheel-cup-hand-card-confirmation-uses-injected-shuffle-for-empty-draw-pile
  (let [seed 20260529
        draw-start (+ (board-card-position test-player-specs 0) board/board-card-count)
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order
                                                 (deck-with-cards-at
                                                  {0 "wheeloffortune"
                                                   draw-start "world"})}
                                  :demo-board-pieces [rose-hand-piece]})
        wheel-card (cards/card-by-id "wheeloffortune")
        prepared-discard (get-in db [:game :draw-pile])
        empty-draw-db (-> db
                          (assoc-in [:game :draw-pile] [])
                          (assoc-in [:game :discard-pile] prepared-discard))
        draw-source-db (-> empty-draw-db
                           (app-state/select-move-source :play-hand-card)
                           (app-state/select-move-hand-card "wheeloffortune")
                           (app-state/select-move-piece :rose-striker)
                           (app-state/select-move-wasteland-target 3 2)
                           (app-state/select-move-territory-card-source :draw-pile-top))
        first-confirmed-db (app-handlers/confirm-move-db draw-source-db {:shuffle-seed seed})
        second-confirmed-db (app-handlers/confirm-move-db draw-source-db {:shuffle-seed seed})
        shuffled-discard (deterministic-shuffle/shuffle-with-seed
                          seed
                          (conj prepared-discard wheel-card))
        expected-card (first shuffled-discard)
        zones (app-state/card-zones first-confirmed-db)
        created-cell (last (app-state/board first-confirmed-db))]
    (is (= (app-state/game first-confirmed-db)
           (app-state/game second-confirmed-db)))
    (is (= :confirm (:stage (app-state/move-selection draw-source-db))))
    (is (= "wheeloffortune"
           (get-in (app-state/move-command draw-source-db)
                   [:source :card-id])))
    (is (:ok? (get-in first-confirmed-db [:move-selection :last-result])))
    (is (= (:id expected-card) (get-in created-cell [:card :id])))
    (is (= (mapv :id (rest shuffled-discard))
           (mapv :id (:draw-pile zones))))
    (is (empty? (:discard-pile zones)))
    (is (not (some #{"wheeloffortune"} (map :id (:hand zones)))))
    (is (true? (get-in first-confirmed-db
                       [:move-selection :last-result :events 0 :reshuffled-discard?])))
    (is (game-schema/valid-game? (app-state/game first-confirmed-db)))))
