(ns gnostica.app-state-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.app-state :as app-state]
            [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as pieces]))

(deftest initialize-builds-app-db-from-shared-game-state
  (let [hand-count (* game-state/starting-hand-size
                      (count app-state/default-player-specs))
        board-deck (drop hand-count cards/deck)
        db (app-state/initialize {:game-options {:shuffle-fn identity}})]
    (is (nil? (app-state/setup-error db)))
    (is (= (board/initial-board board-deck identity)
           (app-state/board db)))
    (is (= pieces/initial-pieces
           (app-state/board-pieces db)))
    (is (= :rose (:id (app-state/current-player db))))
    (is (= app-state/default-selected-board-index
           (app-state/selected-board-index db)))))

(deftest initialize-records-explicit-setup-errors
  (let [db (app-state/initialize {:player-specs [{:id :solo}]})]
    (is (= :invalid-player-count
           (:code (app-state/setup-error db))))
    (is (nil? (app-state/game db)))
    (is (empty? (app-state/board db)))
    (is (empty? (app-state/board-pieces db)))))

(deftest selecting-board-card-updates-selected-territory
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}})
        selected-db (app-state/select-board-card db 4)]
    (is (= 4 (app-state/selected-board-index selected-db)))
    (is (= (get (app-state/board db) 4)
           (app-state/selected-board-cell selected-db)))
    (is (= (pieces/pieces-for-space pieces/initial-pieces 4)
           (app-state/selected-board-pieces selected-db)))))

(deftest selecting-an-unknown-board-card-is-ignored
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}})]
    (is (= db (app-state/select-board-card db 99)))))

(deftest card-zones-derive-from-authoritative-game-state
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}})
        zones (app-state/card-zones db)
        expected-hand (take game-state/starting-hand-size cards/deck)
        expected-draw-count (- (count cards/deck)
                               (* game-state/starting-hand-size
                                  (count app-state/default-player-specs))
                               board/board-card-count)]
    (is (= (mapv :id expected-hand)
           (mapv :id (:hand zones))))
    (is (= expected-draw-count (:draw-count zones)))
    (is (= expected-draw-count (count (:draw-pile zones))))
    (is (zero? (:discard-count zones)))
    (is (empty? (:discard-pile zones)))
    (is (nil? (:discard-top-card zones)))))

(deftest card-zones-reflect-draw-and-discard-game-updates
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}})
        discarded-card (first (get-in db [:game :draw-pile]))
        updated-db (-> db
                       (update-in [:game :draw-pile] #(vec (rest %)))
                       (assoc-in [:game :discard-pile] [discarded-card]))
        zones (app-state/card-zones updated-db)]
    (is (= (dec (count (get-in db [:game :draw-pile])))
           (:draw-count zones)))
    (is (= 1 (:discard-count zones)))
    (is (= (:id discarded-card)
           (:id (:discard-top-card zones))))))

(defn- source-option [db source-id]
  (some #(when (= source-id (:id %)) %)
        (app-state/move-source-options db)))

(deftest move-source-options-reflect-current-game-state
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}})]
    (is (:enabled? (source-option db :activate-territory)))
    (is (:enabled? (source-option db :play-hand-card)))
    (is (:enabled? (source-option db :orient-piece)))
    (is (not (:enabled? (source-option db :draw-cards))))
    (is (not (:enabled? (source-option db :place-initial-small))))))

(deftest activating-a-board-territory-uses-board-and-piece-selections
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}})
        source-db (app-state/select-move-source db :activate-territory)
        piece-db (app-state/select-move-piece source-db :rose-scout)
        target-db (app-state/select-board-card piece-db 4)
        confirmed-db (app-state/confirm-move target-db)]
    (is (= :piece (:stage (app-state/move-selection source-db))))
    (is (= {:source-board-index 0}
           (app-state/move-params source-db)))
    (is (= :target (:stage (app-state/move-selection piece-db))))
    (is (= {:source-board-index 0
            :piece-id :rose-scout}
           (app-state/move-params piece-db)))
    (is (= :confirm (:stage (app-state/move-selection target-db))))
    (is (= {:source-board-index 0
            :piece-id :rose-scout
            :target-board-index 4}
           (app-state/move-params target-db)))
    (is (= :move-transition-unavailable
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= (app-state/game target-db)
           (app-state/game confirmed-db)))))

(deftest playing-a-hand-card-stages-card_piece_and_target
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}})
        card-id (get-in db [:game :players-by-id :rose :hand 0 :id])
        source-db (app-state/select-move-source db :play-hand-card)
        card-db (app-state/select-move-hand-card source-db card-id)
        piece-db (app-state/select-move-piece card-db :rose-striker)
        target-db (app-state/select-board-card piece-db 3)]
    (is (= :hand-card (:stage (app-state/move-selection source-db))))
    (is (= :piece (:stage (app-state/move-selection card-db))))
    (is (= :target (:stage (app-state/move-selection piece-db))))
    (is (= :confirm (:stage (app-state/move-selection target-db))))
    (is (= {:hand-card-id card-id
            :piece-id :rose-striker
            :target-board-index 3}
           (app-state/move-params target-db)))))

(deftest incomplete-moves-report_recoverable_errors
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}})
        source-db (app-state/select-move-source db :activate-territory)
        confirmed-db (app-state/confirm-move source-db)
        recovered-db (app-state/select-move-piece confirmed-db :rose-scout)]
    (is (= :piece (:stage (app-state/move-selection source-db))))
    (is (= :incomplete-move
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= (app-state/game source-db)
           (app-state/game confirmed-db)))
    (is (nil? (get-in recovered-db [:move-selection :error])))
    (is (= :target (:stage (app-state/move-selection recovered-db))))))

(deftest placing-an-initial-small-piece-is-available-with-no_board_pieces
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}
                                  :demo-board-pieces []})
        source-db (app-state/select-move-source db :place-initial-small)
        oriented-db (app-state/set-move-orientation source-db :north)]
    (is (not (:enabled? (source-option db :activate-territory))))
    (is (:enabled? (source-option db :place-initial-small)))
    (is (= :orientation (:stage (app-state/move-selection source-db))))
    (is (= {:target-board-index 0}
           (app-state/move-params source-db)))
    (is (= :confirm (:stage (app-state/move-selection oriented-db))))
    (is (= {:target-board-index 0
            :orientation :north}
           (app-state/move-params oriented-db)))))
