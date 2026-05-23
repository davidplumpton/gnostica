(ns gnostica.app-state-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.app-state :as app-state]
            [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as pieces]))

(def test-player-specs
  [{:id :rose}
   {:id :indigo}])

(def rose-source-piece
  {:id :rose-scout
   :player-id :rose
   :space-index 0
   :size :small
   :orientation :east})

(def rose-hand-piece
  {:id :rose-striker
   :player-id :rose
   :space-index 8
   :size :medium
   :orientation :south})

(defn- deck-starting-with [card-ids]
  (let [front-ids (set card-ids)]
    (vec
     (concat
      (map cards/card-by-id card-ids)
      (remove #(contains? front-ids (:id %)) cards/deck)))))

(defn- deck-with-card-at [index card-id]
  (let [card (cards/card-by-id card-id)
        remaining-cards (remove #(= card-id (:id %)) cards/deck)]
    (vec
     (concat
      (take index remaining-cards)
      [card]
      (drop index remaining-cards)))))

(defn- board-card-position [player-specs board-index]
  (+ (* game-state/starting-hand-size (count player-specs))
     board-index))

(defn- piece-by-id [db piece-id]
  (some #(when (= piece-id (:id %)) %)
        (app-state/board-pieces db)))

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
           (app-state/selected-board-index db)))
    (is (= :always
           (app-state/card-icon-mode db)))
    (is (false? (app-state/hotkey-help-open? db)))))

(deftest card-icon-mode-can-be-initialized-and-toggled
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}
                                  :card-icon-mode :popup})]
    (is (= :popup (app-state/card-icon-mode db)))
    (is (= :always
           (app-state/card-icon-mode (app-state/toggle-card-icon-mode db))))
    (is (= :always
           (app-state/card-icon-mode
            (app-state/set-card-icon-mode db :unknown))))))

(deftest hotkey-help-visibility-can-be-controlled
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}})]
    (is (false? (app-state/hotkey-help-open? db)))
    (is (true? (app-state/hotkey-help-open?
                (app-state/open-hotkey-help db))))
    (is (false? (app-state/hotkey-help-open?
                 (app-state/close-hotkey-help
                  (app-state/open-hotkey-help db)))))
    (is (false? (app-state/hotkey-help-open?
                 (app-state/set-hotkey-help-open db nil))))))

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
  (let [deck-order (deck-with-card-at (board-card-position test-player-specs 0)
                                      "cups2")
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-source-piece]})
        source-db (app-state/select-move-source db :activate-territory)
        piece-db (app-state/select-move-piece source-db :rose-scout)
        target-db (app-state/select-board-card piece-db 4)
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
            :target-board-index 4}
           (app-state/move-params target-db)))
    (is (= :confirm (:stage (app-state/move-selection oriented-db))))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 4
            :size :small
            :orientation :east}
           created-piece))
    (is (= 4 (get-in confirmed-db [:game :pieces :stashes :rose :small])))
    (is (= :source (:stage (app-state/move-selection confirmed-db))))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))))

(deftest playing-a-hand-card-stages-card_piece_and_target
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}})
        card-id (get-in db [:game :players-by-id :rose :hand 0 :id])
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

(deftest playing-a-cup-hand-card-confirms-through-game-state
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2"])}
                                  :demo-board-pieces [rose-hand-piece]})
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

(deftest rejected-cup-confirmation-keeps-staged-selection
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:shuffle-fn identity}
                                  :demo-board-pieces [rose-source-piece]})
        oriented-db (-> db
                        (app-state/select-move-source :activate-territory)
                        (app-state/select-move-piece :rose-scout)
                        (app-state/select-board-card 4)
                        (app-state/set-move-orientation :north))
        confirmed-db (app-state/confirm-move oriented-db)]
    (is (= :rejected (:stage (app-state/move-selection confirmed-db))))
    (is (= :source-card-not-cup
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= (app-state/move-params oriented-db)
           (app-state/move-params confirmed-db)))
    (is (= (app-state/game oriented-db)
           (app-state/game confirmed-db)))))

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
