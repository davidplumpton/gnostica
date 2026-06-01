(ns gnostica.app-state.move-selection-sword-test
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

(deftest sword-hand-card-can_shrink_own_piece_and_reorient_survivor
  (let [target-piece {:id :rose-sword-target
                      :player-id :rose
                      :space-index 4
                      :size :medium
                      :orientation :north}
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["swords2"])}
                                  :demo-board-pieces [rose-rod-minion target-piece]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "swords2")
                     (app-state/select-move-piece :rose-rod-minion))
        kind-db (app-state/select-move-sword-target-kind piece-db :piece)
        target-db (app-state/select-move-target-piece kind-db :rose-sword-target)
        damage-db (app-state/set-move-damage target-db 1)
        oriented-db (app-state/set-move-orientation damage-db :west)
        confirmed-db (app-state/confirm-move oriented-db)
        zones (app-state/card-zones confirmed-db)
        shrunk-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (= :sword (app-state/move-power piece-db)))
    (is (= :sword-target-kind (:stage (app-state/move-selection piece-db))))
    (is (= [:piece :territory]
           (mapv :id (get-in (app-state/move-panel-view piece-db)
                             [:controls :sword-target-kind-options]))))
    (is (= [:rose-rod-minion :rose-sword-target]
           (mapv :id (app-state/move-target-piece-options kind-db))))
    (is (= :damage (:stage (app-state/move-selection target-db))))
    (is (= [1 2] (app-state/move-damage-options target-db)))
    (is (= :orientation (:stage (app-state/move-selection damage-db))))
    (is (true? (get-in (app-state/move-panel-view damage-db)
                       [:controls :sword-orientation-available?])))
    (is (= :confirm (:stage (app-state/move-selection oriented-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "swords2"
                     :piece-id :rose-rod-minion}
            :target {:kind :piece
                     :piece-id :rose-sword-target}
            :damage 1
            :orientation :west
            :sword-variant :sword}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 4
            :size :small
            :orientation :west}
           shrunk-piece))
    (is (= ["swords2"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"swords2"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest sword-hand-card-can_shrink_territory_with_hand_replacement
  (let [deck-order (deck-with-cards-at {0 "swords2"
                                        1 "cups2"
                                        (board-card-position test-player-specs 4) "cupsking"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion]})
        kind-db (-> db
                    (app-state/select-move-source :play-hand-card)
                    (app-state/select-move-hand-card "swords2")
                    (app-state/select-move-piece :rose-rod-minion)
                    (app-state/select-move-sword-target-kind :territory))
        target-db (app-state/select-board-card kind-db 4)
        damage-db (app-state/set-move-damage target-db 1)
        replacement-db (app-state/select-move-replacement-card damage-db "cups2")
        confirmed-db (app-state/confirm-move replacement-db)
        zones (app-state/card-zones confirmed-db)
        shrunk-cell (board-cell-by-index confirmed-db 4)
        replacement-option-ids (mapv :id (app-state/move-replacement-card-options damage-db))]
    (is (= [4] (mapv :index (app-state/move-target-board-options kind-db))))
    (is (= :damage (:stage (app-state/move-selection target-db))))
    (is (= [1 2] (app-state/move-damage-options target-db)))
    (is (= :replacement-card (:stage (app-state/move-selection damage-db))))
    (is (some #{"cups2"} replacement-option-ids))
    (is (not (some #{"swords2"} replacement-option-ids)))
    (is (= :confirm (:stage (app-state/move-selection replacement-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "swords2"
                     :piece-id :rose-rod-minion}
            :target {:kind :territory
                     :board-index 4}
            :damage 1
            :replacement-card-source :hand
            :replacement-card-id "cups2"
            :sword-variant :sword}
           (app-state/move-command replacement-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= "cups2" (get-in shrunk-cell [:card :id])))
    (is (= ["swords2" "cupsking"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"swords2" "cups2"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest tower-sword-can_stage_discard_pile_territory_replacement
  (let [deck-order (deck-with-cards-at {0 "tower"
                                        (board-card-position test-player-specs 4) "sun"})
        discarded-card (cards/card-by-id "cupsking")
        db (-> (app-state/initialize {:player-specs test-player-specs
                                      :game-options {:deck-order deck-order}
                                      :demo-board-pieces [rose-rod-minion]})
               (update-in [:game :draw-pile]
                          #(vec (remove (fn [card]
                                          (= (:id discarded-card) (:id card)))
                                        %)))
               (assoc-in [:game :discard-pile] [discarded-card]))
        target-db (-> db
                      (app-state/select-move-source :play-hand-card)
                      (app-state/select-move-hand-card "tower")
                      (app-state/select-move-piece :rose-rod-minion)
                      (app-state/select-move-power :sword)
                      (app-state/select-move-sword-target-kind :territory)
                      (app-state/select-board-card 4))
        damage-db (app-state/set-move-damage target-db 1)
        source-db (app-state/select-move-territory-card-source damage-db :discard-pile)
        replacement-db (app-state/select-move-replacement-card source-db "cupsking")
        confirmed-db (app-state/confirm-move replacement-db)
        zones (app-state/card-zones confirmed-db)
        shrunk-cell (board-cell-by-index confirmed-db 4)]
    (is (= [:hand :discard-pile]
           (mapv :id (app-state/move-territory-card-source-options damage-db))))
    (is (= :replacement-card-source
           (:stage (app-state/move-selection damage-db))))
    (is (= ["cupsking"]
           (mapv :id (app-state/move-replacement-card-options source-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "tower"
                     :piece-id :rose-rod-minion}
            :target {:kind :territory
                     :board-index 4}
            :damage 1
            :replacement-card-source :discard-pile
            :replacement-card-id "cupsking"
            :sword-variant :sword-from-discard}
           (app-state/move-command replacement-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= "cupsking" (get-in shrunk-cell [:card :id])))
    (is (= ["tower" "sun"] (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest justice-hand-card-can_stage_hand_trade_then_sword
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["justice"])}
                                  :demo-board-pieces [rose-rod-minion
                                                      indigo-rod-target]})
        trade-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "justice")
                     (app-state/select-move-piece :rose-rod-minion)
                     (app-state/select-move-power :justice)
                     (app-state/select-move-target-piece :indigo-rod-target))
        kind-db (app-state/select-move-sword-target-kind trade-db :piece)
        target-db (app-state/select-move-target-piece kind-db :indigo-rod-target)
        damage-db (app-state/set-move-damage target-db 1)
        confirmed-db (app-state/confirm-move damage-db)
        zones (app-state/card-zones confirmed-db)]
    (is (= [:justice :sword]
           (mapv :id (app-state/move-power-options
                      (-> db
                          (app-state/select-move-source :play-hand-card)
                          (app-state/select-move-hand-card "justice")
                          (app-state/select-move-piece :rose-rod-minion))))))
    (is (= :sword-target-kind (:stage (app-state/move-selection trade-db))))
    (is (= [{:power :trade-hand
             :piece-id :rose-rod-minion
             :target {:kind :piece
                      :piece-id :indigo-rod-target}}]
           (get-in trade-db [:move-selection :params :major-actions])))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "justice"
                     :piece-id :rose-rod-minion}
            :hand-trade-target {:kind :piece
                                :piece-id :indigo-rod-target}
            :target {:kind :piece
                     :piece-id :indigo-rod-target}
            :damage 1
            :sword-variant :sword}
           (app-state/move-command damage-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (nil? (piece-by-id confirmed-db :indigo-rod-target)))
    (is (= ["justice"] (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest justice-hand-card-can-stage-hand-trade-only
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["justice"])}
                                  :demo-board-pieces [rose-rod-minion
                                                      indigo-rod-target]})
        rose-hand-before (mapv :id (get-in db [:game :players-by-id :rose :hand]))
        indigo-hand-before (mapv :id (get-in db [:game :players-by-id :indigo :hand]))
        power-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "justice")
                     (app-state/select-move-piece :rose-rod-minion)
                     (app-state/select-move-power :justice))
        trade-only-db (app-state/set-move-major-action-count power-db 1)
        ready-db (app-state/select-move-target-piece trade-only-db
                                                     :indigo-rod-target)
        confirmed-db (app-state/confirm-move ready-db)
        zones (app-state/card-zones confirmed-db)]
    (is (= [{:id 1 :label "Trade only"}
            {:id 2 :label "Use both"}]
           (get-in (app-state/move-panel-view power-db)
                   [:controls :major-action-count-options])))
    (is (= 2 (get-in (app-state/move-panel-view power-db)
                     [:controls :major-action-count])))
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "justice"
                     :piece-id :rose-rod-minion}
            :hand-trade-target {:kind :piece
                                :piece-id :indigo-rod-target}}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= [:justice/hands-traded]
           (mapv :type (get-in confirmed-db [:move-selection :last-result :events]))))
    (is (= indigo-hand-before
           (mapv :id (get-in confirmed-db [:game :players-by-id :rose :hand]))))
    (is (= (vec (remove #{"justice"} rose-hand-before))
           (mapv :id (get-in confirmed-db [:game :players-by-id :indigo :hand]))))
    (is (some? (piece-by-id confirmed-db :indigo-rod-target)))
    (is (= ["justice"] (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest death-hand-card-can_stage_two_sword_actions
  (let [target-piece {:id :indigo-death-target
                      :player-id :indigo
                      :space-index 4
                      :size :medium
                      :orientation :north}
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["death"])}
                                  :demo-board-pieces [rose-rod-minion target-piece]})
        action-db (-> db
                      (app-state/select-move-source :play-hand-card)
                      (app-state/select-move-hand-card "death")
                      (app-state/select-move-piece :rose-rod-minion)
                      (app-state/select-move-power :death)
                      (app-state/set-move-sword-action-count 2))
        first-db (-> action-db
                     (app-state/select-move-sword-target-kind :piece)
                     (app-state/select-move-target-piece :indigo-death-target)
                     (app-state/set-move-damage 1))
        second-db (-> first-db
                      (app-state/select-move-sword-target-kind :piece)
                      (app-state/select-move-target-piece :indigo-death-target)
                      (app-state/set-move-damage 1))
        confirmed-db (app-state/confirm-move second-db)
        zones (app-state/card-zones confirmed-db)]
    (is (= :sword-action-count (:stage (app-state/move-selection
                                        (-> db
                                            (app-state/select-move-source :play-hand-card)
                                            (app-state/select-move-hand-card "death")
                                            (app-state/select-move-piece :rose-rod-minion)
                                            (app-state/select-move-power :death))))))
    (is (= [1 2]
           (get-in (app-state/move-panel-view action-db)
                   [:controls :sword-action-count-options])))
    (is (= :sword-target-kind (:stage (app-state/move-selection first-db))))
    (is (= [{:power :sword
             :target {:kind :piece
                      :piece-id :indigo-death-target}
             :damage 1
             :piece-id :rose-rod-minion}]
           (get-in first-db [:move-selection :params :major-actions])))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "death"
                     :piece-id :rose-rod-minion}
            :sword-actions [{:target {:kind :piece
                                      :piece-id :indigo-death-target}
                             :damage 1
                             :piece-id :rose-rod-minion}
                            {:target {:kind :piece
                                      :piece-id :indigo-death-target}
                             :damage 1
                             :piece-id :rose-rod-minion}]}
           (app-state/move-command second-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (nil? (piece-by-id confirmed-db :indigo-death-target)))
    (is (= ["death"] (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest tower-hand-card-can_stage_orient_then_sword
  (let [tower-minion (assoc rose-rod-minion :orientation :north)
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["tower"])}
                                  :demo-board-pieces [tower-minion indigo-rod-target]})
        orient-db (-> db
                      (app-state/select-move-source :play-hand-card)
                      (app-state/select-move-hand-card "tower")
                      (app-state/select-move-piece :rose-rod-minion)
                      (app-state/select-move-power :tower)
                      (app-state/set-move-minion-orientation :east))
        kind-db (app-state/select-move-sword-target-kind orient-db :piece)
        target-db (app-state/select-move-target-piece kind-db :indigo-rod-target)
        damage-db (app-state/set-move-damage target-db 1)
        confirmed-db (app-state/confirm-move damage-db)
        source-piece (piece-by-id confirmed-db :rose-rod-minion)
        zones (app-state/card-zones confirmed-db)]
    (is (= [:tower :sword]
           (mapv :id (app-state/move-power-options
                      (-> db
                          (app-state/select-move-source :play-hand-card)
                          (app-state/select-move-hand-card "tower")
                          (app-state/select-move-piece :rose-rod-minion))))))
    (is (= :sword-target-kind (:stage (app-state/move-selection orient-db))))
    (is (= [{:power :orient-minion
             :piece-id :rose-rod-minion
             :orientation :east}]
           (get-in orient-db [:move-selection :params :major-actions])))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "tower"
                     :piece-id :rose-rod-minion}
            :minion-orientation :east
            :target {:kind :piece
                     :piece-id :indigo-rod-target}
            :damage 1
            :sword-variant :sword-from-discard}
           (app-state/move-command damage-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= :east (:orientation source-piece)))
    (is (nil? (piece-by-id confirmed-db :indigo-rod-target)))
    (is (= ["tower"] (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest moon-hand-card-can_stage_rod_then_sword
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["moon"])}
                                  :demo-board-pieces [rose-rod-minion
                                                      indigo-rod-target]})
        rod-db (-> db
                   (app-state/select-move-source :play-hand-card)
                   (app-state/select-move-hand-card "moon")
                   (app-state/select-move-piece :rose-rod-minion)
                   (app-state/select-move-power :moon)
                   (app-state/select-move-rod-mode :move-minion)
                   (app-state/set-move-distance 1)
                   (app-state/set-move-orientation :up))
        kind-db (app-state/select-move-sword-target-kind rod-db :piece)
        target-db (app-state/select-move-target-piece kind-db :indigo-rod-target)
        damage-db (app-state/set-move-damage target-db 1)
        confirmed-db (app-state/confirm-move damage-db)
        moved-piece (piece-by-id confirmed-db :rose-rod-minion)
        zones (app-state/card-zones confirmed-db)]
    (is (= [:moon :rod :sword]
           (mapv :id (app-state/move-power-options
                      (-> db
                          (app-state/select-move-source :play-hand-card)
                          (app-state/select-move-hand-card "moon")
                          (app-state/select-move-piece :rose-rod-minion))))))
    (is (= :sword-target-kind (:stage (app-state/move-selection rod-db))))
    (is (= [{:power :rod
             :mode :move-minion
             :distance 1
             :orientation :up
             :piece-id :rose-rod-minion}]
           (get-in rod-db [:move-selection :params :major-actions])))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "moon"
                     :piece-id :rose-rod-minion}
            :rod {:mode :move-minion
                  :distance 1
                  :orientation :up
                  :piece-id :rose-rod-minion}
            :sword {:target {:kind :piece
                             :piece-id :indigo-rod-target}
                    :damage 1
                    :piece-id :rose-rod-minion}}
           (app-state/move-command damage-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-rod-minion
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :up}
           moved-piece))
    (is (nil? (piece-by-id confirmed-db :indigo-rod-target)))
    (is (= ["moon"] (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest rejected-sword-confirmation-keeps-staged-selection
  (let [deck-order (deck-with-cards-at {0 "swords2"
                                        1 "cups2"
                                        (board-card-position test-player-specs 4) "cupsking"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion]})
        replacement-db (-> db
                           (app-state/select-move-source :play-hand-card)
                           (app-state/select-move-hand-card "swords2")
                           (app-state/select-move-piece :rose-rod-minion)
                           (app-state/select-move-sword-target-kind :territory)
                           (app-state/select-board-card 4)
                           (app-state/set-move-damage 1)
                           (app-state/select-move-replacement-card "cups2"))
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
(deftest incomplete-moves-report_recoverable_errors
  (let [deck-order (deck-with-card-at (board-card-position test-player-specs 0)
                                      "cups2")
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-source-piece]})
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
