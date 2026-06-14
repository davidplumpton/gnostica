(ns gnostica.app-state.move-selection-magician-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.app-state :as app-state]
            [gnostica.cards :as cards]
            [gnostica.game-schema :as game-schema]
            [gnostica.game-state :as game-state]
            [gnostica.test-support.app-db :refer [piece-by-id]]
            [gnostica.test-support.app-state :refer [indigo-rod-target
                                                     rose-hand-cup-territory-piece
                                                     rose-rod-minion
                                                     rose-source-piece
                                                     test-player-specs]]
            [gnostica.test-support.deck :refer [board-card-position
                                                deck-starting-with
                                                deck-with-card-at]]
            [gnostica.test-support.game-state :refer [all-card-ids]]))

(def magician-power-options [:cup :rod :disc :sword])

(defn- magician-hand-db [pieces]
  (app-state/initialize
   {:player-specs test-player-specs
    :game-options {:deck-order (deck-starting-with ["magician"])}
    :demo-board-pieces pieces}))

(defn- hand-piece-db [pieces piece-id]
  (-> (magician-hand-db pieces)
      (app-state/select-move-source :play-hand-card)
      (app-state/select-move-hand-card "magician")
      (app-state/select-move-piece piece-id)))

(defn- assert-card-accounting [game]
  (is (= (count cards/deck) (count (all-card-ids game))))
  (is (= (count cards/deck) (count (set (all-card-ids game))))))

(defn- assert-confirmed-magician-hand-source
  [{:keys [ready-db confirmed-db command-kind variant-key source]}]
  (let [command (app-state/move-command ready-db)
        result (get-in confirmed-db [:move-selection :last-result])
        event (first (:events result))
        zones (app-state/card-zones confirmed-db)
        game (app-state/game confirmed-db)]
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (game-state/valid-command? command-kind command))
    (is (= :wild-suits (get command variant-key)))
    (is (:ok? result))
    (is (game-state/valid-result? result))
    (is (= :wild-suits (get event variant-key)))
    (is (= source (:source event)))
    (is (= ["magician"] (mapv :id (:discard-pile zones))))
    (is (= 1 (:discard-count zones)))
    (is (= "magician" (:id (:discard-top-card zones))))
    (is (= 5 (count (:hand zones))))
    (is (not (some #{"magician"} (map :id (:hand zones)))))
    (assert-card-accounting game)
    (is (game-schema/valid-game? game))))

(def indigo-magician-sword-target
  (assoc indigo-rod-target
         :id :indigo-magician-sword-target
         :size :medium))

(def magician-hand-confirmation-cases
  [{:label "Cup"
    :power :cup
    :piece-id :rose-striker
    :pieces [rose-hand-cup-territory-piece]
    :command-kind :cup
    :variant-key :cup-variant
    :stage (fn [piece-db]
             (-> piece-db
                 (app-state/select-move-power :cup)
                 (app-state/select-board-card 3)
                 (app-state/set-move-orientation :west)))
    :assert-state (fn [confirmed-db]
                    (is (= {:id :rose-small-1
                            :player-id :rose
                            :space-index 3
                            :size :small
                            :orientation :west}
                           (piece-by-id confirmed-db :rose-small-1))))}
   {:label "Rod"
    :power :rod
    :piece-id :rose-rod-minion
    :pieces [rose-rod-minion]
    :command-kind :rod
    :variant-key :rod-variant
    :stage (fn [piece-db]
             (-> piece-db
                 (app-state/select-move-power :rod)
                 (app-state/select-move-rod-mode :move-minion)
                 (app-state/set-move-distance 1)
                 (app-state/set-move-orientation :south)))
    :assert-state (fn [confirmed-db]
                    (is (= {:id :rose-rod-minion
                            :player-id :rose
                            :space-index 4
                            :size :medium
                            :orientation :south}
                           (piece-by-id confirmed-db :rose-rod-minion))))}
   {:label "Disc"
    :power :disc
    :piece-id :rose-rod-minion
    :pieces [rose-rod-minion]
    :command-kind :disc
    :variant-key :disc-variant
    :stage (fn [piece-db]
             (-> piece-db
                 (app-state/select-move-power :disc)
                 (app-state/select-move-disc-target-kind :piece)
                 (app-state/select-move-target-piece :rose-rod-minion)))
    :assert-state (fn [confirmed-db]
                    (is (= {:id :rose-large-1
                            :player-id :rose
                            :space-index 3
                            :size :large
                            :orientation :east}
                           (piece-by-id confirmed-db :rose-large-1))))}
   {:label "Sword"
    :power :sword
    :piece-id :rose-rod-minion
    :pieces [rose-rod-minion indigo-magician-sword-target]
    :command-kind :sword
    :variant-key :sword-variant
    :stage (fn [piece-db]
             (-> piece-db
                 (app-state/select-move-power :sword)
                 (app-state/select-move-sword-target-kind :piece)
                 (app-state/select-move-target-piece :indigo-magician-sword-target)
                 (app-state/set-move-damage 1)))
    :assert-state (fn [confirmed-db]
                    (is (= {:id :indigo-small-1
                            :player-id :indigo
                            :space-index 4
                            :size :small
                            :orientation :north}
                           (piece-by-id confirmed-db :indigo-small-1))))}])

(deftest magician-hand-card-exposes-and-confirms-each-suit-choice
  (doseq [{:keys [label power piece-id pieces command-kind variant-key stage
                  assert-state]}
          magician-hand-confirmation-cases]
    (testing label
      (let [piece-db (hand-piece-db pieces piece-id)
            source {:kind :hand-card
                    :card-id "magician"
                    :piece-id piece-id}
            ready-db (stage piece-db)
            confirmed-db (app-state/confirm-move ready-db)]
        (is (= magician-power-options
               (mapv :id (app-state/move-power-options piece-db))))
        (is (= power (app-state/move-power ready-db)))
        (assert-confirmed-magician-hand-source
         {:ready-db ready-db
          :confirmed-db confirmed-db
          :command-kind command-kind
          :variant-key variant-key
          :source source})
        (assert-state confirmed-db)))))

(deftest magician-territory-source-exposes-and-confirms-wild-suit-choice
  (let [deck-order (deck-with-card-at
                    (board-card-position test-player-specs 0)
                    "magician")
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order deck-order}
             :demo-board-pieces [rose-source-piece]})
        piece-db (-> db
                     (app-state/select-move-source :activate-territory)
                     (app-state/select-move-piece :rose-scout))
        ready-db (-> piece-db
                     (app-state/select-move-power :cup)
                     (app-state/select-board-card 1)
                     (app-state/set-move-orientation :south))
        command (app-state/move-command ready-db)
        confirmed-db (app-state/confirm-move ready-db)
        result (get-in confirmed-db [:move-selection :last-result])
        event (first (:events result))
        zones (app-state/card-zones confirmed-db)
        game (app-state/game confirmed-db)]
    (is (= magician-power-options
           (mapv :id (app-state/move-power-options piece-db))))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 0
                     :piece-id :rose-scout}
            :cup-variant :wild-suits
            :target {:kind :territory
                     :board-index 1}
            :orientation :south}
           command))
    (is (game-state/valid-command? :cup command))
    (is (:ok? result))
    (is (game-state/valid-result? result))
    (is (= :wild-suits (:cup-variant event)))
    (is (= {:kind :territory
            :board-index 0
            :piece-id :rose-scout}
           (:source event)))
    (is (= [] (mapv :id (:discard-pile zones))))
    (is (= 0 (:discard-count zones)))
    (is (= 6 (count (:hand zones))))
    (is (= "magician"
           (get-in (app-state/board-cell-by-index confirmed-db 0) [:card :id])))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 1
            :size :small
            :orientation :south}
           (piece-by-id confirmed-db :rose-small-1)))
    (assert-card-accounting game)
    (is (game-schema/valid-game? game))))
