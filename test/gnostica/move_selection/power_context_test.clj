(ns gnostica.move-selection.power-context-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.app-state :as app-state]
            [gnostica.cards :as cards]
            [gnostica.move-selection.power-context :as power]
            [gnostica.test-support.app-state :refer [rose-source-piece
                                                     test-player-specs]]
            [gnostica.test-support.deck :refer [board-card-position
                                                deck-starting-with
                                                deck-with-cards-at]]))

(defn- db-with-board-card [board-index card-id]
  (app-state/initialize
   {:player-specs test-player-specs
    :game-options {:deck-order
                   (deck-with-cards-at
                    {(board-card-position test-player-specs board-index)
                     card-id})}
    :demo-board-pieces [rose-source-piece]}))

(deftest world-copy-derives-active-power-from-copied-card
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order
                            (deck-with-cards-at
                             {0 "world"
                              (board-card-position test-player-specs 3)
                              "empress"})}
             :demo-board-pieces [rose-source-piece]})
        source :play-hand-card
        base-params {:hand-card-id "world"
                     :piece-id :rose-scout}
        copy-params (assoc base-params
                           :copied-board-index 3
                           :copied-power :empress)
        second-step-params (assoc copy-params
                                  :major-actions
                                  [{:power :orient-minion
                                    :piece-id :rose-scout
                                    :orientation :north}])
        copied-cell (power/world-copy-board-cell db 3)]
    (is (= :world (power/selected-power db source base-params)))
    (is (true? (power/world-move? db source base-params)))
    (is (= "empress" (get-in copied-cell [:card :id])))
    (is (= [:empress :cup]
           (power/world-copied-power-ids-for-card (:card copied-cell))))
    (is (= :empress
           (power/selected-world-copied-power db source copy-params)))
    (is (= :empress (power/active-power db source copy-params)))
    (is (true? (power/composite-major-move? db source copy-params)))
    (is (= :orient-minion
           (power/active-composite-action-power db source copy-params)))
    (is (= :cup
           (power/active-composite-action-power db source second-step-params)))))

(deftest fool-play-derives-active-power-from-revealed-card
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["fool"])}
             :demo-board-pieces [rose-source-piece]})
        source :play-hand-card
        params {:hand-card-id "fool"
                :piece-id :rose-scout
                :fool-reveal-count 1
                :fool-active-reveal {:card-id "sun"
                                     :choice :play}
                :fool-play-power :disc}]
    (is (true? (power/fool-move? db source params)))
    (is (true? (power/fool-active-play? db source params)))
    (is (= [:cup :disc :sun] (power/fool-play-power-options params)))
    (is (= :disc (power/selected-fool-play-power db source params)))
    (is (= :disc (power/active-power db source params)))
    (is (= "sun" (:id (power/active-card db source params))))
    (is (= :disc (power/selected-disc-variant db source params)))))

(deftest suit-power-context-derives-card-variant-capabilities
  (testing "Wheel of Fortune exposes the draw-pile Cup variant"
    (let [db (db-with-board-card 0 "wheeloffortune")
          params {:source-board-index 0
                  :piece-id :rose-scout}]
      (is (= :cup (power/selected-power db :activate-territory params)))
      (is (= :wheel-cup
             (power/selected-cup-variant db :activate-territory params)))
      (is (= [:hand :draw-pile-top]
             (power/territory-card-source-option-ids
              db
              :activate-territory
              params)))))
  (testing "Star exposes discard-pile Disc replacement behavior"
    (let [db (db-with-board-card 0 "star")
          params {:source-board-index 0
                  :piece-id :rose-scout}]
      (is (= :disc (power/selected-power db :activate-territory params)))
      (is (true? (power/star-disc-source? db :activate-territory params)))
      (is (= :disc-from-discard
             (power/selected-disc-variant db :activate-territory params)))))
  (testing "Tower can use the discard-pile Sword variant when Sword is selected"
    (let [db (db-with-board-card 0 "tower")
          params {:source-board-index 0
                  :piece-id :rose-scout
                  :power :sword}]
      (is (= :sword (power/selected-power db :activate-territory params)))
      (is (= :sword-from-discard
             (power/selected-sword-variant db :activate-territory params))))))

(deftest replacement-card-value-predicates-stay-in-power-context
  (let [one-point (cards/card-by-id "cups2")
        two-point (cards/card-by-id "cupsking")
        three-point (cards/card-by-id "sun")]
    (is (true? (power/card-worth-disc-actions-more? two-point one-point 1)))
    (is (false? (power/card-worth-disc-actions-more? one-point one-point 1)))
    (is (true? (power/card-worth-sword-damage-less? one-point three-point 2)))
    (is (false? (power/card-worth-sword-damage-less? one-point three-point 3)))))
