(ns gnostica.app-state.move-panel-test
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

(deftest move-panel-view-control-groups-track-staged-powers
  (let [composite-db (app-state/initialize
                      {:player-specs test-player-specs
                       :game-options {:deck-order (deck-starting-with ["empress"])}
                       :demo-board-pieces [rose-hand-piece]})
        composite-orient-db (-> composite-db
                                (app-state/select-move-source :play-hand-card)
                                (app-state/select-move-hand-card "empress")
                                (app-state/select-move-piece :rose-striker)
                                (app-state/select-move-power :empress))
        composite-cup-db (app-state/set-move-minion-orientation
                          composite-orient-db
                          :east)
        moon-db (app-state/initialize
                 {:player-specs test-player-specs
                  :game-options {:deck-order (deck-starting-with ["moon"])}
                  :demo-board-pieces [rose-rod-minion indigo-rod-target]})
        moon-rod-db (-> moon-db
                        (app-state/select-move-source :play-hand-card)
                        (app-state/select-move-hand-card "moon")
                        (app-state/select-move-piece :rose-rod-minion)
                        (app-state/select-move-power :moon))
        moon-sword-db (-> moon-rod-db
                          (app-state/select-move-rod-mode :move-minion)
                          (app-state/set-move-distance 1)
                          (app-state/set-move-orientation :up))
        sun-db (-> (app-state/initialize
                    {:player-specs test-player-specs
                     :game-options {:deck-order (deck-starting-with ["sun"])}
                     :demo-board-pieces [rose-hand-cup-enemy-piece]})
                   (app-state/select-move-source :play-hand-card)
                   (app-state/select-move-hand-card "sun")
                   (app-state/select-move-piece :rose-striker)
                   (app-state/select-move-power :sun))
        world-db (app-state/initialize
                  {:player-specs test-player-specs
                   :game-options {:deck-order
                                  (deck-with-cards-at
                                   {0 "world"
                                    (board-card-position test-player-specs 3) "empress"})}
                   :demo-board-pieces [(assoc rose-source-piece
                                              :orientation :north)]})
        world-copy-db (-> world-db
                          (app-state/select-move-source :play-hand-card)
                          (app-state/select-move-hand-card "world")
                          (app-state/select-move-piece :rose-scout))
        world-copied-power-db (app-state/select-move-world-copy world-copy-db 3)
        world-orient-db (app-state/select-move-power world-copied-power-db :empress)
        world-cup-db (app-state/set-move-minion-orientation world-orient-db :east)]
    (is (= [{:type :hand-card}
            {:type :piece}
            {:type :power}
            {:type :minion-orientation
             :power :empress
             :action-power :orient-minion}]
           (move-control-group-summary composite-orient-db)))
    (is (= [{:type :hand-card}
            {:type :piece}
            {:type :power}
            {:type :cup
             :power :empress
             :action-power :cup}]
           (move-control-group-summary composite-cup-db)))
    (is (= [{:type :hand-card}
            {:type :piece}
            {:type :power}
            {:type :rod
             :power :moon
             :action-power :rod}]
           (move-control-group-summary moon-rod-db)))
    (is (= [{:type :hand-card}
            {:type :piece}
            {:type :power}
            {:type :sword
             :power :moon
             :action-power :sword}]
           (move-control-group-summary moon-sword-db)))
    (is (= [{:type :hand-card}
            {:type :piece}
            {:type :power}
            {:type :sun
             :power :sun}]
           (move-control-group-summary sun-db)))
    (is (= [{:type :hand-card}
            {:type :piece}
            {:type :power}
            {:type :world-copy
             :power :world}]
           (move-control-group-summary world-copy-db)))
    (is (= [{:type :hand-card}
            {:type :piece}
            {:type :power}
            {:type :world-copy
             :power :world}
            {:type :world-copied-power
             :power :world}]
           (move-control-group-summary world-copied-power-db)))
    (is (= [{:type :hand-card}
            {:type :piece}
            {:type :power}
            {:type :world-copy
             :power :world}
            {:type :world-copied-power
             :power :world}
            {:type :minion-orientation
             :power :empress
             :action-power :orient-minion}]
           (move-control-group-summary world-orient-db)))
    (is (= [{:type :hand-card}
            {:type :piece}
            {:type :power}
            {:type :world-copy
             :power :world}
            {:type :world-copied-power
             :power :world}
            {:type :cup
             :power :empress
             :action-power :cup}]
           (move-control-group-summary world-cup-db)))))
(deftest move-panel-subscription-view-matches-app-state-facade
  (let [composite-db (app-state/initialize
                      {:player-specs test-player-specs
                       :game-options {:deck-order (deck-starting-with ["empress"])}
                       :demo-board-pieces [rose-hand-piece]})
        hand-source-db (app-state/select-move-source composite-db :play-hand-card)
        composite-power-db (-> hand-source-db
                               (app-state/select-move-hand-card "empress")
                               (app-state/select-move-piece :rose-striker)
                               (app-state/select-move-power :empress))
        composite-cup-db (app-state/set-move-minion-orientation
                          composite-power-db
                          :east)
        gesture-db (app-state/start-gesture-intent
                    composite-db
                    {:source {:kind :hand-card
                              :card-id "empress"}
                     :fields {:piece-id :rose-striker
                              :power :empress
                              :minion-orientation :east}
                     :target {:kind :territory
                              :board-index 3}})
        world-db (app-state/initialize
                  {:player-specs test-player-specs
                   :game-options
                   {:deck-order
                    (deck-with-cards-at
                     {0 "world"
                      (board-card-position test-player-specs 3) "empress"})}
                   :demo-board-pieces [(assoc rose-source-piece
                                              :orientation :north)]})
        world-copy-db (-> world-db
                          (app-state/select-move-source :play-hand-card)
                          (app-state/select-move-hand-card "world")
                          (app-state/select-move-piece :rose-scout)
                          (app-state/select-move-world-copy 3))]
    (doseq [[label db] [["hand source" hand-source-db]
                        ["ordered major" composite-power-db]
                        ["ordered major next step" composite-cup-db]
                        ["gesture-staged move" gesture-db]
                        ["world copy" world-copy-db]]]
      (testing label
        (is (= (app-state/move-panel-view db)
               (move-panel-subscription-view db)))))))
(deftest action-ribbon-tracks-ordered-major-steps
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["empress"])}
             :demo-board-pieces [rose-hand-piece]})
        power-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "empress")
                     (app-state/select-move-piece :rose-striker)
                     (app-state/select-move-power :empress))
        orient-db (app-state/set-move-minion-orientation power-db :north)
        target-db (app-state/select-board-card orient-db 5)
        ready-db (app-state/set-move-orientation target-db :north)]
    (is (= {:visible? true
            :power :empress
            :power-label "Empress"
            :summary "Empress"
            :ready? false}
           (select-keys (:action-ribbon (app-state/move-panel-view power-db))
                        [:visible? :power :power-label :summary :ready?])))
    (is (= [{:power :orient-minion
             :status :active}
            {:power :cup
             :status :pending}]
           (action-ribbon-step-summary (app-state/move-panel-view power-db))))
    (is (= [{:power :orient-minion
             :status :done}
            {:power :cup
             :status :active}]
           (action-ribbon-step-summary (app-state/move-panel-view orient-db))))
    (is (= [{:power :orient-minion
             :status :done}
            {:power :cup
             :status :ready}]
           (action-ribbon-step-summary (app-state/move-panel-view ready-db))))
    (is (true? (get-in (app-state/move-panel-view ready-db)
                       [:action-ribbon :ready?])))))
(deftest action-ribbon-represents-trade-only-major-paths
  (let [justice-db (app-state/initialize
                    {:player-specs test-player-specs
                     :game-options {:deck-order (deck-starting-with ["justice"])}
                     :demo-board-pieces [rose-rod-minion
                                         indigo-rod-target]})
        justice-power-db (-> justice-db
                             (app-state/select-move-source :play-hand-card)
                             (app-state/select-move-hand-card "justice")
                             (app-state/select-move-piece :rose-rod-minion)
                             (app-state/select-move-power :justice))
        justice-trade-only-db (app-state/set-move-major-action-count
                               justice-power-db
                               1)
        justice-ready-db (app-state/select-move-target-piece
                          justice-trade-only-db
                          :indigo-rod-target)
        hanged-target {:id :indigo-hanged-target
                       :player-id :indigo
                       :space-index 4
                       :size :small
                       :orientation :north}
        hanged-db (app-state/initialize
                   {:player-specs test-player-specs
                    :game-options {:deck-order (deck-starting-with ["hangedman"])}
                    :demo-board-pieces [rose-rod-minion
                                        hanged-target]})
        hanged-trade-only-db (-> hanged-db
                                 (app-state/select-move-source :play-hand-card)
                                 (app-state/select-move-hand-card "hangedman")
                                 (app-state/select-move-piece :rose-rod-minion)
                                 (app-state/select-move-power :hanged-man)
                                 (app-state/set-move-major-action-count 1))]
    (is (= [{:power :trade-hand
             :status :active}
            {:power :sword
             :status :skipped}]
           (action-ribbon-step-summary
            (app-state/move-panel-view justice-trade-only-db))))
    (is (= [{:power :trade-hand
             :status :done}
            {:power :sword
             :status :skipped}]
           (action-ribbon-step-summary
            (app-state/move-panel-view justice-ready-db))))
    (is (= [{:power :rod
             :status :skipped}
            {:power :trade-hand
             :status :active}]
           (action-ribbon-step-summary
            (app-state/move-panel-view hanged-trade-only-db))))
    (is (= [{:id 1 :label "Trade only"}
            {:id 2 :label "Use both"}]
           (get-in (app-state/move-panel-view justice-power-db)
                   [:controls :major-action-count-options])))))
(deftest action-ribbon-reuses-current-staging-for-gestures-and-world-copies
  (let [gesture-db (app-state/initialize
                    {:player-specs test-player-specs
                     :game-options {:deck-order (deck-starting-with ["empress"])}
                     :demo-board-pieces [rose-hand-piece]})
        pending-db (app-state/start-gesture-intent
                    gesture-db
                    {:source {:kind :hand-card
                              :card-id "empress"}
                     :fields {:piece-id :rose-striker
                              :power :empress
                              :minion-orientation :east}
                     :target {:kind :territory
                              :board-index 3}})
        world-db (app-state/initialize
                  {:player-specs test-player-specs
                   :game-options
                   {:deck-order
                    (deck-with-cards-at
                     {0 "world"
                      (board-card-position test-player-specs 3) "empress"})}
                   :demo-board-pieces [(assoc rose-source-piece
                                              :orientation :north)]})
        world-copy-db (-> world-db
                          (app-state/select-move-source :play-hand-card)
                          (app-state/select-move-hand-card "world")
                          (app-state/select-move-piece :rose-scout)
                          (app-state/select-move-world-copy 3)
                          (app-state/select-move-power :empress))
        world-ribbon (:action-ribbon (app-state/move-panel-view world-copy-db))]
    (is (= [{:power :orient-minion
             :status :done}
            {:power :cup
             :status :active}]
           (action-ribbon-step-summary
            (app-state/pending-move-tray-view pending-db))))
    (is (= "World copies Empress"
           (:summary world-ribbon)))
    (is (= [{:power :world-copy
             :status :done
             :board-index 3}
            {:power :world-power
             :status :done}
            {:power :orient-minion
             :status :active}
            {:power :cup
             :status :pending}]
           (action-ribbon-step-summary
            (app-state/move-panel-view world-copy-db))))))
