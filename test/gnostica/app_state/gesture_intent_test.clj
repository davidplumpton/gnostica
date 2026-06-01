(ns gnostica.app-state.gesture-intent-test
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

(deftest gesture-intent-stages-existing-move-selection-fields
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["cups2"])}
             :demo-board-pieces [rose-hand-cup-territory-piece]})
        original-game (app-state/game db)
        pending-db (app-state/start-gesture-intent
                    db
                    {:source {:kind :hand-card
                              :card-id "cups2"}
                     :fields {:piece-id :rose-striker
                              :orientation :north}
                     :target {:kind :territory
                              :board-index 3}})
        tray (app-state/pending-move-tray-view pending-db)
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "cups2"
                          :piece-id :rose-striker}
                 :cup-variant :cup
                 :target {:kind :territory
                          :board-index 3}
                 :orientation :north}
        confirmed-db (app-state/confirm-move pending-db)]
    (is (= original-game (app-state/game pending-db)))
    (is (true? (get-in pending-db [:gesture-intent :active?])))
    (is (= {:hand-card-id "cups2"
            :piece-id :rose-striker
            :target-board-index 3
            :orientation :north}
           (app-state/move-params pending-db)))
    (is (= :confirm (:stage (app-state/move-selection pending-db))))
    (is (= command (app-state/move-command pending-db)))
    (is (= command (:preview-command tray)))
    (is (true? (:ready? tray)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (false? (get-in confirmed-db [:gesture-intent :active?])))
    (is (not= original-game (app-state/game confirmed-db)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest gesture-intent-rejections-do-not-mutate-game
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["cups2"])}
             :demo-board-pieces [rose-hand-cup-territory-piece]})
        rejected-db (app-state/start-gesture-intent
                     db
                     {:source {:kind :hand-card
                               :card-id "cups2"}
                      :fields {:piece-id :rose-striker
                               :orientation :north}
                      :target {:kind :territory
                               :board-index 8}})
        tray (app-state/pending-move-tray-view rejected-db)]
    (is (= (app-state/game db)
           (app-state/game rejected-db)))
    (is (= :invalid-cup-target
           (get-in rejected-db [:gesture-intent :error :code])))
    (is (= :invalid-cup-target
           (get-in tray [:error :code])))
    (is (false? (:ready? tray)))
    (is (game-schema/valid-game? (app-state/game rejected-db)))))
(deftest gesture-intent-arms-direct-territory-and-draw-sources
  (let [territory-db (app-state/initialize
                      {:player-specs test-player-specs
                       :game-options {:deck-order
                                      (deck-with-card-at
                                       (board-card-position test-player-specs 3)
                                       "cups2")}
                       :demo-board-pieces [rose-rod-minion]})
        territory-gesture-db (app-state/start-gesture-intent
                              territory-db
                              {:source {:kind :territory
                                        :board-index 3}})
        draw-gesture-db (app-state/start-gesture-intent
                         territory-db
                         {:source {:kind :draw-pile}})
        draw-targets (app-state/move-legal-targets draw-gesture-db)]
    (is (= :activate-territory
           (get-in territory-gesture-db [:gesture-intent :move-source])))
    (is (= {:source-board-index 3}
           (app-state/move-params territory-gesture-db)))
    (is (= :piece
           (:stage (app-state/move-selection territory-gesture-db))))
    (is (= :rose-rod-minion
           (:id (first (app-state/move-piece-options territory-gesture-db)))))
    (is (= :draw-cards
           (get-in draw-gesture-db [:gesture-intent :move-source])))
    (is (= :draw-count
           (:stage (app-state/move-selection draw-gesture-db))))
    (is (= :draw
           (get-in draw-targets [:draw-pile :role])))
    (is (every? #(= :discard (:role %))
                (:hand-cards draw-targets)))))
(deftest gesture-intent-cancellation-and-detailed-entry-preserve-selection
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["cups2"])}
             :demo-board-pieces [rose-hand-cup-territory-piece]})
        pending-db (app-state/start-gesture-intent
                    db
                    {:source {:kind :hand-card
                              :card-id "cups2"}
                     :fields {:piece-id :rose-striker}
                     :target {:kind :territory
                              :board-index 3}})
        detailed-db (app-state/open-gesture-detailed-entry pending-db)
        completed-db (app-state/set-move-orientation detailed-db :east)
        cancelled-db (app-state/cancel-move pending-db)
        tray (app-state/pending-move-tray-view pending-db)]
    (is (= :orientation (:stage (app-state/move-selection pending-db))))
    (is (= [:target-resolution]
           (:missing-fields tray)))
    (is (= [:orientation]
           (mapv :field (:alternatives tray))))
    (is (= [:up :north :east :south :west]
           (mapv :id (:options (alternative-by-field tray :orientation)))))
    (is (true? (app-state/panel-open? detailed-db :move)))
    (is (true? (get-in detailed-db [:gesture-intent :detailed?])))
    (is (= {:hand-card-id "cups2"
            :piece-id :rose-striker
            :target-board-index 3}
           (app-state/move-params detailed-db)))
    (is (= :confirm (:stage (app-state/move-selection completed-db))))
    (is (= :east (:orientation (app-state/move-command completed-db))))
    (is (= (app-state/game db)
           (app-state/game cancelled-db)))
    (is (= (app-state/empty-move-selection)
           (app-state/move-selection cancelled-db)))
    (is (false? (get-in cancelled-db [:gesture-intent :active?])))))
(deftest detailed-entry-default-opens-panel-without-losing-staged-data
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["cups2"])}
             :direct-manipulation {:pointer-drag-enabled? false}
             :demo-board-pieces [rose-hand-cup-territory-piece]})
        source-db (-> db
                      (app-state/select-move-source :play-hand-card)
                      (app-state/select-move-hand-card "cups2"))
        default-db (app-state/set-detailed-entry-default source-db true)
        pending-db (app-state/start-gesture-intent
                    default-db
                    {:source {:kind :hand-card
                              :card-id "cups2"}
                     :fields {:piece-id :rose-striker}
                     :target {:kind :territory
                              :board-index 3}})
        tray (app-state/pending-move-tray-view pending-db)]
    (is (= {:hand-card-id "cups2"}
           (app-state/move-params default-db)))
    (is (= {:pointer-drag-enabled? false
            :detailed-entry-available? true
            :detailed-entry-default? true}
           (:direct-manipulation (app-state/move-panel-view default-db))))
    (is (true? (app-state/panel-open? default-db :move)))
    (is (true? (get-in pending-db [:gesture-intent :detailed?])))
    (is (true? (:detailed-open? tray)))
    (is (true? (:detailed-entry-available? tray)))
    (is (= {:hand-card-id "cups2"
            :piece-id :rose-striker
            :target-board-index 3}
           (app-state/move-params pending-db)))
    (is (= :orientation (:stage (app-state/move-selection pending-db))))
    (is (= (app-state/game db)
           (app-state/game pending-db)))))
(deftest direct-gesture-commands-match-staged-move-panel-commands
  (let [cup-db (app-state/initialize
                {:player-specs test-player-specs
                 :game-options {:deck-order (deck-starting-with ["cups2"])}
                 :demo-board-pieces [rose-hand-cup-territory-piece]})
        rod-db (app-state/initialize
                {:player-specs test-player-specs
                 :game-options {:deck-order (deck-starting-with ["wands2"])}
                 :demo-board-pieces [rose-rod-minion]})
        disc-db (app-state/initialize
                 {:player-specs test-player-specs
                  :game-options {:deck-order (deck-starting-with ["coins2"])}
                  :demo-board-pieces [rose-rod-minion indigo-rod-target]})
        sword-db (app-state/initialize
                  {:player-specs test-player-specs
                   :game-options {:deck-order (deck-starting-with ["swords2"])}
                   :demo-board-pieces [rose-rod-minion indigo-rod-target]})
        orient-db (app-state/initialize
                   {:player-specs test-player-specs
                    :game-options {:shuffle-fn identity}
                    :demo-board-pieces [rose-source-piece]})
        initial-db (app-state/initialize
                    {:player-specs test-player-specs
                     :game-options {:shuffle-fn identity}
                     :demo-board-pieces []})
        fool-db (app-state/initialize
                 {:player-specs test-player-specs
                  :game-options {:deck-order (deck-starting-with ["fool"])}
                  :demo-board-pieces [rose-hand-piece]})
        cases [{:label "Cup"
                :db cup-db
                :gesture {:source {:kind :hand-card
                                   :card-id "cups2"}
                          :fields {:piece-id :rose-striker
                                   :orientation :north}
                          :target {:kind :territory
                                   :board-index 3}}
                :staged #(-> %
                             (app-state/select-move-source :play-hand-card)
                             (app-state/select-move-hand-card "cups2")
                             (app-state/select-move-piece :rose-striker)
                             (app-state/select-board-card 3)
                             (app-state/set-move-orientation :north))}
               {:label "Rod"
                :db rod-db
                :gesture {:source {:kind :hand-card
                                   :card-id "wands2"}
                          :fields {:piece-id :rose-rod-minion
                                   :rod-mode :move-minion
                                   :distance 1
                                   :orientation :east}}
                :staged #(-> %
                             (app-state/select-move-source :play-hand-card)
                             (app-state/select-move-hand-card "wands2")
                             (app-state/select-move-piece :rose-rod-minion)
                             (app-state/select-move-rod-mode :move-minion)
                             (app-state/set-move-distance 1)
                             (app-state/set-move-orientation :east))}
               {:label "Disc"
                :db disc-db
                :gesture {:source {:kind :hand-card
                                   :card-id "coins2"}
                          :fields {:piece-id :rose-rod-minion}
                          :target {:kind :piece
                                   :piece-id :indigo-rod-target}}
                :staged #(-> %
                             (app-state/select-move-source :play-hand-card)
                             (app-state/select-move-hand-card "coins2")
                             (app-state/select-move-piece :rose-rod-minion)
                             (app-state/select-move-disc-target-kind :piece)
                             (app-state/select-move-target-piece :indigo-rod-target))}
               {:label "Sword"
                :db sword-db
                :gesture {:source {:kind :hand-card
                                   :card-id "swords2"}
                          :fields {:piece-id :rose-rod-minion
                                   :damage 1}
                          :target {:kind :piece
                                   :piece-id :indigo-rod-target}}
                :staged #(-> %
                             (app-state/select-move-source :play-hand-card)
                             (app-state/select-move-hand-card "swords2")
                             (app-state/select-move-piece :rose-rod-minion)
                             (app-state/select-move-sword-target-kind :piece)
                             (app-state/select-move-target-piece :indigo-rod-target)
                             (app-state/set-move-damage 1))}
               {:label "Orient"
                :db orient-db
                :gesture {:source {:kind :piece
                                   :piece-id :rose-scout}
                          :fields {:orientation :west}}
                :staged #(-> %
                             (app-state/select-move-source :orient-piece)
                             (app-state/select-move-piece :rose-scout)
                             (app-state/set-move-orientation :west))}
               {:label "Initial placement"
                :db initial-db
                :gesture {:source {:kind :stash-piece
                                   :player-id :rose
                                   :size :small}
                          :target {:kind :wasteland
                                   :row 0
                                   :col 3}
                          :fields {:orientation :north}}
                :staged #(-> %
                             (app-state/select-move-source :place-initial-small)
                             (app-state/select-move-wasteland-target 0 3)
                             (app-state/set-move-orientation :north))}
               {:label "Major"
                :db fool-db
                :gesture {:source {:kind :hand-card
                                   :card-id "fool"}
                          :fields {:piece-id :rose-striker
                                   :power :fool
                                   :fool-reveal-count 0}}
                :staged #(-> %
                             (app-state/select-move-source :play-hand-card)
                             (app-state/select-move-hand-card "fool")
                             (app-state/select-move-piece :rose-striker)
                             (app-state/set-move-fool-reveal-count 0))}]]
    (doseq [{:keys [label db gesture staged]} cases
            :let [direct-db (app-state/start-gesture-intent db gesture)
                  staged-db (staged db)
                  tray (app-state/pending-move-tray-view direct-db)
                  command (app-state/move-command staged-db)]]
      (is (= (app-state/game db) (app-state/game direct-db))
          (str label " gesture should not mutate game state before confirmation"))
      (is (= :confirm (:stage (app-state/move-selection direct-db)))
          (str label " gesture should reach confirmation"))
      (is (= command (app-state/move-command direct-db))
          (str label " gesture command should match staged controls"))
      (is (= command (:preview-command tray))
          (str label " pending tray should expose the same command preview"))
      (is (true? (:can-confirm? tray))
          (str label " pending tray should allow confirmation")))))
(deftest detailed-entry-fallback-completes-staged-controls-when-dragging-disabled
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["cups2"])}
             :direct-manipulation {:pointer-drag-enabled? false
                                   :detailed-entry-default? true}
             :demo-board-pieces [rose-hand-cup-territory-piece]})
        ready-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "cups2")
                     (app-state/select-move-piece :rose-striker)
                     (app-state/select-board-card 3)
                     (app-state/set-move-orientation :north))
        confirmed-db (app-state/confirm-move ready-db)
        blocked-card-id (:id (first (app-state/current-player-hand confirmed-db)))
        blocked-gesture-db (app-state/start-gesture-intent
                            confirmed-db
                            {:source {:kind :hand-card
                                      :card-id blocked-card-id}})
        created-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (= {:pointer-drag-enabled? false
            :detailed-entry-available? true
            :detailed-entry-default? true}
           (:direct-manipulation (app-state/move-panel-view db))))
    (is (true? (app-state/panel-open? db :move)))
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (not= (app-state/game db) (app-state/game confirmed-db)))
    (is (= 3 (:space-index created-piece)))
    (is (= :north (:orientation created-piece)))
    (is (= (app-state/game confirmed-db)
           (app-state/game blocked-gesture-db)))
    (is (= :move-source-unavailable
           (get-in blocked-gesture-db [:gesture-intent :error :code])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest gesture-intent-maps-cup-wasteland-resolution-to-one-point-card
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["cups2" "coins2"])}
             :demo-board-pieces [rose-hand-piece]})
        pending-db (app-state/start-gesture-intent
                    db
                    {:source {:kind :hand-card
                              :card-id "cups2"}
                     :fields {:piece-id :rose-striker}
                     :target {:kind :wasteland
                              :row 3
                              :col 2}})
        tray (app-state/pending-move-tray-view pending-db)]
    (is (= :one-point-card (:stage (app-state/move-selection pending-db))))
    (is (= [:target-resolution]
           (:missing-fields tray)))
    (is (= [:one-point-card-id]
           (mapv :field (:alternatives tray))))
    (is (some #{"coins2"}
              (mapv :id (:options (alternative-by-field
                                   tray
                                   :one-point-card-id)))))
    (is (not (some #{"cups2"}
                   (mapv :id (:options (alternative-by-field
                                        tray
                                        :one-point-card-id))))))))
(deftest gesture-intent-maps-sun-disc-pending-requirements-to-concrete-choices
  (let [piece-db (app-state/initialize
                  {:player-specs test-player-specs
                   :game-options {:deck-order (deck-starting-with ["sun"])}
                   :demo-board-pieces [rose-hand-cup-enemy-piece
                                       rose-rod-target
                                       indigo-rod-target]})
        piece-mode-db (-> piece-db
                          (app-state/select-move-source :play-hand-card)
                          (app-state/select-move-hand-card "sun")
                          (app-state/select-move-piece :rose-striker)
                          (app-state/select-move-power :sun)
                          (app-state/select-move-target-piece :indigo-rod-target)
                          (app-state/select-move-sun-disc-mode :piece))
        piece-pending-db (app-state/start-gesture-intent
                          piece-mode-db
                          {:preserve-selection? true})
        piece-tray (app-state/pending-move-tray-view piece-pending-db)
        territory-db (app-state/initialize
                      {:player-specs test-player-specs
                       :game-options {:deck-order
                                      (deck-with-cards-at
                                       {0 "sun"
                                        1 "cupsking"
                                        (board-card-position test-player-specs 4) "cups2"})}
                       :demo-board-pieces [rose-rod-minion]})
        territory-mode-db (-> territory-db
                              (app-state/select-move-source :play-hand-card)
                              (app-state/select-move-hand-card "sun")
                              (app-state/select-move-piece :rose-rod-minion)
                              (app-state/select-move-power :sun)
                              (app-state/select-board-card 4)
                              (app-state/set-move-orientation :north)
                              (app-state/select-move-sun-disc-mode :territory))
        territory-pending-db (app-state/start-gesture-intent
                              territory-mode-db
                              {:preserve-selection? true})
        territory-tray (app-state/pending-move-tray-view territory-pending-db)
        replacement-pending-db (-> territory-mode-db
                                   (app-state/select-board-card 4)
                                   (app-state/start-gesture-intent
                                    {:preserve-selection? true}))
        replacement-tray (app-state/pending-move-tray-view replacement-pending-db)]
    (is (= [:sun-disc-target-piece-id]
           (:missing-fields piece-tray)))
    (is (= [:target-piece-id]
           (mapv :field (:alternatives piece-tray))))
    (is (some #{:rose-striker}
              (mapv :id (:options (alternative-by-field
                                   piece-tray
                                   :target-piece-id)))))
    (is (= [:sun-disc-target-board-index :sun-disc-replacement-card-id]
           (:missing-fields territory-tray)))
    (is (= [:target-board-index]
           (mapv :field (:alternatives territory-tray))))
    (is (= [4]
           (mapv :index (:options (alternative-by-field
                                   territory-tray
                                   :target-board-index)))))
    (is (= [:sun-disc-replacement-card-id]
           (:missing-fields replacement-tray)))
    (is (= [:replacement-card-id]
           (mapv :field (:alternatives replacement-tray))))
    (is (= ["cupsking"]
           (mapv :id (:options (alternative-by-field
                                replacement-tray
                                :replacement-card-id)))))))
(deftest gesture-intent-stages-initial-placement-from-stash-piece
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:shuffle-fn identity}
                                  :demo-board-pieces []})
        original-game (app-state/game db)
        source-only-db (app-state/start-gesture-intent
                        db
                        {:source {:kind :stash-piece
                                  :player-id :rose
                                  :size :small}})
        source-only-targets (app-state/move-legal-targets source-only-db)
        partial-db (app-state/start-gesture-intent
                    db
                    {:source {:kind :stash-piece
                              :player-id :rose
                              :size :small}
                     :target {:kind :wasteland
                              :row 0
                              :col 3}})
        partial-preview (:move-preview (app-state/board-view partial-db))
        pending-db (app-state/start-gesture-intent
                    db
                    {:source {:kind :stash-piece
                              :player-id :rose
                              :size :small}
                     :target {:kind :wasteland
                              :row 0
                              :col 3}
                     :fields {:orientation :north}})
        confirmed-db (app-state/confirm-move pending-db)
        created-piece (piece-by-id confirmed-db :rose-small-1)
        territory-pending-db (app-state/start-gesture-intent
                              db
                              {:source {:kind :stash-piece
                                        :player-id :rose
                                        :size :small}
                               :target {:kind :territory
                                        :board-index 3}
                               :fields {:orientation :west}})
        territory-confirmed-db (app-state/confirm-move territory-pending-db)
        territory-piece (piece-by-id territory-confirmed-db :rose-small-1)]
    (is (= original-game (app-state/game source-only-db)))
    (is (= :target (:stage (app-state/move-selection source-only-db))))
    (is (= {} (app-state/move-params source-only-db)))
    (is (= 9 (count (filter :enabled? (:territories source-only-targets)))))
    (is (= 12 (count (filter :enabled? (:wastelands source-only-targets)))))
    (is (= source-only-targets
           (:legal-targets (app-state/board-view source-only-db))))
    (is (= original-game (app-state/game pending-db)))
    (is (= :place-initial-small
           (get-in pending-db [:gesture-intent :move-source])))
    (is (= {:target-wasteland {:kind :wasteland
                               :row 0
                               :col 3}
            :orientation :north}
           (app-state/move-params pending-db)))
    (is (= {:source :place-initial-small
            :player-id :rose
            :target {:kind :wasteland
                     :row 0
                     :col 3}
            :orientation :north}
           (app-state/move-command pending-db)))
    (is (= :orientation
           (get-in partial-preview [:orientation-compass :field])))
    (is (= {:kind :placement
            :player-id :rose
            :piece-size :small
            :orientation nil}
           (select-keys (:placement partial-preview)
                        [:kind :player-id :piece-size :orientation])))
    (is (= {:kind :wasteland
            :row 0
            :col 3}
           (select-keys (get-in partial-preview [:placement :target-space])
                        [:kind :row :col])))
    (is (= "Place small piece" (:summary partial-preview)))
    (is (= {:kind :wasteland
            :row 0
            :col 3}
           (select-keys (get-in partial-preview [:orientation-compass :space])
                        [:kind :row :col])))
    (is (= [:up :north :east :south :west]
           (mapv :id (get-in partial-preview [:orientation-compass :options]))))
    (is (true? (:can-confirm? (app-state/pending-move-tray-view pending-db))))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space {:kind :wasteland
                    :row 0
                    :col 3}
            :size :small
            :orientation :north}
           created-piece))
    (is (= original-game (app-state/game territory-pending-db)))
    (is (= {:target-board-index 3
            :orientation :west}
           (app-state/move-params territory-pending-db)))
    (is (= {:source :place-initial-small
            :player-id :rose
            :target {:kind :territory
                     :board-index 3}
            :orientation :west}
           (app-state/move-command territory-pending-db)))
    (is (true? (:can-confirm? (app-state/pending-move-tray-view
                               territory-pending-db))))
    (is (:ok? (get-in territory-confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 3
            :size :small
            :orientation :west}
           territory-piece))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))
    (is (game-schema/valid-game? (app-state/game territory-confirmed-db)))))
(deftest drag-orientation-keys-update-initial-placement-before-drop
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:shuffle-fn identity}
                                  :demo-board-pieces []})
        input {:source {:kind :stash-piece
                        :player-id :rose
                        :size :small}}
        source-db (app-state/start-gesture-intent db input)
        east-result (app-state/gesture-drag-orientation-result
                     source-db
                     input
                     (gesture-input/orientation-key-request {:key "ArrowRight"}))
        east-db (app-state/set-gesture-drag-orientation source-db east-result)
        east-input (gesture-input/with-drag-orientation
                     input
                     (:orientation east-result))
        south-result (app-state/gesture-drag-orientation-result
                      east-db
                      east-input
                      (gesture-input/orientation-key-request {:key "O"}))
        south-db (app-state/set-gesture-drag-orientation east-db south-result)
        south-input (gesture-input/with-drag-orientation
                      east-input
                      (:orientation south-result))
        pending-db (app-state/start-gesture-intent
                    south-db
                    (assoc south-input
                           :target {:kind :territory
                                    :board-index 3}))]
    (is (= {:handled? true
            :accepted? true
            :orientation :east}
           east-result))
    (is (= :east (get-in east-db [:move-selection :params :orientation])))
    (is (= :target (:stage (app-state/move-selection east-db))))
    (is (= {:handled? true
            :accepted? true
            :orientation :south}
           south-result))
    (is (= :south (get-in south-input [:source :orientation])))
    (is (= {:target-board-index 3
            :orientation :south}
           (app-state/move-params pending-db)))
    (is (= {:source :place-initial-small
            :player-id :rose
            :target {:kind :territory
                     :board-index 3}
            :orientation :south}
           (app-state/move-command pending-db)))
    (is (true? (:can-confirm? (app-state/pending-move-tray-view
                               pending-db))))))
(deftest gesture-intent-stages-direct-rod-piece-movement
  (let [enemy-db (app-state/initialize
                  {:player-specs test-player-specs
                   :game-options {:deck-order (deck-starting-with ["wands2"])}
                   :demo-board-pieces [rose-rod-minion
                                       indigo-rod-target]})
        enemy-active-db (-> enemy-db
                            (app-state/select-move-source :play-hand-card)
                            (app-state/select-move-hand-card "wands2")
                            (app-state/select-move-piece :rose-rod-minion)
                            (app-state/select-move-rod-mode :push-piece))
        enemy-pending-db (app-state/start-gesture-intent
                          enemy-active-db
                          {:preserve-selection? true
                           :fields {:target-piece-id :indigo-rod-target}
                           :target {:kind :territory
                                    :board-index 5}})
        enemy-confirmed-db (app-state/confirm-move enemy-pending-db)
        enemy-piece (piece-by-id enemy-confirmed-db :indigo-rod-target)
        own-db (app-state/initialize
                {:player-specs test-player-specs
                 :game-options {:deck-order (deck-starting-with ["wands2"])}
                 :demo-board-pieces [rose-rod-minion
                                     (assoc rose-rod-target :space-index 4)]})
        own-active-db (-> own-db
                          (app-state/select-move-source :play-hand-card)
                          (app-state/select-move-hand-card "wands2")
                          (app-state/select-move-piece :rose-rod-minion)
                          (app-state/select-move-rod-mode :push-piece))
        own-pending-db (app-state/start-gesture-intent
                        own-active-db
                        {:preserve-selection? true
                         :fields {:target-piece-id :rose-rod-target
                                  :orientation :south}
                         :target {:kind :territory
                                  :board-index 5}})
        own-confirmed-db (app-state/confirm-move own-pending-db)
        own-piece (piece-by-id own-confirmed-db :rose-rod-target)]
    (is (= {:hand-card-id "wands2"
            :piece-id :rose-rod-minion
            :rod-mode :push-piece
            :target-piece-id :indigo-rod-target
            :distance 1}
           (app-state/move-params enemy-pending-db)))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "wands2"
                     :piece-id :rose-rod-minion}
            :rod-variant :rod
            :mode :push-piece
            :distance 1
            :target {:kind :piece
                     :piece-id :indigo-rod-target}}
           (app-state/move-command enemy-pending-db)))
    (is (:ok? (get-in enemy-confirmed-db [:move-selection :last-result])))
    (is (= :north (:orientation enemy-piece)))
    (is (= 5 (:space-index enemy-piece)))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "wands2"
                     :piece-id :rose-rod-minion}
            :rod-variant :rod
            :mode :push-piece
            :distance 1
            :target {:kind :piece
                     :piece-id :rose-rod-target}
            :orientation :south}
           (app-state/move-command own-pending-db)))
    (is (:ok? (get-in own-confirmed-db [:move-selection :last-result])))
    (is (= :south (:orientation own-piece)))
    (is (= 5 (:space-index own-piece)))
    (is (game-schema/valid-game? (app-state/game enemy-confirmed-db)))
    (is (game-schema/valid-game? (app-state/game own-confirmed-db)))))
(deftest drag-orientation-keys-apply-only-to-eligible-rod-piece-drags
  (let [own-db (app-state/initialize
                {:player-specs test-player-specs
                 :game-options {:deck-order (deck-starting-with ["wands2"])}
                 :demo-board-pieces [rose-rod-minion
                                     (assoc rose-rod-target :space-index 4)]})
        own-active-db (-> own-db
                          (app-state/select-move-source :play-hand-card)
                          (app-state/select-move-hand-card "wands2")
                          (app-state/select-move-piece :rose-rod-minion)
                          (app-state/select-move-rod-mode :push-piece))
        own-input {:preserve-selection? true
                   :fields {:target-piece-id :rose-rod-target}}
        own-drag-db (app-state/start-gesture-intent own-active-db own-input)
        own-result (app-state/gesture-drag-orientation-result
                    own-drag-db
                    own-input
                    (gesture-input/orientation-key-request {:key "ArrowDown"}))
        own-oriented-db (app-state/set-gesture-drag-orientation
                         own-drag-db
                         own-result)
        own-oriented-input (gesture-input/with-drag-orientation
                             own-input
                             (:orientation own-result))
        own-pending-db (app-state/start-gesture-intent
                        own-oriented-db
                        (assoc own-oriented-input
                               :target {:kind :territory
                                        :board-index 5}))
        enemy-db (app-state/initialize
                  {:player-specs test-player-specs
                   :game-options {:deck-order (deck-starting-with ["wands2"])}
                   :demo-board-pieces [rose-rod-minion
                                       indigo-rod-target]})
        enemy-active-db (-> enemy-db
                            (app-state/select-move-source :play-hand-card)
                            (app-state/select-move-hand-card "wands2")
                            (app-state/select-move-piece :rose-rod-minion)
                            (app-state/select-move-rod-mode :push-piece))
        enemy-input {:preserve-selection? true
                     :fields {:target-piece-id :indigo-rod-target}}
        enemy-drag-db (app-state/start-gesture-intent enemy-active-db enemy-input)
        enemy-result (app-state/gesture-drag-orientation-result
                      enemy-drag-db
                      enemy-input
                      (gesture-input/orientation-key-request {:key "ArrowDown"}))
        enemy-rejected-db (app-state/set-gesture-drag-orientation
                           enemy-drag-db
                           enemy-result)
        enemy-pending-db (app-state/start-gesture-intent
                          enemy-rejected-db
                          (assoc enemy-input
                                 :target {:kind :territory
                                          :board-index 5}))]
    (is (= {:handled? true
            :accepted? true
            :orientation :south}
           own-result))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "wands2"
                     :piece-id :rose-rod-minion}
            :rod-variant :rod
            :mode :push-piece
            :distance 1
            :target {:kind :piece
                     :piece-id :rose-rod-target}
            :orientation :south}
           (app-state/move-command own-pending-db)))
    (is (= :drag-orientation-unavailable
           (get-in enemy-rejected-db [:gesture-intent :error :code])))
    (is (false? (:accepted? enemy-result)))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "wands2"
                     :piece-id :rose-rod-minion}
            :rod-variant :rod
            :mode :push-piece
            :distance 1
            :target {:kind :piece
                     :piece-id :indigo-rod-target}}
           (app-state/move-command enemy-pending-db)))))
(deftest gesture-intent-previews-rod-destination-rejections
  (let [full-pieces [rose-rod-minion
                     {:id :rose-full-small
                      :player-id :rose
                      :space-index 4
                      :size :small
                      :orientation :up}
                     {:id :indigo-full-small
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
                     {:id :indigo-full-medium
                      :player-id :indigo
                      :space-index 4
                      :size :medium
                      :orientation :south}]
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order
                            (deck-with-card-at
                             (board-card-position test-player-specs 3)
                             "wands2")}
             :demo-board-pieces full-pieces})
        original-game (app-state/game db)
        pending-db (app-state/start-gesture-intent
                    db
                    {:source {:kind :territory
                              :board-index 3}
                     :fields {:piece-id :rose-rod-minion
                              :rod-mode :move-minion
                              :distance 1
                              :orientation :south}})
        tray (app-state/pending-move-tray-view pending-db)
        preview (:move-preview (app-state/board-view pending-db))
        confirmed-db (app-state/confirm-move pending-db)]
    (is (= :confirm (:stage (app-state/move-selection pending-db))))
    (is (= :target-territory-full
           (get-in tray [:preview-result :error :code])))
    (is (= :target-territory-full
           (get-in tray [:error :code])))
    (is (false? (:can-confirm? tray)))
    (is (= original-game (app-state/game pending-db)))
    (is (= :disabled (:status preview)))
    (is (= :rod (get-in preview [:movement :power])))
    (is (= 4 (get-in preview [:movement :destination-space :board-index])))
    (is (= [{:kind :territory
             :board-index 4
             :row 1
             :col 1}]
           (mapv #(select-keys % [:kind :board-index :row :col])
                 (get-in preview [:movement :path]))))
    (is (= {:field :orientation
            :selected-orientation :south}
           (select-keys (:orientation-compass preview)
                        [:field :selected-orientation])))
    (is (= :rejected (:stage (app-state/move-selection confirmed-db))))
    (is (= :target-territory-full
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= original-game (app-state/game confirmed-db)))))
(deftest gesture-intent-stages-hermit-piece-and-territory-relocation
  (let [enemy-target (assoc indigo-rod-target :space-index 1)
        piece-db (app-state/initialize
                  {:player-specs test-player-specs
                   :game-options {:deck-order (deck-starting-with ["hermit"])}
                   :demo-board-pieces [rose-source-piece
                                       enemy-target]})
        piece-pending-db (app-state/start-gesture-intent
                          piece-db
                          {:source {:kind :hand-card
                                    :card-id "hermit"}
                           :fields {:piece-id :rose-scout
                                    :power :hermit
                                    :target-piece-id :indigo-rod-target}
                           :target {:kind :territory
                                    :board-index 3}})
        piece-confirmed-db (app-state/confirm-move piece-pending-db)
        moved-piece (piece-by-id piece-confirmed-db :indigo-rod-target)
        territory-db (app-state/initialize
                      {:player-specs test-player-specs
                       :game-options {:deck-order (deck-starting-with ["hermit"])}
                       :demo-board-pieces [(assoc rose-rod-minion
                                                  :space-index 3
                                                  :orientation :east)]})
        target-card (get-in territory-db [:game :board 4 :card])
        territory-pending-db (app-state/start-gesture-intent
                              territory-db
                              {:source {:kind :hand-card
                                        :card-id "hermit"}
                               :fields {:piece-id :rose-rod-minion
                                        :power :hermit
                                        :target-board-index 4}
                               :target {:kind :wasteland
                                        :row 1
                                        :col 3}})
        territory-confirmed-db (app-state/confirm-move territory-pending-db)
        moved-cell (board-cell-by-index territory-confirmed-db 4)]
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "hermit"
                     :piece-id :rose-scout}
            :target {:kind :piece
                     :piece-id :indigo-rod-target}
            :destination {:kind :territory
                          :board-index 3}}
           (app-state/move-command piece-pending-db)))
    (is (:ok? (get-in piece-confirmed-db [:move-selection :last-result])))
    (is (= :north (:orientation moved-piece)))
    (is (= 3 (:space-index moved-piece)))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "hermit"
                     :piece-id :rose-rod-minion}
            :target {:kind :territory
                     :board-index 4}
            :destination {:kind :wasteland
                          :row 1
                          :col 3}}
           (app-state/move-command territory-pending-db)))
    (is (:ok? (get-in territory-confirmed-db [:move-selection :last-result])))
    (is (= {:index 4
            :row 1
            :col 3
            :orientation :portrait
            :face :up
            :card target-card}
           moved-cell))
    (is (game-schema/valid-game? (app-state/game piece-confirmed-db)))
    (is (game-schema/valid-game? (app-state/game territory-confirmed-db)))))
(deftest gesture-intent-stages-disc-replacement-card-drop-onto-territory
  (let [deck-order (deck-with-cards-at {0 "coins2"
                                        1 "cupsking"
                                        (board-card-position test-player-specs 4) "cups2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion]})
        kind-db (-> db
                    (app-state/select-move-source :play-hand-card)
                    (app-state/select-move-hand-card "coins2")
                    (app-state/select-move-piece :rose-rod-minion)
                    (app-state/select-move-disc-target-kind :territory))
        pending-db (app-state/start-gesture-intent
                    kind-db
                    {:preserve-selection? true
                     :fields {:replacement-card-source :hand
                              :replacement-card-id "cupsking"}
                     :target {:kind :territory
                              :board-index 4}})
        tray (app-state/pending-move-tray-view pending-db)
        preview (:move-preview (app-state/board-view pending-db))]
    (is (= (app-state/game db)
           (app-state/game pending-db)))
    (is (= :confirm (:stage (app-state/move-selection pending-db))))
    (is (= {:hand-card-id "coins2"
            :piece-id :rose-rod-minion
            :disc-target-kind :territory
            :target-board-index 4
            :replacement-card-source :hand
            :replacement-card-id "cupsking"}
           (app-state/move-params pending-db)))
    (is (true? (:can-confirm? tray)))
    (is (= {:power :disc
            :status :pending
            :summary "Grow territory 1 to 2"}
           (select-keys (:mutation preview)
                        [:power :status :summary])))
    (is (= {:kind :territory
            :board-index 4}
           (select-keys (get-in preview [:mutation :target-space])
                        [:kind :board-index])))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "coins2"
                     :piece-id :rose-rod-minion}
            :disc-variant :disc
            :target {:kind :territory
                     :board-index 4}
            :replacement-card-source :hand
            :replacement-card-id "cupsking"}
           (:preview-command tray)))))
(deftest gesture-intent-infers-disc-piece-target-kind-from-piece-drop
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["coins2"])}
                                  :demo-board-pieces [rose-rod-minion
                                                      indigo-rod-target]})
        pending-db (app-state/start-gesture-intent
                    db
                    {:source {:kind :hand-card
                              :card-id "coins2"}
                     :fields {:piece-id :rose-rod-minion}
                     :target {:kind :piece
                              :piece-id :indigo-rod-target}})]
    (is (= (app-state/game db)
           (app-state/game pending-db)))
    (is (= :confirm (:stage (app-state/move-selection pending-db))))
    (is (= {:hand-card-id "coins2"
            :piece-id :rose-rod-minion
            :disc-target-kind :piece
            :target-piece-id :indigo-rod-target}
           (app-state/move-params pending-db)))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "coins2"
                     :piece-id :rose-rod-minion}
            :disc-variant :disc
            :target {:kind :piece
                     :piece-id :indigo-rod-target}}
           (app-state/move-command pending-db)))))
(deftest gesture-intent-stages-sword-damage-and-replacement-card-drop
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
        pending-db (app-state/start-gesture-intent
                    kind-db
                    {:preserve-selection? true
                     :fields {:damage 1
                              :replacement-card-source :hand
                              :replacement-card-id "cups2"}
                     :target {:kind :territory
                              :board-index 4}})
        preview (:move-preview (app-state/board-view pending-db))
        confirmed-db (app-state/confirm-move pending-db)]
    (is (= (app-state/game db)
           (app-state/game pending-db)))
    (is (= :confirm (:stage (app-state/move-selection pending-db))))
    (is (= {:hand-card-id "swords2"
            :piece-id :rose-rod-minion
            :sword-target-kind :territory
            :target-board-index 4
            :damage 1
            :replacement-card-source :hand
            :replacement-card-id "cups2"}
           (app-state/move-params pending-db)))
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
           (app-state/move-command pending-db)))
    (is (= {:power :sword
            :status :pending
            :summary "Reduce territory 2 to 1"}
           (select-keys (:mutation preview)
                        [:power :status :summary])))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= "cups2" (get-in (board-cell-by-index confirmed-db 4) [:card :id])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest gesture-intent-stages-discard-pile-replacements-only-for-permitted-sources
  (let [star-db (app-state/initialize
                 {:player-specs test-player-specs
                  :game-options {:deck-order
                                 (deck-with-cards-at
                                  {0 "star"
                                   (board-card-position test-player-specs 4) "cupsking"})}
                  :demo-board-pieces [(assoc rose-rod-minion
                                             :orientation :north)]})
        star-target-db (-> star-db
                           (app-state/select-move-source :play-hand-card)
                           (app-state/select-move-hand-card "star")
                           (app-state/select-move-piece :rose-rod-minion)
                           (app-state/set-move-minion-orientation :east)
                           (app-state/select-move-disc-target-kind :territory)
                           (app-state/select-board-card 4))
        star-targets (app-state/move-legal-targets star-target-db)
        star-pending-db (app-state/start-gesture-intent
                         star-target-db
                         {:preserve-selection? true
                          :fields {:replacement-card-source :discard-pile
                                   :replacement-card-id "star"}
                          :target {:kind :territory
                                   :board-index 4}})
        normal-db (-> (app-state/initialize
                       {:player-specs test-player-specs
                        :game-options {:deck-order
                                       (deck-with-cards-at
                                        {0 "swords2"
                                         (board-card-position test-player-specs 4) "cupsking"})}
                        :demo-board-pieces [rose-rod-minion]})
                      (update-in [:game :draw-pile]
                                 #(vec (remove (fn [card]
                                                 (= "cups2" (:id card)))
                                               %)))
                      (assoc-in [:game :discard-pile]
                                [(cards/card-by-id "cups2")]))
        normal-target-db (-> normal-db
                             (app-state/select-move-source :play-hand-card)
                             (app-state/select-move-hand-card "swords2")
                             (app-state/select-move-piece :rose-rod-minion)
                             (app-state/select-move-sword-target-kind :territory)
                             (app-state/select-board-card 4)
                             (app-state/set-move-damage 1))
        normal-targets (app-state/move-legal-targets normal-target-db)
        rejected-db (app-state/start-gesture-intent
                     normal-target-db
                     {:preserve-selection? true
                      :fields {:replacement-card-source :discard-pile
                               :replacement-card-id "cups2"}
                      :target {:kind :territory
                               :board-index 4}})]
    (is (= :replacement-card-source
           (:stage (app-state/move-selection star-target-db))))
    (is (= :legal (:status (hand-card-target star-targets "star"))))
    (is (= :discard-pile
           (:replacement-card-source (hand-card-target star-targets "star"))))
    (is (= :confirm (:stage (app-state/move-selection star-pending-db))))
    (is (= :discard-pile
           (get-in (app-state/move-command star-pending-db)
                   [:replacement-card-source])))
    (is (= :disabled (:status (discard-card-target normal-targets "cups2"))))
    (is (= :replacement-card-source-unavailable
           (get-in (discard-card-target normal-targets "cups2")
                   [:error :code])))
    (is (= (app-state/game normal-target-db)
           (app-state/game rejected-db)))
    (is (= :invalid-territory-card-source
           (get-in rejected-db [:gesture-intent :error :code])))))
