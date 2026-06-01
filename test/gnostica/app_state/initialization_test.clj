(ns gnostica.app-state.initialization-test
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

(deftest initialize-builds-app-db-from-shared-game-state
  (let [hand-count (* game-state/starting-hand-size
                      (count app-state/default-player-specs))
        board-deck (drop hand-count cards/deck)
        db (app-state/initialize {:game-options {:shuffle-fn identity}})]
    (is (nil? (app-state/setup-error db)))
    (is (= (board/initial-board board-deck identity)
           (app-state/board db)))
    (is (empty? (app-state/board-pieces db)))
    (is (= 5 (get-in db [:game :players-by-id :rose :stash :small])))
    (is (= 5 (get-in db [:game :players-by-id :rose :stash :medium])))
    (is (= (get-in db [:game :players-by-id :rose :stash])
           (get-in db [:game :pieces :stashes :rose])))
    (is (game-schema/valid-game? (app-state/game db)))
    (is (= :rose (:id (app-state/current-player db))))
    (is (= app-state/default-selected-board-index
           (app-state/selected-board-index db)))
    (is (= :always
           (app-state/card-icon-mode db)))
    (is (= app-state/default-open-panels
           (app-state/open-panels db)))
    (is (false? (app-state/hotkey-help-open? db)))
    (is (false? (app-state/icon-help-open? db)))
    (is (= app-state/default-three-runtime-status
           (app-state/three-runtime-status db)))))
(deftest initialize-event-handler-is-deterministic-with-injected-seed
  (let [opts {:player-specs test-player-specs}
        first-db (app-handlers/initialize-db opts {:shuffle-seed 8675309})
        second-db (app-handlers/initialize-db opts {:shuffle-seed 8675309})
        explicit-db (app-handlers/initialize-db
                     {:player-specs test-player-specs
                      :game-options {:deck-order cards/deck}}
                     {:shuffle-seed 8675309})
        seeded-deck (deterministic-shuffle/shuffle-with-seed 8675309 cards/deck)]
    (is (= (app-state/game first-db)
           (app-state/game second-db)))
    (is (= (mapv :id (take game-state/starting-hand-size seeded-deck))
           (mapv :id (app-state/current-player-hand first-db))))
    (is (= (mapv :id (take game-state/starting-hand-size cards/deck))
           (mapv :id (app-state/current-player-hand explicit-db))))
    (is (game-schema/valid-game? (app-state/game first-db)))))
(deftest initialize-applies-explicit-demo-pieces
  (let [db (app-state/initialize
            (fixtures/merge-init-options
             {:game-options {:shuffle-fn identity}}
             (fixtures/demo-init-options app-state/default-player-specs)))]
    (is (= fixtures/demo-board-pieces
           (app-state/board-pieces db)))
    (is (= 4 (get-in db [:game :players-by-id :rose :stash :small])))
    (is (= 4 (get-in db [:game :players-by-id :rose :stash :medium])))
    (is (game-schema/valid-game? (app-state/game db)))))
(deftest explicit-demo-pieces-follow-participating-players
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:shuffle-fn identity}
                                  :demo-board-pieces (fixtures/demo-board-pieces-for
                                                      test-player-specs)})]
    (is (= #{:rose :indigo}
           (set (map :player-id (app-state/board-pieces db)))))
    (is (game-schema/valid-game? (app-state/game db)))))
(deftest card-icon-mode-can-be-initialized-and-toggled
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}
                                  :card-icon-mode :popup})]
    (is (= :popup (app-state/card-icon-mode db)))
    (is (= :always
           (app-state/card-icon-mode (app-state/toggle-card-icon-mode db))))
    (is (= :always
           (app-state/card-icon-mode
            (app-state/set-card-icon-mode db :unknown))))))
(deftest overlay-panels-can-be-initialized-and-toggled
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}
                                  :open-panels #{:cards :unknown}})
        move-open-db (app-state/set-panel-open db :move true)
        cards-closed-db (app-state/toggle-panel move-open-db :cards)]
    (is (= #{:cards} (app-state/open-panels db)))
    (is (true? (app-state/panel-open? db :cards)))
    (is (false? (app-state/panel-open? db :move)))
    (is (= #{:cards :move} (app-state/open-panels move-open-db)))
    (is (= #{:move} (app-state/open-panels cards-closed-db)))
    (is (= cards-closed-db
           (app-state/toggle-panel cards-closed-db :unknown)))))
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
(deftest icon-help-visibility-can-be-controlled
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}})
        icon-db (app-state/open-icon-help db)
        hotkey-db (app-state/open-hotkey-help icon-db)
        closed-db (app-state/close-help-dialogs icon-db)]
    (is (false? (app-state/icon-help-open? db)))
    (is (true? (app-state/icon-help-open? icon-db)))
    (is (false? (app-state/hotkey-help-open? icon-db)))
    (is (true? (app-state/hotkey-help-open? hotkey-db)))
    (is (false? (app-state/icon-help-open? hotkey-db)))
    (is (false? (app-state/icon-help-open?
                 (app-state/close-icon-help icon-db))))
    (is (false? (app-state/icon-help-open?
                 (app-state/set-icon-help-open db nil))))
    (is (false? (app-state/icon-help-open? closed-db)))
    (is (false? (app-state/hotkey-help-open? closed-db)))))
(deftest initialize-records-explicit-setup-errors
  (let [db (app-state/initialize {:player-specs [{:id :solo}]})]
    (is (= :invalid-player-count
           (:code (app-state/setup-error db))))
    (is (nil? (app-state/game db)))
    (is (empty? (app-state/board db)))
    (is (empty? (app-state/board-pieces db)))))
(deftest initialize-can-start-in-local-lobby
  (let [db (app-state/initialize {:start-in-lobby? true
                                  :player-specs test-player-specs})
        lobby-view (app-state/lobby-view db)
        header-view (app-state/header-view db)
        app-view (app-state/app-view db)]
    (is (nil? (app-state/game db)))
    (is (true? (app-state/lobby-active? db)))
    (is (= [:rose :indigo]
           (mapv :id (:players lobby-view))))
    (is (= ["Rose" "Indigo"]
           (mapv :name (:players lobby-view))))
    (is (= 2 (:player-count lobby-view)))
    (is (true? (:can-start? lobby-view)))
    (is (true? (:can-add? lobby-view)))
    (is (true? (:lobby? header-view)))
    (is (nil? (:current-player header-view)))
    (is (true? (:lobby? app-view)))))
(deftest bypass-lobby-initializes-game-for-fixtures
  (let [db (app-state/initialize {:start-in-lobby? true
                                  :bypass-lobby? true
                                  :player-specs test-player-specs
                                  :game-options {:shuffle-fn identity}
                                  :demo-board-pieces (fixtures/demo-board-pieces-for
                                                      test-player-specs)})]
    (is (nil? (app-state/lobby db)))
    (is (some? (app-state/game db)))
    (is (= #{:rose :indigo}
           (set (map :player-id (app-state/board-pieces db)))))
    (is (game-schema/valid-game? (app-state/game db)))))
