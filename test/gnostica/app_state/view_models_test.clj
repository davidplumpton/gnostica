(ns gnostica.app-state.view-models-test
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

(deftest header-view-exposes-scoreboard-and-end-turn-controls
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order
                                                 (deck-with-cards-at
                                                  {(board-card-position test-player-specs 0) "sun"})}
                                  :demo-board-pieces [rose-source-piece]})
        header-view (app-state/header-view db)
        challenged-db (app-state/end-turn db {:announce-challenge? true})
        challenged-header (app-state/header-view challenged-db)]
    (is (true? (:can-end-turn? header-view)))
    (is (true? (:can-announce-challenge? header-view)))
    (is (true? (:show-turn-actions? header-view)))
    (is (= 9 (get-in header-view [:game-status :target-score])))
    (is (= 3 (get-in header-view [:game-status :players 0 :score])))
    (is (:ok? (:turn-result challenged-db)))
    (is (= :indigo (get-in challenged-db [:game :turn :current-player-id])))
    (is (= :rose (get-in challenged-header [:game-status :active-challenge-player-id])))
    (is (false? (:can-announce-challenge? challenged-header)))
    (is (game-schema/valid-game? (app-state/game challenged-db)))))
(deftest header-view-keeps-turn-actions-visible-while-initial-placement-blocked
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:shuffle-fn identity}})
        header-view (app-state/header-view db)]
    (is (= :rose (get-in header-view [:current-player :id])))
    (is (true? (:show-turn-actions? header-view)))
    (is (false? (:can-end-turn? header-view)))
    (is (false? (:can-announce-challenge? header-view)))))
(deftest header-view-challenge-availability-follows-facade
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:shuffle-fn identity}
                                  :demo-board-pieces [rose-source-piece]})
        challenged-db (app-state/end-turn db {:announce-challenge? true})
        finished-db (assoc-in db [:game :phase] :finished)
        eliminated-db (mark-game-player-eliminated db :rose)
        lobby-db (app-state/initialize {:start-in-lobby? true
                                        :player-specs test-player-specs})
        cases {:available db
               :active-challenge challenged-db
               :finished finished-db
               :eliminated-current-player eliminated-db
               :lobby lobby-db}]
    (doseq [[label case-db] cases
            :let [header-view (app-state/header-view case-db)]]
      (is (= (app-state/can-announce-challenge? case-db)
             (:can-announce-challenge? header-view))
          (str "header challenge availability should match facade for "
               (name label))))
    (is (false? (:can-announce-challenge?
                 (app-state/header-view eliminated-db))))))
(deftest pre-game-view-models-handle-missing-game
  (let [db (app-state/initialize {:start-in-lobby? true
                                  :player-specs test-player-specs})
        board-view (app-state/board-view db)
        card-view (app-state/card-zones-view db)
        territory-view (app-state/territory-view db)
        move-view (app-state/move-panel-view db)]
    (is (true? (:empty? board-view)))
    (is (empty? (:cells board-view)))
    (is (nil? (:current-player card-view)))
    (is (empty? (get-in card-view [:zones :hand])))
    (is (nil? (:cell territory-view)))
    (is (nil? (:current-player move-view)))
    (is (false? (:show-turn-actions? (app-state/header-view db))))
    (is (every? (comp false? :enabled?) (:source-options move-view)))))
(deftest selecting-board-card-updates-selected-territory
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}
                                  :demo-board-pieces fixtures/demo-board-pieces})
        selected-db (app-state/select-board-card db 4)]
    (is (= 4 (app-state/selected-board-index selected-db)))
    (is (= (get (app-state/board db) 4)
           (app-state/selected-board-cell selected-db)))
    (is (= (pieces/pieces-for-space fixtures/demo-board-pieces 4)
           (app-state/selected-board-pieces selected-db)))))
(deftest board-view-model-aggregates-fallback-layout-data
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:shuffle-fn identity}
                                  :demo-board-pieces [rose-source-piece]})
        view (app-state/board-view (assoc db :three-renderer-error "No WebGL"))]
    (is (= 9 (count (:cells view))))
    (is (= [rose-source-piece] (:board-pieces view)))
    (is (= [rose-source-piece]
           (get (:pieces-by-space view) (pieces/territory-space 0))))
    (is (= 12 (count (:wastelands view))))
    (is (= {:min-row -1
            :max-row 3
            :min-col -1
            :max-col 3}
           (:space-bounds view)))
    (is (= app-state/default-selected-board-index
           (:selected-index view)))
    (is (= :always (:card-icon-mode view)))
    (is (= "No WebGL" (:renderer-error view)))
    (is (false? (:three-renderer-available? view)))
    (is (= "Three.js WebGL rendering is unavailable; using the CSS board. No WebGL"
           (:three-renderer-message view)))))
(deftest board-view-keeps-overflow-space-pieces-visible-to-target-descriptors
  (let [player-specs (mapv #(select-keys % [:id :name]) pieces/players)
        db (app-state/initialize {:player-specs player-specs
                                  :game-options {:shuffle-fn identity}
                                  :demo-board-pieces fixtures/overflow-board-pieces})
        view (app-state/board-view db)
        overflow-space (pieces/territory-space 4)
        overflow-pieces (get (:pieces-by-space view) overflow-space)
        overflow-ids (set (map :id overflow-pieces))
        descriptor-ids (->> (get-in view [:legal-targets :pieces])
                            (filter #(= overflow-space (:space-key %)))
                            (map :piece-id)
                            set)]
    (is (< pieces/max-pieces-per-space (count overflow-pieces)))
    (is (= overflow-ids descriptor-ids))
    (is (= overflow-ids
           (set (map (comp :id second)
                     (layout/visible-piece-slots overflow-pieces)))))))
(deftest board-view-model-uses-stored-three-runtime-status
  (let [runtime-status {:ok? true
                        :code :ready
                        :revision "128"
                        :expected-revision "128"
                        :message "Three.js r128 runtime is ready."}
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:shuffle-fn identity}
                                  :three-runtime-status runtime-status})
        view (app-state/board-view db)
        fallback-view (app-state/board-view
                       (app-state/set-three-runtime-status
                        db
                        {:ok? false
                         :code :three-revision-mismatch
                         :revision "999"
                         :expected-revision "128"
                         :message "Three.js revision 999 is incompatible."}))]
    (is (= runtime-status (:three-runtime-status view)))
    (is (true? (:three-renderer-available? view)))
    (is (= "128" (:three-revision view)))
    (is (= "Three.js r128 runtime is ready."
           (:three-renderer-message view)))
    (is (false? (:three-renderer-available? fallback-view)))
    (is (= "999" (:three-revision fallback-view)))
    (is (= "Three.js revision 999 is incompatible."
           (:three-renderer-message fallback-view)))))
(deftest board-view-exposes-direct-manipulation-capability
  (let [default-db (app-state/initialize {:player-specs test-player-specs
                                          :game-options {:shuffle-fn identity}})
        detailed-db (app-state/initialize {:player-specs test-player-specs
                                           :game-options {:shuffle-fn identity}
                                           :direct-manipulation-enabled? false})
        detailed-default-db (app-state/initialize
                             {:player-specs test-player-specs
                              :game-options {:shuffle-fn identity}
                              :direct-manipulation
                              {:pointer-drag-enabled? false
                               :detailed-entry-default? true}})]
    (is (= {:pointer-drag-enabled? true
            :detailed-entry-available? true
            :detailed-entry-default? false}
           (:direct-manipulation (app-state/board-view default-db))))
    (is (= {:pointer-drag-enabled? false
            :detailed-entry-available? true
            :detailed-entry-default? false}
           (:direct-manipulation (app-state/board-view detailed-db))))
    (is (= {:pointer-drag-enabled? false
            :detailed-entry-available? true
            :detailed-entry-default? true}
           (:direct-manipulation (app-state/board-view detailed-default-db))))
    (is (true? (app-state/panel-open? detailed-default-db :move)))))
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
(deftest composed-ui-view-models-collect-feature-state
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}
                                  :demo-board-pieces [rose-source-piece]})
        selected-db (app-state/select-board-card db 0)
        card-view (app-state/card-zones-view db)
        territory-view (app-state/territory-view selected-db)
        move-view (app-state/move-panel-view db)
        header-view (app-state/header-view db)
        help-view (app-state/help-dialogs-view (app-state/open-hotkey-help db))
        app-view (app-state/app-view db)]
    (is (= :rose (get-in card-view [:current-player :id])))
    (is (= :always (:card-icon-mode card-view)))
    (is (= game-state/starting-hand-size
           (count (get-in card-view [:zones :hand]))))
    (is (= 0 (get-in territory-view [:cell :index])))
    (is (= [rose-source-piece] (:selected-pieces territory-view)))
    (is (= :rose (get-in header-view [:current-player :id])))
    (is (= app-state/default-open-panels (:open-panels header-view)))
    (is (seq (:source-options move-view)))
    (is (contains? (:controls move-view) :piece-options))
    (is (true? (:hotkey-help-open? help-view)))
    (is (false? (:icon-help-open? help-view)))
    (is (= :always (:card-icon-mode app-view)))
    (is (= app-state/default-open-panels (:open-panels app-view)))
    (is (nil? (:setup-error app-view)))))
(deftest territory-view-uses-board-index-after-gaps-and-appends
  (let [deck-order (deck-with-cards-at {0 "coins2"
                                        (board-card-position test-player-specs 3) "cups2"
                                        (board-card-position test-player-specs 5) "swords2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion]})
        gapped-db (remove-board-cell db 4)
        gap-selected-db (app-state/select-board-card gapped-db 5)
        create-result (game-state/apply-cup-move
                       (app-state/game gapped-db)
                       {:player-id :rose
                        :source {:kind :territory
                                 :board-index 3
                                 :piece-id :rose-rod-minion}
                        :target {:kind :wasteland
                                 :row 1
                                 :col 1}
                        :one-point-card-id "coins2"})
        created-db (assoc gapped-db :game (:state create-result))
        created-selected-db (app-state/select-board-card created-db 9)]
    (is (:ok? create-result))
    (is (= "swords2"
           (get-in (app-state/territory-view gap-selected-db)
                   [:cell :card :id])))
    (is (= "coins2"
           (get-in (app-state/territory-view created-selected-db)
                   [:cell :card :id])))))
