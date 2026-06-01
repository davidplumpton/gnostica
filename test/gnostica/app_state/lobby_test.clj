(ns gnostica.app-state.lobby-test
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

(deftest lobby-can-add-remove-and-enforce-player-limits
  (let [db (app-state/initialize {:start-in-lobby? true
                                  :player-specs test-player-specs})
        full-db (-> db
                    app-state/add-lobby-player
                    app-state/add-lobby-player
                    app-state/add-lobby-player
                    app-state/add-lobby-player)
        overfull-db (app-state/add-lobby-player full-db)
        first-slot-id (-> full-db app-state/lobby-players first :slot-id)
        removed-db (app-state/remove-lobby-player full-db first-slot-id)
        underfilled-db (-> db
                           (app-state/remove-lobby-player 1))]
    (is (= game-state/max-players
           (count (app-state/lobby-players full-db))))
    (is (false? (:can-add? (app-state/lobby-view full-db))))
    (is (= :too-many-players
           (get-in overfull-db [:lobby :error :code])))
    (is (= (dec game-state/max-players)
           (count (app-state/lobby-players removed-db))))
    (is (= 1 (count (app-state/lobby-players underfilled-db))))
    (is (= :too-few-players
           (get-in underfilled-db [:lobby :error :code])))
    (is (false? (:can-start? (app-state/lobby-view underfilled-db))))))
(deftest lobby-edits-names-and-rejects-duplicate-colours
  (let [db (app-state/initialize {:start-in-lobby? true
                                  :player-specs test-player-specs})
        renamed-db (app-state/set-lobby-player-name db 1 "Ada")
        duplicate-db (app-state/set-lobby-player-colour renamed-db 2 :rose)
        recoloured-db (app-state/set-lobby-player-colour renamed-db 2 :gold)
        gold-player (second (app-state/lobby-players recoloured-db))]
    (is (= "Ada" (get-in renamed-db [:lobby :players 0 :name])))
    (is (= :duplicate-player-colour
           (get-in duplicate-db [:lobby :error :code])))
    (is (= :indigo
           (get-in duplicate-db [:lobby :players 1 :id])))
    (is (= :gold (:id gold-player)))
    (is (= "Gold" (:name gold-player)))
    (is (nil? (get-in recoloured-db [:lobby :error])))))
(deftest local-controller-lobby-metadata-can-cover-multiple-seats
  (let [controller {:id "local-dev"
                    :name "Local dev"}
        db (app-state/initialize {:start-in-lobby? true
                                  :player-specs test-player-specs
                                  :local-controller controller})
        added-db (app-state/add-lobby-player db)
        lobby-view (app-state/lobby-view added-db)
        started-db (app-state/start-lobby-game db {:shuffle-fn identity})]
    (is (= controller (:local-controller (app-state/lobby db))))
    (is (= controller (:local-controller lobby-view)))
    (is (true? (:local-control? lobby-view)))
    (is (= ["local-dev" "local-dev" "local-dev"]
           (mapv :controller-id (:players lobby-view))))
    (is (= ["Local dev" "Local dev" "Local dev"]
           (mapv :controller-name (:players lobby-view))))
    (is (nil? (app-state/lobby started-db)))
    (is (= [:rose :indigo]
           (mapv :id (get-in started-db [:game :players]))))
    (is (every? nil?
                (map :controller-id (get-in started-db [:game :players]))))
    (is (game-schema/valid-game? (app-state/game started-db)))))
(deftest starting-lobby-game-uses-selected-players-and-injected-shuffle
  (let [db (app-state/initialize {:start-in-lobby? true
                                  :player-specs [{:id :rose
                                                  :name "Ada"}
                                                 {:id :indigo
                                                  :name "Babbage"}]})
        started-db (app-handlers/start-lobby-game-db db {:shuffle-seed 8675309})
        seeded-deck (deterministic-shuffle/shuffle-with-seed 8675309 cards/deck)]
    (is (nil? (app-state/lobby started-db)))
    (is (nil? (app-state/setup-error started-db)))
    (is (= "Ada" (get-in started-db [:game :players-by-id :rose :name])))
    (is (= "Babbage" (get-in started-db [:game :players-by-id :indigo :name])))
    (is (= [:rose :indigo]
           (get-in started-db [:game :turn :order])))
    (is (= (mapv :id (take game-state/starting-hand-size seeded-deck))
           (mapv :id (app-state/current-player-hand started-db))))
    (is (game-schema/valid-game? (app-state/game started-db)))))
(deftest lobby-starting-bid-stages-card-picks-before-confirming-game
  (let [db (app-state/initialize
            {:start-in-lobby? true
             :player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at
                                          {0 "cupsking"
                                           6 "fool"})}})
        bidding-db (app-state/start-lobby-bidding db)
        bidding-view (app-state/lobby-view bidding-db)
        selected-db (-> bidding-db
                        (app-state/select-lobby-bid-card :rose "cupsking")
                        (app-state/select-lobby-bid-card :indigo "fool"))
        revealed-db (app-state/reveal-lobby-bids selected-db)
        revealed-view (app-state/lobby-view revealed-db)
        premature-db (app-state/confirm-lobby-bidding revealed-db)
        rose-redraw-db (app-state/select-lobby-redraw-card revealed-db
                                                           :rose
                                                           "fool")
        rose-redraw-view (app-state/lobby-view rose-redraw-db)
        redrawn-db (app-state/select-lobby-redraw-card rose-redraw-db
                                                       :indigo
                                                       "cupsking")
        redrawn-view (app-state/lobby-view redrawn-db)
        started-db (app-state/confirm-lobby-bidding redrawn-db)
        state (app-state/game started-db)]
    (is (nil? (app-state/game bidding-db)))
    (is (true? (get-in bidding-view [:starting-bid :active?])))
    (is (= :choosing (get-in bidding-view [:starting-bid :stage])))
    (is (= ["cupsking"]
           (take 1 (mapv :id (:bid-card-options
                              (first (:players bidding-view)))))))
    (is (nil? (app-state/game revealed-db)))
    (is (= :redrawing (get-in revealed-view [:starting-bid :stage])))
    (is (= :indigo (get-in revealed-view [:starting-bid :winner-id])))
    (is (false? (get-in revealed-view [:starting-bid :can-confirm?])))
    (is (= :rose (get-in revealed-view
                         [:starting-bid :redraw :active-player-id])))
    (is (= ["cupsking" "fool"]
           (mapv :id (get-in revealed-view
                             [:starting-bid :redraw :card-options]))))
    (is (nil? (app-state/game premature-db)))
    (is (= :starting-bid-redraw-incomplete
           (get-in premature-db [:lobby :error :code])))
    (is (= :indigo (get-in rose-redraw-view
                           [:starting-bid :redraw :active-player-id])))
    (is (= ["fool"]
           (get-in rose-redraw-view
                   [:starting-bid :redraw-order 0 :card-ids])))
    (is (= ["cupsking"]
           (mapv :id (get-in rose-redraw-view
                             [:starting-bid :redraw :card-options]))))
    (is (= :resolved (get-in redrawn-view [:starting-bid :stage])))
    (is (true? (get-in redrawn-view [:starting-bid :can-confirm?])))
    (is (nil? (app-state/lobby started-db)))
    (is (= :indigo (get-in state [:setup :starting-player-id])))
    (is (= [:indigo :rose] (get-in state [:turn :order])))
    (is (= :indigo (get-in state [:turn :current-player-id])))
    (is (= game-state/starting-hand-size
           (count (get-in state [:players-by-id :rose :hand]))))
    (is (= game-state/starting-hand-size
           (count (get-in state [:players-by-id :indigo :hand]))))
    (is (= [:rose :indigo]
           (get-in state [:setup :bid-redraw-order])))
    (is (= [{:player-id :rose
             :card-ids ["fool"]}
            {:player-id :indigo
             :card-ids ["cupsking"]}]
           (get-in state [:setup :bid-redraws])))
    (is (game-schema/valid-game? state))))
(deftest lobby-starting-bid-masks-selected-cards-before-reveal
  (let [db (app-state/initialize
            {:start-in-lobby? true
             :player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at
                                          {0 "cupsking"
                                           1 "cupsqueen"
                                           6 "fool"})}})
        bidding-db (app-state/start-lobby-bidding db)
        selected-db (-> bidding-db
                        (app-state/select-lobby-bid-card :rose "cupsking")
                        (app-state/select-lobby-bid-card :indigo "fool"))
        selected-view (app-state/lobby-view selected-db)
        [rose-view indigo-view] (:players selected-view)
        cleared-db (app-state/select-lobby-bid-card selected-db :rose "")
        cleared-view (app-state/lobby-view cleared-db)
        changed-db (app-state/select-lobby-bid-card cleared-db :rose "cupsqueen")
        changed-view (app-state/lobby-view changed-db)
        revealed-view (app-state/lobby-view
                       (app-state/reveal-lobby-bids changed-db))
        cupsking (cards/card-by-id "cupsking")
        cupsqueen (cards/card-by-id "cupsqueen")
        fool (cards/card-by-id "fool")]
    (is (true? (get-in selected-view [:starting-bid :can-reveal?])))
    (is (every? true? (map :bid-ready? [rose-view indigo-view])))
    (is (every? true? (map :bid-card-selected? [rose-view indigo-view])))
    (is (= [nil nil] (mapv :bid-card-id [rose-view indigo-view])))
    (is (= [[] []] (mapv :bid-card-options [rose-view indigo-view])))
    (is (not (contains-data-value? (:id cupsking) selected-view)))
    (is (not (contains-data-value? (:title cupsking) selected-view)))
    (is (not (contains-data-value? (:id fool) selected-view)))
    (is (not (contains-data-value? (:title fool) selected-view)))
    (is (false? (get-in cleared-view [:starting-bid :can-reveal?])))
    (is (some #(= "cupsking" (:id %))
              (-> cleared-view :players first :bid-card-options)))
    (is (true? (get-in changed-view [:starting-bid :can-reveal?])))
    (is (not (contains-data-value? (:id cupsqueen) changed-view)))
    (is (not (contains-data-value? (:title cupsqueen) changed-view)))
    (is (contains-data-value? (:title cupsqueen) revealed-view))
    (is (contains-data-value? (:title fool) revealed-view))))
(deftest lobby-starting-bid-card-changes-clear-stale-bid-errors
  (let [db (app-state/initialize
            {:start-in-lobby? true
             :player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at
                                          {0 "cupsking"
                                           1 "cupsqueen"
                                           6 "fool"})}})
        bidding-db (app-state/start-lobby-bidding db)
        invalid-db (app-state/select-lobby-bid-card bidding-db
                                                     :rose
                                                     "not-a-bid-card")
        cleared-invalid-db (app-state/select-lobby-bid-card invalid-db
                                                            :rose
                                                            "")
        incomplete-db (-> bidding-db
                          (app-state/select-lobby-bid-card :rose "cupsking")
                          app-state/reveal-lobby-bids)
        changed-after-incomplete-db
        (app-state/select-lobby-bid-card incomplete-db :rose "cupsqueen")
        unrelated-error {:code :too-few-players
                         :message "Unrelated validation"
                         :data {:count 1
                                :minimum game-state/min-players}}
        unrelated-error-db (assoc-in bidding-db [:lobby :error]
                                     unrelated-error)
        preserved-error-db (app-state/select-lobby-bid-card
                            unrelated-error-db
                            :rose
                            "")]
    (is (= :invalid-starting-bid-card
           (get-in invalid-db [:lobby :error :code])))
    (is (nil? (get-in cleared-invalid-db [:lobby :error])))
    (is (= :incomplete-starting-bid-round
           (get-in incomplete-db [:lobby :error :code])))
    (is (nil? (get-in changed-after-incomplete-db [:lobby :error])))
    (is (= "cupsqueen"
           (get-in changed-after-incomplete-db
                   [:lobby :starting-bid :current-bids :rose])))
    (is (= unrelated-error
           (get-in preserved-error-db [:lobby :error])))))
(deftest lobby-starting-bid-redraws-follow-order-and-prevent_invalid_choices
  (let [three-player-specs [{:id :rose}
                            {:id :indigo}
                            {:id :gold}]
        db (app-state/initialize
            {:start-in-lobby? true
             :player-specs three-player-specs
             :game-options {:deck-order (deck-with-cards-at
                                          {0 "cupsking"
                                           1 "coins2"
                                           6 "swordsking"
                                           7 "cups3"
                                           12 "wandsqueen"
                                           13 "world"})}})
        redrawing-db (-> db
                         app-state/start-lobby-bidding
                         (app-state/select-lobby-bid-card :rose "cupsking")
                         (app-state/select-lobby-bid-card :indigo "swordsking")
                         (app-state/select-lobby-bid-card :gold "wandsqueen")
                         app-state/reveal-lobby-bids
                         (app-state/select-lobby-bid-card :rose "coins2")
                         (app-state/select-lobby-bid-card :indigo "cups3")
                         (app-state/select-lobby-bid-card :gold "world")
                         app-state/reveal-lobby-bids)
        redrawing-view (app-state/lobby-view redrawing-db)
        inactive-db (app-state/select-lobby-redraw-card redrawing-db
                                                        :rose
                                                        "world")
        first-redraw-db (app-state/select-lobby-redraw-card redrawing-db
                                                            :indigo
                                                            "world")
        duplicate-db (app-state/select-lobby-redraw-card first-redraw-db
                                                         :indigo
                                                         "world")
        second-redraw-db (app-state/select-lobby-redraw-card first-redraw-db
                                                             :indigo
                                                             "cupsking")
        second-redraw-view (app-state/lobby-view second-redraw-db)]
    (is (nil? (app-state/game redrawing-db)))
    (is (= [:indigo :rose :gold]
           (mapv :player-id
                 (get-in redrawing-view [:starting-bid :redraw-order]))))
    (is (= :indigo (get-in redrawing-view
                           [:starting-bid :redraw :active-player-id])))
    (is (= ["cupsking" "swordsking" "wandsqueen"
            "coins2" "cups3" "world"]
           (mapv :id (get-in redrawing-view
                             [:starting-bid :redraw :card-options]))))
    (is (= :inactive-starting-bid-redraw-player
           (get-in inactive-db [:lobby :error :code])))
    (is (= {} (get-in inactive-db [:lobby :starting-bid :redraws])))
    (is (= :invalid-bid-redraw-card
           (get-in duplicate-db [:lobby :error :code])))
    (is (= {:indigo ["world"]}
           (get-in duplicate-db [:lobby :starting-bid :redraws])))
    (is (= :rose (get-in second-redraw-view
                         [:starting-bid :redraw :active-player-id])))
    (is (= ["swordsking" "wandsqueen" "coins2" "cups3"]
           (mapv :id (get-in second-redraw-view
                             [:starting-bid :redraw :card-options]))))
    (is (nil? (app-state/game second-redraw-db)))))
(deftest lobby-starting-bid-redraw-selections-can-be-cleared-and-changed
  (let [db (app-state/initialize
            {:start-in-lobby? true
             :player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at
                                          {0 "cupsking"
                                           6 "fool"})}})
        redrawing-db (-> db
                         app-state/start-lobby-bidding
                         (app-state/select-lobby-bid-card :rose "cupsking")
                         (app-state/select-lobby-bid-card :indigo "fool")
                         app-state/reveal-lobby-bids)
        rose-redraw-db (app-state/select-lobby-redraw-card redrawing-db
                                                           :rose
                                                           "fool")
        rose-redraw-view (app-state/lobby-view rose-redraw-db)
        cleared-db (app-state/select-lobby-redraw-card rose-redraw-db
                                                       :rose
                                                       "")
        cleared-view (app-state/lobby-view cleared-db)
        changed-db (app-state/select-lobby-redraw-card cleared-db
                                                       :rose
                                                       "cupsking")
        changed-view (app-state/lobby-view changed-db)
        resolved-db (app-state/select-lobby-redraw-card changed-db
                                                        :indigo
                                                        "fool")
        resolved-cleared-db (app-state/select-lobby-redraw-card resolved-db
                                                                :rose
                                                                "")
        resolved-cleared-view (app-state/lobby-view resolved-cleared-db)
        final-db (app-state/select-lobby-redraw-card resolved-cleared-db
                                                     :rose
                                                     "cupsking")
        started-db (app-state/confirm-lobby-bidding final-db)
        state (app-state/game started-db)]
    (is (= {:rose ["fool"]}
           (get-in rose-redraw-db [:lobby :starting-bid :redraws])))
    (is (true? (get-in rose-redraw-view
                       [:starting-bid :redraw-order 0 :can-clear?])))
    (is (= :indigo (get-in rose-redraw-view
                           [:starting-bid :redraw :active-player-id])))
    (is (= {}
           (get-in cleared-db [:lobby :starting-bid :redraws])))
    (is (= :redrawing (get-in cleared-view [:starting-bid :stage])))
    (is (= :rose (get-in cleared-view
                         [:starting-bid :redraw :active-player-id])))
    (is (= ["cupsking" "fool"]
           (mapv :id (get-in cleared-view
                             [:starting-bid :redraw :card-options]))))
    (is (= {:rose ["cupsking"]}
           (get-in changed-db [:lobby :starting-bid :redraws])))
    (is (= :indigo (get-in changed-view
                           [:starting-bid :redraw :active-player-id])))
    (is (= ["fool"]
           (mapv :id (get-in changed-view
                             [:starting-bid :redraw :card-options]))))
    (is (= :resolved (get-in (app-state/lobby-view resolved-db)
                             [:starting-bid :stage])))
    (is (= :redrawing (get-in resolved-cleared-view
                              [:starting-bid :stage])))
    (is (= :rose (get-in resolved-cleared-view
                         [:starting-bid :redraw :active-player-id])))
    (is (= ["cupsking"]
           (mapv :id (get-in resolved-cleared-view
                             [:starting-bid :redraw :card-options]))))
    (is (nil? (app-state/lobby started-db)))
    (is (= [{:player-id :rose
             :card-ids ["cupsking"]}
            {:player-id :indigo
             :card-ids ["fool"]}]
           (get-in state [:setup :bid-redraws])))
    (is (game-schema/valid-game? state))))
(deftest casual-lobby-start-is-blocked-while-starting-bid-is-staged
  (let [db (app-state/initialize
            {:start-in-lobby? true
             :player-specs test-player-specs})
        bidding-db (app-state/start-lobby-bidding db {:shuffle-fn identity})
        app-state-start-db (app-state/start-lobby-game bidding-db
                                                       {:shuffle-fn identity})
        handler-start-db (app-handlers/start-lobby-game-db
                          bidding-db
                          {:shuffle-seed 8675309})]
    (doseq [attempted-db [app-state-start-db handler-start-db]]
      (is (nil? (app-state/game attempted-db)))
      (is (= (get-in bidding-db [:lobby :starting-bid])
             (get-in attempted-db [:lobby :starting-bid])))
      (is (= :starting-bid-active
             (get-in attempted-db [:lobby :error :code])))
      (is (= :choosing
             (get-in attempted-db [:lobby :error :data :stage])))
      (is (false? (:can-start? (app-state/lobby-view attempted-db)))))))
(deftest lobby-starting-bid-repeats-after-a-tied-round
  (let [db (app-state/initialize
            {:start-in-lobby? true
             :player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at
                                          {0 "cupsking"
                                           1 "cupsqueen"
                                           6 "swordsking"
                                           7 "swords10"})}})
        tie-db (-> db
                   app-state/start-lobby-bidding
                   (app-state/select-lobby-bid-card :rose "cupsking")
                   (app-state/select-lobby-bid-card :indigo "swordsking")
                   app-state/reveal-lobby-bids)
        tie-view (app-state/lobby-view tie-db)
        rose-options (-> tie-view :players first :bid-card-options)
        resolved-db (-> tie-db
                        (app-state/select-lobby-bid-card :rose "cupsqueen")
                        (app-state/select-lobby-bid-card :indigo "swords10")
                        app-state/reveal-lobby-bids)
        redrawn-db (-> resolved-db
                       (app-state/select-lobby-redraw-card :indigo
                                                           "cupsking")
                       (app-state/select-lobby-redraw-card :indigo
                                                           "swordsking")
                       (app-state/select-lobby-redraw-card :rose
                                                           "cupsqueen")
                       (app-state/select-lobby-redraw-card :rose
                                                           "swords10"))
        started-db (app-state/confirm-lobby-bidding redrawn-db)
        state (app-state/game started-db)]
    (is (nil? (app-state/game tie-db)))
    (is (= :choosing (get-in tie-view [:starting-bid :stage])))
    (is (= 2 (get-in tie-view [:starting-bid :round-number])))
    (is (= [:rose :indigo]
           (get-in tie-view [:starting-bid :history 0 :tied-player-ids])))
    (is (not (some #(= "cupsking" (:id %)) rose-options)))
    (is (= :rose (get-in (app-state/lobby-view resolved-db)
                         [:starting-bid :winner-id])))
    (is (= :rose (get-in state [:setup :starting-player-id])))
    (is (= 2 (count (get-in state [:setup :bid-history]))))
    (is (every? #(= game-state/starting-hand-size (count (:hand %)))
                (:players state)))
    (is (game-schema/valid-game? state))))
(deftest lobby-bidding-handler-uses-injected-shuffle-for_initial_hands
  (let [db (app-state/initialize {:start-in-lobby? true
                                  :player-specs test-player-specs})
        bidding-db (app-handlers/start-lobby-bidding-db db {:shuffle-seed 8675309})
        seeded-deck (deterministic-shuffle/shuffle-with-seed 8675309 cards/deck)
        rose-options (-> (app-state/lobby-view bidding-db)
                         :players
                         first
                         :bid-card-options)]
    (is (nil? (app-state/game bidding-db)))
    (is (= (mapv :id (take game-state/starting-hand-size seeded-deck))
           (mapv :id rose-options)))))
(deftest lobby-target-score-flows-into-started-game
  (let [db (app-state/initialize {:start-in-lobby? true
                                  :player-specs test-player-specs})
        target-db (app-state/set-lobby-target-score db "10")
        invalid-db (app-state/set-lobby-target-score db "7")
        started-db (app-state/start-lobby-game target-db {:shuffle-fn identity})]
    (is (= 10 (:target-score (app-state/lobby-view target-db))))
    (is (= :invalid-target-score
           (get-in invalid-db [:lobby :error :code])))
    (is (= 10 (get-in started-db [:game :setup :target-score])))
    (is (game-schema/valid-game? (app-state/game started-db)))))
