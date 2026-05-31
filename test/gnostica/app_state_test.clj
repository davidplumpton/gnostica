(ns gnostica.app-state-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.app.handlers :as app-handlers]
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
            [gnostica.test-support.deck :refer [board-card-position
                                                deck-starting-with
                                                deck-with-card-at
                                                deck-with-cards-at]]))

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

(def rose-rod-minion
  {:id :rose-rod-minion
   :player-id :rose
   :space-index 3
   :size :medium
   :orientation :east})

(def rose-rod-target
  {:id :rose-rod-target
   :player-id :rose
   :space-index 5
   :size :small
   :orientation :north})

(def indigo-rod-target
  {:id :indigo-rod-target
   :player-id :indigo
   :space-index 4
   :size :small
   :orientation :north})

(def rose-hand-cup-territory-piece
  (assoc rose-hand-piece
         :space-index 6
         :orientation :north))

(def rose-hand-cup-enemy-piece
  (assoc rose-hand-piece
         :space-index 3
         :orientation :east))

(defn- contains-data-value? [needle data]
  (boolean
   (some #(= needle %)
         (tree-seq coll? seq data))))

(defn- move-control-group-summary [db]
  (mapv #(select-keys % [:type :power :action-power])
        (:control-groups (app-state/move-panel-view db))))

(defn- action-ribbon-step-summary [view]
  (mapv #(select-keys % [:power :status :board-index :compound?])
        (get-in view [:action-ribbon :steps])))

(defn- alternative-by-field [tray field]
  (some #(when (= field (:field %)) %)
        (:alternatives tray)))

(def ^:private expected-major-power-options
  {"fool" [:fool]
   "magician" [:cup :rod :disc :sword]
   "high-priestess" [:high-priestess]
   "empress" [:empress :cup]
   "emperor" [:emperor :rod]
   "hierophant" [:hierophant]
   "lovers" [:lovers :cup :rod]
   "chariot" [:chariot :rod]
   "justice" [:justice :sword]
   "hermit" [:hermit]
   "wheeloffortune" [:cup]
   "strength" [:disc]
   "hangedman" [:hanged-man :rod]
   "death" [:death :sword]
   "temperance" [:temperance :cup]
   "devil" [:devil]
   "tower" [:tower :sword]
   "star" [:disc]
   "moon" [:moon :rod :sword]
   "sun" [:cup :disc :sun]
   "judgement" [:judgement]
   "world" [:world]})

(deftest move-power-registry-covers-implemented-browser-powers
  (is (= (set move-registry/move-power-order)
         (set (keys move-registry/move-power-registry))))
  (doseq [power move-registry/move-power-order
          :let [definition (move-registry/power-definition power)]]
    (is (= power (:id definition)) power)
    (is (seq (:label definition)) power)
    (is (keyword? (move-registry/power-command-builder power)) power)
    (is (fn? (move-registry/power-transition-fn power)) power)
    (is (contains? #{:static :fool :world :composite-major :sword-major}
                   (move-registry/power-control-kind power))
        power))
  (doseq [[card-id expected-options] expected-major-power-options]
    (is (= expected-options
           (move-registry/power-ids-for-card (cards/card-by-id card-id)))
        card-id)))

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
    (is (= 9 (get-in header-view [:game-status :target-score])))
    (is (= 3 (get-in header-view [:game-status :players 0 :score])))
    (is (:ok? (:turn-result challenged-db)))
    (is (= :indigo (get-in challenged-db [:game :turn :current-player-id])))
    (is (= :rose (get-in challenged-header [:game-status :active-challenge-player-id])))
    (is (false? (:can-announce-challenge? challenged-header)))
    (is (game-schema/valid-game? (app-state/game challenged-db)))))

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

(deftest major-card-power-options-cover-implemented-browser-powers
  (doseq [[card-id expected-options] expected-major-power-options]
    (let [db (app-state/initialize {:player-specs test-player-specs
                                    :game-options {:deck-order
                                                   (deck-starting-with
                                                    [card-id])}
                                    :demo-board-pieces [rose-hand-piece]})
          piece-db (-> db
                       (app-state/select-move-source :play-hand-card)
                       (app-state/select-move-hand-card card-id)
                       (app-state/select-move-piece :rose-striker))
          expected-power (when (= 1 (count expected-options))
                           (first expected-options))]
      (is (= expected-options
             (mapv :id (app-state/move-power-options piece-db)))
          card-id)
      (is (= expected-power
             (app-state/move-power piece-db))
          card-id))))

(deftest unsupported-card-powers-fail-explicitly-without-mutating-game
  (let [blank-card {:id "blank-major"
                    :title "Blank Major"
                    :image "/images/blank-major.png"
                    :arcana :major
                    :group "Major Arcana"}
        db (-> (app-state/initialize {:player-specs test-player-specs
                                      :game-options {:shuffle-fn identity}
                                      :demo-board-pieces [rose-hand-piece]})
               (replace-game-player-hand :rose [blank-card]))
        ready-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "blank-major")
                     (app-state/select-move-piece :rose-striker))
        confirmed-db (app-state/confirm-move ready-db)]
    (is (empty? (app-state/move-power-options ready-db)))
    (is (= :unavailable (app-state/move-power ready-db)))
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "blank-major"
                     :piece-id :rose-striker}
            :power :unavailable
            :card-id "blank-major"}
           (app-state/move-command ready-db)))
    (is (= :rejected (:stage (app-state/move-selection confirmed-db))))
    (is (= :move-transition-unavailable
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= (app-state/game ready-db)
           (app-state/game confirmed-db)))))

(deftest selecting-surviving-territory-after-destruction-uses-board-index
  (let [source-piece {:id :rose-gap-source
                      :player-id :rose
                      :space-index 5
                      :size :small
                      :orientation :up}
        deck-order (deck-with-cards-at {(board-card-position test-player-specs 5) "cups2"
                                        (board-card-position test-player-specs 6) "swords2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [source-piece]})
        gapped-db (remove-board-cell db 4)
        selected-db (app-state/select-board-card gapped-db 5)
        source-db (app-state/select-move-source selected-db :activate-territory)
        piece-db (app-state/select-move-piece source-db :rose-gap-source)]
    (is (= 5 (app-state/selected-board-index selected-db)))
    (is (= 5 (get-in (app-state/selected-board-cell selected-db) [:index])))
    (is (= [source-piece] (app-state/selected-board-pieces selected-db)))
    (is (= [5] (mapv :index (app-state/move-source-board-options gapped-db))))
    (is (= {:source-board-index 5}
           (app-state/move-params source-db)))
    (is (= [:cup] (mapv :id (app-state/move-power-options piece-db))))))

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

(deftest legal-target-descriptors-track-gapped-and-appended-board-indexes
  (let [deck-order (deck-with-cards-at {0 "coins2"
                                        (board-card-position test-player-specs 3) "cups2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion]})
        gapped-db (remove-board-cell db 4)
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
        source-db (-> created-db
                      (app-state/select-board-card 3)
                      (app-state/select-move-source :activate-territory)
                      (app-state/select-move-piece :rose-rod-minion))
        targets (app-state/move-legal-targets source-db)
        board-targets (:legal-targets (app-state/board-view source-db))
        card-zone-targets (:legal-targets (app-state/card-zones-view source-db))]
    (is (:ok? create-result))
    (is (= :target (:stage (app-state/move-selection source-db))))
    (is (= [0 1 2 3 5 6 7 8 9]
           (mapv :board-index (:territories targets))))
    (is (nil? (territory-target targets 4)))
    (is (= :legal (:status (territory-target targets 9))))
    (is (= :disabled (:status (territory-target targets 3))))
    (is (= :invalid-cup-target
           (get-in (territory-target targets 3) [:error :code])))
    (is (= targets board-targets))
    (is (= targets card-zone-targets))))

(deftest legal-target-descriptors-cover-orient-and-initial-placement
  (let [piece-db (-> (app-state/initialize {:player-specs test-player-specs
                                            :game-options {:shuffle-fn identity}
                                            :demo-board-pieces [rose-source-piece]})
                     (app-state/select-move-source :orient-piece))
        piece-targets (app-state/move-legal-targets piece-db)
        initial-db (-> (app-state/initialize {:player-specs test-player-specs
                                              :game-options {:shuffle-fn identity}})
                       (assoc :selected-board-index 99)
                       (app-state/select-move-source :place-initial-small))
        initial-targets (app-state/move-legal-targets initial-db)
        hand-db (-> (app-state/initialize
                     {:player-specs test-player-specs
                      :game-options {:deck-order (deck-starting-with ["cups2"])}
                      :demo-board-pieces [rose-source-piece]})
                    (app-state/select-move-source :play-hand-card))
        hand-targets (app-state/move-legal-targets hand-db)]
    (is (= :piece (:stage (app-state/move-selection piece-db))))
    (is (= :legal (:status (piece-target piece-targets :rose-scout))))
    (is (= :minion (:role (piece-target piece-targets :rose-scout))))
    (is (= :target (:stage (app-state/move-selection initial-db))))
    (is (= 9 (count (filter :enabled? (:territories initial-targets)))))
    (is (= 12 (count (filter :enabled? (:wastelands initial-targets)))))
    (is (= [{:kind :stash-piece
             :role :source
             :player-id :rose
             :size :small
             :count 5
             :active? true
             :enabled? true
             :status :legal}]
           (:stash-pieces initial-targets)))
    (is (= :legal (:status (hand-card-target hand-targets "cups2"))))
    (is (= :source (:role (hand-card-target hand-targets "cups2"))))
    (is (= hand-targets
           (:legal-targets (app-state/card-zones-view hand-db))))))

(deftest legal-target-descriptors-cover-rod-disc-and-sword-targets
  (let [rod-db (-> (app-state/initialize
                    {:player-specs test-player-specs
                     :game-options {:deck-order (deck-with-card-at
                                                 (board-card-position test-player-specs 0)
                                                 "wands2")}
                     :demo-board-pieces [rose-source-piece]})
                   (app-state/select-move-source :activate-territory)
                   (app-state/select-move-piece :rose-scout)
                   (app-state/select-move-rod-mode :push-territory))
        disc-db (-> (app-state/initialize
                     {:player-specs test-player-specs
                      :game-options {:deck-order (deck-with-cards-at
                                                  {0 "coins2"
                                                   1 "cupsking"
                                                   (board-card-position test-player-specs 4) "cups2"})}
                      :demo-board-pieces [rose-rod-minion]})
                    (app-state/select-move-source :play-hand-card)
                    (app-state/select-move-hand-card "coins2")
                    (app-state/select-move-piece :rose-rod-minion)
                    (app-state/select-move-disc-target-kind :territory))
        sword-db (-> (app-state/initialize
                      {:player-specs test-player-specs
                       :game-options {:deck-order (deck-with-cards-at
                                                   {0 "swords2"
                                                    1 "cups2"
                                                    (board-card-position test-player-specs 4) "cupsking"})}
                       :demo-board-pieces [rose-rod-minion]})
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "swords2")
                     (app-state/select-move-piece :rose-rod-minion)
                     (app-state/select-move-sword-target-kind :territory))]
    (is (= [1] (mapv :index (app-state/move-target-board-options rod-db))))
    (is (= :legal (:status (territory-target (app-state/move-legal-targets rod-db) 1))))
    (is (= :disabled (:status (territory-target (app-state/move-legal-targets rod-db) 8))))
    (is (= [4] (mapv :index (app-state/move-target-board-options disc-db))))
    (is (= :legal (:status (territory-target (app-state/move-legal-targets disc-db) 4))))
    (is (= :invalid-disc-target
           (get-in (territory-target (app-state/move-legal-targets disc-db) 0)
                   [:error :code])))
    (is (= [4] (mapv :index (app-state/move-target-board-options sword-db))))
    (is (= :legal (:status (territory-target (app-state/move-legal-targets sword-db) 4))))
    (is (= :invalid-sword-target
           (get-in (territory-target (app-state/move-legal-targets sword-db) 0)
                   [:error :code])))))

(deftest resolver-probed-targets-confirm-with-shared-command-builders
  (testing "Rod"
    (let [db (app-state/initialize {:player-specs test-player-specs
                                    :game-options {:deck-order (deck-starting-with ["wands2"])}
                                    :demo-board-pieces [rose-rod-minion
                                                        indigo-rod-target]})
          source-db (-> db
                        (app-state/select-move-source :play-hand-card)
                        (app-state/select-move-hand-card "wands2")
                        (app-state/select-move-piece :rose-rod-minion)
                        (app-state/select-move-rod-mode :push-piece))]
      (is (some #(= :indigo-rod-target (:id %))
                (app-state/move-target-piece-options source-db)))
      (is (:ok? (get-in (-> source-db
                            (app-state/select-move-target-piece :indigo-rod-target)
                            (app-state/set-move-distance 1)
                            app-state/confirm-move)
                      [:move-selection :last-result])))))
  (testing "Disc"
    (let [db (app-state/initialize {:player-specs test-player-specs
                                    :game-options {:deck-order
                                                   (deck-with-cards-at
                                                    {0 "coins2"
                                                     1 "cupsking"
                                                     (board-card-position test-player-specs 4) "cups2"})}
                                    :demo-board-pieces [rose-rod-minion]})
          source-db (-> db
                        (app-state/select-move-source :play-hand-card)
                        (app-state/select-move-hand-card "coins2")
                        (app-state/select-move-piece :rose-rod-minion)
                        (app-state/select-move-disc-target-kind :territory))]
      (is (some #(= 4 (:index %))
                (app-state/move-target-board-options source-db)))
      (is (:ok? (get-in (-> source-db
                            (app-state/select-board-card 4)
                            (app-state/select-move-replacement-card "cupsking")
                            app-state/confirm-move)
                      [:move-selection :last-result])))))
  (testing "Sword"
    (let [db (app-state/initialize {:player-specs test-player-specs
                                    :game-options {:deck-order
                                                   (deck-with-cards-at
                                                    {0 "swords2"
                                                     1 "cups2"
                                                     (board-card-position test-player-specs 4) "cupsking"})}
                                    :demo-board-pieces [rose-rod-minion]})
          source-db (-> db
                        (app-state/select-move-source :play-hand-card)
                        (app-state/select-move-hand-card "swords2")
                        (app-state/select-move-piece :rose-rod-minion)
                        (app-state/select-move-sword-target-kind :territory))]
      (is (some #(= 4 (:index %))
                (app-state/move-target-board-options source-db)))
      (is (:ok? (get-in (-> source-db
                            (app-state/select-board-card 4)
                            (app-state/set-move-damage 1)
                            (app-state/select-move-replacement-card "cups2")
                            app-state/confirm-move)
                      [:move-selection :last-result])))))
  (testing "Sun"
    (let [db (app-state/initialize {:player-specs test-player-specs
                                    :game-options {:deck-order (deck-starting-with ["sun"])}
                                    :demo-board-pieces [rose-hand-cup-enemy-piece
                                                        rose-rod-target
                                                        indigo-rod-target]})
          source-db (-> db
                        (app-state/select-move-source :play-hand-card)
                        (app-state/select-move-hand-card "sun")
                        (app-state/select-move-piece :rose-striker)
                        (app-state/select-move-power :sun)
                        (app-state/select-move-target-piece :indigo-rod-target)
                        (app-state/select-move-sun-disc-mode :piece))]
      (is (some #(= :rose-striker (:id %))
                (app-state/move-target-piece-options source-db)))
      (is (:ok? (get-in (-> source-db
                            (app-state/select-move-target-piece :rose-striker)
                            (app-state/set-move-sun-disc-orientation :west)
                            app-state/confirm-move)
                      [:move-selection :last-result])))))
  (testing "Moon"
    (let [db (app-state/initialize {:player-specs test-player-specs
                                    :game-options {:deck-order (deck-starting-with ["moon"])}
                                    :demo-board-pieces [rose-rod-minion
                                                        indigo-rod-target]})
          source-db (-> db
                        (app-state/select-move-source :play-hand-card)
                        (app-state/select-move-hand-card "moon")
                        (app-state/select-move-piece :rose-rod-minion)
                        (app-state/select-move-power :moon)
                        (app-state/select-move-rod-mode :move-minion)
                        (app-state/set-move-distance 1)
                        (app-state/set-move-orientation :up)
                        (app-state/select-move-sword-target-kind :piece))]
      (is (some #(= :indigo-rod-target (:id %))
                (app-state/move-target-piece-options source-db)))
      (is (:ok? (get-in (-> source-db
                            (app-state/select-move-target-piece :indigo-rod-target)
                            (app-state/set-move-damage 1)
                            app-state/confirm-move)
                      [:move-selection :last-result])))))
  (testing "World copied Rod"
    (let [db (app-state/initialize {:player-specs test-player-specs
                                    :game-options {:deck-order
                                                   (deck-with-cards-at
                                                    {0 "world"
                                                     (board-card-position test-player-specs 3) "magician"})}
                                    :demo-board-pieces [rose-rod-minion
                                                        indigo-rod-target]})
          source-db (-> db
                        (app-state/select-move-source :play-hand-card)
                        (app-state/select-move-hand-card "world")
                        (app-state/select-move-piece :rose-rod-minion)
                        (app-state/select-move-world-copy 3)
                        (app-state/select-move-power :rod)
                        (app-state/select-move-rod-mode :push-piece))]
      (is (some #(= :indigo-rod-target (:id %))
                (app-state/move-target-piece-options source-db)))
      (is (:ok? (get-in (-> source-db
                            (app-state/select-move-target-piece :indigo-rod-target)
                            (app-state/set-move-distance 1)
                            app-state/confirm-move)
                      [:move-selection :last-result]))))))

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

(defn- source-option [db source-id]
  (some #(when (= source-id (:id %)) %)
        (app-state/move-source-options db)))

(deftest move-source-options-reflect-current-game-state
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}
                                  :demo-board-pieces fixtures/demo-board-pieces})]
    (is (:enabled? (source-option db :activate-territory)))
    (is (:enabled? (source-option db :play-hand-card)))
    (is (:enabled? (source-option db :orient-piece)))
    (is (:enabled? (source-option db :draw-cards)))
    (is (not (:enabled? (source-option db :place-initial-small))))))

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

(deftest sun-piece-cup-target-stays-distinct-from-disc-target-in-move-panel-view
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["sun"])}
             :demo-board-pieces [rose-hand-cup-enemy-piece
                                 rose-rod-target
                                 indigo-rod-target]})
        cup-target-db (-> db
                          (app-state/select-move-source :play-hand-card)
                          (app-state/select-move-hand-card "sun")
                          (app-state/select-move-piece :rose-striker)
                          (app-state/select-move-power :sun)
                          (app-state/select-move-target-piece :indigo-rod-target))
        skip-db (app-state/select-move-sun-disc-mode cup-target-db :skip)
        disc-piece-db (-> cup-target-db
                          (app-state/select-move-sun-disc-mode :piece)
                          (app-state/select-move-target-piece :rose-striker))
        skip-controls (:controls (app-state/move-panel-view skip-db))
        disc-piece-controls (:controls (app-state/move-panel-view disc-piece-db))]
    (is (= :indigo-rod-target
           (get-in skip-controls [:sun-cup-target-piece :piece-id])))
    (is (nil? (:sun-disc-target-piece skip-controls)))
    (is (= :indigo-rod-target
           (get-in disc-piece-controls [:sun-cup-target-piece :piece-id])))
    (is (= :rose-striker
           (get-in disc-piece-controls [:sun-disc-target-piece :piece-id])))
    (is (= {:target-piece-id :indigo-rod-target
            :sun-disc-mode :piece
            :sun-disc-target-piece-id :rose-striker}
           (select-keys (app-state/move-params disc-piece-db)
                        [:target-piece-id
                         :sun-disc-mode
                         :sun-disc-target-piece-id])))))

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

(deftest move-panel-registry-covers-emitted-control-groups
  (let [emitted-types (move-registry/control-renderer-types)
        registry-types (set (keys move-registry/control-renderer-definitions))
        power-renderer-types (set (mapcat #(or (:renderer-control-keys
                                                (move-registry/power-definition %))
                                               (move-registry/power-control-groups %))
                                           move-registry/move-power-order))
        missing-registry-types (sort (remove registry-types emitted-types))
        missing-power-renderers (sort (remove emitted-types power-renderer-types))
        nil-renderer-keys (sort (keep (fn [type]
                                        (when (nil? (move-registry/control-renderer-key type))
                                          type))
                                      emitted-types))]
    (is (seq emitted-types))
    (is (= [] missing-registry-types))
    (is (= [] missing-power-renderers))
    (is (= [] nil-renderer-keys))))

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

(deftest no-piece-player-with-hand-room-must-place-initial-small
  (let [initial-db (app-state/initialize {:player-specs test-player-specs
                                          :game-options {:shuffle-fn identity}})
        original-hand (app-state/current-player-hand initial-db)
        discarded-card (last original-hand)
        db (-> initial-db
               (replace-game-player-hand :rose (vec (butlast original-hand)))
               (update-in [:game :discard-pile] conj discarded-card))
        draw-option (source-option db :draw-cards)
        initial-option (source-option db :place-initial-small)
        header-view (app-state/header-view db)
        ended-db (app-state/end-turn db)
        source-db (app-state/select-move-source db :draw-cards)]
    (is (not (:enabled? draw-option)))
    (is (= "A player with no pieces must place their initial small piece instead of drawing cards."
           (:reason draw-option)))
    (is (:enabled? initial-option))
    (is (false? (:can-end-turn? header-view)))
    (is (false? (:can-announce-challenge? header-view)))
    (is (= :initial-placement-required
           (get-in ended-db [:turn-result :error :code])))
    (is (= :rose
           (get-in ended-db [:game :turn :current-player-id])))
    (is (= "Place first piece" (:label initial-option)))
    (is (re-find #"Special rule" (:summary initial-option)))
    (is (= :move-source-unavailable
           (get-in source-db [:move-selection :error :code])))
    (is (= :draw-cards
           (get-in source-db [:move-selection :error :data :source])))
    (is (game-schema/valid-game? (app-state/game db)))))

(deftest drawing-cards-confirms-through-game-state
  (let [initial-db (app-state/initialize {:player-specs test-player-specs
                                          :game-options {:shuffle-fn identity}
                                          :demo-board-pieces [rose-source-piece]})
        original-hand (app-state/current-player-hand initial-db)
        discarded-card (last original-hand)
        shortened-hand (vec (butlast original-hand))
        draw-card (first (get-in initial-db [:game :draw-pile]))
        db (-> initial-db
               (replace-game-player-hand :rose shortened-hand)
               (update-in [:game :discard-pile] conj discarded-card))
        empty-draw-db (assoc-in db [:game :draw-pile] [])
        source-db (app-state/select-move-source db :draw-cards)
        confirmed-db (app-state/confirm-move source-db)
        blocked-confirm-db (app-state/confirm-move
                            (assoc confirmed-db
                                   :move-selection
                                   (app-state/move-selection source-db)))
        ended-db (app-state/end-turn confirmed-db)
        zones (app-state/card-zones confirmed-db)]
    (is (:enabled? (source-option db :draw-cards)))
    (is (= 1 (app-state/max-draw-count empty-draw-db)))
    (is (= :confirm (:stage (app-state/move-selection source-db))))
    (is (= {:discard-card-ids []
            :draw-count 1}
           (app-state/move-params source-db)))
    (is (= {:source :draw-cards
            :player-id :rose
            :discard-card-ids []
            :draw-count 1}
           (app-state/move-command source-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (app-state/turn-action-consumed? confirmed-db))
    (is (every? (comp not :enabled?)
                (app-state/move-source-options confirmed-db)))
    (is (= #{"The current player has already taken a turn action."}
           (set (map :reason (app-state/move-source-options confirmed-db)))))
    (is (= :rejected (:stage (app-state/move-selection blocked-confirm-db))))
    (is (= :move-source-unavailable
           (get-in blocked-confirm-db [:move-selection :error :code])))
    (is (= "The current player has already taken a turn action."
           (get-in blocked-confirm-db [:move-selection :error :message])))
    (is (false? (app-state/turn-action-consumed? ended-db)))
    (is (= (mapv :id (conj shortened-hand draw-card))
           (mapv :id (:hand zones))))
    (is (= [(:id discarded-card)]
           (mapv :id (:discard-pile zones))))
    (is (= (dec (count (get-in db [:game :draw-pile])))
           (:draw-count zones)))))

(deftest no-piece-draw-confirmation-is-rejected
  (let [initial-db (app-state/initialize {:player-specs test-player-specs
                                          :game-options {:shuffle-fn identity}
                                          :demo-board-pieces [rose-source-piece]})
        original-hand (app-state/current-player-hand initial-db)
        discarded-card (last original-hand)
        db (-> initial-db
               (replace-game-player-hand :rose (vec (butlast original-hand)))
               (update-in [:game :discard-pile] conj discarded-card))
        staged-db (app-state/select-move-source db :draw-cards)
        no-piece-db (update staged-db :game game-state/with-board-pieces [])
        confirmed-db (app-state/confirm-move no-piece-db)]
    (is (= :confirm (:stage (app-state/move-selection staged-db))))
    (is (= :rejected (:stage (app-state/move-selection confirmed-db))))
    (is (= :move-source-unavailable
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= (app-state/game no-piece-db)
           (app-state/game confirmed-db)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest full-hand-draw-can-select-discards-before-drawing
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:shuffle-fn identity}
                                  :demo-board-pieces [rose-source-piece]})
        original-hand (app-state/current-player-hand db)
        discarded-card (last original-hand)
        draw-card (first (get-in db [:game :draw-pile]))
        source-db (app-state/select-move-source db :draw-cards)
        discard-db (app-state/toggle-move-discard-card source-db (:id discarded-card))
        controls (:controls (app-state/move-panel-view source-db))
        confirmed-db (app-state/confirm-move discard-db)
        zones (app-state/card-zones confirmed-db)]
    (is (:enabled? (source-option db :draw-cards)))
    (is (= :draw-count (:stage (app-state/move-selection source-db))))
    (is (= {:discard-card-ids []}
           (app-state/move-params source-db)))
    (is (= (mapv :id original-hand)
           (mapv :id (:discard-card-options controls))))
    (is (empty? (:draw-options controls)))
    (is (= :confirm (:stage (app-state/move-selection discard-db))))
    (is (= [0 1] (app-state/draw-count-options discard-db)))
    (is (= {:discard-card-ids [(:id discarded-card)]
            :draw-count 1}
           (app-state/move-params discard-db)))
    (is (= {:source :draw-cards
            :player-id :rose
            :discard-card-ids [(:id discarded-card)]
            :draw-count 1}
           (app-state/move-command discard-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= (mapv :id (conj (vec (butlast original-hand)) draw-card))
           (mapv :id (:hand zones))))
    (is (= [(:id discarded-card)]
           (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest confirm-move-event-handler-is-deterministic-with-injected-draw-shuffle-seed
  (let [initial-db (app-state/initialize {:player-specs test-player-specs
                                          :game-options {:shuffle-fn identity}
                                          :demo-board-pieces [rose-source-piece]})
        original-hand (app-state/current-player-hand initial-db)
        shortened-hand (vec (drop 2 original-hand))
        first-draw-card (first (get-in initial-db [:game :draw-pile]))
        prepared-discard (vec (concat (take 2 original-hand)
                                      (rest (get-in initial-db [:game :draw-pile]))))
        db (-> initial-db
               (replace-game-player-hand :rose shortened-hand)
               (assoc-in [:game :draw-pile] [first-draw-card])
               (assoc-in [:game :discard-pile] prepared-discard))
        ready-db (-> db
                     (app-state/select-move-source :draw-cards)
                     (app-state/set-move-draw-count 2))
        first-confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260524})
        second-confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260524})
        shuffled-discard (deterministic-shuffle/shuffle-with-seed 20260524 prepared-discard)
        expected-drawn [first-draw-card (first shuffled-discard)]
        zones (app-state/card-zones first-confirmed-db)]
    (is (= (app-state/game first-confirmed-db)
           (app-state/game second-confirmed-db)))
    (is (= (mapv :id (concat shortened-hand expected-drawn))
           (mapv :id (:hand zones))))
    (is (empty? (:discard-pile zones)))
    (is (= (mapv :id (rest shuffled-discard))
           (mapv :id (:draw-pile zones))))
    (is (true? (get-in first-confirmed-db
                       [:move-selection :last-result :events 0 :reshuffled-discard?])))
    (is (game-schema/valid-game? (app-state/game first-confirmed-db)))))

(deftest fool-hand-card-can_skip_two_reveals_with_injected_shuffle
  (let [seed 20260528
        initial-db (app-state/initialize {:player-specs test-player-specs
                                          :game-options {:deck-order (deck-starting-with ["fool"])}
                                          :demo-board-pieces [rose-hand-piece]})
        prepared-discard (vec (get-in initial-db [:game :draw-pile]))
        db (-> initial-db
               (assoc-in [:game :draw-pile] [])
               (assoc-in [:game :discard-pile] prepared-discard))
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "fool")
                     (app-state/select-move-piece :rose-striker))
        count-db (app-state/set-move-fool-reveal-count piece-db 2)
        first-reveal-db (app-handlers/reveal-move-fool-card-db count-db {:shuffle-seed seed})
        first-skip-db (app-state/skip-move-fool-reveal first-reveal-db)
        second-reveal-db (app-handlers/reveal-move-fool-card-db first-skip-db
                                                                 {:shuffle-seed (inc seed)})
        reveal-db (app-state/skip-move-fool-reveal second-reveal-db)
        confirmed-db (app-handlers/confirm-move-db reveal-db {:shuffle-seed (+ seed 2)})
        repeated-confirmed-db (app-handlers/confirm-move-db reveal-db {:shuffle-seed (+ seed 3)})
        expected-shuffled (deterministic-shuffle/shuffle-with-seed
                           seed
                           (conj prepared-discard (cards/card-by-id "fool")))
        events (get-in confirmed-db [:move-selection :last-result :events])
        command (app-state/move-command reveal-db)
        zones (app-state/card-zones confirmed-db)]
    (is (= :fool-reveal-count (:stage (app-state/move-selection piece-db))))
    (is (= [0 1 2] (app-state/move-fool-reveal-count-options piece-db)))
    (is (= :fool-reveal-card (:stage (app-state/move-selection count-db))))
    (is (= (-> expected-shuffled first :id)
           (get-in first-reveal-db [:move-selection :params :fool-active-reveal :card-id])))
    (is (= :fool-reveal-card (:stage (app-state/move-selection first-skip-db))))
    (is (= (-> expected-shuffled second :id)
           (get-in second-reveal-db [:move-selection :params :fool-active-reveal :card-id])))
    (is (= :confirm (:stage (app-state/move-selection reveal-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "fool"
                     :piece-id :rose-striker}
            :reveals [{} {}]}
           (dissoc command :shuffle-fn)))
    (is (= (app-state/game confirmed-db)
           (app-state/game repeated-confirmed-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= [:fool/card-revealed :fool/card-revealed]
           (mapv :type events)))
    (is (= (mapv :id (take 2 expected-shuffled))
           (mapv :card-id events)))
    (is (not (some #{"fool"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest fool-hand-card-can_play_first_reveal_and_skip_second
  (let [draw-start (+ (* game-state/starting-hand-size (count test-player-specs))
                      board/board-card-count)
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at {0 "fool"
                                                              draw-start "cups2"
                                                              (inc draw-start) "wands2"})}
             :demo-board-pieces [rose-hand-cup-territory-piece]})
        ready-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "fool")
                     (app-state/select-move-piece :rose-striker)
                     (app-state/set-move-fool-reveal-count 2)
                     app-state/reveal-move-fool-card
                     app-state/play-move-fool-reveal
                     (app-state/select-board-card 3)
                     (app-state/set-move-orientation :north)
                     app-state/reveal-move-fool-card
                     app-state/skip-move-fool-reveal)
        command (app-state/move-command ready-db)
        confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260528})
        events (get-in confirmed-db [:move-selection :last-result :events])
        created-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "fool"
                     :piece-id :rose-striker}
            :reveals [{:power :cup
                       :piece-id :rose-striker
                       :play-command {:target {:kind :territory
                                               :board-index 3}
                                      :orientation :north
                                      :cup-variant :cup}}
                      {}]}
           command))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= [:fool/card-revealed
            :cup/small-piece-created
            :fool/card-revealed]
           (mapv :type events)))
    (is (= 3 (:space-index created-piece)))
    (is (= :north (:orientation created-piece)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest fool-hand-card-can_skip_first_reveal_and_play_second
  (let [draw-start (+ (* game-state/starting-hand-size (count test-player-specs))
                      board/board-card-count)
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at {0 "fool"
                                                              draw-start "wands2"
                                                              (inc draw-start) "cups2"})}
             :demo-board-pieces [rose-hand-cup-territory-piece]})
        ready-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "fool")
                     (app-state/select-move-piece :rose-striker)
                     (app-state/set-move-fool-reveal-count 2)
                     app-state/reveal-move-fool-card
                     app-state/skip-move-fool-reveal
                     app-state/reveal-move-fool-card
                     app-state/play-move-fool-reveal
                     (app-state/select-board-card 3)
                     (app-state/set-move-orientation :east))
        command (app-state/move-command ready-db)
        confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260528})
        events (get-in confirmed-db [:move-selection :last-result :events])]
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= [{} {:power :cup
                :piece-id :rose-striker
                :play-command {:target {:kind :territory
                                        :board-index 3}
                               :orientation :east
                               :cup-variant :cup}}]
           (:reveals command)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= [false true]
           (mapv :played? (filter #(= :fool/card-revealed (:type %)) events))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest fool-hand-card-can_play_two_reveals
  (let [draw-start (+ (* game-state/starting-hand-size (count test-player-specs))
                      board/board-card-count)
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at {0 "fool"
                                                              draw-start "cups2"
                                                              (inc draw-start) "wands2"})}
             :demo-board-pieces [rose-hand-cup-territory-piece]})
        ready-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "fool")
                     (app-state/select-move-piece :rose-striker)
                     (app-state/set-move-fool-reveal-count 2)
                     app-state/reveal-move-fool-card
                     app-state/play-move-fool-reveal
                     (app-state/select-board-card 3)
                     (app-state/set-move-orientation :north)
                     app-state/reveal-move-fool-card
                     app-state/play-move-fool-reveal
                     (app-state/select-move-rod-mode :move-minion)
                     (app-state/set-move-distance 1)
                     (app-state/set-move-orientation :east))
        confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260528})
        events (get-in confirmed-db [:move-selection :last-result :events])
        moved-piece (piece-by-id confirmed-db :rose-striker)]
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= [:fool/card-revealed
            :cup/small-piece-created
            :fool/card-revealed
            :rod/minion-moved]
           (mapv :type events)))
    (is (= 3 (:space-index moved-piece)))
    (is (= :east (:orientation moved-piece)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest fool-hand-card-can_play_revealed_major
  (let [draw-start (+ (* game-state/starting-hand-size (count test-player-specs))
                      board/board-card-count)
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at {0 "fool"
                                                              draw-start "hangedman"})}
             :demo-board-pieces [rose-rod-minion indigo-rod-target]})
        rose-hand-before (mapv :id (get-in db [:game :players-by-id :rose :hand]))
        indigo-hand-before (mapv :id (get-in db [:game :players-by-id :indigo :hand]))
        ready-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "fool")
                     (app-state/select-move-piece :rose-rod-minion)
                     (app-state/set-move-fool-reveal-count 1)
                     app-state/reveal-move-fool-card
                     app-state/play-move-fool-reveal
                     (app-state/select-move-fool-play-power :hanged-man)
                     (app-state/set-move-major-action-count 1)
                     (app-state/select-move-target-piece :indigo-rod-target))
        command (app-state/move-command ready-db)
        confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260528})
        events (get-in confirmed-db [:move-selection :last-result :events])]
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= :hangedman (get-in command [:reveals 0 :power])))
    (is (= [:fool/card-revealed :hanged-man/hands-traded]
           (mapv :type events)))
    (is (= indigo-hand-before
           (mapv :id (get-in confirmed-db [:game :players-by-id :rose :hand]))))
    (is (= (vec (remove #{"fool"} rose-hand-before))
           (mapv :id (get-in confirmed-db [:game :players-by-id :indigo :hand]))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest world-copy-of-fool_uses_reveal_play_flow
  (let [draw-start (+ (* game-state/starting-hand-size (count test-player-specs))
                      board/board-card-count)
        world-index 0
        fool-index 1
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at
                                          {(board-card-position test-player-specs world-index) "world"
                                           (board-card-position test-player-specs fool-index) "fool"
                                           draw-start "cups2"})}
             :demo-board-pieces [rose-source-piece]})
        ready-db (-> db
                     (app-state/select-move-source :activate-territory)
                     (app-state/select-board-card world-index)
                     (app-state/select-move-piece :rose-scout)
                     (app-state/select-move-world-copy fool-index)
                     (app-state/set-move-fool-reveal-count 1)
                     app-state/reveal-move-fool-card
                     app-state/play-move-fool-reveal
                     (app-state/select-board-card fool-index)
                     (app-state/set-move-orientation :east))
        command (app-state/move-command ready-db)
        confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260528})
        created-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= fool-index (:copied-board-index command)))
    (is (= :cup (get-in command [:reveals 0 :power])))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= fool-index (:space-index created-piece)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest high-priestess-hand-card-can_stage_two_redraw_passes
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order
                                                 (deck-starting-with
                                                  ["high-priestess" "cups2" "wands2"
                                                   "coins2" "swords2" "cups3"])}
                                  :demo-board-pieces [rose-hand-piece]})
        first-drawn-card (first (get-in db [:game :draw-pile]))
        second-drawn-card (second (get-in db [:game :draw-pile]))
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "high-priestess")
                     (app-state/select-move-piece :rose-striker))
        count-db (app-state/set-move-high-priestess-redraw-count piece-db 2)
        first-pass-db (-> count-db
                          (app-state/toggle-move-high-priestess-discard-card 1 "cups2")
                          (app-state/set-move-high-priestess-draw-count 1 1))
        second-pass-options (second (app-state/move-high-priestess-redraw-options
                                     first-pass-db))
        second-pass-db (-> first-pass-db
                           (app-state/toggle-move-high-priestess-discard-card 2 "wands2")
                           (app-state/set-move-high-priestess-draw-count 2 1))
        confirmed-db (app-state/confirm-move second-pass-db)
        zones (app-state/card-zones confirmed-db)]
    (is (= :high-priestess-redraw-count
           (:stage (app-state/move-selection piece-db))))
    (is (= [0 1 2] (app-state/move-high-priestess-redraw-count-options piece-db)))
    (is (= :high-priestess-redraw
           (:stage (app-state/move-selection count-db))))
    (is (= [1 2]
           (mapv :pass-index (app-state/move-high-priestess-redraw-options count-db))))
    (is (= ["wands2" "coins2" "swords2" "cups3" (:id first-drawn-card)]
           (mapv :id (:discard-card-options second-pass-options))))
    (is (= :confirm (:stage (app-state/move-selection second-pass-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "high-priestess"
                     :piece-id :rose-striker}
            :redraws [{:discard-card-ids ["cups2"]
                       :draw-count 1}
                      {:discard-card-ids ["wands2"]
                       :draw-count 1}]}
           (app-state/move-command second-pass-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= ["coins2" "swords2" "cups3" (:id first-drawn-card) (:id second-drawn-card)]
           (mapv :id (:hand zones))))
    (is (= ["high-priestess" "cups2" "wands2"]
           (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest high-priestess-redraw-staging-prevents_duplicate_discards_across_passes
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order
                                                 (deck-starting-with
                                                  ["high-priestess" "cups2" "wands2"
                                                   "coins2" "swords2" "cups3"])}
                                  :demo-board-pieces [rose-hand-piece]})
        count-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "high-priestess")
                     (app-state/select-move-piece :rose-striker)
                     (app-state/set-move-high-priestess-redraw-count 2))
        first-pass-db (-> count-db
                          (app-state/toggle-move-high-priestess-discard-card 1 "cups2")
                          (app-state/set-move-high-priestess-draw-count 1 0))
        second-pass-options (second (app-state/move-high-priestess-redraw-options
                                     first-pass-db))
        duplicate-db (app-state/toggle-move-high-priestess-discard-card
                      first-pass-db
                      2
                      "cups2")]
    (is (not (some #{"cups2"} (map :id (:discard-card-options second-pass-options)))))
    (is (= [] (:selected-discard-card-ids second-pass-options)))
    (is (= :invalid-high-priestess-discard-card
           (get-in duplicate-db [:move-selection :error :code])))
    (is (= [] (:selected-discard-card-ids
               (second (app-state/move-high-priestess-redraw-options duplicate-db)))))))

(deftest high-priestess-second_redraw_can_discard_card_drawn_in_first_pass
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order
                                                 (deck-starting-with
                                                  ["high-priestess" "cups2" "wands2"
                                                   "coins2" "swords2" "cups3"])}
                                  :demo-board-pieces [rose-hand-piece]})
        first-drawn-card (first (get-in db [:game :draw-pile]))
        drawn-card-id (:id first-drawn-card)
        first-pass-db (-> db
                          (app-state/select-move-source :play-hand-card)
                          (app-state/select-move-hand-card "high-priestess")
                          (app-state/select-move-piece :rose-striker)
                          (app-state/set-move-high-priestess-redraw-count 2)
                          (app-state/toggle-move-high-priestess-discard-card 1 "cups2")
                          (app-state/set-move-high-priestess-draw-count 1 1))
        second-pass-options (second (app-state/move-high-priestess-redraw-options
                                     first-pass-db))
        second-pass-db (-> first-pass-db
                           (app-state/toggle-move-high-priestess-discard-card
                            2
                            drawn-card-id)
                           (app-state/set-move-high-priestess-draw-count 2 0))
        confirmed-db (app-state/confirm-move second-pass-db)
        zones (app-state/card-zones confirmed-db)]
    (is (some #{drawn-card-id} (map :id (:discard-card-options second-pass-options))))
    (is (= :confirm (:stage (app-state/move-selection second-pass-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "high-priestess"
                     :piece-id :rose-striker}
            :redraws [{:discard-card-ids ["cups2"]
                       :draw-count 1}
                      {:discard-card-ids [drawn-card-id]
                       :draw-count 0}]}
           (app-state/move-command second-pass-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= ["wands2" "coins2" "swords2" "cups3"]
           (mapv :id (:hand zones))))
    (is (= ["high-priestess" "cups2" drawn-card-id]
           (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest judgement-hand-card-can_stage_source_card_draw_after_cost
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["judgement"])}
                                  :demo-board-pieces [rose-hand-piece]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "judgement")
                     (app-state/select-move-piece :rose-striker))
        card-db (app-state/toggle-move-judgement-card piece-db "judgement")
        confirmed-db (app-state/confirm-move card-db)
        zones (app-state/card-zones confirmed-db)]
    (is (= :confirm (:stage (app-state/move-selection piece-db))))
    (is (= ["judgement"]
           (mapv :id (app-state/move-judgement-card-options piece-db))))
    (is (= 1 (app-state/move-judgement-card-maximum piece-db)))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "judgement"
                     :piece-id :rose-striker}
            :piece-id :rose-striker
            :card-ids ["judgement"]}
           (app-state/move-command card-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= "judgement" (:id (last (:hand zones)))))
    (is (empty? (:discard-pile zones)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest activating-a-board-territory-uses-board-and-piece-selections
  (let [deck-order (deck-with-card-at (board-card-position test-player-specs 0)
                                      "cups2")
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-source-piece]})
        source-db (app-state/select-move-source db :activate-territory)
        piece-db (app-state/select-move-piece source-db :rose-scout)
        target-db (app-state/select-board-card piece-db 1)
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
            :target-board-index 1}
           (app-state/move-params target-db)))
    (is (= :confirm (:stage (app-state/move-selection oriented-db))))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 1
            :size :small
            :orientation :east}
           created-piece))
    (is (= 3 (get-in confirmed-db [:game :pieces :stashes :rose :small])))
    (is (= 3 (get-in confirmed-db [:game :players-by-id :rose :stash :small])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))
    (is (= :source (:stage (app-state/move-selection confirmed-db))))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))))

(deftest playing-a-hand-card-stages-card_piece_and_target
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2"])}
                                  :demo-board-pieces [rose-hand-cup-territory-piece]})
        card-id "cups2"
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

(deftest selecting-new-territory-created-after-a-board-gap-uses-board-index
  (let [deck-order (deck-with-cards-at {0 "coins2"
                                        (board-card-position test-player-specs 3) "cups2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion]})
        gapped-db (remove-board-cell db 4)
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
        selected-db (app-state/select-board-card created-db 9)
        source-db (-> created-db
                      (app-state/select-board-card 3)
                      (app-state/select-move-source :activate-territory)
                      (app-state/select-move-piece :rose-rod-minion))
        target-db (app-state/select-board-card source-db 9)]
    (is (:ok? create-result))
    (is (= 9 (get-in (app-state/selected-board-cell selected-db) [:index])))
    (is (= :orientation (:stage (app-state/move-selection target-db))))
    (is (= {:source-board-index 3
            :piece-id :rose-rod-minion
            :target-board-index 9}
           (app-state/move-params target-db)))))

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

(deftest rod-territory-source-can-move-the-acting-minion
  (let [deck-order (deck-with-card-at (board-card-position test-player-specs 0)
                                      "wands2")
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-source-piece]})
        source-db (app-state/select-move-source db :activate-territory)
        piece-db (app-state/select-move-piece source-db :rose-scout)
        mode-db (app-state/select-move-rod-mode piece-db :move-minion)
        distance-db (app-state/set-move-distance mode-db 1)
        oriented-db (app-state/set-move-orientation distance-db :south)
        confirmed-db (app-state/confirm-move oriented-db)
        moved-piece (piece-by-id confirmed-db :rose-scout)]
    (is (= :rod (app-state/move-power piece-db)))
    (is (= :rod-mode (:stage (app-state/move-selection piece-db))))
    (is (= :distance (:stage (app-state/move-selection mode-db))))
    (is (= :orientation (:stage (app-state/move-selection distance-db))))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 0
                     :piece-id :rose-scout}
            :rod-variant :rod
            :mode :move-minion
            :distance 1
            :orientation :south}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-scout
            :player-id :rose
            :space-index 1
            :size :small
            :orientation :south}
           moved-piece))))

(deftest rod-unbounded-territory-source-can-move-into-full-territory
  (let [deck-order (deck-with-card-at (board-card-position test-player-specs 3)
                                      "emperor")
        full-pieces [rose-rod-minion
                     {:id :rose-target-medium
                      :player-id :rose
                      :space-index 4
                      :size :medium
                      :orientation :up}
                     indigo-rod-target
                     {:id :indigo-target-large
                      :player-id :indigo
                      :space-index 4
                      :size :large
                      :orientation :south}]
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces full-pieces})
        oriented-db (-> db
                        (app-state/select-move-source :activate-territory)
                        (app-state/select-move-piece :rose-rod-minion)
                        (app-state/select-move-power :rod)
                        (app-state/select-move-rod-mode :move-minion)
                        (app-state/set-move-distance 1)
                        (app-state/set-move-orientation :south))
        confirmed-db (app-state/confirm-move oriented-db)
        moved-piece (piece-by-id confirmed-db :rose-rod-minion)
        destination-pieces (filter #(= 4 (:space-index %))
                                   (app-state/board-pieces confirmed-db))]
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 3
                     :piece-id :rose-rod-minion}
            :rod-variant :rod-unbounded
            :mode :move-minion
            :distance 1
            :orientation :south}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-rod-minion
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :south}
           moved-piece))
    (is (= 4 (count destination-pieces)))))

(deftest rod-hand-card-can-push-a-piece-and_discard_source
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["wands2"])}
                                  :demo-board-pieces [rose-rod-minion
                                                      indigo-rod-target]})
        target-db (-> db
                      (app-state/select-move-source :play-hand-card)
                      (app-state/select-move-hand-card "wands2")
                      (app-state/select-move-piece :rose-rod-minion)
                      (app-state/select-move-rod-mode :push-piece)
                      (app-state/select-move-target-piece :indigo-rod-target))
        distance-db (app-state/set-move-distance target-db 1)
        confirmed-db (app-state/confirm-move distance-db)
        zones (app-state/card-zones confirmed-db)
        pushed-piece (piece-by-id confirmed-db :indigo-rod-target)]
    (is (= :distance (:stage (app-state/move-selection target-db))))
    (is (= :confirm (:stage (app-state/move-selection distance-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "wands2"
                     :piece-id :rose-rod-minion}
            :rod-variant :rod
            :mode :push-piece
            :distance 1
            :target {:kind :piece
                     :piece-id :indigo-rod-target}}
           (app-state/move-command distance-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :indigo-rod-target
            :player-id :indigo
            :space-index 5
            :size :small
            :orientation :north}
           pushed-piece))
    (is (= ["wands2"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"wands2"} (map :id (:hand zones)))))))

(deftest rod-hand-card-can-push-a-territory_from_board_click_target
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["wands2"])}
                                  :demo-board-pieces [(assoc rose-rod-minion :space-index 4)
                                                      rose-rod-target]})
        target-card (get-in db [:game :board 5 :card])
        target-db (-> db
                      (app-state/select-move-source :play-hand-card)
                      (app-state/select-move-hand-card "wands2")
                      (app-state/select-move-piece :rose-rod-minion)
                      (app-state/select-move-rod-mode :push-territory)
                      (app-state/select-board-card 5))
        distance-db (app-state/set-move-distance target-db 1)
        confirmed-db (app-state/confirm-move distance-db)
        moved-cell (board-cell-by-index confirmed-db 5)
        old-target-piece (piece-by-id confirmed-db :rose-rod-target)]
    (is (= 5 (app-state/selected-board-index target-db)))
    (is (= :distance (:stage (app-state/move-selection target-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "wands2"
                     :piece-id :rose-rod-minion}
            :rod-variant :rod
            :mode :push-territory
            :target {:kind :territory
                     :board-index 5}
            :distance 1}
           (app-state/move-command distance-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:index 5
            :row 1
            :col 3
            :orientation :portrait
            :face :up
            :card target-card}
           moved-cell))
    (is (= {:id :rose-rod-target
            :player-id :rose
            :space {:kind :wasteland
                    :row 1
                    :col 2}
            :size :small
            :orientation :north}
           old-target-piece))))

(deftest rejected-rod-confirmation-keeps-staged-selection
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["wands2"])}
                                  :demo-board-pieces [(assoc rose-rod-minion
                                                             :orientation :up)]})
        oriented-db (-> db
                        (app-state/select-move-source :play-hand-card)
                        (app-state/select-move-hand-card "wands2")
                        (app-state/select-move-piece :rose-rod-minion)
                        (app-state/select-move-rod-mode :move-minion)
                        (app-state/set-move-distance 1)
                        (app-state/set-move-orientation :east))
        confirmed-db (app-state/confirm-move oriented-db)]
    (is (= :rejected (:stage (app-state/move-selection confirmed-db))))
    (is (= :rod-minion-upright
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= (app-state/move-params oriented-db)
           (app-state/move-params confirmed-db)))
    (is (= (app-state/game oriented-db)
           (app-state/game confirmed-db)))))

(deftest playing-a-cup-hand-card-confirms-through-game-state
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2"])}
                                  :demo-board-pieces [rose-hand-cup-territory-piece]})
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

(deftest playing-a-cup-hand-card-can-create-an-enemy-piece
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2"])}
                                  :demo-board-pieces [rose-hand-cup-enemy-piece
                                                      indigo-rod-target]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "cups2")
                     (app-state/select-move-piece :rose-striker))
        target-db (app-state/select-move-target-piece piece-db :indigo-rod-target)
        confirmed-db (app-state/confirm-move target-db)
        zones (app-state/card-zones confirmed-db)
        created-piece (piece-by-id confirmed-db :indigo-small-1)]
    (is (= [:indigo-rod-target]
           (mapv :id (app-state/move-target-piece-options piece-db))))
    (is (= :confirm (:stage (app-state/move-selection target-db))))
    (is (= {:hand-card-id "cups2"
            :piece-id :rose-striker
            :target-piece-id :indigo-rod-target}
           (app-state/move-params target-db)))
	    (is (= {:player-id :rose
	            :source {:kind :hand-card
	                     :card-id "cups2"
	                     :piece-id :rose-striker}
	            :cup-variant :cup
	            :target {:kind :piece
	                     :piece-id :indigo-rod-target}}
	           (app-state/move-command target-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= ["cups2"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"cups2"} (map :id (:hand zones)))))
    (is (= {:id :indigo-small-1
            :player-id :indigo
            :space-index 4
            :size :small
            :orientation :north}
           created-piece))
    (is (= 3 (get-in confirmed-db [:game :pieces :stashes :indigo :small])))
    (is (= 3 (get-in confirmed-db [:game :players-by-id :indigo :stash :small])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest cup-browser-targets-are-limited-to-the-minion-target-space
  (let [far-enemy {:id :indigo-far-cup-target
                   :player-id :indigo
                   :space-index 8
                   :size :small
                   :orientation :north}
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2"])}
                                  :demo-board-pieces [rose-hand-cup-enemy-piece
                                                      indigo-rod-target
                                                      far-enemy]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "cups2")
                     (app-state/select-move-piece :rose-striker))
        invalid-board-db (app-state/select-board-card piece-db 8)
        invalid-piece-db (app-state/select-move-target-piece
                          piece-db
                          :indigo-far-cup-target)]
    (is (= [4]
           (mapv :index (app-state/move-target-board-options piece-db))))
    (is (= [:indigo-rod-target]
           (mapv :id (app-state/move-target-piece-options piece-db))))
    (is (= :invalid-cup-target
           (get-in invalid-board-db [:move-selection :error :code])))
    (is (= :invalid-target-piece
           (get-in invalid-piece-db [:move-selection :error :code])))
    (is (= :target (:stage (app-state/move-selection invalid-board-db))))
    (is (= :target (:stage (app-state/move-selection invalid-piece-db))))))

(deftest hierophant-hand-card-can_replace_a_target_piece_from_move_panel
  (let [rose-major-minion {:id :rose-major-minion
                           :player-id :rose
                           :space-index 4
                           :size :medium
                           :orientation :up}
        indigo-target {:id :indigo-major-target
                       :player-id :indigo
                       :space-index 4
                       :size :medium
                       :orientation :north}
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order
                                                 (deck-starting-with ["hierophant"])}
                                  :demo-board-pieces [rose-major-minion
                                                      indigo-target]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "hierophant")
                     (app-state/select-move-piece :rose-major-minion))
        target-db (app-state/select-move-target-piece piece-db :indigo-major-target)
        oriented-db (app-state/set-move-orientation target-db :west)
        confirmed-db (app-state/confirm-move oriented-db)
        replacement (piece-by-id confirmed-db :rose-medium-1)
        zones (app-state/card-zones confirmed-db)]
    (is (= :hierophant (app-state/move-power piece-db)))
    (is (= :target-piece (:stage (app-state/move-selection piece-db))))
    (is (= [:indigo-major-target :rose-major-minion]
           (sort (mapv :id (app-state/move-target-piece-options piece-db)))))
    (is (= :orientation (:stage (app-state/move-selection target-db))))
    (is (= :confirm (:stage (app-state/move-selection oriented-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "hierophant"
                     :piece-id :rose-major-minion}
            :target {:kind :piece
                     :piece-id :indigo-major-target}
            :orientation :west}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-medium-1
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :west}
           replacement))
    (is (nil? (piece-by-id confirmed-db :indigo-major-target)))
    (is (= ["hierophant"] (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest devil-territory-source-can_stage_retargeted_orientation_actions
  (let [devil-minion {:id :rose-devil-minion
                      :player-id :rose
                      :space-index 4
                      :size :medium
                      :orientation :up}
        enemy-target {:id :indigo-devil-target
                      :player-id :indigo
                      :space-index 5
                      :size :small
                      :orientation :north}
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order
                            (deck-with-card-at
                             (board-card-position test-player-specs 4)
                             "devil")}
             :demo-board-pieces [devil-minion enemy-target]})
        source-db (-> db
                      (app-state/select-board-card 4)
                      (app-state/select-move-source :activate-territory)
                      (app-state/select-move-piece :rose-devil-minion))
        action-count-db (app-state/set-move-devil-action-count source-db 2)
        first-target-db (app-state/select-move-target-piece action-count-db
                                                            :rose-devil-minion)
        first-action-db (app-state/set-move-orientation first-target-db :east)
        second-target-db (app-state/select-move-target-piece first-action-db
                                                             :indigo-devil-target)
        ready-db (app-state/set-move-orientation second-target-db :south)
        confirmed-db (app-state/confirm-move ready-db)]
    (is (= :devil (app-state/move-power source-db)))
    (is (= :devil-action-count (:stage (app-state/move-selection source-db))))
    (is (= [1 2 3]
           (get-in (app-state/move-panel-view source-db)
                   [:controls :devil-action-count-options])))
    (is (= [:rose-devil-minion]
           (mapv :id (app-state/move-target-piece-options action-count-db))))
    (is (= :orientation (:stage (app-state/move-selection first-target-db))))
    (is (= :target-piece (:stage (app-state/move-selection first-action-db))))
    (is (= [{:power :orient-target
             :piece-id :rose-devil-minion
             :target {:kind :piece
                      :piece-id :rose-devil-minion}
             :orientation :east}]
           (get-in first-action-db [:move-selection :params :major-actions])))
    (is (some #{:indigo-devil-target}
              (mapv :id (app-state/move-target-piece-options first-action-db))))
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 4
                     :piece-id :rose-devil-minion}
            :actions [{:power :orient-target
                       :piece-id :rose-devil-minion
                       :target {:kind :piece
                                :piece-id :rose-devil-minion}
                       :orientation :east}
                      {:power :orient-target
                       :piece-id :rose-devil-minion
                       :target {:kind :piece
                                :piece-id :indigo-devil-target}
                       :orientation :south}]}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= :east (:orientation (piece-by-id confirmed-db :rose-devil-minion))))
    (is (= :south (:orientation (piece-by-id confirmed-db :indigo-devil-target))))
    (is (= [:devil/piece-oriented :devil/piece-oriented]
           (mapv :type (get-in confirmed-db [:move-selection :last-result :events]))))
    (is (empty? (get-in confirmed-db [:game :discard-pile])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest playing-a-cup-hand-card-can-create-a_wasteland_territory
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2" "coins2"])}
                                  :demo-board-pieces [rose-hand-piece]})
        source-db (app-state/select-move-source db :play-hand-card)
        card-db (app-state/select-move-hand-card source-db "cups2")
        piece-db (app-state/select-move-piece card-db :rose-striker)
        invalid-wasteland-db (app-state/select-move-wasteland-target piece-db 0 3)
        wasteland-db (app-state/select-move-wasteland-target piece-db 3 2)
        one-point-db (app-state/select-move-one-point-card wasteland-db "coins2")
        confirmed-db (app-state/confirm-move one-point-db)
        zones (app-state/card-zones confirmed-db)
        created-cell (last (app-state/board confirmed-db))]
    (is (= [{:kind :wasteland :row 3 :col 2 :id "wasteland-3-2"}]
           (mapv #(select-keys % [:kind :row :col :id])
                 (app-state/move-target-wasteland-options piece-db))))
    (is (= :invalid-wasteland-target
           (get-in invalid-wasteland-db [:move-selection :error :code])))
    (is (= :one-point-card (:stage (app-state/move-selection wasteland-db))))
    (is (= {:hand-card-id "cups2"
            :piece-id :rose-striker
            :target-wasteland {:kind :wasteland
                               :row 3
                               :col 2}}
           (app-state/move-params wasteland-db)))
    (is (some #{"coins2"}
              (mapv :id (app-state/move-one-point-card-options wasteland-db))))
    (is (not (some #{"cups2"}
                   (mapv :id (app-state/move-one-point-card-options wasteland-db)))))
	    (is (= {:player-id :rose
	            :source {:kind :hand-card
	                     :card-id "cups2"
	                     :piece-id :rose-striker}
	            :cup-variant :cup
	            :target {:kind :wasteland
	                     :row 3
	                     :col 2}
	            :territory-card-source :hand
	            :one-point-card-id "coins2"}
	           (app-state/move-command one-point-db)))
    (is (= :confirm (:stage (app-state/move-selection one-point-db))))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= ["cups2"] (mapv :id (:discard-pile zones))))
    (is (= 4 (count (:hand zones))))
    (is (not (some #{"cups2" "coins2"} (map :id (:hand zones)))))
    (is (= {:index 9
            :row 3
            :col 2
            :orientation :landscape
            :face :up
            :card (cards/card-by-id "coins2")}
           created-cell))))

(deftest cup-unbounded-board-source-can-place-into-full-territory
  (let [deck-order (deck-with-card-at (board-card-position test-player-specs 0)
                                      "empress")
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order deck-order}
             :demo-board-pieces [rose-source-piece
                                 (assoc indigo-rod-target :space-index 1)
                                 {:id :rose-target-small
                                  :player-id :rose
                                  :space-index 1
                                  :size :small
                                  :orientation :north}
                                 {:id :indigo-target-large
                                  :player-id :indigo
                                  :space-index 1
                                  :size :large
                                  :orientation :west}]})
        oriented-db (-> db
                        (app-state/select-move-source :activate-territory)
                        (app-state/select-move-piece :rose-scout)
                        (app-state/select-move-power :cup)
                        (app-state/select-board-card 1)
                        (app-state/set-move-orientation :up))
        confirmed-db (app-state/confirm-move oriented-db)
        target-piece-ids (->> (app-state/board-pieces confirmed-db)
                              (filter #(= 1 (:space-index %)))
                              (mapv :id))]
    (is (= :cup (app-state/move-power oriented-db)))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 0
                     :piece-id :rose-scout}
            :cup-variant :cup-unbounded
            :target {:kind :territory
                     :board-index 1}
            :orientation :up}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= [:indigo-rod-target
            :rose-target-small
            :indigo-target-large
            :rose-small-1]
           target-piece-ids))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest empress-territory-source-confirms-ordered-major-actions
  (let [deck-order (deck-with-card-at (board-card-position test-player-specs 0)
                                      "empress")
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order deck-order}
             :demo-board-pieces [(assoc rose-source-piece :orientation :north)
                                 (assoc indigo-rod-target :space-index 1)
                                 {:id :rose-target-small
                                  :player-id :rose
                                  :space-index 1
                                  :size :small
                                  :orientation :north}
                                 {:id :indigo-target-large
                                  :player-id :indigo
                                  :space-index 1
                                  :size :large
                                  :orientation :west}]})
        orient-db (-> db
                      (app-state/select-move-source :activate-territory)
                      (app-state/select-move-piece :rose-scout)
                      (app-state/select-move-power :empress)
                      (app-state/set-move-minion-orientation :east))
        target-db (app-state/select-board-card orient-db 1)
        ready-db (app-state/set-move-orientation target-db :up)
        confirmed-db (app-state/confirm-move ready-db)
        source-piece (piece-by-id confirmed-db :rose-scout)
        target-piece-ids (->> (app-state/board-pieces confirmed-db)
                              (filter #(= 1 (:space-index %)))
                              (mapv :id))]
    (is (= :target (:stage (app-state/move-selection orient-db))))
    (is (= [{:power :orient-minion
             :piece-id :rose-scout
             :orientation :east}]
           (get-in orient-db [:move-selection :params :major-actions])))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 0
                     :piece-id :rose-scout}
            :actions [{:power :orient-minion
                       :piece-id :rose-scout
                       :orientation :east}
                      {:power :cup
                       :piece-id :rose-scout
                       :target {:kind :territory
                                :board-index 1}
                       :orientation :up}]}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= :east (:orientation source-piece)))
    (is (= [:indigo-rod-target
            :rose-target-small
            :indigo-target-large
            :rose-small-1]
           target-piece-ids))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest lovers-hand-card-confirms-promoted-ordered-major-actions
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["lovers"])}
             :demo-board-pieces [(assoc rose-rod-minion :orientation :east)]})
        rod-db (-> db
                   (app-state/select-move-source :play-hand-card)
                   (app-state/select-move-hand-card "lovers")
                   (app-state/select-move-piece :rose-rod-minion)
                   (app-state/select-move-power :lovers)
                   (app-state/select-move-rod-mode :move-minion)
                   (app-state/set-move-distance 1)
                   (app-state/set-move-orientation :east))
        target-db (app-state/select-board-card rod-db 5)
        ready-db (app-state/set-move-orientation target-db :up)
        confirmed-db (app-state/confirm-move ready-db)
        zones (app-state/card-zones confirmed-db)
        moved-piece (piece-by-id confirmed-db :rose-rod-minion)
        created-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (= :target (:stage (app-state/move-selection rod-db))))
    (is (= [{:power :rod
             :mode :move-minion
             :distance 1
             :orientation :east
             :piece-id :rose-rod-minion}]
           (get-in rod-db [:move-selection :params :major-actions])))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "lovers"
                     :piece-id :rose-rod-minion}
            :actions [{:power :rod
                       :mode :move-minion
                       :distance 1
                       :orientation :east
                       :piece-id :rose-rod-minion}
                      {:power :cup
                       :piece-id :rose-rod-minion
                       :target {:kind :territory
                                :board-index 5}
                       :orientation :up}]}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-rod-minion
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :east}
           moved-piece))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 5
            :size :small
            :orientation :up}
           created-piece))
    (is (= ["lovers"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"lovers"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest temperance-hand-card-filters-each-cup-action-to-the-target-space
  (let [db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-starting-with ["temperance"])}
             :demo-board-pieces [rose-hand-cup-enemy-piece]})
        first-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "temperance")
                     (app-state/select-move-piece :rose-striker)
                     (app-state/select-move-power :temperance))
        invalid-first-db (app-state/select-board-card first-db 8)
        second-db (-> first-db
                      (app-state/select-board-card 4)
                      (app-state/set-move-orientation :north))
        invalid-second-db (app-state/select-board-card second-db 8)]
    (is (= [4]
           (mapv :index (app-state/move-target-board-options first-db))))
    (is (= :invalid-cup-target
           (get-in invalid-first-db [:move-selection :error :code])))
    (is (= [{:power :cup
             :piece-id :rose-striker
             :target {:kind :territory
                      :board-index 4}
             :orientation :north}]
           (get-in second-db [:move-selection :params :major-actions])))
    (is (= [4]
           (mapv :index (app-state/move-target-board-options second-db))))
    (is (= :invalid-cup-target
           (get-in invalid-second-db [:move-selection :error :code])))
    (is (= :target (:stage (app-state/move-selection invalid-second-db))))))

(deftest wheel-cup-hand-card-can-use_top_draw_pile_for_wasteland_territory
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["wheeloffortune"])}
                                  :demo-board-pieces [rose-hand-piece]})
        draw-card (first (get-in db [:game :draw-pile]))
        wasteland-db (-> db
                         (app-state/select-move-source :play-hand-card)
                         (app-state/select-move-hand-card "wheeloffortune")
                         (app-state/select-move-piece :rose-striker)
                         (app-state/select-move-wasteland-target 3 2))
        draw-source-db (app-state/select-move-territory-card-source
                        wasteland-db
                        :draw-pile-top)
        confirmed-db (app-state/confirm-move draw-source-db)
        zones (app-state/card-zones confirmed-db)
        created-cell (last (app-state/board confirmed-db))]
    (is (= :territory-card-source
           (:stage (app-state/move-selection wasteland-db))))
    (is (= [:hand :draw-pile-top]
           (mapv :id (app-state/move-territory-card-source-options wasteland-db))))
    (is (= :confirm (:stage (app-state/move-selection draw-source-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "wheeloffortune"
                     :piece-id :rose-striker}
            :cup-variant :wheel-cup
            :target {:kind :wasteland
                     :row 3
                     :col 2}
            :territory-card-source :draw-pile-top}
           (app-state/move-command draw-source-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= draw-card (:card created-cell)))
    (is (= ["wheeloffortune"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"wheeloffortune"} (map :id (:hand zones)))))
    (is (= (dec (count (get-in db [:game :draw-pile])))
           (:draw-count zones)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest wheel-cup-hand-card-confirmation-uses-injected-shuffle-for-empty-draw-pile
  (let [seed 20260529
        draw-start (+ (board-card-position test-player-specs 0) board/board-card-count)
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order
                                                 (deck-with-cards-at
                                                  {0 "wheeloffortune"
                                                   draw-start "world"})}
                                  :demo-board-pieces [rose-hand-piece]})
        wheel-card (cards/card-by-id "wheeloffortune")
        prepared-discard (get-in db [:game :draw-pile])
        empty-draw-db (-> db
                          (assoc-in [:game :draw-pile] [])
                          (assoc-in [:game :discard-pile] prepared-discard))
        draw-source-db (-> empty-draw-db
                           (app-state/select-move-source :play-hand-card)
                           (app-state/select-move-hand-card "wheeloffortune")
                           (app-state/select-move-piece :rose-striker)
                           (app-state/select-move-wasteland-target 3 2)
                           (app-state/select-move-territory-card-source :draw-pile-top))
        first-confirmed-db (app-handlers/confirm-move-db draw-source-db {:shuffle-seed seed})
        second-confirmed-db (app-handlers/confirm-move-db draw-source-db {:shuffle-seed seed})
        shuffled-discard (deterministic-shuffle/shuffle-with-seed
                          seed
                          (conj prepared-discard wheel-card))
        expected-card (first shuffled-discard)
        zones (app-state/card-zones first-confirmed-db)
        created-cell (last (app-state/board first-confirmed-db))]
    (is (= (app-state/game first-confirmed-db)
           (app-state/game second-confirmed-db)))
    (is (= :confirm (:stage (app-state/move-selection draw-source-db))))
    (is (= "wheeloffortune"
           (get-in (app-state/move-command draw-source-db)
                   [:source :card-id])))
    (is (:ok? (get-in first-confirmed-db [:move-selection :last-result])))
    (is (= (:id expected-card) (get-in created-cell [:card :id])))
    (is (= (mapv :id (rest shuffled-discard))
           (mapv :id (:draw-pile zones))))
    (is (empty? (:discard-pile zones)))
    (is (not (some #{"wheeloffortune"} (map :id (:hand zones)))))
    (is (true? (get-in first-confirmed-db
                       [:move-selection :last-result :events 0 :reshuffled-discard?])))
    (is (game-schema/valid-game? (app-state/game first-confirmed-db)))))

(deftest disc-hand-card-can-grow-a_target_piece
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["coins2"])}
                                  :demo-board-pieces [rose-rod-minion
                                                      indigo-rod-target]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "coins2")
                     (app-state/select-move-piece :rose-rod-minion))
        kind-db (app-state/select-move-disc-target-kind piece-db :piece)
        target-db (app-state/select-move-target-piece kind-db :indigo-rod-target)
        confirmed-db (app-state/confirm-move target-db)
        zones (app-state/card-zones confirmed-db)
        grown-piece (piece-by-id confirmed-db :indigo-medium-1)]
    (is (= :disc (app-state/move-power piece-db)))
    (is (= :disc-target-kind (:stage (app-state/move-selection piece-db))))
    (is (= [:rose-rod-minion :indigo-rod-target]
           (mapv :id (app-state/move-target-piece-options kind-db))))
    (is (= :confirm (:stage (app-state/move-selection target-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "coins2"
                     :piece-id :rose-rod-minion}
            :disc-variant :disc
            :target {:kind :piece
                     :piece-id :indigo-rod-target}}
           (app-state/move-command target-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :indigo-medium-1
            :player-id :indigo
            :space-index 4
            :size :medium
            :orientation :north}
           grown-piece))
    (is (= ["coins2"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"coins2"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest disc-hand-card-can-grow-a_territory_with_hand_replacement
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
        target-db (app-state/select-board-card kind-db 4)
        replacement-db (app-state/select-move-replacement-card target-db "cupsking")
        confirmed-db (app-state/confirm-move replacement-db)
        zones (app-state/card-zones confirmed-db)
        grown-cell (board-cell-by-index confirmed-db 4)]
    (is (= [4] (mapv :index (app-state/move-target-board-options kind-db))))
    (is (= :replacement-card (:stage (app-state/move-selection target-db))))
    (is (= ["cupsking"]
           (mapv :id (app-state/move-replacement-card-options target-db))))
    (is (= :confirm (:stage (app-state/move-selection replacement-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "coins2"
                     :piece-id :rose-rod-minion}
            :disc-variant :disc
            :target {:kind :territory
                     :board-index 4}
            :replacement-card-source :hand
            :replacement-card-id "cupsking"}
           (app-state/move-command replacement-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= "cupsking" (get-in grown-cell [:card :id])))
    (is (= ["coins2" "cups2"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"coins2" "cupsking"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest disc-targets-surviving-territory-after-destruction-gap-by-board-index
  (let [targeting-piece {:id :rose-gap-disc-minion
                         :player-id :rose
                         :space-index 2
                         :size :medium
                         :orientation :south}
        deck-order (deck-with-cards-at {0 "coins2"
                                        1 "cupsking"
                                        (board-card-position test-player-specs 5) "cups2"
                                        (board-card-position test-player-specs 6) "swords2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [targeting-piece]})
        gapped-db (remove-board-cell db 4)
        kind-db (-> gapped-db
                    (app-state/select-move-source :play-hand-card)
                    (app-state/select-move-hand-card "coins2")
                    (app-state/select-move-piece :rose-gap-disc-minion)
                    (app-state/select-move-disc-target-kind :territory))
        target-db (app-state/select-board-card kind-db 5)]
    (is (= [5] (mapv :index (app-state/move-target-board-options kind-db))))
    (is (= :replacement-card (:stage (app-state/move-selection target-db))))
    (is (= {:hand-card-id "coins2"
            :piece-id :rose-gap-disc-minion
            :disc-target-kind :territory
            :target-board-index 5}
           (app-state/move-params target-db)))
    (is (= ["cupsking"]
           (mapv :id (app-state/move-replacement-card-options target-db))))))

(deftest star-disc-can_orient_minion_before_discard_pile_replacement
  (let [deck-order (deck-with-cards-at {0 "star"
                                        (board-card-position test-player-specs 4) "cupsking"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [(assoc rose-rod-minion
                                                             :orientation :north)]})
        piece-db (-> db
                      (app-state/select-move-source :play-hand-card)
                      (app-state/select-move-hand-card "star")
                      (app-state/select-move-piece :rose-rod-minion))
        oriented-db (app-state/set-move-minion-orientation piece-db :east)
        kind-db (app-state/select-move-disc-target-kind oriented-db :territory)
        target-db (-> kind-db
                      (app-state/select-board-card 4))
        source-db (app-state/select-move-territory-card-source target-db :discard-pile)
        replacement-db (app-state/select-move-replacement-card source-db "star")
        confirmed-db (app-state/confirm-move replacement-db)
        zones (app-state/card-zones confirmed-db)
        grown-cell (board-cell-by-index confirmed-db 4)
        oriented-piece (piece-by-id confirmed-db :rose-rod-minion)]
    (is (= :minion-orientation
           (:stage (app-state/move-selection piece-db))))
    (is (true? (get-in (app-state/move-panel-view piece-db)
                       [:controls :disc-minion-orientation-required?])))
    (is (= :disc-target-kind
           (:stage (app-state/move-selection oriented-db))))
    (is (= [4] (mapv :index (app-state/move-target-board-options kind-db))))
    (is (= [:hand :discard-pile]
           (mapv :id (app-state/move-territory-card-source-options target-db))))
    (is (= :replacement-card-source
           (:stage (app-state/move-selection target-db))))
    (is (= ["star"]
           (mapv :id (app-state/move-replacement-card-options source-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "star"
                     :piece-id :rose-rod-minion}
            :disc-variant :disc-from-discard
            :minion-orientation :east
            :target {:kind :territory
                     :board-index 4}
            :replacement-card-source :discard-pile
            :replacement-card-id "star"}
           (app-state/move-command replacement-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= :east (:orientation oriented-piece)))
    (is (= "star" (get-in grown-cell [:card :id])))
    (is (= ["cupsking"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"star"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest strength-disc-can_stage_two_piece_growth_actions
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["strength"])}
                                  :demo-board-pieces [(assoc rose-rod-minion
                                                             :size :small)]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "strength")
                     (app-state/select-move-piece :rose-rod-minion))
        action-db (app-state/set-move-disc-action-count piece-db 2)
        kind-db (app-state/select-move-disc-target-kind action-db :piece)
        target-db (app-state/select-move-target-piece kind-db :rose-rod-minion)
        confirmed-db (app-state/confirm-move target-db)
        zones (app-state/card-zones confirmed-db)
        grown-piece (piece-by-id confirmed-db :rose-large-1)]
    (is (= :disc-action-count (:stage (app-state/move-selection piece-db))))
    (is (= [1 2]
           (get-in (app-state/move-panel-view piece-db)
                   [:controls :disc-action-count-options])))
    (is (= :disc-target-kind (:stage (app-state/move-selection action-db))))
    (is (= :confirm (:stage (app-state/move-selection target-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "strength"
                     :piece-id :rose-rod-minion}
            :disc-variant :disc
            :disc-actions [{:target {:kind :piece
                                     :piece-id :rose-rod-minion}}
                           {:target {:kind :piece
                                     :piece-id :rose-rod-minion}}]}
           (app-state/move-command target-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-large-1
            :player-id :rose
            :space-index 3
            :size :large
            :orientation :east}
           grown-piece))
    (is (= ["strength"] (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest strength-disc-can_stage_two_territory_growth_actions
  (let [deck-order (deck-with-cards-at {0 "strength"
                                        1 "star"
                                        2 "cupsking"
                                        (board-card-position test-player-specs 4) "cups2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion]})
        target-db (-> db
                      (app-state/select-move-source :play-hand-card)
                      (app-state/select-move-hand-card "strength")
                      (app-state/select-move-piece :rose-rod-minion)
                      (app-state/set-move-disc-action-count 2)
                      (app-state/select-move-disc-target-kind :territory)
                      (app-state/select-board-card 4))
        replacement-db (app-state/select-move-replacement-card target-db "star")
        confirmed-db (app-state/confirm-move replacement-db)
        zones (app-state/card-zones confirmed-db)
        grown-cell (board-cell-by-index confirmed-db 4)
        replacement-option-ids (mapv :id (app-state/move-replacement-card-options target-db))]
    (is (some #{"star"} replacement-option-ids))
    (is (not (some #{"cupsking"} replacement-option-ids)))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "strength"
                     :piece-id :rose-rod-minion}
            :disc-variant :disc
            :disc-actions [{:target {:kind :territory
                                     :board-index 4}
                            :replacement-card-source :hand
                            :replacement-card-id "star"}
                           {:target {:kind :territory
                                     :board-index 4}
                            :replacement-card-source :hand
                            :replacement-card-id "star"}]}
           (app-state/move-command replacement-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= "star" (get-in grown-cell [:card :id])))
    (is (= ["strength" "cups2"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"strength" "star"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest rejected-disc-confirmation-keeps-staged-selection
  (let [deck-order (deck-with-cards-at {0 "coins2"
                                        1 "cupsking"
                                        (board-card-position test-player-specs 4) "cups2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion]})
        replacement-db (-> db
                           (app-state/select-move-source :play-hand-card)
                           (app-state/select-move-hand-card "coins2")
                           (app-state/select-move-piece :rose-rod-minion)
                           (app-state/select-move-disc-target-kind :territory)
                           (app-state/select-board-card 4)
                           (app-state/select-move-replacement-card "cupsking"))
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

(deftest sun-hand-card-can_stage_created_piece_shortcut
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["sun"])}
                                  :demo-board-pieces [rose-hand-cup-territory-piece]})
        power-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "sun")
                     (app-state/select-move-piece :rose-striker))
        staged-db (app-state/select-move-power power-db :sun)
        target-db (app-state/select-board-card staged-db 3)
        oriented-db (app-state/set-move-orientation target-db :south)
        disc-db (app-state/select-move-sun-disc-mode oriented-db :created-piece)
        confirmed-db (app-state/confirm-move disc-db)
        zones (app-state/card-zones confirmed-db)
        created-piece (piece-by-id confirmed-db :rose-medium-1)]
    (is (= :power (:stage (app-state/move-selection power-db))))
    (is (= [:cup :disc :sun]
           (mapv :id (app-state/move-power-options power-db))))
    (is (= :sun (app-state/move-power staged-db)))
    (is (= :orientation (:stage (app-state/move-selection target-db))))
    (is (= :sun-disc-mode (:stage (app-state/move-selection oriented-db))))
    (is (= [:skip :created-piece :piece :territory]
           (mapv :id (app-state/move-sun-disc-mode-options oriented-db))))
    (is (= :confirm (:stage (app-state/move-selection disc-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "sun"
                     :piece-id :rose-striker}
            :cup {:target {:kind :territory
                           :board-index 3}
                  :orientation :south}
            :disc {:target {:kind :created-piece}
                   :orientation :south}}
           (app-state/move-command disc-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-medium-1
            :player-id :rose
            :space-index 3
            :size :medium
            :orientation :south}
           created-piece))
    (is (= ["sun"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"sun"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest sun-hand-card-can_stage_created_territory_shortcut
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with
                                                              ["sun" "cupsking"])}
                                  :demo-board-pieces [rose-hand-piece]})
        wasteland-db (-> db
                         (app-state/select-move-source :play-hand-card)
                         (app-state/select-move-hand-card "sun")
                         (app-state/select-move-piece :rose-striker)
                         (app-state/select-move-power :sun)
                         (app-state/select-move-wasteland-target 3 2))
        mode-db (app-state/select-move-sun-disc-mode wasteland-db :created-territory)
        replacement-db (app-state/select-move-replacement-card mode-db "cupsking")
        confirmed-db (app-state/confirm-move replacement-db)
        zones (app-state/card-zones confirmed-db)
        created-cell (last (app-state/board confirmed-db))]
    (is (= :sun-disc-mode (:stage (app-state/move-selection wasteland-db))))
    (is (= [:skip :created-territory :piece :territory]
           (mapv :id (app-state/move-sun-disc-mode-options wasteland-db))))
    (is (= ["cupsking"]
           (mapv :id (app-state/move-replacement-card-options mode-db))))
    (is (= :confirm (:stage (app-state/move-selection replacement-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "sun"
                     :piece-id :rose-striker}
            :cup {:target {:kind :wasteland
                           :row 3
                           :col 2}}
            :disc {:target {:kind :created-territory}
                   :replacement-card-source :hand
                   :replacement-card-id "cupsking"}}
           (app-state/move-command replacement-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:row 3
            :col 2}
           (select-keys created-cell [:row :col])))
    (is (= "cupsking" (get-in created-cell [:card :id])))
    (is (= ["sun"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"sun" "cupsking"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest sun-hand-card-can_stage_existing_piece_disc_reorientation
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["sun"])}
                                  :demo-board-pieces [rose-hand-cup-enemy-piece
                                                      rose-rod-target
                                                      indigo-rod-target]})
        cup-db (-> db
                   (app-state/select-move-source :play-hand-card)
                   (app-state/select-move-hand-card "sun")
                   (app-state/select-move-piece :rose-striker)
                   (app-state/select-move-power :sun)
                   (app-state/select-move-target-piece :indigo-rod-target))
        mode-db (app-state/select-move-sun-disc-mode cup-db :piece)
        target-db (app-state/select-move-target-piece mode-db :rose-striker)
        oriented-db (app-state/set-move-sun-disc-orientation target-db :west)
        confirmed-db (app-state/confirm-move oriented-db)
        grown-piece (piece-by-id confirmed-db :rose-large-1)]
    (is (= :sun-disc-mode (:stage (app-state/move-selection cup-db))))
    (is (= [:skip :piece :territory]
           (mapv :id (app-state/move-sun-disc-mode-options cup-db))))
    (is (true? (app-state/move-sun-disc-orientation-available? target-db)))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "sun"
                     :piece-id :rose-striker}
            :cup {:target {:kind :piece
                           :piece-id :indigo-rod-target}}
            :disc {:target {:kind :piece
                            :piece-id :rose-striker}
                   :orientation :west}}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-large-1
            :player-id :rose
            :space-index 3
            :size :large
            :orientation :west}
           grown-piece))
    (is (= ["sun"] (mapv :id (get-in confirmed-db [:game :discard-pile]))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

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

(deftest hanged-man-hand-card-can-stage-hand-trade-only
  (let [hanged-target {:id :indigo-hanged-target
                       :player-id :indigo
                       :space-index 4
                       :size :small
                       :orientation :north}
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["hangedman"])}
                                  :demo-board-pieces [rose-rod-minion
                                                      hanged-target]})
        rose-hand-before (mapv :id (get-in db [:game :players-by-id :rose :hand]))
        indigo-hand-before (mapv :id (get-in db [:game :players-by-id :indigo :hand]))
        power-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "hangedman")
                     (app-state/select-move-piece :rose-rod-minion)
                     (app-state/select-move-power :hanged-man))
        trade-only-db (app-state/set-move-major-action-count power-db 1)
        ready-db (app-state/select-move-target-piece trade-only-db
                                                     :indigo-hanged-target)
        confirmed-db (app-state/confirm-move ready-db)
        zones (app-state/card-zones confirmed-db)]
    (is (= [{:type :hand-card}
            {:type :piece}
            {:type :power}
            {:type :major-action-count
             :power :hanged-man}
            {:type :target-piece
             :power :hanged-man
             :action-power :trade-hand}]
           (move-control-group-summary trade-only-db)))
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "hangedman"
                     :piece-id :rose-rod-minion}
            :actions [{:power :trade-hand
                       :piece-id :rose-rod-minion
                       :target {:kind :piece
                                :piece-id :indigo-hanged-target}}]}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= [:hanged-man/hands-traded]
           (mapv :type (get-in confirmed-db [:move-selection :last-result :events]))))
    (is (= 3 (:space-index (piece-by-id confirmed-db :rose-rod-minion))))
    (is (= indigo-hand-before
           (mapv :id (get-in confirmed-db [:game :players-by-id :rose :hand]))))
    (is (= (vec (remove #{"hangedman"} rose-hand-before))
           (mapv :id (get-in confirmed-db [:game :players-by-id :indigo :hand]))))
    (is (= ["hangedman"] (mapv :id (:discard-pile zones))))
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

(deftest world-hand-card-can_stage_copied_composite_major
  (let [deck-order (deck-with-cards-at {0 "world"
                                        (board-card-position test-player-specs 3) "empress"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [(assoc rose-source-piece
                                                             :orientation :north)]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "world")
                     (app-state/select-move-piece :rose-scout))
        copy-db (app-state/select-move-world-copy piece-db 3)
        power-db (app-state/select-move-power copy-db :empress)
        orient-db (app-state/set-move-minion-orientation power-db :east)
        target-db (app-state/select-board-card orient-db 1)
        ready-db (app-state/set-move-orientation target-db :up)
        confirmed-db (app-state/confirm-move ready-db)
        zones (app-state/card-zones confirmed-db)
        source-piece (piece-by-id confirmed-db :rose-scout)
        created-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (= :world (app-state/move-power piece-db)))
    (is (= :world-copy (:stage (app-state/move-selection piece-db))))
    (is (some #(= 3 (:index %)) (app-state/move-world-copy-options piece-db)))
    (is (= :copied-power (:stage (app-state/move-selection copy-db))))
    (is (= [:empress :cup]
           (mapv :id (app-state/move-world-copied-power-options copy-db))))
    (is (= :minion-orientation (:stage (app-state/move-selection power-db))))
    (is (= :empress (app-state/move-world-copied-power power-db)))
    (is (= [{:power :orient-minion
             :piece-id :rose-scout
             :orientation :east}]
           (get-in orient-db [:move-selection :params :major-actions])))
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "world"
                     :piece-id :rose-scout}
            :copied-board-index 3
            :actions [{:power :orient-minion
                       :piece-id :rose-scout
                       :orientation :east}
                      {:power :cup
                       :piece-id :rose-scout
                       :target {:kind :territory
                                :board-index 1}
                       :orientation :up}]}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= :east (:orientation source-piece)))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 1
            :size :small
            :orientation :up}
           created-piece))
    (is (= ["world"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"world"} (map :id (:hand zones)))))
    (is (= "empress" (get-in (board-cell-by-index confirmed-db 3) [:card :id])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest world-hand-card-can_stage_copied_death_major
  (let [target-piece {:id :indigo-world-death-target
                      :player-id :indigo
                      :space-index 4
                      :size :medium
                      :orientation :north}
        deck-order (deck-with-cards-at {0 "world"
                                        (board-card-position test-player-specs 5) "death"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-rod-minion
                                                      target-piece]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "world")
                     (app-state/select-move-piece :rose-rod-minion))
        copy-db (app-state/select-move-world-copy piece-db 5)
        power-db (app-state/select-move-power copy-db :death)
        action-count-db (app-state/set-move-sword-action-count power-db 2)
        first-db (-> action-count-db
                     (app-state/select-move-sword-target-kind :piece)
                     (app-state/select-move-target-piece :indigo-world-death-target)
                     (app-state/set-move-damage 1))
        ready-db (-> first-db
                     (app-state/select-move-sword-target-kind :piece)
                     (app-state/select-move-target-piece :indigo-world-death-target)
                     (app-state/set-move-damage 1))
        confirmed-db (app-state/confirm-move ready-db)
        zones (app-state/card-zones confirmed-db)]
    (is (= :copied-power (:stage (app-state/move-selection copy-db))))
    (is (= [:death :sword]
           (mapv :id (app-state/move-world-copied-power-options copy-db))))
    (is (= :sword-action-count (:stage (app-state/move-selection power-db))))
    (is (= :sword-target-kind (:stage (app-state/move-selection action-count-db))))
    (is (= :sword-target-kind (:stage (app-state/move-selection first-db))))
    (is (= [{:power :sword
             :target {:kind :piece
                      :piece-id :indigo-world-death-target}
             :damage 1
             :piece-id :rose-rod-minion}]
           (get-in first-db [:move-selection :params :major-actions])))
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "world"
                     :piece-id :rose-rod-minion}
            :copied-board-index 5
            :sword-actions [{:target {:kind :piece
                                      :piece-id :indigo-world-death-target}
                             :damage 1
                             :piece-id :rose-rod-minion}
                            {:target {:kind :piece
                                      :piece-id :indigo-world-death-target}
                             :damage 1
                             :piece-id :rose-rod-minion}]}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (nil? (piece-by-id confirmed-db :indigo-world-death-target)))
    (is (= ["world"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"world"} (map :id (:hand zones)))))
    (is (= "death" (get-in (board-cell-by-index confirmed-db 5) [:card :id])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest world-territory-source-can_stage_copied_suit_power
  (let [deck-order (deck-with-cards-at {(board-card-position test-player-specs 0) "world"
                                        (board-card-position test-player-specs 3) "magician"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-source-piece]})
        piece-db (-> db
                     (app-state/select-move-source :activate-territory)
                     (app-state/select-move-piece :rose-scout))
        copy-db (app-state/select-move-world-copy piece-db 3)
        power-db (app-state/select-move-power copy-db :cup)
        target-db (app-state/select-board-card power-db 1)
        ready-db (app-state/set-move-orientation target-db :east)
        confirmed-db (app-state/confirm-move ready-db)
        created-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (= :world-copy (:stage (app-state/move-selection piece-db))))
    (is (= [:cup :rod :disc :sword]
           (mapv :id (app-state/move-world-copied-power-options copy-db))))
    (is (= :target (:stage (app-state/move-selection power-db))))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 0
                     :piece-id :rose-scout}
            :copied-board-index 3
            :copied-power :cup
            :target {:kind :territory
                     :board-index 1}
            :orientation :east
            :cup-variant :wild-suits}
           (app-state/move-command ready-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 1
            :size :small
            :orientation :east}
           created-piece))
    (is (empty? (get-in confirmed-db [:game :discard-pile])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest world-copy-selection-rejects_minor_and_world_targets
  (let [deck-order (deck-with-cards-at {(board-card-position test-player-specs 0) "world"
                                        (board-card-position test-player-specs 3) "cups2"})
        db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order deck-order}
                                  :demo-board-pieces [rose-source-piece]})
        piece-db (-> db
                     (app-state/select-move-source :activate-territory)
                     (app-state/select-move-piece :rose-scout))
        minor-db (app-state/select-move-world-copy piece-db 3)
        self-db (app-state/select-move-world-copy piece-db 0)]
    (is (= :world-copy (:stage (app-state/move-selection piece-db))))
    (is (= :invalid-world-copy
           (get-in minor-db [:move-selection :error :code])))
    (is (= :invalid-world-copy
           (get-in self-db [:move-selection :error :code])))
    (is (= (app-state/game piece-db)
           (app-state/game minor-db)))
    (is (= (app-state/game piece-db)
           (app-state/game self-db)))))

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

(deftest orienting-a_piece_confirms_through_game_state
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:shuffle-fn identity}
                                  :demo-board-pieces [rose-source-piece]})
        piece-db (-> db
                     (app-state/select-move-source :orient-piece)
                     (app-state/select-move-piece :rose-scout))
        oriented-db (app-state/set-move-orientation piece-db :south)
        confirmed-db (app-state/confirm-move oriented-db)
        oriented-piece (piece-by-id confirmed-db :rose-scout)]
    (is (= :orientation (:stage (app-state/move-selection piece-db))))
    (is (= {:source :orient-piece
            :player-id :rose
            :piece-id :rose-scout
            :orientation :south}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-scout
            :player-id :rose
            :space-index 0
            :size :small
            :orientation :south}
           oriented-piece))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest placing-an-initial-small-piece-can_target_empty_wasteland
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}
                                  :demo-board-pieces []})
        source-db (app-state/select-move-source db :place-initial-small)
        wasteland-db (app-state/select-move-wasteland-target source-db 0 3)
        oriented-db (app-state/set-move-orientation wasteland-db :north)
        confirmed-db (app-state/confirm-move oriented-db)
        created-piece (piece-by-id confirmed-db :rose-small-1)
        board-view (app-state/board-view confirmed-db)]
    (is (not (:enabled? (source-option db :activate-territory))))
    (is (:enabled? (source-option db :place-initial-small)))
    (is (= 9 (count (app-state/move-target-board-options source-db))))
    (is (= 12 (count (app-state/move-target-wasteland-options source-db))))
    (is (= :orientation (:stage (app-state/move-selection source-db))))
    (is (= {:target-board-index 0}
           (app-state/move-params source-db)))
    (is (= :confirm (:stage (app-state/move-selection oriented-db))))
    (is (= {:target-wasteland {:kind :wasteland
                               :row 0
                               :col 3}
            :orientation :north}
           (app-state/move-params oriented-db)))
    (is (= {:source :place-initial-small
            :player-id :rose
            :target {:kind :wasteland
                     :row 0
                     :col 3}
            :orientation :north}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space {:kind :wasteland
                    :row 0
                    :col 3}
            :size :small
            :orientation :north}
           created-piece))
    (is (= [created-piece]
           (get (:pieces-by-space board-view) (pieces/wasteland-space 0 3))))
    (is (= 4 (get-in confirmed-db [:game :players-by-id :rose :stash :small])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest rejected-initial-placement-confirmation-keeps-staged-selection
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}
                                  :demo-board-pieces []})
        oriented-db (-> db
                        (app-state/select-move-source :place-initial-small)
                        (app-state/set-move-orientation :north))
        stale-game (game-state/with-board-pieces
                    (app-state/game oriented-db)
                    [{:id :indigo-blocker
                      :player-id :indigo
                      :space-index 0
                      :size :small
                      :orientation :up}])
        stale-db (assoc oriented-db :game stale-game)
        confirmed-db (app-state/confirm-move stale-db)]
    (is (= :confirm (:stage (app-state/move-selection stale-db))))
    (is (= :rejected (:stage (app-state/move-selection confirmed-db))))
    (is (= :target-space-occupied
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= (app-state/move-params stale-db)
           (app-state/move-params confirmed-db)))
    (is (= stale-game
           (app-state/game confirmed-db)))))
