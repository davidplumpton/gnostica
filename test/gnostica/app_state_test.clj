(ns gnostica.app-state-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.app.handlers :as app-handlers]
            [gnostica.app-state :as app-state]
            [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.deterministic-shuffle :as deterministic-shuffle]
            [gnostica.fixtures :as fixtures]
            [gnostica.game-schema :as game-schema]
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

(defn- deck-with-cards-at [placements]
  (let [placed-ids (set (vals placements))]
    (loop [index 0
           remaining-cards (vec (remove #(contains? placed-ids (:id %)) cards/deck))
           result []]
      (cond
        (= (count result) (count cards/deck))
        (vec result)

        (contains? placements index)
        (recur (inc index)
               remaining-cards
               (conj result (cards/card-by-id (get placements index))))

        :else
        (recur (inc index)
               (subvec remaining-cards 1)
               (conj result (first remaining-cards)))))))

(defn- board-card-position [player-specs board-index]
  (+ (* game-state/starting-hand-size (count player-specs))
     board-index))

(defn- piece-by-id [db piece-id]
  (some #(when (= piece-id (:id %)) %)
        (app-state/board-pieces db)))

(defn- board-cell-by-index [db board-index]
  (some #(when (= board-index (:index %)) %)
        (app-state/board db)))

(defn- replace-game-player-hand [db player-id hand]
  (let [players (mapv (fn [player]
                        (if (= player-id (:id player))
                          (assoc player :hand (vec hand))
                          player))
                      (get-in db [:game :players]))]
    (assoc-in (assoc-in db [:game :players] players)
              [:game :players-by-id]
              (into {} (map (juxt :id identity) players)))))

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

(deftest board-view-model-aggregates-fallback-layout-data
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:shuffle-fn identity}
                                  :demo-board-pieces [rose-source-piece]})
        view (app-state/board-view (assoc db :three-renderer-error "No WebGL"))]
    (is (= 9 (count (:cells view))))
    (is (= [rose-source-piece] (:board-pieces view)))
    (is (= [rose-source-piece]
           (get-in view [:pieces-by-space 0])))
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

(deftest no-piece-player-with-hand-room-must-place-initial-small
  (let [initial-db (app-state/initialize {:player-specs test-player-specs
                                          :game-options {:shuffle-fn identity}})
        original-hand (app-state/current-player-hand initial-db)
        discarded-card (last original-hand)
        db (-> initial-db
               (replace-game-player-hand :rose (vec (butlast original-hand)))
               (update-in [:game :discard-pile] conj discarded-card))
        draw-option (source-option db :draw-cards)
        source-db (app-state/select-move-source db :draw-cards)]
    (is (not (:enabled? draw-option)))
    (is (= "The current player has no pieces on the board."
           (:reason draw-option)))
    (is (:enabled? (source-option db :place-initial-small)))
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
    (is (= 3 (get-in confirmed-db [:game :pieces :stashes :rose :small])))
    (is (= 3 (get-in confirmed-db [:game :players-by-id :rose :stash :small])))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))
    (is (= :source (:stage (app-state/move-selection confirmed-db))))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))))

(deftest playing-a-hand-card-stages-card_piece_and_target
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2"])}
                                  :demo-board-pieces [rose-hand-piece]})
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

(deftest playing-a-cup-hand-card-can-create-an-enemy-piece
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2"])}
                                  :demo-board-pieces [rose-hand-piece
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

(deftest playing-a-cup-hand-card-can-create-a_wasteland_territory
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["cups2" "coins2"])}
                                  :demo-board-pieces [rose-hand-piece]})
        source-db (app-state/select-move-source db :play-hand-card)
        card-db (app-state/select-move-hand-card source-db "cups2")
        piece-db (app-state/select-move-piece card-db :rose-striker)
        wasteland-db (app-state/select-move-wasteland-target piece-db 0 3)
        one-point-db (app-state/select-move-one-point-card wasteland-db "coins2")
        confirmed-db (app-state/confirm-move one-point-db)
        zones (app-state/card-zones confirmed-db)
        created-cell (last (app-state/board confirmed-db))]
    (is (= 12 (count (app-state/move-target-wasteland-options piece-db))))
    (is (= :one-point-card (:stage (app-state/move-selection wasteland-db))))
    (is (= {:hand-card-id "cups2"
            :piece-id :rose-striker
            :target-wasteland {:kind :wasteland
                               :row 0
                               :col 3}}
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
	                     :row 0
	                     :col 3}
	            :territory-card-source :hand
	            :one-point-card-id "coins2"}
	           (app-state/move-command one-point-db)))
    (is (= :confirm (:stage (app-state/move-selection one-point-db))))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= ["cups2"] (mapv :id (:discard-pile zones))))
    (is (= 4 (count (:hand zones))))
    (is (not (some #{"cups2" "coins2"} (map :id (:hand zones)))))
    (is (= {:index 9
            :row 0
            :col 3
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
                                 indigo-rod-target
                                 {:id :rose-target-small
                                  :player-id :rose
                                  :space-index 4
                                  :size :small
                                  :orientation :north}
                                 {:id :indigo-target-large
                                  :player-id :indigo
                                  :space-index 4
                                  :size :large
                                  :orientation :west}]})
        oriented-db (-> db
                        (app-state/select-move-source :activate-territory)
                        (app-state/select-move-piece :rose-scout)
                        (app-state/select-board-card 4)
                        (app-state/set-move-orientation :up))
        confirmed-db (app-state/confirm-move oriented-db)
        target-piece-ids (->> (app-state/board-pieces confirmed-db)
                              (filter #(= 4 (:space-index %)))
                              (mapv :id))]
    (is (= :cup (app-state/move-power oriented-db)))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 0
                     :piece-id :rose-scout}
            :cup-variant :cup-unbounded
            :target {:kind :territory
                     :board-index 4}
            :orientation :up}
           (app-state/move-command oriented-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= [:indigo-rod-target
            :rose-target-small
            :indigo-target-large
            :rose-small-1]
           target-piece-ids))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest wheel-cup-hand-card-can-use_top_draw_pile_for_wasteland_territory
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["wheeloffortune"])}
                                  :demo-board-pieces [rose-hand-piece]})
        draw-card (first (get-in db [:game :draw-pile]))
        wasteland-db (-> db
                         (app-state/select-move-source :play-hand-card)
                         (app-state/select-move-hand-card "wheeloffortune")
                         (app-state/select-move-piece :rose-striker)
                         (app-state/select-move-wasteland-target 0 3))
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
                     :row 0
                     :col 3}
            :territory-card-source :draw-pile-top}
           (app-state/move-command draw-source-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= draw-card (:card created-cell)))
    (is (= ["wheeloffortune"] (mapv :id (:discard-pile zones))))
    (is (not (some #{"wheeloffortune"} (map :id (:hand zones)))))
    (is (= (dec (count (get-in db [:game :draw-pile])))
           (:draw-count zones)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

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

(deftest sun-specific-hand-card-source-reports-unavailable-transition
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["sun"])}
                                  :demo-board-pieces [rose-source-piece]})
        power-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "sun")
                     (app-state/select-move-piece :rose-scout))
        staged-db (app-state/select-move-power power-db :sun)
        confirmed-db (app-state/confirm-move staged-db)]
    (is (= :power (:stage (app-state/move-selection power-db))))
    (is (= [:cup :disc :sun]
           (mapv :id (app-state/move-power-options power-db))))
    (is (= :sun (app-state/move-power staged-db)))
    (is (= :confirm (:stage (app-state/move-selection staged-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "sun"
                     :piece-id :rose-scout}
            :power :sun
            :card-id "sun"}
           (app-state/move-command staged-db)))
    (is (= :rejected (:stage (app-state/move-selection confirmed-db))))
    (is (= :move-transition-unavailable
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= (app-state/move-params staged-db)
           (app-state/move-params confirmed-db)))
    (is (= (app-state/game staged-db)
           (app-state/game confirmed-db)))))

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
        created-piece (piece-by-id confirmed-db :rose-small-1)]
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
