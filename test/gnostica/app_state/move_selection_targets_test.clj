(ns gnostica.app-state.move-selection-targets-test
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
        highlighted-initial-territories (->> (:territories initial-targets)
                                             (filter :highlight?)
                                             (mapv :board-index))
        highlighted-initial-wastelands (->> (:wastelands initial-targets)
                                            (filter :highlight?)
                                            (mapv #(select-keys % [:row :col])))
        hand-db (-> (app-state/initialize
                     {:player-specs test-player-specs
                      :game-options {:deck-order (deck-starting-with ["cups2"])}
                      :demo-board-pieces [rose-source-piece]})
                    (app-state/select-move-source :play-hand-card))
        hand-targets (app-state/move-legal-targets hand-db)]
    (is (= :piece (:stage (app-state/move-selection piece-db))))
    (is (= :legal (:status (piece-target piece-targets :rose-scout))))
    (is (= :minion (:role (piece-target piece-targets :rose-scout))))
    (is (= :orientation (:stage (app-state/move-selection initial-db))))
    (is (= 9 (count (filter :enabled? (:territories initial-targets)))))
    (is (= 12 (count (filter :enabled? (:wastelands initial-targets)))))
    (is (= [4] highlighted-initial-territories))
    (is (empty? highlighted-initial-wastelands))
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
(deftest move-source-options-reflect-current-game-state
  (let [db (app-state/initialize {:game-options {:shuffle-fn identity}
                                  :demo-board-pieces fixtures/demo-board-pieces})]
    (is (:enabled? (source-option db :activate-territory)))
    (is (:enabled? (source-option db :play-hand-card)))
    (is (:enabled? (source-option db :orient-piece)))
    (is (:enabled? (source-option db :draw-cards)))
    (is (not (:enabled? (source-option db :place-initial-small))))))
