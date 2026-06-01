(ns gnostica.app-state.move-selection-placement-test
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
