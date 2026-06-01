(ns gnostica.move-selection.confirmation-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.app-state :as app-state]
            [gnostica.game-schema :as game-schema]
            [gnostica.game-state :as game-state]
            [gnostica.test-support.app-db :refer [piece-by-id]]))

(def ^:private test-player-specs
  [{:id :rose}
   {:id :indigo}])

(def ^:private rose-piece
  {:id :rose-scout
   :player-id :rose
   :space-index 0
   :size :small
   :orientation :east})

(deftest successful-confirmation-consumes-turn-action
  (let [ready-db (-> (app-state/initialize {:player-specs test-player-specs
                                            :game-options {:shuffle-fn identity}
                                            :demo-board-pieces [rose-piece]})
                     (app-state/select-move-source :orient-piece)
                     (app-state/select-move-piece :rose-scout)
                     (app-state/set-move-orientation :south))
        confirmed-db (app-state/confirm-move ready-db)
        selection (app-state/move-selection confirmed-db)]
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= :south (:orientation (piece-by-id confirmed-db :rose-scout))))
    (is (app-state/turn-action-consumed? confirmed-db))
    (is (= :orient-piece (get-in confirmed-db [:turn-action :source])))
    (is (= :source (:stage selection)))
    (is (nil? (:source selection)))
    (is (empty? (:params selection)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))

(deftest rejected-confirmation-preserves-staged-selection-and-game
  (let [ready-db (-> (app-state/initialize {:player-specs test-player-specs
                                            :game-options {:shuffle-fn identity}
                                            :demo-board-pieces []})
                     (app-state/select-move-source :place-initial-small)
                     (app-state/set-move-orientation :north))
        stale-game (game-state/with-board-pieces
                     (app-state/game ready-db)
                     [{:id :indigo-blocker
                       :player-id :indigo
                       :space-index 0
                       :size :small
                       :orientation :up}])
        stale-db (assoc ready-db :game stale-game)
        confirmed-db (app-state/confirm-move stale-db)]
    (is (= :confirm (:stage (app-state/move-selection stale-db))))
    (is (= :rejected (:stage (app-state/move-selection confirmed-db))))
    (is (= :target-space-occupied
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= (app-state/move-params stale-db)
           (app-state/move-params confirmed-db)))
    (is (= stale-game (app-state/game confirmed-db)))
    (is (false? (app-state/turn-action-consumed? confirmed-db)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
