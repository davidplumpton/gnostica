(ns gnostica.app-state.lobby-start
  (:require [gnostica.app-state.db :as db]
            [gnostica.app-state.lobby-bidding :as bidding]
            [gnostica.app-state.lobby-setup :as setup]
            [gnostica.game-state :as game-state]))

(defn game-options-with-transition-shuffle [game-options transition-options]
  (let [game-options (or game-options {})
        shuffle-fn (:shuffle-fn transition-options)]
    (cond-> game-options
      (and shuffle-fn
           (not (contains? game-options :deck-order))
           (not (contains? game-options :shuffle-fn)))
      (assoc :shuffle-fn shuffle-fn))))

(defn start-lobby-bidding
  ([app-db] (start-lobby-bidding app-db {}))
  ([app-db transition-options]
   (if-not (setup/lobby-active? app-db)
     app-db
     (let [app-db (-> app-db setup/clear-lobby-starting-bid setup/refresh-lobby-error)
           lobby (setup/lobby app-db)
           validation-error (setup/lobby-validation-error lobby)]
       (if validation-error
         (assoc-in app-db [:lobby :error] validation-error)
         (let [start-options (:start-options lobby)
               game-options (dissoc
                             (game-options-with-transition-shuffle
                              (:game-options start-options)
                              transition-options)
                             :starting-bids)
               result (game-state/create-game (setup/lobby-player-specs lobby)
                                              game-options)]
           (if (:ok? result)
             (-> app-db
                 (assoc :move-selection (db/empty-move-selection))
                 (assoc-in [:lobby :starting-bid]
                           (bidding/initial-starting-bid (:state result)))
                 (update :lobby dissoc :error)
                 (dissoc :setup-error :game :turn-action))
             (assoc-in app-db [:lobby :error] (:error result)))))))))

(defn confirm-lobby-bidding [app-db]
  (let [starting-bid (get-in app-db [:lobby :starting-bid])]
    (cond
      (nil? starting-bid)
      app-db

      (= :redrawing (:stage starting-bid))
      (assoc-in app-db [:lobby :error]
                (setup/lobby-error :starting-bid-redraw-incomplete
                                   "Finish bid-card redraws before starting the game."
                                   {:active-player-id
                                    (bidding/starting-bid-active-redraw-player-id
                                     starting-bid)
                                    :redraw-order (:redraw-order starting-bid)}))

      (not= :resolved (:stage starting-bid))
      (assoc-in app-db [:lobby :error]
                (setup/lobby-error :starting-bid-unresolved
                                   "Reveal a winning bid before starting the game."
                                   {:stage (:stage starting-bid)}))

      :else
      (let [result (game-state/apply-starting-bids
                    (:initial-game starting-bid)
                    {:rounds (:rounds starting-bid)
                     :redraws (:redraws starting-bid)})]
        (if (:ok? result)
          (-> app-db
              (assoc :game (db/state-with-demo-board-pieces
                            (:state result)
                            (get-in app-db [:lobby :start-options])))
              (assoc :move-selection (db/empty-move-selection))
              (dissoc :turn-action)
              (dissoc :setup-error :lobby))
          (assoc-in app-db [:lobby :error] (:error result)))))))

(defn start-lobby-game
  ([app-db] (start-lobby-game app-db {}))
  ([app-db transition-options]
   (if-not (setup/lobby-active? app-db)
     app-db
     (let [starting-bid (get-in app-db [:lobby :starting-bid])
           app-db (setup/refresh-lobby-error app-db)
           lobby (setup/lobby app-db)
           validation-error (setup/lobby-validation-error lobby)]
       (cond
         starting-bid
         (assoc-in app-db [:lobby :error]
                   (setup/lobby-error :starting-bid-active
                                      "Finish or cancel bidding before starting a casual game."
                                      {:stage (:stage starting-bid)}))

         validation-error
         (assoc-in app-db [:lobby :error] validation-error)

         :else
         (let [start-options (:start-options lobby)
               game-options (game-options-with-transition-shuffle
                             (:game-options start-options)
                             transition-options)]
           (db/initialize-game-db
            (assoc app-db :move-selection (db/empty-move-selection))
            start-options
            (setup/lobby-player-specs lobby)
            game-options)))))))
