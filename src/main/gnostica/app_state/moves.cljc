(ns gnostica.app-state.moves
  (:require [gnostica.app-state.db :as db]
            [gnostica.app-state.view-models :as views]
            [gnostica.game-state :as game-state]
            [gnostica.gesture-intent :as gesture-intent]
            [gnostica.move-selection :as move-selection]
            #?(:clj [gnostica.app-state.facade-macros
                     :refer [def-facade-aliases]]))
  #?(:cljs
     (:require-macros [gnostica.app-state.facade-macros
                       :refer [def-facade-aliases]])))

(def-facade-aliases
  move-selection
  gnostica.app-state.facade-exports/move-selection-alias-vars)

(defn- apply-end-turn-result [app-db result]
  (if (:ok? result)
    (-> app-db
        (assoc :game (:state result)
               :turn-result result
               :move-selection (assoc (db/empty-move-selection)
                                      :last-result result))
        (dissoc :turn-action))
    (assoc app-db :turn-result result)))

(defn end-turn
  ([app-db]
   (end-turn app-db {}))
  ([app-db command]
   (let [player-id (or (:player-id command)
                       (views/current-player-id app-db))
         result (if-let [state (db/game app-db)]
                  (game-state/end-turn
                   state
                   (assoc command :player-id player-id))
                  (game-state/failure :missing-game
                                      "Cannot end a turn before a game has started."
                                      {}))]
     (apply-end-turn-result app-db result))))

(defn confirm-move
  ([app-db] (confirm-move app-db {}))
  ([app-db transition-options]
   (let [confirmed-db (move-selection/confirm-move app-db transition-options)]
     (if (true? (get-in confirmed-db [:move-selection :last-result :ok?]))
       (gesture-intent/cancel-gesture-intent confirmed-db)
       (gesture-intent/refresh-gesture-intent confirmed-db)))))
