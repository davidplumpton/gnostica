(ns gnostica.app-state
  (:require [gnostica.game-state :as game-state]
            [gnostica.pieces :as pieces]))

(def default-player-specs
  (mapv #(select-keys % [:id :name]) pieces/players))

(def default-selected-board-index 0)

(def default-demo-board-pieces
  pieces/initial-pieces)

(defn- state-with-demo-board-pieces [state demo-board-pieces]
  (cond-> state
    (some? demo-board-pieces)
    (assoc-in [:pieces :on-board] (vec demo-board-pieces))))

(defn initialize
  ([] (initialize {}))
  ([{:keys [player-specs game-options selected-board-index demo-board-pieces]
     :or {player-specs default-player-specs
          game-options {}
          selected-board-index default-selected-board-index
          demo-board-pieces default-demo-board-pieces}}]
   (let [result (game-state/create-game player-specs game-options)
         base-db {:selected-board-index selected-board-index
                  :three-texture-errors []}]
     (if (:ok? result)
       (assoc base-db :game (state-with-demo-board-pieces (:state result)
                                                          demo-board-pieces))
       (assoc base-db :setup-error (:error result))))))

(defn game [db]
  (:game db))

(defn setup-error [db]
  (:setup-error db))

(defn board [db]
  (get-in db [:game :board] []))

(defn board-pieces [db]
  (get-in db [:game :pieces :on-board] []))

(defn selected-board-index [db]
  (:selected-board-index db))

(defn selected-board-cell [db]
  (get (board db) (selected-board-index db)))

(defn selected-board-pieces [db]
  (pieces/pieces-for-space (board-pieces db) (selected-board-index db)))

(defn current-player [db]
  (some-> (game db) game-state/current-player))

(defn select-board-card [db index]
  (if (contains? (board db) index)
    (assoc db :selected-board-index index)
    db))
