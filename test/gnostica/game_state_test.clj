(ns gnostica.game-state-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]))

(def player-specs
  [{:id :rose
    :name "Rose"}
   {:id :indigo
    :name "Indigo"}])

(deftest creates-deterministic-initial-state
  (let [{:keys [ok? state]} (game-state/create-game player-specs {:shuffle-fn identity})]
    (is ok?)
    (is (= (board/initial-board cards/deck identity)
           (:board state)))
    (is (= (mapv :id (drop board/board-card-count cards/deck))
           (mapv :id (:draw-pile state))))
    (is (empty? (:discard-pile state)))
    (is (= #{:rose :indigo}
           (set (keys (get-in state [:pieces :stashes])))))))

(deftest records-phase-and-current-player-shape
  (let [{:keys [state]} (game-state/create-game player-specs {:shuffle-fn identity})]
    (is (= :setup (:phase state)))
    (is (= {:order [:rose :indigo]
            :current-player-index 0
            :current-player-id :rose
            :round 1}
           (:turn state)))
    (is (= :rose (:id (game-state/current-player state))))
    (is (= {:small 5
            :medium 5
            :large 5}
           (get-in state [:pieces :stashes :rose])))))

(deftest explicit-deck-order-controls-board-and-draw-pile
  (let [deck-order (vec (reverse cards/deck))
        {:keys [state]} (game-state/create-game player-specs {:deck-order deck-order})
        board-card-ids (mapv (comp :id :card) (:board state))]
    (is (= (mapv :id (take board/board-card-count deck-order))
           board-card-ids))
    (is (= (mapv :id (drop board/board-card-count deck-order))
           (mapv :id (:draw-pile state))))))

(deftest rejects-invalid-player-counts
  (doseq [player-specs [[] [{:id :solo}] (mapv (fn [index]
                                                 {:id (keyword (str "player-" index))})
                                               (range 7))]]
    (let [{:keys [ok? error]} (game-state/create-game player-specs {:shuffle-fn identity})]
      (is (false? ok?))
      (is (= :invalid-player-count (:code error))))))

(deftest advances-turn-with-success-result
  (let [{:keys [state]} (game-state/create-game player-specs {:shuffle-fn identity})
        {:keys [ok? state events]} (game-state/advance-turn state)]
    (is ok?)
    (is (= :indigo (get-in state [:turn :current-player-id])))
    (is (= :indigo (:id (game-state/current-player state))))
    (is (= [{:type :turn/advanced
             :player-id :indigo
             :round 1}]
           events))))
