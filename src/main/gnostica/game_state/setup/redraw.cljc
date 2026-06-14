(ns gnostica.game-state.setup.redraw
  (:require [gnostica.game-state.collections :as collections]
            [gnostica.game-state.constants :as constants]
            [gnostica.game-state.hands :as hands]
            [gnostica.game-state.players :as players]
            [gnostica.game-state.result :as result]))

(defn- state-player-ids [state]
  (mapv :id (:players state)))

(defn- player-index-in [players player-id]
  (first
   (keep-indexed (fn [index player]
                   (when (= player-id (:id player))
                     index))
                 players)))

(defn counterclockwise-redraw-order [players winner-id]
  (let [player-ids (mapv :id players)
        winner-index (player-index-in players winner-id)
        player-count (count player-ids)]
    (mapv (fn [offset]
            (get player-ids (mod (- winner-index offset) player-count)))
          (range 1 (inc player-count)))))

(defn- redraw-card-ids-for [redraws player-id]
  (vec (get redraws player-id [])))

(defn- redraw-shape-error [state redraws]
  (let [player-id-set (set (state-player-ids state))]
    (cond
      (not (map? redraws))
      (result/failure
       :invalid-bid-redraws
       "Bid redraws must provide a map from player id to selected bid card ids."
       {:redraws redraws})

      (seq (remove player-id-set (keys redraws)))
      (result/failure
       :unknown-bid-redraw-players
       "Bid redraws can only include players in the game."
       {:unknown-player-ids (vec (sort-by str (remove player-id-set (keys redraws))))
        :player-ids (state-player-ids state)}))))

(defn- apply-redraw-card [state available-cards player-id card-id]
  (if-let [card (get available-cards card-id)]
    {:ok? true
     :state (hands/append-cards-to-hand state player-id [card])
     :available-cards (dissoc available-cards card-id)
     :card card}
    (result/failure
     :invalid-bid-redraw-card
     "Players can only redraw from bid cards that are still available."
     {:player-id player-id
      :card-id card-id
      :available-card-ids (vec (sort (keys available-cards)))})))

(defn- apply-player-redraws [state available-cards player-id card-ids]
  (loop [next-state state
         next-available-cards available-cards
         remaining-card-ids (seq card-ids)
         selected-cards []]
    (if-let [card-id (first remaining-card-ids)]
      (let [{:keys [ok? state available-cards] :as redraw-result}
            (apply-redraw-card next-state next-available-cards player-id card-id)]
        (if ok?
          (recur state
                 available-cards
                 (next remaining-card-ids)
                 (conj selected-cards (:card redraw-result)))
          redraw-result))
      {:ok? true
       :state next-state
       :available-cards next-available-cards
       :selected-card-ids (mapv :id selected-cards)})))

(defn apply-bid-redraws [state bid-cards redraw-order redraws]
  (if-let [error (redraw-shape-error state redraws)]
    error
    (loop [next-state state
           available-cards (into {} (map (juxt :id identity)) bid-cards)
           remaining-player-ids (seq redraw-order)
           redraw-history []]
      (if-let [player-id (first remaining-player-ids)]
        (let [needed-count (- constants/starting-hand-size
                              (count (get-in next-state [:players-by-id player-id :hand])))
              selected-card-ids (redraw-card-ids-for redraws player-id)]
          (if (not= needed-count (count selected-card-ids))
            (result/failure
             :invalid-bid-redraw-count
             "Each player must redraw until their hand has six cards again."
             {:player-id player-id
              :expected-count needed-count
              :actual-count (count selected-card-ids)
              :selected-card-ids selected-card-ids})
            (let [{:keys [ok? state available-cards selected-card-ids] :as redraw-result}
                  (apply-player-redraws next-state
                                        available-cards
                                        player-id
                                        selected-card-ids)]
              (if ok?
                (recur state
                       available-cards
                       (next remaining-player-ids)
                       (conj redraw-history
                             {:player-id player-id
                              :card-ids selected-card-ids}))
                redraw-result))))
        (if (seq available-cards)
          (result/failure
           :unused-bid-redraw-cards
           "All bid cards must be redrawn before the game starts."
           {:remaining-card-ids (vec (sort (keys available-cards)))})
          {:ok? true
           :state next-state
           :redraw-history redraw-history})))))

(defn rotate-players-to-starting-player [state winner-id]
  (let [players (:players state)
        winner-index (player-index-in players winner-id)
        rotated-players (collections/rotate-vector-from-index players winner-index)]
    (assoc state
           :players rotated-players
           :players-by-id (players/rebuild-players-by-id rotated-players)
           :turn (players/initial-turn rotated-players))))
