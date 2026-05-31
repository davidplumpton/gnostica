(ns gnostica.test-support.game-state
  (:require [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]
            [gnostica.test-support.deck :as test-deck]))

(def player-specs
  [{:id :rose
    :name "Rose"}
   {:id :indigo
    :name "Indigo"}])

(def three-player-specs
  [{:id :rose
    :name "Rose"}
   {:id :indigo
    :name "Indigo"}
   {:id :gold
    :name "Gold"}])

(defn all-card-ids [state]
  (vec
   (concat
    (map :id (mapcat :hand (:players state)))
    (map (comp :id :card) (:board state))
    (map :id (:draw-pile state))
    (map :id (:discard-pile state)))))

(defn deterministic-game
  ([]
   (deterministic-game player-specs))
  ([game-player-specs]
   (:state (game-state/create-game game-player-specs {:shuffle-fn identity}))))

(defn state-with-pieces
  ([pieces]
   (state-with-pieces player-specs pieces))
  ([game-player-specs pieces]
   (game-state/with-board-pieces (deterministic-game game-player-specs) pieces)))

(defn state-with-board-cards
  ([board-index->card-id]
   (state-with-board-cards board-index->card-id {}))
  ([board-index->card-id opts]
   (state-with-board-cards player-specs board-index->card-id opts))
  ([game-player-specs board-index->card-id opts]
   (:state (game-state/create-game
            game-player-specs
            (assoc opts
                   :deck-order
                   (test-deck/deck-with-cards-at
                    (into {}
                          (map (fn [[board-index card-id]]
                                 [(test-deck/board-card-position
                                   game-player-specs
                                   board-index)
                                  card-id]))
                          board-index->card-id)))))))

(defn player-hand-ids [state player-id]
  (mapv :id (get-in state [:players-by-id player-id :hand])))

(defn- with-players [state players]
  (assoc state
         :players players
         :players-by-id (into {} (map (juxt :id identity) players))))

(defn replace-player-hand [state player-id hand]
  (with-players state
    (mapv (fn [player]
            (if (= player-id (:id player))
              (assoc player :hand (vec hand))
              player))
          (:players state))))

(defn set-player-eliminated [state player-id eliminated?]
  (with-players state
    (mapv (fn [player]
            (if (= player-id (:id player))
              (assoc player :eliminated? eliminated?)
              player))
          (:players state))))

(defn remove-card-id [cards card-id]
  (vec (remove #(= card-id (:id %)) cards)))

(defn move-card-to-discard [state card-id]
  (let [card (cards/card-by-id card-id)]
    (-> (reduce (fn [next-state player-id]
                  (replace-player-hand next-state
                                       player-id
                                       (remove-card-id
                                        (get-in next-state
                                                [:players-by-id player-id :hand])
                                        card-id)))
                state
                (map :id (:players state)))
        (update :draw-pile remove-card-id card-id)
        (update :discard-pile conj card))))
