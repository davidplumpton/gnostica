(ns gnostica.test-support.deck
  (:require [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]))

(defn card-ids [cards]
  (mapv :id cards))

(defn hand-card-count [player-count]
  (* game-state/starting-hand-size player-count))

(defn- player-count [player-specs-or-count]
  (if (integer? player-specs-or-count)
    player-specs-or-count
    (count player-specs-or-count)))

(defn board-card-position
  ([board-index]
   (board-card-position 2 board-index))
  ([player-specs-or-count board-index]
   (+ (hand-card-count (player-count player-specs-or-count))
      board-index)))

(defn draw-pile-position
  ([draw-index]
   (draw-pile-position 2 draw-index))
  ([player-specs-or-count draw-index]
   (+ (board-card-position player-specs-or-count board/board-card-count)
      draw-index)))

(defn deck-starting-with [card-ids]
  (let [front-ids (set card-ids)]
    (vec
     (concat
      (map cards/card-by-id card-ids)
      (remove #(contains? front-ids (:id %)) cards/deck)))))

(defn deck-with-card-at [index card-id]
  (let [card (cards/card-by-id card-id)
        remaining-cards (remove #(= card-id (:id %)) cards/deck)]
    (vec
     (concat
      (take index remaining-cards)
      [card]
      (drop index remaining-cards)))))

(defn deck-with-cards-at [placements]
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

(defn deck-with-board-card
  ([board-index card-id]
   (deck-with-board-card 2 board-index card-id))
  ([player-specs-or-count board-index card-id]
   (deck-with-card-at (board-card-position player-specs-or-count board-index)
                      card-id)))
