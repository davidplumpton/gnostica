(ns gnostica.feature-world
  (:require [clojure.set :as set]
            [gnostica.cards :as cards]
            [gnostica.game-schema :as game-schema]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as pieces]))

(defn player-specs [player-count]
  (mapv #(select-keys % [:id])
        (take player-count pieces/players)))

(defn create-deterministic-game [world player-count]
  (let [specs (player-specs player-count)
        result (game-state/create-game specs {:shuffle-fn identity})]
    (cond-> (assoc world
                   :player-specs specs
                   :last-result result)
      (:ok? result) (assoc :state (:state result)))))

(defn apply-action [world action f & args]
  (let [result (apply f (:state world) args)]
    (cond-> (assoc world
                   :last-action action
                   :last-result result)
      (:ok? result) (assoc :state (:state result)))))

(defn state-at [world path]
  (get-in (:state world) path))

(defn schema-explanation [world]
  (game-schema/explain-game (:state world)))

(defn valid-state? [world]
  (nil? (schema-explanation world)))

(defn board-cells [world]
  (state-at world [:board]))

(defn players [world]
  (state-at world [:players]))

(defn hand-sizes [world]
  (mapv (comp count :hand) (players world)))

(defn face-up-board-card-count [world]
  (count (filter #(= :up (:face %)) (board-cells world))))

(defn all-card-ids [world]
  (let [state (:state world)]
    (vec
     (concat
      (map :id (mapcat :hand (:players state)))
      (map (comp :id :card) (:board state))
      (map :id (:draw-pile state))
      (map :id (:discard-pile state))))))

(defn duplicate-card-ids [ids]
  (->> ids
       frequencies
       (filter (fn [[_ occurrences]]
                 (< 1 occurrences)))
       (map first)
       sort
       vec))

(defn deck-accounting [world]
  (let [ids (all-card-ids world)
        actual (set ids)
        expected (set (map :id cards/deck))]
    {:expected-count (count cards/deck)
     :actual-count (count ids)
     :unique-count (count actual)
     :duplicates (duplicate-card-ids ids)
     :missing (vec (sort (set/difference expected actual)))
     :extra (vec (sort (set/difference actual expected)))}))

(defn complete-deck-accounting? [world]
  (let [{:keys [expected-count actual-count unique-count duplicates missing extra]} (deck-accounting world)]
    (and (= expected-count actual-count unique-count)
         (empty? duplicates)
         (empty? missing)
         (empty? extra))))

(defn setup-summary [world]
  {:player-count (count (players world))
   :hand-sizes (hand-sizes world)
   :board-card-count (count (board-cells world))
   :face-up-board-card-count (face-up-board-card-count world)
   :draw-pile-count (count (state-at world [:draw-pile]))
   :discard-pile-count (count (state-at world [:discard-pile]))
   :deck-accounting (deck-accounting world)
   :schema-explanation (schema-explanation world)})
