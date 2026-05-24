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

(defn- hand-card-count [player-count]
  (* game-state/starting-hand-size player-count))

(defn deck-starting-with [card-ids]
  (let [front-ids (set card-ids)]
    (vec
     (concat
      (map cards/card-by-id card-ids)
      (remove #(contains? front-ids (:id %)) cards/deck)))))

(defn deck-with-board-card [player-count board-index card-id]
  (let [card (cards/card-by-id card-id)
        other-cards (vec (remove #(= card-id (:id %)) cards/deck))
        deck-index (+ (hand-card-count player-count)
                      board-index)]
    (vec
     (concat
      (take deck-index other-cards)
      [card]
      (drop deck-index other-cards)))))

(def rose-rod-minion
  {:id :rose-rod-minion
   :player-id :rose
   :space-index 3
   :size :medium
   :orientation :east})

(def indigo-rod-target
  {:id :indigo-rod-target
   :player-id :indigo
   :space-index 4
   :size :small
   :orientation :north})

(def rose-territory-passenger
  {:id :rose-territory-passenger
   :player-id :rose
   :space-index 5
   :size :medium
   :orientation :up})

(def rose-landing-minion
  {:id :rose-landing-minion
   :player-id :rose
   :space {:kind :wasteland
           :row 1
           :col 3}
   :size :small
   :orientation :west})

(def indigo-territory-guard
  {:id :indigo-territory-guard
   :player-id :indigo
   :space-index 5
   :size :small
   :orientation :north})

(defn- create-game-with-options [world options]
  (let [specs (player-specs 2)
        result (game-state/create-game specs options)]
    (cond-> (assoc world
                   :player-specs specs
                   :last-result result)
      (:ok? result) (assoc :state (:state result)))))

(defn- put-pieces [world pieces]
  (update world :state game-state/with-board-pieces pieces))

(defn- with-rod-fixture [world fixture]
  (assoc world :rod-fixture fixture))

(defn create-rod-territory-source-game [world board-index orientation]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-with-board-card 2 board-index "wands2")})
        piece (assoc rose-rod-minion
                     :space-index board-index
                     :orientation orientation)]
    (-> world
        (put-pieces [piece])
        (with-rod-fixture {:source-kind :territory
                           :source-board-index board-index
                           :piece-id :rose-rod-minion}))))

(defn create-rod-hand-card-piece-game [world minion-board-index minion-orientation target-board-index target-orientation]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-starting-with ["wands2"])})
        minion (assoc rose-rod-minion
                      :space-index minion-board-index
                      :orientation minion-orientation)
        target (assoc indigo-rod-target
                      :space-index target-board-index
                      :orientation target-orientation)]
    (-> world
        (put-pieces [minion target])
        (with-rod-fixture {:source-kind :hand-card
                           :source-card-id "wands2"
                           :piece-id :rose-rod-minion
                           :target-piece-id :indigo-rod-target}))))

(defn create-rod-territory-push-own-landing-game [world minion-board-index minion-orientation target-board-index landing-row landing-col]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-starting-with ["wands2"])})
        minion (assoc rose-rod-minion
                      :space-index minion-board-index
                      :orientation minion-orientation)
        passenger (assoc rose-territory-passenger
                         :space-index target-board-index)
        landing-piece (assoc rose-landing-minion
                             :space {:kind :wasteland
                                     :row landing-row
                                     :col landing-col})]
    (-> world
        (put-pieces [minion passenger landing-piece])
        (with-rod-fixture {:source-kind :hand-card
                           :source-card-id "wands2"
                           :piece-id :rose-rod-minion
                           :target-board-index target-board-index}))))

(defn create-rod-full-destination-game [world]
  (let [world (create-rod-territory-source-game world 3 :east)]
    (put-pieces world
                [rose-rod-minion
                 {:id :rose-target-minion
                  :player-id :rose
                  :space-index 4
                  :size :medium
                  :orientation :up}
                 {:id :indigo-target-minion
                  :player-id :indigo
                  :space-index 4
                  :size :small
                  :orientation :north}
                 {:id :indigo-target-guard
                  :player-id :indigo
                  :space-index 4
                  :size :large
                  :orientation :south}])))

(defn create-rod-enemy-territory-push-game [world]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-with-board-card 2 4 "wands2")})
        minion (assoc rose-rod-minion :space-index 4)]
    (-> world
        (put-pieces [minion indigo-territory-guard])
        (with-rod-fixture {:source-kind :territory
                           :source-board-index 4
                           :piece-id :rose-rod-minion
                           :target-board-index 5}))))

(defn create-rod-enemy-landing-territory-push-game [world]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-with-board-card 2 4 "wands2")})
        minion (assoc rose-rod-minion :space-index 4)
        enemy-landing {:id :indigo-landing-minion
                       :player-id :indigo
                       :space {:kind :wasteland
                               :row 1
                               :col 3}
                       :size :small
                       :orientation :south}]
    (-> world
        (put-pieces [minion enemy-landing])
        (with-rod-fixture {:source-kind :territory
                           :source-board-index 4
                           :piece-id :rose-rod-minion
                           :target-board-index 5}))))

(defn rod-source [{:keys [source-kind source-board-index source-card-id piece-id]}]
  (case source-kind
    :territory {:kind :territory
                :board-index source-board-index
                :piece-id piece-id}
    :hand-card {:kind :hand-card
                :card-id source-card-id
                :piece-id piece-id}))

(defn apply-action [world action f & args]
  (let [previous-state (:state world)
        result (apply f (:state world) args)]
    (cond-> (assoc world
                   :last-action action
                   :last-result result
                   :previous-state previous-state)
      (:ok? result) (assoc :state (:state result)))))

(defn apply-rod-command [world command]
  (apply-action world :rod-move game-state/apply-rod-move command))

(defn apply-rod-minion-move [world distance orientation]
  (let [fixture (:rod-fixture world)
        command (cond-> {:player-id :rose
                         :source (rod-source fixture)
                         :mode :move-minion
                         :distance distance}
                  orientation (assoc :orientation orientation))]
    (apply-rod-command world command)))

(defn apply-rod-piece-push [world distance orientation]
  (let [fixture (:rod-fixture world)
        command (cond-> {:player-id :rose
                         :source (rod-source fixture)
                         :mode :push-piece
                         :target {:kind :piece
                                  :piece-id (:target-piece-id fixture)}
                         :distance distance}
                  orientation (assoc :orientation orientation))]
    (apply-rod-command world command)))

(defn apply-rod-territory-push [world distance]
  (let [fixture (:rod-fixture world)
        command {:player-id :rose
                 :source (rod-source fixture)
                 :mode :push-territory
                 :target {:kind :territory
                          :board-index (:target-board-index fixture)}
                 :distance distance}]
    (apply-rod-command world command)))

(defn state-at [world path]
  (get-in (:state world) path))

(defn piece-by-id [world piece-id]
  (some #(when (= piece-id (:id %)) %)
        (state-at world [:pieces :on-board])))

(defn board-cell-by-index [world board-index]
  (some #(when (= board-index (:index %)) %)
        (state-at world [:board])))

(defn board-cell-at [world row col]
  (some #(when (and (= row (:row %))
                    (= col (:col %)))
           %)
        (state-at world [:board])))

(defn player-hand-ids [world player-id]
  (mapv :id (get-in (:state world) [:players-by-id player-id :hand])))

(defn discard-ids [world]
  (mapv :id (state-at world [:discard-pile])))

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
