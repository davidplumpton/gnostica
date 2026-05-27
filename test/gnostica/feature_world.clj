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

(defn- board-deck-position [player-count board-index]
  (+ (hand-card-count player-count) board-index))

(defn deck-starting-with [card-ids]
  (let [front-ids (set card-ids)]
    (vec
     (concat
      (map cards/card-by-id card-ids)
      (remove #(contains? front-ids (:id %)) cards/deck)))))

(defn deck-with-cards-at [position->card-id]
  (let [placed-card-ids (set (vals position->card-id))]
    (loop [index 0
           filler-cards (remove #(contains? placed-card-ids (:id %)) cards/deck)
           deck []]
      (if (= (count cards/deck) (count deck))
        (vec deck)
        (if-let [card-id (get position->card-id index)]
          (recur (inc index)
                 filler-cards
                 (conj deck (cards/card-by-id card-id)))
          (recur (inc index)
                 (rest filler-cards)
                 (conj deck (first filler-cards))))))))

(defn create-official-starting-bid-game [world]
  (let [specs (player-specs 3)
        result (game-state/create-game
                specs
                {:deck-order (deck-with-cards-at
                              {0 "cupsking"
                               1 "coins2"
                               6 "swordsking"
                               7 "cups3"
                               12 "wandsqueen"
                               13 "world"})
                 :starting-bids
                 {:rounds [{:rose "cupsking"
                            :indigo "swordsking"
                            :gold "wandsqueen"}
                           {:rose "coins2"
                            :indigo "cups3"
                            :gold "world"}]
                  :redraws {:indigo ["world" "cupsking"]
                            :rose ["swordsking" "wandsqueen"]
                            :gold ["coins2" "cups3"]}}})]
    (cond-> (assoc world
                   :player-specs specs
                   :last-result result)
      (:ok? result) (assoc :state (:state result)))))

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

(def rose-disc-minion
  {:id :rose-disc-minion
   :player-id :rose
   :space-index 3
   :size :medium
   :orientation :east})

(def indigo-disc-target
  {:id :indigo-disc-target
   :player-id :indigo
   :space-index 4
   :size :small
   :orientation :north})

(def rose-medium-pieces
  [{:id :rose-medium-a
    :player-id :rose
    :space-index 0
    :size :medium
    :orientation :north}
   {:id :rose-medium-b
    :player-id :rose
    :space-index 1
    :size :medium
    :orientation :east}
   {:id :rose-medium-c
    :player-id :rose
    :space-index 2
    :size :medium
    :orientation :south}
   {:id :rose-medium-d
    :player-id :rose
    :space-index 4
    :size :medium
    :orientation :west}
   {:id :rose-medium-e
    :player-id :rose
    :space-index 5
    :size :medium
    :orientation :up}])

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

(defn- with-disc-fixture [world fixture]
  (assoc world :disc-fixture fixture))

(defn- with-draw-major-fixture [world fixture]
  (assoc world :draw-major-fixture fixture))

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

(defn create-rod-unbounded-full-destination-game [world]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-with-board-card 2 3 "emperor")})]
    (-> world
        (put-pieces [rose-rod-minion
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
                      :orientation :south}])
        (with-rod-fixture {:source-kind :territory
                           :source-board-index 3
                           :piece-id :rose-rod-minion}))))

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

(defn create-disc-territory-source-piece-game [world size board-index orientation]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-with-board-card 2 board-index "coins2")})
        minion (assoc rose-disc-minion
                      :space-index board-index
                      :size size
                      :orientation orientation)]
    (-> world
        (put-pieces [minion])
        (with-disc-fixture {:source-kind :territory
                            :source-board-index board-index
                            :piece-id :rose-disc-minion
                            :target-piece-id :rose-disc-minion}))))

(defn create-disc-hand-card-piece-game [world minion-board-index minion-orientation target-board-index target-orientation]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-starting-with ["coins2"])})
        minion (assoc rose-disc-minion
                      :space-index minion-board-index
                      :orientation minion-orientation)
        target (assoc indigo-disc-target
                      :space-index target-board-index
                      :orientation target-orientation)]
    (-> world
        (put-pieces [minion target])
        (with-disc-fixture {:source-kind :hand-card
                            :source-card-id "coins2"
                            :piece-id :rose-disc-minion
                            :target-piece-id :indigo-disc-target}))))

(defn create-disc-hand-card-territory-growth-game [world]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-with-cards-at
                             {0 "coins2"
                              1 "cupsking"
                              (board-deck-position 2 4) "cups2"})})]
    (-> world
        (put-pieces [rose-disc-minion])
        (with-disc-fixture {:source-kind :hand-card
                            :source-card-id "coins2"
                            :piece-id :rose-disc-minion
                            :target-board-index 4}))))

(defn create-disc-territory-source-territory-growth-game [world]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-with-cards-at
                             {(board-deck-position 2 3) "coins2"
                              (board-deck-position 2 4) "cups2"})})]
    (-> world
        (put-pieces [rose-disc-minion])
        (with-disc-fixture {:source-kind :territory
                            :source-board-index 3
                            :piece-id :rose-disc-minion
                            :target-board-index 4}))))

(defn create-disc-enemy-occupied-territory-growth-game [world]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-with-cards-at
                             {0 "cupsking"
                              (board-deck-position 2 3) "coins2"
                              (board-deck-position 2 4) "cups2"})})
        guard (assoc indigo-disc-target :space-index 4)]
    (-> world
        (put-pieces [rose-disc-minion guard])
        (with-disc-fixture {:source-kind :territory
                            :source-board-index 3
                            :piece-id :rose-disc-minion
                            :target-board-index 4}))))

(defn create-disc-no-medium-stash-game [world board-index orientation]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-with-board-card 2 board-index "coins2")})
        minion (assoc rose-disc-minion
                      :space-index board-index
                      :size :small
                      :orientation orientation)]
    (-> world
        (put-pieces (vec (cons minion rose-medium-pieces)))
        (with-disc-fixture {:source-kind :territory
                            :source-board-index board-index
                            :piece-id :rose-disc-minion
                            :target-piece-id :rose-disc-minion}))))

(defn create-star-disc-territory-growth-game [world]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-with-cards-at
                             {0 "star"
                              (board-deck-position 2 4) "cupsking"})})]
    (-> world
        (put-pieces [rose-disc-minion])
        (with-disc-fixture {:source-kind :hand-card
                            :source-card-id "star"
                            :piece-id :rose-disc-minion
                            :target-board-index 4}))))

(defn create-strength-disc-shortcut-game [world board-index orientation]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-starting-with ["strength"])})
        minion (assoc rose-disc-minion
                      :space-index board-index
                      :size :small
                      :orientation orientation)]
    (-> world
        (put-pieces (vec (cons minion rose-medium-pieces)))
        (with-disc-fixture {:source-kind :hand-card
                            :source-card-id "strength"
                            :piece-id :rose-disc-minion
                            :target-piece-id :rose-disc-minion}))))

(defn create-fool-hand-card-reveal-game [world]
  (let [draw-start (board-deck-position 2 9)
        world (create-game-with-options
               world
               {:deck-order (deck-with-cards-at
                             {0 "fool"
                              draw-start "cups2"
                              (inc draw-start) "wands2"})})]
    (-> world
        (put-pieces [rose-disc-minion])
        (with-draw-major-fixture {:source-card-id "fool"}))))

(defn create-high-priestess-hand-card-redraw-game [world]
  (let [draw-start (board-deck-position 2 9)
        world (create-game-with-options
               world
               {:deck-order (deck-with-cards-at
                             {0 "high-priestess"
                              1 "cups2"
                              2 "wands2"
                              3 "coins2"
                              4 "swords2"
                              5 "cups3"
                              draw-start "cups4"
                              (inc draw-start) "wands4"})})]
    (-> world
        (put-pieces [rose-disc-minion])
        (with-draw-major-fixture {:source-card-id "high-priestess"
                                  :first-discard-card-id "cups2"
                                  :first-drawn-card-id "cups4"}))))

(defn create-judgement-hand-card-limit-game [world]
  (let [draw-start (board-deck-position 2 9)
        world (create-game-with-options
               world
               {:deck-order (deck-with-cards-at
                             {0 "judgement"
                              1 "cups2"
                              2 "wands2"
                              3 "coins2"
                              4 "swords2"
                              5 "cups3"
                              draw-start "cups4"})})
        discard-card (first (get-in world [:state :draw-pile]))]
    (-> world
        (update :state assoc
                :draw-pile (vec (rest (get-in world [:state :draw-pile])))
                :discard-pile [discard-card])
        (put-pieces [(assoc rose-disc-minion :size :medium)])
        (with-draw-major-fixture {:source-card-id "judgement"
                                  :piece-id :rose-disc-minion
                                  :discard-card-id (:id discard-card)}))))

(defn create-endgame-winning-challenge-game [world]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-with-cards-at
                             {(board-deck-position 2 0) "cups2"
                              (board-deck-position 2 1) "cupsking"
                              (board-deck-position 2 2) "sun"
                              (board-deck-position 2 3) "magician"
                              (board-deck-position 2 4) "wheeloffortune"})})]
    (-> world
        (put-pieces [{:id :rose-spot
                      :player-id :rose
                      :space-index 0
                      :size :small
                      :orientation :north}
                     {:id :rose-royalty
                      :player-id :rose
                      :space-index 1
                      :size :small
                      :orientation :north}
                     {:id :rose-major-a
                      :player-id :rose
                      :space-index 2
                      :size :small
                      :orientation :north}
                     {:id :rose-major-b
                      :player-id :rose
                      :space-index 3
                      :size :small
                      :orientation :north}
                     {:id :indigo-piece
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}])
        (update :state game-state/with-current-scores))))

(defn create-endgame-failing-challenge-game [world]
  (let [world (create-game-with-options
               world
               {:deck-order (deck-with-cards-at
                             {(board-deck-position 2 0) "sun"
                              (board-deck-position 2 4) "magician"})})]
    (-> world
        (put-pieces [{:id :rose-major
                      :player-id :rose
                      :space-index 0
                      :size :small
                      :orientation :north}
                     {:id :indigo-piece
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}])
        (update :state game-state/with-current-scores))))

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

(defn disc-source [{:keys [source-kind source-board-index source-card-id piece-id]}]
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

(defn apply-disc-command [world command]
  (apply-action world :disc-move game-state/apply-disc-move command))

(defn apply-disc-piece-growth [world orientation]
  (let [fixture (:disc-fixture world)
        command (cond-> {:player-id :rose
                         :source (disc-source fixture)
                         :target {:kind :piece
                                  :piece-id (:target-piece-id fixture)}}
                  orientation (assoc :orientation orientation))]
    (apply-disc-command world command)))

(defn apply-disc-territory-growth [world replacement-source replacement-card-id]
  (let [fixture (:disc-fixture world)
        command (cond-> {:player-id :rose
                         :source (disc-source fixture)
                         :target {:kind :territory
                                  :board-index (:target-board-index fixture)}}
                  replacement-source
                  (assoc :replacement-card-source replacement-source)
                  replacement-card-id
                  (assoc :replacement-card-id replacement-card-id))]
    (apply-disc-command world command)))

(defn apply-strength-disc-piece-shortcut [world]
  (let [fixture (:disc-fixture world)
        target {:kind :piece
                :piece-id (:target-piece-id fixture)}
        command {:player-id :rose
                 :source (disc-source fixture)
                 :disc-actions [{:target target}
                                {:target target}]}]
    (apply-disc-command world command)))

(defn apply-draw-major-command [world command]
  (let [transition (case (get-in command [:source :card-id])
                     "fool" game-state/apply-fool-move
                     "high-priestess" game-state/apply-high-priestess-move
                     "judgement" game-state/apply-judgement-move)]
    (apply-action world :draw-major transition command)))

(defn apply-fool-skip-reveals [world reveal-count]
  (let [fixture (:draw-major-fixture world)
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id (:source-card-id fixture)}
                 :reveals (vec (repeat reveal-count {}))
                 :shuffle-fn identity}]
    (apply-draw-major-command world command)))

(defn apply-high-priestess-redraws [world]
  (let [fixture (:draw-major-fixture world)
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id (:source-card-id fixture)}
                 :redraws [{:discard-card-ids [(:first-discard-card-id fixture)]
                            :draw-count 1}
                           {:discard-card-ids [(:first-drawn-card-id fixture)]
                            :draw-count 1}]
                 :shuffle-fn identity}]
    (apply-draw-major-command world command)))

(defn apply-judgement-over-hand-limit [world]
  (let [fixture (:draw-major-fixture world)
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id (:source-card-id fixture)}
                 :piece-id (:piece-id fixture)
                 :card-ids [(:source-card-id fixture)
                            (:discard-card-id fixture)]}]
    (apply-draw-major-command world command)))

(defn apply-end-turn
  ([world player-id]
   (apply-end-turn world player-id false))
  ([world player-id announce-challenge?]
   (apply-action world
                 :end-turn
                 game-state/end-turn
                 {:player-id player-id
                  :announce-challenge? announce-challenge?})))

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

(defn player-score [world player-id]
  (get-in (:state world) [:players-by-id player-id :score]))

(defn player-eliminated? [world player-id]
  (true? (get-in (:state world) [:players-by-id player-id :eliminated?])))

(defn active-challenge-player-id [world]
  (game-state/active-challenge-player-id (:state world)))

(defn starting-player-id [world]
  (state-at world [:setup :starting-player-id]))

(defn bid-history [world]
  (state-at world [:setup :bid-history]))

(defn bid-redraw-order [world]
  (state-at world [:setup :bid-redraw-order]))

(defn winner [world]
  (:winner (:state world)))

(defn player-piece-count [world player-id]
  (count (filter #(= player-id (:player-id %))
                 (state-at world [:pieces :on-board]))))

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
