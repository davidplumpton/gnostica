(ns gnostica.game-state.setup.creation
  (:require [clojure.string :as str]
            [gnostica.board :as board]
            [gnostica.game-state.constants :as constants]
            [gnostica.game-state.deck :as deck]
            [gnostica.game-state.pieces :as piece-state]
            [gnostica.game-state.players :as players]
            [gnostica.game-state.result :as result]
            [gnostica.pieces :as pieces]))

(defn- player-id [player-spec]
  (:id player-spec))

(defn- duplicate-player-ids [player-specs]
  (->> player-specs
       (map player-id)
       frequencies
       (filter (fn [[_ occurrences]]
                 (> occurrences 1)))
       (map first)
       vec))

(defn- missing-player-id-indexes [player-specs]
  (->> player-specs
       (map-indexed vector)
       (keep (fn [[index player-spec]]
               (when (nil? (player-id player-spec))
                 index)))
       vec))

(defn- unknown-player-ids [player-specs]
  (->> player-specs
       (map player-id)
       (remove #(contains? pieces/players-by-id %))
       distinct
       vec))

(defn- valid-css-color? [value]
  (and (string? value)
       (some? (re-matches #"#[0-9a-fA-F]{6}" value))))

(defn- valid-three-color? [value]
  (and (integer? value)
       (<= 0 value 0xffffff)))

(defn- missing-or-invalid-player-fields [player]
  (cond-> []
    (nil? (:id player))
    (conj :id)

    (not (and (string? (:name player))
              (not (str/blank? (:name player)))))
    (conj :name)

    (not (valid-three-color? (:color player)))
    (conj :color)

    (not (valid-css-color? (:css-color player)))
    (conj :css-color)))

(defn- invalid-player-metadata [player-specs]
  (->> player-specs
       (map-indexed
        (fn [index player-spec]
          (let [player (when (map? player-spec)
                         (merge (get pieces/players-by-id (player-id player-spec))
                                player-spec))
                invalid-fields (missing-or-invalid-player-fields player)]
            (when (seq invalid-fields)
              {:index index
               :id (player-id player-spec)
               :invalid-fields invalid-fields}))))
       (remove nil?)
       vec))

(defn- validate-player-specs [player-specs]
  (if-not (sequential? player-specs)
    (result/failure :invalid-player-specs
                    "Player specs must be a sequential collection."
                    {:player-specs player-specs})
    (let [missing-id-indexes (missing-player-id-indexes player-specs)
          duplicate-ids (duplicate-player-ids player-specs)
          unknown-ids (unknown-player-ids player-specs)
          invalid-players (invalid-player-metadata player-specs)]
      (cond
        (not (players/valid-player-count? player-specs))
        (result/failure :invalid-player-count
                        "Gnostica requires between two and six players."
                        {:count (count player-specs)
                         :minimum constants/min-players
                         :maximum constants/max-players})

        (seq missing-id-indexes)
        (result/failure :invalid-player-specs
                        "Each player spec must include an :id."
                        {:missing-id-indexes missing-id-indexes})

        (seq duplicate-ids)
        (result/failure :duplicate-player-ids
                        "Player ids must be unique."
                        {:duplicate-ids duplicate-ids})

        (seq unknown-ids)
        (result/failure :unknown-player-ids
                        "Player ids must reference known player metadata."
                        {:unknown-ids unknown-ids
                         :known-ids (mapv :id pieces/players)})

        (seq invalid-players)
        (result/failure
         :invalid-player-metadata
         "Player metadata must include a name, Three.js color, and CSS color."
         {:players invalid-players
          :required-fields constants/required-player-fields})))))

(defn- normalize-player [index player-spec]
  (let [id (player-id player-spec)
        piece-player (get pieces/players-by-id id)]
    (merge
     (select-keys piece-player [:id :name :color :css-color])
     player-spec
     {:order-index index
      :hand []
      :score 0
      :challenge nil
      :eliminated? false
      :stash (piece-state/initial-stash)
      :bid nil})))

(defn- target-score-option [opts]
  (if (contains? opts :target-score)
    (:target-score opts)
    constants/default-target-score))

(defn- validate-target-score [target-score]
  (when-not (contains? constants/allowed-target-scores target-score)
    (result/failure
     :invalid-target-score
     "Gnostica target score must be 8, 9, or 10."
     {:target-score target-score
      :allowed-target-scores (vec (sort constants/allowed-target-scores))})))

(defn- deal-starting-hands [players deck]
  (let [hand-card-count (* constants/starting-hand-size (count players))
        hand-cards (take hand-card-count deck)
        hands (partition constants/starting-hand-size hand-cards)
        board-deck (vec (drop hand-card-count deck))]
    {:players (mapv (fn [player hand]
                      (assoc player :hand (vec hand)))
                    players
                    hands)
     :board-deck board-deck}))

(defn create-base-game
  [player-specs opts]
  (if-let [error (validate-player-specs player-specs)]
    error
    (let [target-score (target-score-option opts)
          minimum-card-count (deck/required-starting-card-count (count player-specs))
          source-deck (deck/deck-source opts)]
      (if-let [error (or (validate-target-score target-score)
                         (deck/validate-deck source-deck minimum-card-count))]
        error
        (let [ordered-cards (deck/ordered-deck opts)]
          (if-let [error (deck/validate-deck ordered-cards minimum-card-count)]
            error
            (let [base-players (mapv normalize-player (range) player-specs)
                  {:keys [players board-deck]} (deal-starting-hands base-players
                                                                    ordered-cards)
                  board-cells (board/initial-board board-deck identity)
                  event {:type :game/created
                         :phase constants/initial-phase
                         :player-ids (mapv :id players)
                         :starting-hand-size constants/starting-hand-size
                         :board-card-count (count board-cells)}
                  state {:phase constants/initial-phase
                         :players players
                         :players-by-id (into {} (map (juxt :id identity) players))
                         :turn (players/initial-turn players)
                         :board board-cells
                         :pieces {:on-board []
                                  :stashes (piece-state/initial-stashes players)}
                         :draw-pile (vec (drop board/board-card-count board-deck))
                         :discard-pile []
                         :setup {:bids {}
                                 :bid-history []
                                 :deck-card-ids (mapv :id ordered-cards)
                                 :starting-player-id nil
                                 :target-score target-score}
                         :winner nil
                         :history [event]}]
              (result/success state [event]))))))))
