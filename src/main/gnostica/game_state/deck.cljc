(ns gnostica.game-state.deck
  (:require [clojure.string :as str]
            [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.game-state.constants :as constants]
            [gnostica.game-state.result :as result]))

(defn deck-source [{:keys [deck deck-order]
                    :or {deck cards/deck}}]
  (if deck-order
    deck-order
    deck))

(defn ordered-deck [{:keys [deck deck-order shuffle-fn]
                     :or {deck cards/deck
                          shuffle-fn shuffle}}]
  (let [ordered (if deck-order
                  deck-order
                  (shuffle-fn deck))]
    (if (sequential? ordered)
      (vec ordered)
      ordered)))

(defn invalid-card-fields [card]
  (if-not (map? card)
    constants/required-card-fields
    (cond-> []
      (not (and (string? (:id card))
                (not (str/blank? (:id card)))))
      (conj :id)

      (not (and (string? (:title card))
                (not (str/blank? (:title card)))))
      (conj :title)

      (not (and (string? (:image card))
                (not (str/blank? (:image card)))))
      (conj :image))))

(defn- invalid-cards [deck]
  (->> deck
       (map-indexed
        (fn [index card]
          (let [invalid-fields (invalid-card-fields card)]
            (when (seq invalid-fields)
              {:index index
               :card-id (:id card)
               :invalid-fields invalid-fields}))))
       (remove nil?)
       vec))

(defn- duplicate-card-ids [deck]
  (->> deck
       (map :id)
       frequencies
       (filter (fn [[_ occurrences]]
                 (> occurrences 1)))
       (map first)
       vec))

(defn required-starting-card-count [player-count]
  (+ board/board-card-count
     (* constants/starting-hand-size player-count)))

(defn validate-deck
  ([deck]
   (validate-deck deck board/board-card-count))
  ([deck minimum-card-count]
   (if-not (sequential? deck)
     (result/failure :invalid-deck
                     "The deck must be an ordered sequential collection of card maps."
                     {:deck deck})
     (let [invalid-deck-cards (invalid-cards deck)
           duplicate-ids (duplicate-card-ids deck)]
       (cond
         (< (count deck) minimum-card-count)
         (result/failure
          :insufficient-deck
          "The deck must contain enough cards to deal player hands and build the territory board."
          {:count (count deck)
           :minimum minimum-card-count
           :starting-hand-size constants/starting-hand-size
           :board-card-count board/board-card-count})

         (seq invalid-deck-cards)
         (result/failure :invalid-deck-cards
                         "Every deck card must include an id, title, and image path."
                         {:invalid-cards invalid-deck-cards
                          :required-fields constants/required-card-fields})

         (seq duplicate-ids)
         (result/failure :duplicate-card-ids
                         "Deck card ids must be unique."
                         {:duplicate-ids duplicate-ids}))))))

(defn- valid-shuffle-card-id [card]
  (when (and (map? card)
             (string? (:id card)))
    (:id card)))

(defn- invalid-shuffle-cards [cards]
  (->> cards
       (map-indexed
        (fn [index card]
          (let [invalid-fields (invalid-card-fields card)]
            (when (seq invalid-fields)
              {:index index
               :card-id (valid-shuffle-card-id card)
               :invalid-fields invalid-fields}))))
       (remove nil?)
       vec))

(defn- missing-shuffle-card-ids [expected-card-ids actual-card-ids]
  (let [actual-counts (frequencies actual-card-ids)]
    (->> (frequencies expected-card-ids)
         (keep (fn [[card-id expected-count]]
                 (when (< (get actual-counts card-id 0) expected-count)
                   card-id)))
         vec)))

(defn- unknown-shuffle-card-ids [expected-card-ids actual-card-ids]
  (let [expected-card-id-set (set expected-card-ids)]
    (->> actual-card-ids
         (remove expected-card-id-set)
         distinct
         vec)))

(defn- duplicate-shuffle-card-ids [card-ids]
  (->> card-ids
       frequencies
       (filter (fn [[_ occurrences]]
                 (> occurrences 1)))
       (map first)
       vec))

(defn- changed-shuffle-card-ids [expected-cards actual-cards]
  (let [expected-cards-by-id (into {} (map (juxt :id identity)) expected-cards)]
    (->> actual-cards
         (keep (fn [card]
                 (let [card-id (valid-shuffle-card-id card)]
                   (when (and (contains? expected-cards-by-id card-id)
                              (not= card (get expected-cards-by-id card-id)))
                     card-id))))
         distinct
         vec)))

(defn- validate-shuffled-discard-cards [discard-cards shuffled-cards]
  (let [expected-card-ids (mapv :id discard-cards)
        actual-card-ids (mapv valid-shuffle-card-id shuffled-cards)
        valid-actual-card-ids (keep valid-shuffle-card-id shuffled-cards)
        invalid-cards (invalid-shuffle-cards shuffled-cards)
        duplicate-card-ids (duplicate-shuffle-card-ids valid-actual-card-ids)
        missing-card-ids (missing-shuffle-card-ids expected-card-ids actual-card-ids)
        unknown-card-ids (unknown-shuffle-card-ids expected-card-ids valid-actual-card-ids)
        changed-card-ids (changed-shuffle-card-ids discard-cards shuffled-cards)]
    (when (or (not= (count discard-cards) (count shuffled-cards))
              (seq invalid-cards)
              (seq duplicate-card-ids)
              (seq missing-card-ids)
              (seq unknown-card-ids)
              (seq changed-card-ids))
      (result/failure
       :invalid-shuffle-result
       "The draw-pile shuffle function must return a one-for-one permutation of the discard pile."
       (cond-> {:expected-count (count discard-cards)
                :actual-count (count shuffled-cards)
                :expected-card-ids expected-card-ids
                :actual-card-ids actual-card-ids}
         (seq invalid-cards)
         (assoc :invalid-cards invalid-cards)

         (seq duplicate-card-ids)
         (assoc :duplicate-card-ids duplicate-card-ids)

         (seq missing-card-ids)
         (assoc :missing-card-ids missing-card-ids)

         (seq unknown-card-ids)
         (assoc :unknown-card-ids unknown-card-ids)

         (seq changed-card-ids)
         (assoc :changed-card-ids changed-card-ids))))))

(defn refresh-draw-pile [state shuffle-fn]
  (cond
    (not (ifn? shuffle-fn))
    (result/failure :invalid-shuffle-fn
                    "Draw-pile refresh requires a callable shuffle function."
                    {:shuffle-fn shuffle-fn})

    (and (empty? (:draw-pile state))
         (seq (:discard-pile state)))
    (let [shuffled-cards (shuffle-fn (:discard-pile state))]
      (cond
        (not (sequential? shuffled-cards))
        (result/failure
         :invalid-shuffle-result
         "The draw-pile shuffle function must return a sequential collection of cards."
         {:result shuffled-cards})

        :else
        (if-let [validation-error (validate-shuffled-discard-cards
                                   (:discard-pile state)
                                   shuffled-cards)]
          validation-error
          {:ok? true
           :state (-> state
                      (assoc :draw-pile (vec shuffled-cards))
                      (assoc :discard-pile []))
           :reshuffled? true})))

    :else
    {:ok? true
     :state state
     :reshuffled? false}))
