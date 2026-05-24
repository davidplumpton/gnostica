(ns gnostica.game-state.draw
  (:require [gnostica.game-state.core :as core]))

(defn- refresh-draw-pile [state shuffle-fn]
  (if (and (empty? (:draw-pile state))
           (seq (:discard-pile state)))
    (let [shuffled-cards (shuffle-fn (:discard-pile state))]
      (if (sequential? shuffled-cards)
        {:ok? true
         :state (-> state
                    (assoc :draw-pile (vec shuffled-cards))
                    (assoc :discard-pile []))
         :reshuffled? true}
        (core/failure :invalid-shuffle-result
                 "The draw-pile shuffle function must return a sequential collection of cards."
                 {:result shuffled-cards})))
    {:ok? true
     :state state
     :reshuffled? false}))

(defn- draw-from-piles [state draw-count shuffle-fn]
  (loop [state state
         remaining draw-count
         drawn-cards []
         reshuffled? false]
    (if (zero? remaining)
      (let [refresh-result (if (pos? draw-count)
                             (refresh-draw-pile state shuffle-fn)
                             {:ok? true
                              :state state
                              :reshuffled? false})]
        (if (:ok? refresh-result)
          {:ok? true
           :state (:state refresh-result)
           :drawn-cards drawn-cards
           :reshuffled? (or reshuffled? (:reshuffled? refresh-result))}
          refresh-result))
      (let [refresh-result (refresh-draw-pile state shuffle-fn)]
        (if-not (:ok? refresh-result)
          refresh-result
          (let [state (:state refresh-result)
                draw-pile (:draw-pile state)]
            (if-let [card (first draw-pile)]
              (recur (assoc state :draw-pile (vec (rest draw-pile)))
                     (dec remaining)
                     (conj drawn-cards card)
                     (or reshuffled? (:reshuffled? refresh-result)))
              (core/failure :insufficient-draw-cards
                       "There are not enough cards available to draw."
                       {:draw-count draw-count
                        :drawn-count (count drawn-cards)}))))))))

(defn apply-draw-move [state command]
  (let [{:keys [player-id draw-count shuffle-fn]} command
        discard-card-ids (or (:discard-card-ids command) [])
        shuffle-fn (or shuffle-fn shuffle)]
    (cond
      (not (map? command))
      (core/failure :invalid-draw-command
               "Draw moves require a command map."
               {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (core/failure :unknown-player
               "Draw moves require a participating player."
               {:player-id player-id})

      (not (core/current-player-id? state player-id))
      (core/failure :not-current-player
               "Only the current player can apply a draw move."
               {:player-id player-id
                :current-player-id (get-in state [:turn :current-player-id])})

      (not (sequential? discard-card-ids))
      (core/failure :invalid-discard-cards
               "Draw moves require :discard-card-ids to be a sequential collection."
               {:discard-card-ids discard-card-ids})

      (seq (core/duplicate-values discard-card-ids))
      (core/failure :duplicate-discard-cards
               "A card can only be discarded once by a draw move."
               {:duplicate-card-ids (core/duplicate-values discard-card-ids)})

      (not (int? draw-count))
      (core/failure :invalid-draw-count
               "Draw moves require an integer draw count."
               {:draw-count draw-count})

      (neg? draw-count)
      (core/failure :invalid-draw-count
               "Draw moves cannot draw a negative number of cards."
               {:draw-count draw-count})

      (not (ifn? shuffle-fn))
      (core/failure :invalid-shuffle-fn
               "Draw moves require a callable shuffle function."
               {:shuffle-fn shuffle-fn})

      :else
      (let [hand (get-in state [:players-by-id player-id :hand])
            hand-by-id (into {} (map (juxt :id identity)) hand)
            missing-card-ids (vec (remove #(contains? hand-by-id %) discard-card-ids))
            cards-to-discard (mapv hand-by-id discard-card-ids)
            post-discard-hand-count (- (count hand) (count discard-card-ids))
            hand-slots (max 0 (- core/starting-hand-size post-discard-hand-count))
            available-cards (+ (count (:draw-pile state))
                               (count (:discard-pile state))
                               (count cards-to-discard))
            maximum-draw-count (min hand-slots available-cards)]
        (cond
          (seq missing-card-ids)
          (core/failure :invalid-discard-cards
                   "Discarded cards must be in the current player's hand."
                   {:player-id player-id
                    :missing-card-ids missing-card-ids})

          (< maximum-draw-count draw-count)
          (core/failure :invalid-draw-count
                   "Draw moves cannot exceed the six-card hand limit or available deck cards."
                   {:draw-count draw-count
                    :maximum maximum-draw-count
                    :hand-size (count hand)
                    :discard-count (count cards-to-discard)
                    :available-cards available-cards})

          :else
          (let [discarded-state (-> state
                                    (core/remove-cards-from-hand player-id discard-card-ids)
                                    (core/discard-cards cards-to-discard))
                draw-result (draw-from-piles discarded-state draw-count shuffle-fn)]
            (if-not (:ok? draw-result)
              draw-result
              (let [drawn-cards (:drawn-cards draw-result)
                    event {:type :draw/cards-drawn
                           :player-id player-id
                           :discarded-card-ids (vec discard-card-ids)
                           :draw-count draw-count
                           :drawn-card-ids (mapv :id drawn-cards)
                           :reshuffled-discard? (true? (:reshuffled? draw-result))}
                    next-state (-> (:state draw-result)
                                   (core/append-cards-to-hand player-id drawn-cards)
                                   (core/append-history event))]
                (core/success next-state [event])))))))))
