(ns gnostica.game-state.draw
  (:require [gnostica.game-state.core :as core]
            [gnostica.game-state.cup :as cup]
            [gnostica.game-state.disc-major :as disc-major]
            [gnostica.game-state.major :as major]
            [gnostica.game-state.major-power :as major-power]
            [gnostica.game-state.rod :as rod]
            [gnostica.game-state.sword-major :as sword-major]
            [gnostica.pieces :as pieces]))

(defn- draw-from-piles [state draw-count shuffle-fn]
  (loop [state state
         remaining draw-count
         drawn-cards []
         reshuffled? false]
    (if (zero? remaining)
      (let [refresh-result (if (pos? draw-count)
                             (core/refresh-draw-pile state shuffle-fn)
                             {:ok? true
                              :state state
                              :reshuffled? false})]
        (if (:ok? refresh-result)
          {:ok? true
           :state (:state refresh-result)
           :drawn-cards drawn-cards
           :reshuffled? (or reshuffled? (:reshuffled? refresh-result))}
          refresh-result))
      (let [refresh-result (core/refresh-draw-pile state shuffle-fn)]
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

(defn- source-card-id [source-result]
  (get-in source-result [:source-card :id]))

(defn- resolve-specific-major-source
  ([state command card-id error-code message]
   (resolve-specific-major-source state command card-id error-code message {}))
  ([state command card-id error-code message source-opts]
   (let [source-result (major/resolve-major-source state command source-opts)]
     (cond
       (not (:ok? source-result))
       source-result

       (not= card-id (source-card-id source-result))
       (core/failure error-code
                     message
                     {:card-id (source-card-id source-result)
                      :required-card-id card-id
                      :source (core/source-summary (:source source-result))})

       :else
       source-result))))

(defn- normalize-action-list [command key label maximum]
  (let [actions (get command key)]
    (cond
      (nil? actions)
      {:ok? true
       :actions []}

      (not (sequential? actions))
      (core/failure :invalid-major-actions
                    (str label " requires a sequential " (name key) " collection.")
                    {key actions})

      (< maximum (count actions))
      (core/failure :invalid-major-actions
                    (str label " can apply at most " maximum " actions.")
                    {:action-count (count actions)
                     :maximum maximum})

      :else
      {:ok? true
       :actions (vec actions)})))

(defn- apply-redraw-pass
  [state player-id source-summary pass-index pass shuffle-fn event-type]
  (let [discard-card-ids (or (:discard-card-ids pass) [])]
    (cond
      (not (map? pass))
      (core/failure :invalid-redraw-pass
                    "Redraw passes require command maps."
                    {:pass-index pass-index
                     :pass pass})

      (not (sequential? discard-card-ids))
      (core/failure :invalid-discard-cards
                    "Redraw passes require :discard-card-ids to be a sequential collection."
                    {:discard-card-ids discard-card-ids})

      (seq (core/duplicate-values discard-card-ids))
      (core/failure :duplicate-discard-cards
                    "A card can only be discarded once in a redraw pass."
                    {:duplicate-card-ids (core/duplicate-values discard-card-ids)})

      (not (int? (:draw-count pass)))
      (core/failure :invalid-draw-count
                    "Redraw passes require an integer draw count."
                    {:draw-count (:draw-count pass)})

      (neg? (:draw-count pass))
      (core/failure :invalid-draw-count
                    "Redraw passes cannot draw a negative number of cards."
                    {:draw-count (:draw-count pass)})

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
            maximum-draw-count (min hand-slots available-cards)
            draw-count (:draw-count pass)]
        (cond
          (seq missing-card-ids)
          (core/failure :invalid-discard-cards
                        "Redraw discarded cards must be in the current player's hand."
                        {:player-id player-id
                         :missing-card-ids missing-card-ids})

          (< maximum-draw-count draw-count)
          (core/failure :invalid-draw-count
                        "Redraw passes cannot exceed the six-card hand limit or available deck cards."
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
                    event {:type event-type
                           :player-id player-id
                           :source source-summary
                           :pass-index pass-index
                           :discarded-card-ids (vec discard-card-ids)
                           :draw-count draw-count
                           :drawn-card-ids (mapv :id drawn-cards)
                           :reshuffled-discard? (true? (:reshuffled? draw-result))}
                    next-state (-> (:state draw-result)
                                   (core/append-cards-to-hand player-id drawn-cards)
                                   (core/append-history event))]
                (core/success next-state [event])))))))))

(defn apply-high-priestess-move-with-source-card-id
  ([state command source-card-id]
   (apply-high-priestess-move-with-source-card-id state command source-card-id {}))
  ([state command source-card-id {:keys [source-opts]}]
   (let [source-result (resolve-specific-major-source
                        state
                        command
                        source-card-id
                        :high-priestess-actions-unavailable
                        "Only High Priestess can apply redraw passes."
                        source-opts)
         redraws-result (normalize-action-list command :redraws "High Priestess" 2)
         shuffle-fn (or (:shuffle-fn command) shuffle)]
     (cond
       (not (:ok? source-result))
       source-result

       (not (:ok? redraws-result))
       redraws-result

       (not (ifn? shuffle-fn))
       (core/failure :invalid-shuffle-fn
                     "High Priestess redraws require a callable shuffle function."
                     {:shuffle-fn shuffle-fn})

       :else
       (let [player-id (:player-id command)
             source-summary (core/source-summary (:source source-result))
             cost-state (major/charge-source-once state source-result)]
         (loop [current-state cost-state
                pass-index 1
                remaining (:actions redraws-result)
                events []]
           (if-let [pass (first remaining)]
             (let [pass-result (apply-redraw-pass current-state
                                                  player-id
                                                  source-summary
                                                  pass-index
                                                  pass
                                                  shuffle-fn
                                                  :high-priestess/redrawn)]
               (if-not (:ok? pass-result)
                 pass-result
                 (recur (:state pass-result)
                        (inc pass-index)
                        (rest remaining)
                        (into events (:events pass-result)))))
             (core/success current-state events))))))))

(defn apply-high-priestess-move [state command]
  (apply-high-priestess-move-with-source-card-id state
                                                 command
                                                 "high-priestess"))

(defn- remove-cards-from-discard [state card-ids]
  (let [card-id-set (set card-ids)]
    (update state :discard-pile
            (fn [discard-pile]
              (vec (remove #(contains? card-id-set (:id %)) discard-pile))))))

(defn- judgement-card-ids [command]
  (or (:card-ids command)
      (:discard-card-ids command)
      []))

(defn- judgement-minion-id [command]
  (or (:piece-id command)
      (get-in command [:source :piece-id])))

(defn- validate-major-minion [state source-result piece-id]
  (let [player-id (:player-id source-result)
        minion-ids (set (:minion-ids source-result))
        piece (when piece-id
                (core/piece-by-id state piece-id))]
    (cond
      (nil? piece-id)
      (core/failure :missing-major-minion
                    "Judgement requires an active minion."
                    {:source (core/source-summary (:source source-result))})

      (not (contains? minion-ids piece-id))
      (core/failure :invalid-major-minion
                    "The acting piece is not a minion for Judgement."
                    {:piece-id piece-id
                     :available-minion-ids (vec (sort-by str minion-ids))})

      (nil? piece)
      (core/failure :invalid-piece
                    "Judgement requires a minion that is still on the board."
                    {:piece-id piece-id})

      (not= player-id (:player-id piece))
      (core/failure :invalid-piece
                    "Judgement minions must belong to the move's player."
                    {:piece-id piece-id
                     :player-id player-id
                     :piece-player-id (:player-id piece)})

      :else
      {:ok? true
       :piece piece
       :piece-id piece-id})))

(defn apply-judgement-move-with-source-card-id
  ([state command source-card-id]
   (apply-judgement-move-with-source-card-id state command source-card-id {}))
  ([state command source-card-id {:keys [source-opts]}]
   (let [source-result (resolve-specific-major-source
                        state
                        command
                        source-card-id
                        :judgement-actions-unavailable
                        "Only Judgement can draw cards from the discard pile."
                        source-opts)
         card-ids (judgement-card-ids command)]
     (if-not (:ok? source-result)
       source-result
       (let [minion-result (validate-major-minion state
                                                  source-result
                                                  (judgement-minion-id command))]
         (cond
           (not (:ok? minion-result))
           minion-result

           (not (sequential? card-ids))
           (core/failure :invalid-judgement-cards
                         "Judgement requires selected discard card ids as a sequential collection."
                         {:card-ids card-ids})

           (seq (core/duplicate-values card-ids))
           (core/failure :duplicate-judgement-cards
                         "Judgement can draw each discard-pile card at most once."
                         {:duplicate-card-ids (core/duplicate-values card-ids)})

           :else
           (let [player-id (:player-id command)
                 cost-state (major/charge-source-once state source-result)
                 discard-by-id (into {} (map (juxt :id identity)) (:discard-pile cost-state))
                 missing-card-ids (vec (remove #(contains? discard-by-id %) card-ids))
                 cards-to-draw (mapv discard-by-id card-ids)
                 hand-count (count (get-in cost-state [:players-by-id player-id :hand]))
                 hand-slots (max 0 (- core/starting-hand-size hand-count))
                 minion-pips (or (pieces/pips (:piece minion-result)) 0)
                 maximum (min minion-pips hand-slots)]
             (cond
               (seq missing-card-ids)
               (core/failure :invalid-judgement-cards
                             "Judgement can only draw selected cards from the discard pile."
                             {:missing-card-ids missing-card-ids})

               (< maximum (count cards-to-draw))
               (core/failure :invalid-judgement-card-count
                             "Judgement cannot draw more cards than the minion's pips or the hand limit allow."
                             {:selected-count (count cards-to-draw)
                              :maximum maximum
                              :minion-pips minion-pips
                              :hand-size hand-count
                              :hand-limit core/starting-hand-size})

               :else
               (let [event {:type :judgement/cards-drawn
                            :player-id player-id
                            :source (core/source-summary (:source source-result))
                            :piece-id (:piece-id minion-result)
                            :card-ids (vec card-ids)
                            :draw-count (count cards-to-draw)
                            :maximum maximum}
                     next-state (-> cost-state
                                    (remove-cards-from-discard card-ids)
                                    (core/append-cards-to-hand player-id cards-to-draw)
                                    (core/append-history event))]
                 (core/success next-state [event]))))))))))

(defn apply-judgement-move [state command]
  (apply-judgement-move-with-source-card-id state command "judgement"))

(def ^:private fool-suit-powers #{:cup :rod :disc :sword})

(defn- card-power-key [card]
  (keyword (:id card)))

(defn- selected-card-power? [card power]
  (and (= :major (:arcana card))
       (or (nil? power)
           (= power (card-power-key card))
           (= power (:id card)))))

(defn- fool-play-command [player-id card action]
  (let [play-command (or (:play-command action)
                         (:command action))
        piece-id (or (:piece-id action)
                     (get-in play-command [:source :piece-id]))
        source (assoc (select-keys (:source play-command) [:piece-id])
                      :kind :hand-card
                      :card-id (:id card)
                      :piece-id piece-id)]
    (assoc play-command
           :player-id player-id
           :source source)))

(defn- apply-fool-suit-play [state power command source-opts]
  (case power
    :cup
    (cup/apply-cup-move-with-opts state
                                  command
                                  {:source-opts source-opts})

    :rod
    (rod/apply-rod-move-with-opts state
                                  command
                                  {:source-opts source-opts})

    :disc
    (disc-major/apply-disc-move-with-opts state
                                          command
                                          {:source-opts source-opts
                                           :charge-source? false})

    :sword
    (sword-major/apply-sword-move-with-opts state
                                            command
                                            {:source-opts source-opts
                                             :charge-source? false})))

(defn- apply-fool-play [state player-id card action shuffle-fn]
  (let [play-command (or (:play-command action)
                         (:command action))
        power (or (:power action)
                  (:power play-command))
        source-opts {:source-card card
                     :source-card-already-discarded? true}]
    (cond
      (not (map? play-command))
      (core/failure :invalid-fool-play-command
                    "Fool reveal play commands require a command map."
                    {:action action})

      (and power
           (not (contains? fool-suit-powers power))
           (not (selected-card-power? card power)))
      (core/failure :invalid-fool-play-power
                    "Fool can route revealed cards through an implemented suit or full-card major power."
                    {:power power
                     :supported-powers (cond-> fool-suit-powers
                                         (= :major (:arcana card))
                                         (conj (card-power-key card)))
                     :card-id (:id card)})

      :else
      (let [command (cond-> (fool-play-command player-id card action)
                      (and shuffle-fn
                           (not (contains? play-command :shuffle-fn)))
                      (assoc :shuffle-fn shuffle-fn))]
        (if (contains? fool-suit-powers power)
          (apply-fool-suit-play state power command source-opts)
          (major-power/apply-card-power state
                                        command
                                        card
                                        {:source-opts source-opts
                                         :shuffle-fn shuffle-fn}))))))

(defn- apply-fool-reveal
  [state player-id source-summary reveal-index action shuffle-fn]
  (cond
    (not (map? action))
    (core/failure :invalid-fool-reveal
                  "Fool reveal actions require command maps."
                  {:reveal-index reveal-index
                   :action action})

    :else
    (let [refresh-result (core/refresh-draw-pile state shuffle-fn)]
      (if-not (:ok? refresh-result)
        refresh-result
        (let [refreshed-state (:state refresh-result)
              revealed-card (first (:draw-pile refreshed-state))]
          (if-not revealed-card
            (core/failure :draw-pile-empty
                          "Fool requires a card in the draw pile to reveal."
                          {:reveal-index reveal-index})
            (let [play? (or (contains? action :play-command)
                            (contains? action :command))
                  event {:type :fool/card-revealed
                         :player-id player-id
                         :source source-summary
                         :reveal-index reveal-index
                         :card-id (:id revealed-card)
                         :played? (true? play?)
                         :reshuffled-discard? (true? (:reshuffled? refresh-result))}
                  reveal-state (-> refreshed-state
                                   (assoc :draw-pile (vec (rest (:draw-pile refreshed-state))))
                                   (core/discard-card revealed-card)
                                   (core/append-history event))]
              (if-not play?
                (core/success reveal-state [event])
                (let [play-result (apply-fool-play reveal-state
                                                   player-id
                                                   revealed-card
                                                   action
                                                   shuffle-fn)]
                  (if-not (:ok? play-result)
                    play-result
                    (core/success (:state play-result)
                                  (concat [event]
                                          (:events play-result)))))))))))))

(defn apply-fool-move-with-source-card-id
  ([state command source-card-id]
   (apply-fool-move-with-source-card-id state command source-card-id {}))
  ([state command source-card-id {:keys [source-opts]}]
   (let [source-result (resolve-specific-major-source
                        state
                        command
                        source-card-id
                        :fool-actions-unavailable
                        "Only Fool can reveal and play draw-pile cards."
                        source-opts)
         reveals-result (normalize-action-list command :reveals "Fool" 2)
         shuffle-fn (or (:shuffle-fn command) shuffle)]
     (cond
       (not (:ok? source-result))
       source-result

       (not (:ok? reveals-result))
       reveals-result

       (not (ifn? shuffle-fn))
       (core/failure :invalid-shuffle-fn
                     "Fool reveals require a callable shuffle function."
                     {:shuffle-fn shuffle-fn})

       :else
       (let [player-id (:player-id command)
             source-summary (core/source-summary (:source source-result))
             cost-state (major/charge-source-once state source-result)]
         (loop [current-state cost-state
                reveal-index 1
                remaining (:actions reveals-result)
                events []]
           (if-let [action (first remaining)]
             (let [reveal-result (apply-fool-reveal current-state
                                                    player-id
                                                    source-summary
                                                    reveal-index
                                                    action
                                                    shuffle-fn)]
               (if-not (:ok? reveal-result)
                 reveal-result
                 (recur (:state reveal-result)
                        (inc reveal-index)
                        (rest remaining)
                        (into events (:events reveal-result)))))
             (core/success current-state events))))))))

(defn apply-fool-move [state command]
  (apply-fool-move-with-source-card-id state command "fool"))

(defmethod major-power/apply-card-power "fool"
  [state command _card {:keys [source-opts]}]
  (apply-fool-move-with-source-card-id state command "fool" {:source-opts source-opts}))

(defmethod major-power/apply-card-power "high-priestess"
  [state command _card {:keys [source-opts shuffle-fn]}]
  (apply-high-priestess-move-with-source-card-id
   state
   (cond-> command
     (and shuffle-fn (not (contains? command :shuffle-fn)))
     (assoc :shuffle-fn shuffle-fn))
   "high-priestess"
   {:source-opts source-opts}))

(defmethod major-power/apply-card-power "judgement"
  [state command _card {:keys [source-opts]}]
  (apply-judgement-move-with-source-card-id state
                                            command
                                            "judgement"
                                            {:source-opts source-opts}))

(defmethod major-power/apply-card-power "wheeloffortune"
  [state command _card {:keys [source-opts]}]
  (cup/apply-cup-move-with-opts state
                                (assoc command :cup-variant :wheel-cup)
                                {:source-opts source-opts}))
