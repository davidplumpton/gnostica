(ns gnostica.game-state.major
  (:require [gnostica.game-state.core :as core]))

(defn- major-card? [card]
  (= :major (:arcana card)))

(defn- piece-ids [pieces]
  (mapv :id pieces))

(defn- owned-pieces-at-cell [state player-id cell]
  (filterv #(= player-id (:player-id %))
           (core/pieces-at-coordinate state (:row cell) (:col cell))))

(defn- source-summary [source-result]
  (core/source-summary (:source source-result)))

(defn- discard-pile-card [state card-id]
  (some (fn [card]
          (when (= card-id (:id card))
            card))
        (:discard-pile state)))

(defn- no-minions-failure [player-id source]
  (core/failure :no-major-minions
                "Major arcana sources require at least one current-player minion."
                {:player-id player-id
                 :source source}))

(defn- resolve-hand-source
  ([state player-id source]
   (resolve-hand-source state player-id source {}))
  ([state player-id source {:keys [source-card source-card-already-discarded?]}]
   (let [hand-card (core/player-hand-card state player-id (:card-id source))
         discard-card (when source-card-already-discarded?
                        (discard-pile-card state (:card-id source)))
         card (cond
                source-card-already-discarded? (or discard-card source-card)
                source-card source-card
                :else hand-card)
         minions (core/player-pieces state player-id)]
    (cond
      (and source-card
           (not= (:card-id source) (:id source-card)))
      (core/failure :invalid-hand-card
                    "Major paid source cards must match the command source card."
                    {:card-id (:card-id source)
                     :source-card-id (:id source-card)})

      (and source-card
           (not source-card-already-discarded?)
           (nil? hand-card))
      (core/failure :invalid-hand-card
                    "Major hand-card sources must reference a card in the player's hand."
                    {:card-id (:card-id source)
                     :player-id player-id})

      (and source-card-already-discarded?
           (nil? discard-card))
      (core/failure :invalid-hand-card
                    "Already-discarded major hand-card sources must reference a card in the discard pile."
                    {:card-id (:card-id source)
                     :player-id player-id})

      (nil? card)
      (core/failure :invalid-hand-card
                    "Major hand-card sources must reference a card in the player's hand."
                    {:card-id (:card-id source)
                     :player-id player-id})

      (not (major-card? card))
      (core/failure :source-card-not-major
                    "Major sequencing requires a major arcana source card."
                    {:card-id (:id card)
                     :source source})

      (empty? minions)
      (no-minions-failure player-id source)

      :else
      {:ok? true
       :player-id player-id
       :source source
       :source-kind :hand-card
       :source-card card
       :source-card-already-discarded? (true? source-card-already-discarded?)
       :discard-source-card? (not source-card-already-discarded?)
       :minion-ids (piece-ids minions)}))))

(defn- resolve-territory-source
  ([state player-id source]
   (resolve-territory-source state player-id source {}))
  ([state player-id source {:keys [source-card]}]
  (let [cell (core/board-cell-by-index state (:board-index source))
        card (or source-card (:card cell))
        minions (when cell
                  (owned-pieces-at-cell state player-id cell))]
    (cond
      (nil? cell)
      (core/failure :invalid-source-territory
                    "Major territory sources must reference an existing board cell."
                    {:board-index (:board-index source)})

      (and source-card
           (not= (get-in cell [:card :id]) (:id source-card)))
      (core/failure :invalid-source-territory
                    "Major paid source cards must match the command source territory."
                    {:board-index (:board-index source)
                     :territory-card-id (get-in cell [:card :id])
                     :source-card-id (:id source-card)})

      (not (major-card? card))
      (core/failure :source-card-not-major
                    "Major sequencing requires a major arcana source card."
                    {:card-id (:id card)
                     :source source})

      (empty? minions)
      (no-minions-failure player-id source)

      :else
      {:ok? true
       :player-id player-id
       :source source
       :source-kind :territory
       :source-card card
       :source-cell cell
       :minion-ids (piece-ids minions)}))))

(defn resolve-major-source
  ([state command]
   (resolve-major-source state command {}))
  ([state command source-opts]
  (let [{:keys [player-id source]} command]
    (cond
      (not (map? command))
      (core/failure :invalid-major-command
                    "Major sequencing requires a command map."
                    {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (core/failure :unknown-player
                    "Major sequencing requires a participating player."
                    {:player-id player-id})

      (not (core/current-player-id? state player-id))
      (core/failure :not-current-player
                    "Only the current player can use a major arcana sequence."
                    {:player-id player-id
                     :current-player-id (get-in state [:turn :current-player-id])})

      (not (map? source))
      (core/failure :invalid-major-source
                    "Major sequencing requires a source map."
                    {:source source})

      (= :hand-card (:kind source))
      (resolve-hand-source state player-id source source-opts)

      (= :territory (:kind source))
      (resolve-territory-source state player-id source source-opts)

      :else
      (core/failure :invalid-major-source
                    "Major sources must be either :hand-card or :territory."
                    {:source source})))))

(defn charge-source-once [state source-result]
  (core/apply-source-cost state
                          (:player-id source-result)
                          source-result))

(defn paid-source-opts [source-result]
  {:source-card (:source-card source-result)
   :source-card-already-discarded? (= :hand-card (:source-kind source-result))})

(defn- sequence-source-opts [source-result spec]
  (cond-> (paid-source-opts source-result)
    (:power-card spec)
    (assoc :power-card (:power-card spec))))

(defn action-source [source-result piece-id]
  (assoc (:source source-result) :piece-id piece-id))

(defn- normalize-actions [actions]
  (cond
    (nil? actions)
    {:ok? true
     :actions []}

    (sequential? actions)
    {:ok? true
     :actions (vec actions)}

    :else
    (core/failure :invalid-major-actions
                  "Major sequence actions must be a sequential collection."
                  {:actions actions})))

(defn- power-order [spec]
  (or (:power-order spec)
      (:powers spec)))

(defn- validate-power-order [powers]
  (cond
    (not (sequential? powers))
    (core/failure :invalid-major-power-order
                  "Major sequence specs require a sequential :power-order."
                  {:power-order powers})

    (empty? powers)
    (core/failure :invalid-major-power-order
                  "Major sequence specs require at least one power."
                  {:power-order powers})

    (not-every? keyword? powers)
    (core/failure :invalid-major-power-order
                  "Major sequence power ids must be keywords."
                  {:power-order powers})

    :else
    {:ok? true
     :power-order (vec powers)}))

(defn- action-power [action]
  (:power action))

(defn- match-action-order [powers actions]
  (loop [power-index 0
         remaining-actions (seq actions)
         matched []]
    (if-not remaining-actions
      {:ok? true
       :actions (vec matched)}
      (let [action (first remaining-actions)
            power (action-power action)]
        (cond
          (not (map? action))
          (core/failure :invalid-major-action
                        "Major sequence actions must be maps."
                        {:action action})

          (not (keyword? power))
          (core/failure :invalid-major-action
                        "Major sequence actions require a keyword :power."
                        {:action action})

          :else
          (if-let [matched-index (first
                                  (keep-indexed
                                   (fn [offset candidate]
                                     (when (= power candidate)
                                       (+ power-index offset)))
                                   (subvec powers power-index)))]
            (recur (inc matched-index)
                   (next remaining-actions)
                   (conj matched
                         (assoc action :major/power-index matched-index)))
            (core/failure :invalid-major-action-order
                          "Major sequence actions must follow the card's fixed power order."
                          {:power power
                           :remaining-power-order (subvec powers power-index)
                           :actions actions})))))))

(defn- allowed-card-ids [spec]
  (cond
    (contains? spec :card-id)
    #{(:card-id spec)}

    (contains? spec :card-ids)
    (set (:card-ids spec))

    :else
    nil))

(defn- validate-source-card [source-result spec]
  (if-let [card-ids (allowed-card-ids spec)]
    (if (contains? card-ids (get-in source-result [:source-card :id]))
      {:ok? true}
      (core/failure :major-card-unavailable
                    "The major sequence spec does not apply to the source card."
                    {:card-id (get-in source-result [:source-card :id])
                     :allowed-card-ids card-ids
                     :source (source-summary source-result)}))
    {:ok? true}))

(defn- action-minion-id [spec action]
  (if-let [f (:action-minion-id-fn spec)]
    (f action)
    (or (:piece-id action)
        (:minion-id action)
        (get-in action [:source :piece-id]))))

(defn- validate-action-minion [state context action]
  (let [player-id (:player-id context)
        piece-id (action-minion-id (:spec context) action)
        minion-ids (:minion-ids context)
        piece (when piece-id
                (core/piece-by-id state piece-id))]
    (cond
      (nil? piece-id)
      (core/failure :missing-major-minion
                    "Major sequence actions require an acting minion."
                    {:action action})

      (not (contains? minion-ids piece-id))
      (core/failure :invalid-major-minion
                    "The acting piece is not a minion for this major sequence."
                    {:piece-id piece-id
                     :available-minion-ids (vec (sort-by str minion-ids))
                     :action action})

      (nil? piece)
      (core/failure :invalid-piece
                    "The acting major-sequence minion must still be on the board."
                    {:piece-id piece-id})

      (not= player-id (:player-id piece))
      (core/failure :invalid-piece
                    "Major-sequence minions must belong to the move's player."
                    {:piece-id piece-id
                     :player-id player-id
                     :piece-player-id (:player-id piece)})

      :else
      {:ok? true
       :piece piece
       :piece-id piece-id})))

(defn- result-affected-piece-ids [result]
  (vec
   (concat (:affected-piece-ids result)
           (map :id (:affected-pieces result)))))

(defn- add-affected-minions [context state result]
  (let [player-id (:player-id context)
        owned-affected-piece-ids
        (->> (result-affected-piece-ids result)
             (keep #(core/piece-by-id state %))
             (filter #(= player-id (:player-id %)))
             (map :id))]
    (update context :minion-ids into owned-affected-piece-ids)))

(defn- shortcut-key [spec action]
  (when-let [f (:shortcut-key-fn spec)]
    (f action)))

(defn- shortcut-result [state context left-action right-action]
  (let [spec (:spec context)
        left-key (shortcut-key spec left-action)
        right-key (shortcut-key spec right-action)]
    (when (and (:shortcut-fn spec)
               (some? left-key)
               (= left-key right-key))
      ((:shortcut-fn spec) state context left-action right-action))))

(defn- apply-one-action [state context action]
  (let [minion-result (validate-action-minion state context action)]
    (if-not (:ok? minion-result)
      minion-result
      ((:apply-action-fn (:spec context))
       state
       (assoc context
              :action-source (action-source context (:piece-id minion-result))
              :action-minion (:piece minion-result))
       action))))

(defn- apply-sequence-actions [state context actions]
  (loop [current-state state
         current-context context
         remaining (seq actions)
         events []]
    (if-not remaining
      (core/success current-state events)
      (let [left-action (first remaining)
            right-action (second remaining)
            minion-result (validate-action-minion current-state
                                                  current-context
                                                  left-action)]
        (if-not (:ok? minion-result)
          minion-result
          (let [shortcut (when right-action
                           (shortcut-result
                            current-state
                            (assoc current-context
                                   :action-source (action-source current-context
                                                                 (:piece-id minion-result))
                                   :action-minion (:piece minion-result))
                            left-action
                            right-action))
                applied (or shortcut
                            (apply-one-action current-state
                                              current-context
                                              left-action))]
            (if-not (:ok? applied)
              applied
              (let [next-state (:state applied)
                    next-context (add-affected-minions current-context
                                                       next-state
                                                       applied)
                    consumed (if shortcut 2 1)]
                (recur next-state
                       next-context
                       (seq (drop consumed remaining))
                       (into events (:events applied)))))))))))

(defn apply-major-sequence [state command spec]
  (let [source-result (resolve-major-source state command (:source-opts spec))]
    (if-not (:ok? source-result)
      source-result
      (let [source-card-result (validate-source-card source-result spec)
            power-order-result (validate-power-order (power-order spec))
            actions-result (normalize-actions (:actions command))]
        (cond
          (not (:ok? source-card-result))
          source-card-result

          (not (:ok? power-order-result))
          power-order-result

          (not (:ok? actions-result))
          actions-result

          (nil? (:apply-action-fn spec))
          (core/failure :invalid-major-sequence-spec
                        "Major sequence specs require an :apply-action-fn."
                        {:spec-keys (vec (keys spec))})

          :else
          (let [order-result (match-action-order (:power-order power-order-result)
                                                 (:actions actions-result))]
            (if-not (:ok? order-result)
              order-result
              (let [cost-state (charge-source-once state source-result)
                    context (-> source-result
                                (assoc :source-opts (sequence-source-opts
                                                     source-result
                                                     spec)
                                       :source-charged? true
                                       :spec spec)
                                (update :minion-ids set))]
                (apply-sequence-actions cost-state
                                        context
                                        (:actions order-result))))))))))
