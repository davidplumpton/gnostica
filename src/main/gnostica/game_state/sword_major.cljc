(ns gnostica.game-state.sword-major
  (:require [gnostica.cards :as cards]
            [gnostica.game-state.core :as core]
            [gnostica.game-state.major :as major]
            [gnostica.game-state.major-power :as major-power]
            [gnostica.game-state.placement :as placement]
            [gnostica.game-state.rod :as rod]
            [gnostica.game-state.spatial :as spatial]
            [gnostica.game-state.sword :as sword]
            [gnostica.pieces :as pieces]))

(def ^:private sword-piece-size-ranks sword/sword-piece-size-ranks)

(defn- resolve-sword-source-command
  ([state command]
   (resolve-sword-source-command state command {}))
  ([state command source-opts]
   (sword/resolve-sword-source-command state command source-opts)))

(defn- resolve-sword-command* [state command source-opts]
  (sword/resolve-sword-command* state command source-opts))

(defn- apply-resolved-sword-action
  ([state player-id result]
   (apply-resolved-sword-action state player-id result {}))
  ([state player-id result opts]
   (sword/apply-resolved-sword-action state player-id result opts)))

(defn- apply-single-sword-move
  ([state command]
   (apply-single-sword-move state command {}))
  ([state command opts]
   (sword/apply-single-sword-move state command opts)))

(defn- source-card-id [source-result]
  (sword/source-card-id source-result))

(defn- source-power-card-id [source-result]
  (sword/source-power-card-id source-result))

(defn- paid-sword-source-opts
  ([source-result]
   (sword/paid-sword-source-opts source-result))
  ([source-result power-card]
   (sword/paid-sword-source-opts source-result power-card)))

(defn- sword-target-coordinate [coordinate orientation]
  (sword/sword-target-coordinate coordinate orientation))

(defn- sword-targetable-coordinate?
  [actor-coordinate target-coordinate orientation target-self?]
  (or target-self?
      (spatial/same-coordinate? target-coordinate
                                (sword-target-coordinate actor-coordinate orientation))))

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

(defn- action-piece-id [command action]
  (or (:piece-id action)
      (get-in command [:source :piece-id])))

(defn- sword-action-command [command source-result action sword-variant]
  (let [piece-id (action-piece-id command action)]
    (merge {:player-id (:player-id command)
            :source (major/action-source source-result piece-id)
            :sword-variant sword-variant}
           (dissoc action :piece-id :power))))

(defn- rod-action-command [command source-result action]
  (let [piece-id (action-piece-id command action)]
    (merge {:player-id (:player-id command)
            :source (major/action-source source-result piece-id)
            :rod-variant :rod}
           (dissoc action :piece-id :power))))

(defn- validate-major-action-minion [state player-id allowed-minion-ids command action]
  (let [piece-id (action-piece-id command action)
        piece (when piece-id
                (core/piece-by-id state piece-id))]
    (cond
      (nil? piece-id)
      (core/failure :missing-major-minion
                    "Major Sword sequences require an acting minion for each action."
                    {:action action})

      (not (contains? allowed-minion-ids piece-id))
      (core/failure :invalid-major-minion
                    "The acting piece is not a minion for this major Sword sequence."
                    {:piece-id piece-id
                     :available-minion-ids (vec (sort-by str allowed-minion-ids))
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

(defn- owned-event-piece-ids [player-id events]
  (->> events
       (keep (fn [event]
               (let [piece (:piece event)]
                 (when (= player-id (:player-id piece))
                   (:id piece)))))
       vec))

(defn- add-owned-event-minions [allowed-minion-ids player-id events]
  (into allowed-minion-ids (owned-event-piece-ids player-id events)))

(defn- normalize-action-list [command key label maximum]
  (let [actions (get command key)]
    (cond
      (not (sequential? actions))
      (core/failure :invalid-major-actions
                    (str label " requires a sequential " (name key) " collection.")
                    {key actions})

      (not (<= 1 (count actions) maximum))
      (core/failure :invalid-major-actions
                    (str label " can apply one or " maximum " actions.")
                    {:action-count (count actions)
                     :maximum maximum})

      :else
      {:ok? true
       :actions (mapv #(dissoc % :power) actions)})))

(defn- same-sword-target? [left-result right-result]
  (let [left (get-in left-result [:command :target])
        right (get-in right-result [:command :target])]
    (and (= (:kind left) (:kind right))
         (case (:kind left)
           :piece (= (:piece-id left) (:piece-id right))
           :territory (= (:board-index left) (:board-index right))
           false))))

(defn- same-action-target? [left-action right-action]
  (let [left (:target left-action)
        right (:target right-action)]
    (and (= (:kind left) (:kind right))
         (case (:kind left)
           :piece (= (:piece-id left) (:piece-id right))
           :territory (or (and (some? (:board-index left))
                               (= (:board-index left) (:board-index right)))
                          (and (int? (:row left))
                               (int? (:col left))
                               (= (:row left) (:row right))
                               (= (:col left) (:col right))))
           false))))

(defn- sword-target-pips [result]
  (case (get-in result [:command :target :kind])
    :piece (get sword-piece-size-ranks (get-in result [:target-piece :size]))
    :territory (cards/card-point-value (get-in result [:target-cell :card]))
    nil))

(defn- death-shortcut-result [left-result right-result]
  (let [left-command (:command left-result)
        right-command (:command right-result)
        target-kind (get-in left-command [:target :kind])
        total-damage (+ (:damage left-command) (:damage right-command))
        target-pips (sword-target-pips left-result)
        destroyed? (= total-damage target-pips)
        orientation (or (:orientation right-command)
                        (:orientation left-command))]
    (cond
      (nil? target-pips)
      (core/failure :invalid-sword-target
                    "Death shortcuts require a piece or territory target with pips."
                    {:target (get left-command :target)})

      (< target-pips total-damage)
      (core/failure :invalid-sword-damage
                    "Death shortcut damage cannot reduce the target below zero pips."
                    {:damage total-damage
                     :target-pips target-pips
                     :target (:target left-command)})

      (and (= :piece target-kind)
           destroyed?
           (some? orientation))
      (core/failure :invalid-orientation
                    "Destroyed pieces cannot be reoriented by a Death shortcut."
                    {:orientation orientation
                     :target (:target left-command)})

      :else
      (let [replacement-card-source (or (:replacement-card-source right-command)
                                        (:replacement-card-source left-command))
            replacement-card-id (or (:replacement-card-id right-command)
                                    (:replacement-card-id left-command))
            command (cond-> (assoc left-command
                                   :damage total-damage
                                   :target (:target left-command))
                      (and (= :piece target-kind) orientation)
                      (-> (assoc :orientation orientation)
                          (assoc-in [:target :orientation] orientation))

                      (and (= :territory target-kind) (not destroyed?)
                           replacement-card-source)
                      (assoc :replacement-card-source replacement-card-source)

                      (and (= :territory target-kind) (not destroyed?)
                           replacement-card-id)
                      (assoc :replacement-card-id replacement-card-id)

                      (and (= :territory target-kind) destroyed?)
                      (dissoc :replacement-card-source :replacement-card-id))]
        (assoc left-result
               :command command
               :destroyed? destroyed?)))))

(defn- apply-death-shortcut [state player-id left-result right-result]
  (let [shortcut-result (death-shortcut-result left-result right-result)]
    (if-not (:ok? shortcut-result)
      shortcut-result
      (apply-resolved-sword-action state
                                   player-id
                                   shortcut-result
                                   {:charge-source? false
                                    :action-count 2
                                    :shortcut? true}))))

(defn- apply-death-actions-sequential
  [state command source-result actions source-opts]
  (let [player-id (:player-id command)]
    (loop [current-state state
           allowed-minion-ids (set (:minion-ids source-result))
           remaining actions
           events []]
      (if-let [action (first remaining)]
        (let [minion-result (validate-major-action-minion current-state
                                                          player-id
                                                          allowed-minion-ids
                                                          command
                                                          action)]
          (if-not (:ok? minion-result)
            minion-result
            (let [action-command (sword-action-command command
                                                       source-result
                                                       action
                                                       :sword)
                  result (apply-single-sword-move current-state
                                                  action-command
                                                  {:source-opts source-opts
                                                   :charge-source? false})]
              (if-not (:ok? result)
                result
                (recur (:state result)
                       (add-owned-event-minions allowed-minion-ids
                                                player-id
                                                (:events result))
                       (rest remaining)
                       (into events (:events result)))))))
        (core/success current-state events)))))

(defn- apply-death-sword-move
  ([state command]
   (apply-death-sword-move state command {}))
  ([state command {:keys [required-card-id power-card source-opts]
                   :or {required-card-id "death"}}]
   (let [source-result (resolve-specific-major-source
                        state
                        command
                        required-card-id
                        :death-actions-unavailable
                        "Only Death can apply multiple Sword actions."
                        source-opts)
         effective-card (or power-card (:source-card source-result))]
     (if-not (:ok? source-result)
       source-result
       (let [actions-result (normalize-action-list command :sword-actions "Death" 2)]
         (cond
           (not= "death" (:id effective-card))
           (core/failure :death-actions-unavailable
                         "Only Death can apply multiple Sword actions."
                         {:card-id (:id effective-card)
                          :source (core/source-summary (:source source-result))})

           (not (:ok? actions-result))
           actions-result

           :else
           (let [player-id (:player-id command)
                 cost-state (major/charge-source-once state source-result)
                 source-opts (paid-sword-source-opts source-result power-card)
                 actions (:actions actions-result)]
             (if (= 2 (count actions))
               (let [[left-action right-action] actions
                     shortcut? (same-action-target? left-action right-action)]
                 (if-not shortcut?
                   (apply-death-actions-sequential cost-state
                                                   command
                                                   source-result
                                                   actions
                                                   source-opts)
                   (let [initial-minion-ids (set (:minion-ids source-result))
                         left-minion (validate-major-action-minion cost-state
                                                                   player-id
                                                                   initial-minion-ids
                                                                   command
                                                                   left-action)
                         right-minion (validate-major-action-minion cost-state
                                                                    player-id
                                                                    initial-minion-ids
                                                                    command
                                                                    right-action)
                         left-command (sword-action-command command
                                                            source-result
                                                            left-action
                                                            :sword)
                         right-command (sword-action-command command
                                                             source-result
                                                             right-action
                                                             :sword)
                         left-result (when (:ok? left-minion)
                                       (resolve-sword-command* cost-state
                                                               left-command
                                                               source-opts))
                         right-result (when (and (:ok? left-minion)
                                                 (:ok? right-minion)
                                                 (:ok? left-result))
                                        (resolve-sword-command* cost-state
                                                                right-command
                                                                source-opts))]
                     (cond
                       (not (:ok? left-minion))
                       left-minion

                       (not (:ok? right-minion))
                       right-minion

                       (not (:ok? left-result))
                       left-result

                       (not (:ok? right-result))
                       right-result

                       (not (same-sword-target? left-result right-result))
                       (core/failure :invalid-death-shortcut
                                     "Death shortcuts require both Sword actions to affect the same target."
                                     {:left-target (:target left-action)
                                      :right-target (:target right-action)})

                       :else
                       (apply-death-shortcut cost-state
                                             player-id
                                             left-result
                                             right-result)))))
               (apply-death-actions-sequential cost-state
                                               command
                                               source-result
                                               actions
                                               source-opts)))))))))

(defn- hand-trade-target-command [command]
  (or (:hand-trade-target command)
      (when-let [piece-id (:hand-trade-target-piece-id command)]
        {:kind :piece
         :piece-id piece-id})))

(defn- resolve-hand-trade-target [state source-result target]
  (let [player-id (:player-id source-result)
        source-piece (:piece source-result)]
    (cond
      (not (map? target))
      (core/failure :invalid-hand-trade-target
                    "Justice hand trades require a target piece map."
                    {:target target})

      (not= :piece (:kind target))
      (core/failure :invalid-hand-trade-target
                    "Justice hand trades target :kind :piece."
                    {:target target})

      (nil? (:piece-id target))
      (core/failure :invalid-hand-trade-target
                    "Justice hand trades require a target piece id."
                    {:target target})

      :else
      (if-let [target-piece (core/piece-by-id state (:piece-id target))]
        (let [target-coordinate (spatial/coordinate-map (core/piece-coordinate state target-piece))
              target-self? (= (:id source-piece) (:id target-piece))]
          (cond
            (nil? target-coordinate)
            (core/failure :invalid-piece-space
                          "Justice hand-trade targets must have a board coordinate."
                          {:piece-id (:id target-piece)})

            (not (sword-targetable-coordinate? (:piece-coordinate source-result)
                                               target-coordinate
                                               (:orientation source-result)
                                               target-self?))
            (core/failure :invalid-hand-trade-target
                          "Justice can trade only with the owner of a piece targeted by the minion."
                          {:piece-id (:id target-piece)
                           :orientation (:orientation source-result)
                           :source-coordinate (:piece-coordinate source-result)
                           :target-coordinate target-coordinate
                           :expected-coordinate (sword-target-coordinate
                                                 (:piece-coordinate source-result)
                                                 (:orientation source-result))})

            :else
            {:ok? true
             :target-piece target-piece
             :target {:kind :piece
                      :piece-id (:id target-piece)
                      :player-id (:player-id target-piece)
                      :row (:row target-coordinate)
                      :col (:col target-coordinate)}}))
        (core/failure :invalid-hand-trade-target
                      "Justice hand trades must reference a piece on the board."
                      {:target target
                       :player-id player-id})))))

(defn- card-ids [cards]
  (mapv :id cards))

(defn- swap-player-hands [state left-player-id right-player-id]
  (let [left-hand (get-in state [:players-by-id left-player-id :hand])
        right-hand (get-in state [:players-by-id right-player-id :hand])]
    (-> state
        (core/update-player left-player-id assoc :hand (vec right-hand))
        (core/update-player right-player-id assoc :hand (vec left-hand)))))

(defn- justice-sword-action-command? [command]
  (or (contains? command :target)
      (contains? command :damage)
      (contains? command :replacement-card-source)
      (contains? command :replacement-card-id)
      (contains? command :orientation)))

(defn- apply-justice-sword-move
  ([state command]
   (apply-justice-sword-move state command {}))
  ([state command source-opts]
   (let [source-result (resolve-sword-source-command state command source-opts)]
     (if-not (:ok? source-result)
       source-result
       (cond
         (not= "justice" (source-power-card-id source-result))
         (core/failure :justice-hand-trade-unavailable
                       "Only Justice can trade hands before applying Sword."
                       {:card-id (source-power-card-id source-result)
                        :source (core/source-summary (:source source-result))})

         :else
         (let [target-result (resolve-hand-trade-target state
                                                        source-result
                                                        (hand-trade-target-command
                                                         command))]
           (if-not (:ok? target-result)
             target-result
             (let [player-id (:player-id command)
                   other-player-id (get-in target-result [:target-piece :player-id])
                   cost-state (core/apply-source-cost
                               state
                               player-id
                               {:source-card (:source-card source-result)
                                :discard-source-card? (:discard-source-card?
                                                       source-result)})
                   player-hand (get-in cost-state [:players-by-id player-id :hand])
                   other-hand (get-in cost-state [:players-by-id other-player-id :hand])
                   event {:type :justice/hands-traded
                          :player-id player-id
                          :source (core/source-summary (:source source-result))
                          :target (:target target-result)
                          :with-player-id other-player-id
                          :player-hand-card-ids (card-ids player-hand)
                          :other-hand-card-ids (card-ids other-hand)}
                   trade-state (-> cost-state
                                   (swap-player-hands player-id other-player-id)
                                   (core/append-history event))
                   sword-command (dissoc command
                                         :hand-trade-target
                                         :hand-trade-target-piece-id)]
               (if-not (justice-sword-action-command? sword-command)
                 (core/success trade-state [event])
                 (let [sword-result (apply-single-sword-move
                                     trade-state
                                     sword-command
                                     {:source-opts (paid-sword-source-opts source-result)
                                      :charge-source? false})]
                   (if-not (:ok? sword-result)
                     sword-result
                     (core/success (:state sword-result)
                                   (concat [event] (:events sword-result))))))))))))))

(defn- apply-tower-sword-move
  ([state command]
   (apply-tower-sword-move state command {}))
  ([state command source-opts]
   (let [source-result (resolve-sword-source-command state command source-opts)]
     (if-not (:ok? source-result)
       source-result
       (if-not (= "tower" (source-power-card-id source-result))
         (core/failure :sword-orient-unavailable
                       "Only Tower Sword can orient a minion before applying Sword."
                       {:card-id (source-power-card-id source-result)
                        :source (core/source-summary (:source source-result))})
         (let [orient-result (placement/apply-orient-move
                              state
                              {:player-id (:player-id command)
                               :piece-id (get-in command [:source :piece-id])
                               :orientation (:minion-orientation command)})]
           (if-not (:ok? orient-result)
             orient-result
             (let [sword-result (apply-single-sword-move
                                 (:state orient-result)
                                 (dissoc command :minion-orientation)
                                 {:source-opts source-opts})]
               (if-not (:ok? sword-result)
                 sword-result
                 (core/success (:state sword-result)
                               (concat (:events orient-result)
                                       (:events sword-result))))))))))))

(defn- overfull-rod-destination [state events]
  (some (fn [event]
          (let [destination (:destination event)]
            (when (and (contains? #{:rod/minion-moved :rod/piece-pushed}
                                  (:type event))
                       (= :territory (:kind destination))
                       (< pieces/max-pieces-per-space
                          (count (core/pieces-at-board-index
                                  state
                                  (:board-index destination)))))
              destination)))
        events))

(defn- overfull-destination-resolved? [state destination]
  (<= (count (core/pieces-at-board-index state (:board-index destination)))
      pieces/max-pieces-per-space))

(defn- apply-moon-rod-action
  [state command source-result allowed-minion-ids action source-opts allow-full?]
  (let [player-id (:player-id command)
        minion-result (validate-major-action-minion state
                                                    player-id
                                                    allowed-minion-ids
                                                    command
                                                    action)]
    (if-not (:ok? minion-result)
      minion-result
      (rod/apply-rod-move-with-opts
       state
       (rod-action-command command source-result action)
       {:source-opts source-opts
        :allow-full-destination? allow-full?}))))

(defn- apply-moon-sword-action
  [state command source-result allowed-minion-ids action source-opts]
  (let [player-id (:player-id command)
        minion-result (validate-major-action-minion state
                                                    player-id
                                                    allowed-minion-ids
                                                    command
                                                    action)]
    (if-not (:ok? minion-result)
      minion-result
      (apply-single-sword-move
       state
       (sword-action-command command source-result action :sword)
       {:source-opts source-opts
        :charge-source? false}))))

(defn apply-moon-move-with-opts
  ([state command]
   (apply-moon-move-with-opts state command {}))
  ([state command {:keys [required-card-id power-card source-opts]
                   :or {required-card-id "moon"}}]
   (let [source-result (resolve-specific-major-source
                        state
                        command
                        required-card-id
                        :moon-actions-unavailable
                        "Only Moon can apply Rod followed by Sword."
                        source-opts)
         effective-card (or power-card (:source-card source-result))]
     (if-not (:ok? source-result)
       source-result
       (cond
         (not= "moon" (:id effective-card))
         (core/failure :moon-actions-unavailable
                       "Only Moon can apply Rod followed by Sword."
                       {:card-id (:id effective-card)
                        :source (core/source-summary (:source source-result))})

         :else
         (let [rod-action (:rod command)
               sword-action (:sword command)]
           (if (and (nil? rod-action)
                    (nil? sword-action))
             (core/failure :invalid-moon-command
                           "Moon moves require a :rod action, a :sword action, or both."
                           {:command command})
             (let [player-id (:player-id command)
                   cost-state (major/charge-source-once state source-result)
                   source-opts (paid-sword-source-opts source-result power-card)
                   initial-minion-ids (set (:minion-ids source-result))]
               (if-not rod-action
                 (apply-moon-sword-action cost-state
                                          command
                                          source-result
                                          initial-minion-ids
                                          sword-action
                                          source-opts)
                 (let [rod-result (apply-moon-rod-action cost-state
                                                         command
                                                         source-result
                                                         initial-minion-ids
                                                         rod-action
                                                         source-opts
                                                         (some? sword-action))]
                   (if-not (:ok? rod-result)
                     rod-result
                     (let [allowed-minion-ids (add-owned-event-minions
                                               initial-minion-ids
                                               player-id
                                               (:events rod-result))
                           overfull-destination (overfull-rod-destination
                                                 (:state rod-result)
                                                 (:events rod-result))]
                       (if sword-action
                         (let [sword-result (apply-moon-sword-action
                                             (:state rod-result)
                                             command
                                             source-result
                                             allowed-minion-ids
                                             sword-action
                                             source-opts)]
                           (cond
                             (not (:ok? sword-result))
                             sword-result

                             (and overfull-destination
                                  (not (overfull-destination-resolved?
                                        (:state sword-result)
                                        overfull-destination)))
                             (core/failure :moon-full-territory-unresolved
                                           "Moon can enter a full territory only when its Sword action leaves no more than three pieces there."
                                           {:destination overfull-destination
                                            :piece-count (count
                                                          (core/pieces-at-board-index
                                                           (:state sword-result)
                                                           (:board-index
                                                            overfull-destination)))
                                            :maximum pieces/max-pieces-per-space})

                             :else
                             (core/success (:state sword-result)
                                           (concat (:events rod-result)
                                                   (:events sword-result)))))
                         (if overfull-destination
                           (core/failure :moon-full-territory-unresolved
                                         "Moon can enter a full territory only when a Sword action leaves no more than three pieces there."
                                         {:destination overfull-destination
                                          :piece-count (count
                                                        (core/pieces-at-board-index
                                                         (:state rod-result)
                                                         (:board-index
                                                          overfull-destination)))
                                          :maximum pieces/max-pieces-per-space})
                           rod-result))))))))))))))

(defn apply-moon-move [state command]
  (apply-moon-move-with-opts state command {}))

(defn- source-opts-with-power-card [source-opts power-card]
  (cond-> source-opts
    power-card
    (assoc :power-card power-card)))

(defn apply-justice-move-with-opts
  ([state command]
   (apply-justice-move-with-opts state command {}))
  ([state command {:keys [source-opts power-card]
                   :or {source-opts {}}}]
   (apply-justice-sword-move
    state
    command
    (source-opts-with-power-card source-opts power-card))))

(defn apply-death-move-with-opts
  ([state command]
   (apply-death-move-with-opts state command {}))
  ([state command {:keys [required-card-id power-card source-opts]
                   :or {source-opts {}}}]
   (let [power-card (or power-card (:power-card source-opts))]
     (apply-death-sword-move
      state
      command
      {:required-card-id (or required-card-id "death")
       :power-card power-card
       :source-opts (source-opts-with-power-card source-opts power-card)}))))

(defn apply-tower-move-with-opts
  ([state command]
   (apply-tower-move-with-opts state command {}))
  ([state command {:keys [source-opts power-card]
                   :or {source-opts {}}}]
   (apply-tower-sword-move
    state
    command
    (source-opts-with-power-card source-opts power-card))))

(defn apply-sword-move-with-opts
  ([state command]
   (apply-sword-move-with-opts state command {}))
  ([state command {:keys [source-opts required-card-id] :as opts
                   :or {source-opts {}}}]
   (cond
     (contains? command :sword-actions)
     (apply-death-sword-move
      state
      command
      {:required-card-id (or required-card-id "death")
       :power-card (:power-card source-opts)
       :source-opts source-opts})

     (or (contains? command :hand-trade-target)
         (contains? command :hand-trade-target-piece-id))
     (apply-justice-sword-move state command source-opts)

     (contains? command :minion-orientation)
     (apply-tower-sword-move state command source-opts)

     (or (contains? command :rod)
         (contains? command :sword))
     (apply-moon-move-with-opts
      state
      command
      {:required-card-id (or required-card-id "moon")
       :power-card (:power-card source-opts)
       :source-opts source-opts})

     :else
     (sword/apply-sword-move-with-opts state command opts))))

(defn apply-sword-move [state command]
  (apply-sword-move-with-opts state command {}))

(defmethod major-power/apply-card-power "justice"
  [state command _card {:keys [source-opts]}]
  (apply-justice-move-with-opts state command {:source-opts source-opts}))

(defmethod major-power/apply-card-power "death"
  [state command _card {:keys [source-opts]}]
  (apply-death-move-with-opts state command {:source-opts source-opts}))

(defmethod major-power/apply-card-power "tower"
  [state command _card {:keys [source-opts]}]
  (apply-tower-move-with-opts state command {:source-opts source-opts}))

(defmethod major-power/apply-card-power "moon"
  [state command _card {:keys [source-opts]}]
  (apply-sword-move-with-opts state command {:source-opts source-opts}))
