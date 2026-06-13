(ns gnostica.game-state.sword
  (:require [gnostica.cards :as cards]
            [gnostica.game-state.card-source :as card-source]
            [gnostica.game-state.core :as core]
            [gnostica.game-state.spatial :as spatial]
            [gnostica.pieces :as pieces]))

(def sword-territory-card-sources #{:hand :discard-pile})

(def sword-piece-size-ranks
  {:small 1
   :medium 2
   :large 3})

(def sword-piece-sizes-by-rank
  (into {} (map (fn [[size rank]] [rank size]) sword-piece-size-ranks)))

(defn- legal-sword-source-piece-size [piece]
  (when (nil? (get sword-piece-size-ranks (:size piece)))
    (core/failure :invalid-piece-size
                  "Sword moves require an acting minion with a legal size."
                  {:piece-id (:id piece)
                   :size (:size piece)})))

(def sword-source-config
  {:suit-label "Sword"
   :variant-key :sword-variant
   :variants-fn cards/sword-variants
   :variant-ids cards/sword-variant-ids
   :source-card-not-suit-code :source-card-not-sword
   :invalid-variant-code :invalid-sword-variant
   :variant-unavailable-code :sword-variant-unavailable
   :command-error-code :invalid-sword-command
   :direction-error-code :invalid-sword-direction
   :invalid-target-code :invalid-sword-target
   :piece-check-fn legal-sword-source-piece-size})

(def sword-replacement-card-config
  {:valid-sources sword-territory-card-sources
   :source-error-code :invalid-sword-replacement-card-source
   :source-error-message
   "Sword territory attacks require a supported replacement card source."
   :piece-error-code :invalid-sword-replacement
   :piece-error-message "Sword piece attacks do not use a replacement card."
   :destroyed-error-code :invalid-sword-replacement
   :destroyed-error-message
   "Destroyed territories do not use a replacement card."
   :variant-key :sword-variant
   :discard-variant :sword-from-discard
   :discard-unavailable-code :sword-variant-option-unavailable
   :discard-unavailable-message
   "Only Tower Sword can attack territory using a discard-pile replacement card."})

(defn- sword-piece-size-after [size damage]
  (get sword-piece-sizes-by-rank
       (- (get sword-piece-size-ranks size -100)
          damage)))

(defn sword-target-coordinate [coordinate orientation]
  (spatial/target-coordinate coordinate orientation))

(defn- sword-targetable-coordinate?
  [actor-coordinate target-coordinate orientation target-self?]
  (or target-self?
      (spatial/same-coordinate? target-coordinate
                                (sword-target-coordinate actor-coordinate orientation))))

(defn- territory-target-cell [state target]
  (card-source/territory-target-cell state target sword-source-config))

(defn- resolve-sword-source
  ([state player-id source sword-variant]
   (resolve-sword-source state player-id source sword-variant {}))
  ([state player-id source sword-variant source-opts]
   (card-source/resolve-suit-source state
                                    player-id
                                    source
                                    sword-variant
                                    source-opts
                                    sword-source-config)))

(defn- resolve-damage [damage max-damage target-pips target]
  (cond
    (not (int? damage))
    (core/failure :invalid-sword-damage
                  "Sword damage must be an integer."
                  {:damage damage
                   :target target})

    (not (pos? damage))
    (core/failure :invalid-sword-damage
                  "Sword attacks must deal at least one pip of damage."
                  {:damage damage
                   :target target})

    (< max-damage damage)
    (core/failure :invalid-sword-damage
                  "Sword damage cannot exceed the acting minion's pip count."
                  {:damage damage
                   :maximum max-damage
                   :target target})

    (< target-pips damage)
    (core/failure :invalid-sword-damage
                  "Sword damage cannot reduce the target below zero pips."
                  {:damage damage
                   :target-pips target-pips
                   :target target})

    :else
    {:ok? true
     :damage damage
     :destroyed? (= damage target-pips)}))

(defn- resolve-piece-orientation [player-id target-piece damage orientation]
  (let [target-pips (get sword-piece-size-ranks (:size target-piece))
        destroyed? (= damage target-pips)]
    (cond
      (nil? orientation)
      {:ok? true}

      destroyed?
      (core/failure :invalid-orientation
                    "Destroyed pieces cannot be reoriented by a Sword attack."
                    {:piece-id (:id target-piece)
                     :orientation orientation})

      (not= player-id (:player-id target-piece))
      (core/failure :invalid-orientation
                    "Enemy pieces retain their original orientation when attacked by a Sword."
                    {:piece-id (:id target-piece)
                     :piece-player-id (:player-id target-piece)
                     :orientation orientation})

      (not (contains? pieces/legal-orientations orientation))
      (core/failure :invalid-orientation
                    "Sword attacks can only reorient current-player pieces to a legal orientation."
                    {:piece-id (:id target-piece)
                     :orientation orientation
                     :legal-orientations pieces/legal-orientations})

      :else
      {:ok? true
       :orientation orientation})))

(defn- normalize-sword-piece-target
  [state player-id source-result target-piece damage requested-orientation]
  (let [{:keys [piece]
         source-orientation :orientation
         source-coordinate :piece-coordinate} source-result
        target-coordinate (spatial/coordinate-map (core/piece-coordinate state target-piece))
        target-self? (= (:id piece) (:id target-piece))
        max-damage (get sword-piece-size-ranks (:size piece))
        target-pips (get sword-piece-size-ranks (:size target-piece))]
    (cond
      (nil? target-coordinate)
      (core/failure :invalid-piece-space
                    "Sword piece targets must have a board coordinate."
                    {:piece-id (:id target-piece)
                     :space-index (:space-index target-piece)
                     :space (:space target-piece)})

      (nil? target-pips)
      (core/failure :invalid-target-piece
                    "Sword piece targets must have a legal size."
                    {:piece-id (:id target-piece)
                     :size (:size target-piece)})

      (not (sword-targetable-coordinate? source-coordinate
                                         target-coordinate
                                         source-orientation
                                         target-self?))
      (core/failure :invalid-sword-target
                    "Sword piece targets must be the minion itself, occupy the current space for upright minions, or occupy the adjacent space in the minion direction."
                    {:piece-id (:id target-piece)
                     :orientation source-orientation
                     :source-coordinate source-coordinate
                     :target-coordinate target-coordinate
                     :expected-coordinate (sword-target-coordinate source-coordinate
                                                                   source-orientation)})

      :else
      (let [damage-result (resolve-damage damage
                                          max-damage
                                          target-pips
                                          {:kind :piece
                                           :piece-id (:id target-piece)})
            orientation-result (when (:ok? damage-result)
                                 (resolve-piece-orientation player-id
                                                            target-piece
                                                            damage
                                                            requested-orientation))
            target-cell (core/target-piece-territory-cell state target-piece)]
        (cond
          (not (:ok? damage-result))
          damage-result

          (not (:ok? orientation-result))
          orientation-result

          :else
          {:ok? true
           :damage (:damage damage-result)
           :destroyed? (:destroyed? damage-result)
           :target (cond-> {:kind :piece
                            :piece-id (:id target-piece)
                            :player-id (:player-id target-piece)
                            :row (:row target-coordinate)
                            :col (:col target-coordinate)}
                     target-cell
                     (assoc :board-index (:index target-cell))

                     (:orientation orientation-result)
                     (assoc :orientation (:orientation orientation-result)))
           :target-piece target-piece})))))

(defn- normalize-sword-territory-target
  [state player-id source-result target damage requested-orientation]
  (if (some? requested-orientation)
    (core/failure :invalid-orientation
                  "Sword territory attacks do not take a piece orientation."
                  {:orientation requested-orientation
                   :target target})
    (let [cell-result (territory-target-cell state target)]
      (if (:ok? cell-result)
        (let [cell (:cell cell-result)
              cell-coordinate (select-keys cell [:row :col])
              {:keys [piece-coordinate]
               source-orientation :orientation
               piece :piece} source-result
              enemy-pieces (core/enemy-pieces-at-coordinate state
                                                            player-id
                                                            (:row cell)
                                                            (:col cell))
              target-pips (cards/card-point-value (:card cell))
              max-damage (get sword-piece-size-ranks (:size piece))]
          (cond
            (not (sword-targetable-coordinate? piece-coordinate
                                               cell-coordinate
                                               source-orientation
                                               false))
            (core/failure :invalid-sword-target
                          "Sword territory targets must be the current space for upright minions or the adjacent space in the minion direction."
                          {:target target
                           :orientation source-orientation
                           :source-coordinate piece-coordinate
                           :target-coordinate cell-coordinate
                           :expected-coordinate (sword-target-coordinate piece-coordinate
                                                                         source-orientation)})

            (seq enemy-pieces)
            (core/failure :target-territory-occupied-by-enemy
                          "Sword territory attacks cannot target a territory occupied by enemy pieces."
                          {:target {:kind :territory
                                    :board-index (:index cell)
                                    :row (:row cell)
                                    :col (:col cell)}
                           :enemy-piece-ids (mapv :id enemy-pieces)})

            (nil? target-pips)
            (core/failure :invalid-target-territory
                          "Sword territory targets must have a point value."
                          {:target target
                           :card-id (get-in cell [:card :id])})

            :else
            (let [damage-result (resolve-damage damage
                                                max-damage
                                                target-pips
                                                {:kind :territory
                                                 :board-index (:index cell)})]
              (if-not (:ok? damage-result)
                damage-result
                {:ok? true
                 :damage (:damage damage-result)
                 :destroyed? (:destroyed? damage-result)
                 :target {:kind :territory
                          :board-index (:index cell)
                          :row (:row cell)
                          :col (:col cell)}
                 :target-cell cell}))))
        cell-result))))

(defn- resolve-sword-target [state player-id source-result target damage orientation]
  (cond
    (not (map? target))
    (core/failure :invalid-sword-target
                  "Sword moves require a target map."
                  {:target target})

    (= :piece (:kind target))
    (cond
      (nil? (:piece-id target))
      (core/failure :invalid-sword-target
                    "Sword piece targets require a target piece id."
                    {:target target})

      :else
      (if-let [target-piece (core/piece-by-id state (:piece-id target))]
        (normalize-sword-piece-target state
                                      player-id
                                      source-result
                                      target-piece
                                      damage
                                      orientation)
        (core/failure :invalid-target-piece
                      "Sword piece targets must reference a piece on the board."
                      {:target target})))

    (= :territory (:kind target))
    (normalize-sword-territory-target state
                                      player-id
                                      source-result
                                      target
                                      damage
                                      orientation)

    :else
    (core/failure :invalid-sword-target
                  "Sword move targets must be :piece or :territory."
                  {:target (spatial/target-summary target)})))

(defn- resolve-replacement-card-options
  [source-result target destroyed? replacement-card-source replacement-card-id]
  (card-source/resolve-replacement-card-options
   source-result
   target
   {:destroyed? destroyed?
    :replacement-card-source replacement-card-source
    :replacement-card-id replacement-card-id}
   sword-replacement-card-config))

(defn resolve-sword-source-command
  ([state command]
   (resolve-sword-source-command state command {}))
  ([state command source-opts]
   (let [player-result (card-source/resolve-current-player-command
                        state
                        command
                        sword-source-config)]
     (if-not (:ok? player-result)
       player-result
       (resolve-sword-source state
                             (:player-id player-result)
                             (:source command)
                             (:sword-variant command)
                             source-opts)))))

(defn resolve-sword-command* [state command source-opts]
  (let [player-result (card-source/resolve-current-player-command
                       state
                       command
                       sword-source-config)]
    (if-not (:ok? player-result)
      player-result
      (let [player-id (:player-id player-result)
            {:keys [target damage orientation replacement-card-source
                    replacement-card-id]} command
            source-result (resolve-sword-source state
                                                player-id
                                                (:source command)
                                                (:sword-variant command)
                                                source-opts)]
        (if-not (:ok? source-result)
          source-result
          (let [target-result (resolve-sword-target state
                                                    player-id
                                                    source-result
                                                    target
                                                    damage
                                                    orientation)]
            (if-not (:ok? target-result)
              target-result
              (let [replacement-result (resolve-replacement-card-options
                                        source-result
                                        (:target target-result)
                                        (:destroyed? target-result)
                                        replacement-card-source
                                        replacement-card-id)]
                (if-not (:ok? replacement-result)
                  replacement-result
                  (let [normalized-command (cond-> {:player-id player-id
                                                    :source (core/source-summary
                                                             (:source source-result))
                                                    :sword-variant (:sword-variant
                                                                    source-result)
                                                    :target (:target target-result)
                                                    :damage (:damage target-result)}
                                             (:orientation (:target target-result))
                                             (assoc :orientation
                                                    (:orientation (:target target-result)))

                                             (:replacement-card-source replacement-result)
                                             (assoc :replacement-card-source
                                                    (:replacement-card-source
                                                     replacement-result))

                                             (:replacement-card-id replacement-result)
                                             (assoc :replacement-card-id
                                                    (:replacement-card-id
                                                     replacement-result)))]
                    (merge {:ok? true
                            :command normalized-command
                            :source-card (:source-card source-result)
                            :discard-source-card? (:discard-source-card?
                                                   source-result)
                            :piece (:piece source-result)
                            :destroyed? (:destroyed? target-result)}
                           (select-keys target-result
                                        [:target-piece :target-cell]))))))))))))

(defn resolve-sword-command
  ([state command]
   (resolve-sword-command* state command {}))
  ([state command source-opts]
   (resolve-sword-command* state command source-opts)))

(defn- piece-space [piece]
  (select-keys piece [:space-index :space]))

(defn- remove-board-cell [state board-index]
  (update state :board
          (fn [cells]
            (vec (remove #(= board-index (:index %)) cells)))))

(defn- remove-piece-by-id [state piece-id]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (vec (remove #(= piece-id (:id %)) board-pieces)))))

(defn- legal-piece-coordinate? [state [row col]]
  (or (some? (core/board-cell-at state row col))
      (core/wasteland-target? state {:kind :wasteland
                                     :row row
                                     :col col})))

(defn- void-pieces [state]
  (->> (get-in state [:pieces :on-board])
       (filterv (fn [piece]
                  (let [coordinate (core/piece-coordinate state piece)]
                    (or (nil? coordinate)
                        (not (legal-piece-coordinate? state coordinate))))))))

(defn- return-pieces-to-stash [state pieces]
  (reduce (fn [next-state piece]
            (-> next-state
                (core/increment-stash (:player-id piece) (:size piece))
                (remove-piece-by-id (:id piece))))
          state
          pieces))

(defn- apply-sword-piece-attack
  [state player-id {:keys [command source-card discard-source-card? target-piece]}
   {:keys [charge-source? action-count shortcut?]
    :or {charge-source? true
         action-count 1}}]
  (let [{:keys [source target sword-variant damage]} command
        owner-id (:player-id target-piece)
        old-size (:size target-piece)
        next-size (sword-piece-size-after old-size damage)
        destroyed? (nil? next-size)]
    (cond
      (and (not destroyed?)
           (not (pos? (core/stash-count state owner-id next-size))))
      (core/failure :no-smaller-piece-available
                    "Sword piece attacks require the target owner to have the smaller replacement piece in stash."
                    {:player-id owner-id
                     :piece-id (:id target-piece)
                     :from-size old-size
                     :to-size next-size})

      :else
      (let [source-cost {:source-card source-card
                         :discard-source-card? discard-source-card?}
            cost-state (if charge-source?
                         (core/apply-source-cost state player-id source-cost)
                         state)
            event-target (select-keys target
                                      [:kind :piece-id :player-id :board-index :row :col])
            base-event (cond-> {:player-id player-id
                                :source source
                                :sword-variant sword-variant
                                :target event-target
                                :damage damage
                                :from-size old-size}
                         (< 1 action-count)
                         (assoc :action-count action-count)

                         shortcut?
                         (assoc :shortcut? true))]
        (if destroyed?
          (let [event (assoc base-event
                             :type :sword/piece-destroyed
                             :destroyed-piece target-piece)
                next-state (-> cost-state
                               (core/increment-stash owner-id old-size)
                               (remove-piece-by-id (:id target-piece))
                               (core/append-history event))]
            (core/success next-state [event]))
          (let [shrunk-piece (merge {:id (core/next-piece-id state owner-id next-size)
                                     :player-id owner-id
                                     :size next-size
                                     :orientation (or (:orientation target)
                                                      (:orientation target-piece))}
                                    (piece-space target-piece))
                event (assoc base-event
                             :type :sword/piece-shrunk
                             :to-size next-size
                             :replaced-piece target-piece
                             :piece shrunk-piece)
                next-state (-> cost-state
                               (core/increment-stash owner-id old-size)
                               (core/decrement-stash owner-id next-size)
                               (core/replace-piece-by-id (:id target-piece) shrunk-piece)
                               (core/append-history event))]
            (core/success next-state [event])))))))

(defn- apply-sword-territory-attack
  [state player-id {:keys [command source-card discard-source-card? target-cell]}
   {:keys [charge-source? action-count shortcut?]
    :or {charge-source? true
         action-count 1}}]
  (let [{:keys [source target sword-variant damage
                replacement-card-source replacement-card-id]} command
        replacement-card-source (or replacement-card-source :hand)
        original-card (:card target-cell)
        original-value (cards/card-point-value original-card)
        replacement-value-target (- original-value damage)
        destroyed? (zero? replacement-value-target)]
    (cond
      (and (not destroyed?)
           (nil? replacement-card-id))
      (core/failure :invalid-sword-replacement
                    "Sword territory attacks require a selected replacement card unless the territory is destroyed."
                    {:target (select-keys target [:kind :board-index :row :col])
                     :replacement-card-source replacement-card-source})

      (not (contains? sword-territory-card-sources replacement-card-source))
      (core/failure :invalid-sword-replacement-card-source
                    "Sword territory attacks require a supported replacement card source."
                    {:replacement-card-source replacement-card-source
                     :valid-sources sword-territory-card-sources})

      (and (not destroyed?)
           (= :hand replacement-card-source)
           (= :hand-card (:kind source))
           (= (:id source-card) replacement-card-id))
      (core/failure :card-already-used
                    "A played source card cannot also become the replacement territory."
                    {:card-id replacement-card-id})

      :else
      (let [source-cost {:source-card source-card
                         :discard-source-card? discard-source-card?}
            cost-state (if charge-source?
                         (core/apply-source-cost state player-id source-cost)
                         state)]
        (if destroyed?
          (let [base-state (-> cost-state
                               (core/move-board-index-pieces-to-wasteland (:index target-cell)
                                                                          (:row target-cell)
                                                                          (:col target-cell))
                               (remove-board-cell (:index target-cell))
                               (core/discard-card original-card))
                destroyed-pieces (void-pieces base-state)
                event (cond-> {:type :sword/territory-destroyed
                               :player-id player-id
                               :source source
                               :sword-variant sword-variant
                               :target (select-keys target [:kind :board-index :row :col])
                               :damage damage
                               :original-card-id (:id original-card)
                               :from-value original-value
                               :destroyed-territory target-cell}
                        (< 1 action-count)
                        (assoc :action-count action-count)

                        shortcut?
                        (assoc :shortcut? true)

                        (seq destroyed-pieces)
                        (assoc :destroyed-pieces destroyed-pieces))
                next-state (-> base-state
                               (return-pieces-to-stash destroyed-pieces)
                               (core/append-history event))]
            (core/success next-state [event]))
          (let [replacement-card (card-source/replacement-card-from-source
                                  cost-state
                                  player-id
                                  replacement-card-source
                                  replacement-card-id)
                replacement-value (cards/card-point-value replacement-card)]
            (cond
              (nil? replacement-card)
              (core/failure :invalid-sword-replacement-card
                            "Sword territory attacks require a replacement card from the selected source."
                            {:card-id replacement-card-id
                             :player-id player-id
                             :replacement-card-source replacement-card-source})

              (not= replacement-value-target replacement-value)
              (core/failure :invalid-sword-replacement-card
                            "Sword territory attacks require a replacement card worth the original territory value minus the selected damage."
                            {:original-card-id (:id original-card)
                             :original-value original-value
                             :replacement-card-id (:id replacement-card)
                             :replacement-value replacement-value
                             :damage damage
                             :required-value replacement-value-target})

              :else
              (let [shrunk-cell (assoc target-cell :card replacement-card)
                    event (cond-> {:type :sword/territory-shrunk
                                   :player-id player-id
                                   :source source
                                   :sword-variant sword-variant
                                   :target (select-keys target [:kind :board-index :row :col])
                                   :damage damage
                                   :replacement-card-source replacement-card-source
                                   :original-card-id (:id original-card)
                                   :replacement-card-id (:id replacement-card)
                                   :from-value original-value
                                   :to-value replacement-value
                                   :territory shrunk-cell}
                            (< 1 action-count)
                            (assoc :action-count action-count)

                            shortcut?
                            (assoc :shortcut? true))
                    next-state (-> cost-state
                                   (card-source/remove-replacement-card
                                    player-id
                                    replacement-card-source
                                    replacement-card-id)
                                   (card-source/replace-board-cell-card
                                    (:index target-cell)
                                    replacement-card)
                                   (core/discard-card original-card)
                                   (core/append-history event))]
                (core/success next-state [event])))))))))

(defn apply-resolved-sword-action
  ([state player-id result]
   (apply-resolved-sword-action state player-id result {}))
  ([state player-id result opts]
   (case (get-in result [:command :target :kind])
     :piece
     (apply-sword-piece-attack state player-id result opts)

     :territory
     (apply-sword-territory-attack state player-id result opts)

     (core/failure :invalid-sword-target
                   "Sword move targets must be :piece or :territory."
                   {:target (get-in result [:command :target])}))))

(defn apply-single-sword-move
  ([state command]
   (apply-single-sword-move state command {}))
  ([state command {:keys [source-opts charge-source?]
                   :or {source-opts {}
                        charge-source? true}}]
   (let [result (resolve-sword-command* state command source-opts)]
     (if-not (:ok? result)
       result
       (apply-resolved-sword-action state
                                    (get-in result [:command :player-id])
                                    result
                                    {:charge-source? charge-source?})))))

(defn source-card-id [source-result]
  (card-source/source-card-id source-result))

(defn source-power-card [source-result]
  (card-source/source-power-card source-result))

(defn source-power-card-id [source-result]
  (card-source/source-power-card-id source-result))

(defn paid-sword-source-opts
  ([source-result]
   (card-source/paid-source-opts source-result))
  ([source-result power-card]
   (card-source/paid-source-opts source-result power-card)))

(defn apply-sword-move-with-opts
  ([state command]
   (apply-sword-move-with-opts state command {}))
  ([state command opts]
   (apply-single-sword-move state command opts)))

(defn apply-sword-move [state command]
  (apply-sword-move-with-opts state command {}))
