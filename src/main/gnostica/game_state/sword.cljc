(ns gnostica.game-state.sword
  (:require [gnostica.cards :as cards]
            [gnostica.game-state.core :as core]
            [gnostica.game-state.major :as major]
            [gnostica.game-state.placement :as placement]
            [gnostica.game-state.rod :as rod]
            [gnostica.pieces :as pieces]))

(def sword-territory-card-sources #{:hand :discard-pile})

(def sword-direction-offsets
  {:north [-1 0]
   :east [0 1]
   :south [1 0]
   :west [0 -1]})

(def sword-piece-size-ranks
  {:small 1
   :medium 2
   :large 3})

(def sword-piece-sizes-by-rank
  (into {} (map (fn [[size rank]] [rank size]) sword-piece-size-ranks)))

(defn- discard-pile-card [state card-id]
  (some (fn [card]
          (when (= card-id (:id card))
            card))
        (:discard-pile state)))

(defn- sword-piece-size-after [size damage]
  (get sword-piece-sizes-by-rank
       (- (get sword-piece-size-ranks size -100)
          damage)))

(defn- coordinate-map [coordinate]
  (cond
    (map? coordinate)
    (when (and (int? (:row coordinate))
               (int? (:col coordinate)))
      (select-keys coordinate [:row :col]))

    (sequential? coordinate)
    (let [[row col] coordinate]
      (when (and (int? row) (int? col))
        {:row row
         :col col}))))

(defn sword-target-coordinate [coordinate orientation]
  (when-let [{:keys [row col]} (coordinate-map coordinate)]
    (cond
      (= :up orientation)
      {:row row
       :col col}

      :else
      (when-let [[row-offset col-offset] (get sword-direction-offsets orientation)]
        {:row (+ row row-offset)
         :col (+ col col-offset)}))))

(defn- same-coordinate? [left right]
  (= (coordinate-map left)
     (coordinate-map right)))

(defn- sword-targetable-coordinate?
  [actor-coordinate target-coordinate orientation target-self?]
  (or target-self?
      (same-coordinate? target-coordinate
                        (sword-target-coordinate actor-coordinate orientation))))

(defn- target-summary [target]
  (select-keys target [:kind :piece-id :board-index :row :col]))

(defn- territory-target-cell [state target]
  (cond
    (not (map? target))
    (core/failure :invalid-sword-target
                  "Sword territory targets require a target map."
                  {:target target})

    (not= :territory (:kind target))
    (core/failure :invalid-sword-target
                  "Sword territory targets must use :kind :territory."
                  {:target target})

    (some? (:board-index target))
    (if-let [cell (core/board-cell-by-index state (:board-index target))]
      {:ok? true
       :cell cell}
      (core/failure :invalid-target-territory
                    "Sword territory targets must reference an existing board cell."
                    {:target target}))

    (and (int? (:row target))
         (int? (:col target)))
    (if-let [cell (core/board-cell-at state (:row target) (:col target))]
      {:ok? true
       :cell cell}
      (core/failure :invalid-target-territory
                    "Sword territory targets must reference an existing board cell."
                    {:target target}))

    :else
    (core/failure :invalid-sword-target
                  "Sword territory targets require a board index or row and column."
                  {:target target})))

(defn- resolve-sword-variant [card requested-variant source]
  (let [variants (cards/sword-variants card)
        variant-set (set variants)]
    (cond
      (empty? variants)
      (core/failure :source-card-not-sword
                    "The source card does not provide a Sword power."
                    {:card-id (:id card)
                     :source source})

      (nil? requested-variant)
      {:ok? true
       :sword-variant (first variants)}

      (not (contains? cards/sword-variant-ids requested-variant))
      (core/failure :invalid-sword-variant
                    "Sword moves require a known Sword variant."
                    {:sword-variant requested-variant
                     :valid-variants cards/sword-variant-ids})

      (contains? variant-set requested-variant)
      {:ok? true
       :sword-variant requested-variant}

      :else
      (core/failure :sword-variant-unavailable
                    "The source card does not provide the selected Sword variant."
                    {:card-id (:id card)
                     :sword-variant requested-variant
                     :available-variants variants}))))

(defn- resolve-sword-source
  ([state player-id source sword-variant]
   (resolve-sword-source state player-id source sword-variant {}))
  ([state player-id source sword-variant
    {:keys [source-card source-card-already-discarded? allow-major-minion?]}]
   (let [piece (core/piece-by-id state (:piece-id source))
         piece-coordinate (when piece
                            (core/piece-coordinate state piece))]
     (cond
       (not (map? source))
       (core/failure :invalid-sword-command
                     "Sword moves require a source map."
                     {:source source})

       (nil? piece)
       (core/failure :invalid-piece
                     "Sword moves require one of the player's pieces as the acting minion."
                     {:piece-id (:piece-id source)})

       (not= player-id (:player-id piece))
       (core/failure :invalid-piece
                     "The acting minion must belong to the move's player."
                     {:piece-id (:piece-id source)
                      :player-id player-id
                      :piece-player-id (:player-id piece)})

       (nil? piece-coordinate)
       (core/failure :invalid-piece-space
                     "Sword moves require an acting minion with a board coordinate."
                     {:piece-id (:piece-id source)
                      :space-index (:space-index piece)
                      :space (:space piece)})

       (not (contains? pieces/legal-orientations (:orientation piece)))
       (core/failure :invalid-sword-direction
                     "Sword moves require the acting minion to have a legal orientation."
                     {:piece-id (:id piece)
                      :orientation (:orientation piece)
                      :legal-orientations pieces/legal-orientations})

       (nil? (get sword-piece-size-ranks (:size piece)))
       (core/failure :invalid-piece-size
                     "Sword moves require an acting minion with a legal size."
                     {:piece-id (:id piece)
                      :size (:size piece)})

       (= :territory (:kind source))
       (let [cell (core/board-cell-by-index state (:board-index source))]
         (cond
           (nil? cell)
           (core/failure :invalid-source-territory
                         "Sword territory sources must reference an existing board cell."
                         {:board-index (:board-index source)})

           (and (not allow-major-minion?)
                (not= (:board-index source) (:space-index piece)))
           (core/failure :source-piece-mismatch
                         "The acting minion must occupy the source territory."
                         {:piece-id (:piece-id source)
                          :piece-space-index (:space-index piece)
                          :source-board-index (:board-index source)})

           :else
           (let [variant-result (resolve-sword-variant (:card cell)
                                                       sword-variant
                                                       source)]
             (if (:ok? variant-result)
               {:ok? true
                :source source
                :source-card (:card cell)
                :sword-variant (:sword-variant variant-result)
                :piece piece
                :piece-coordinate (coordinate-map piece-coordinate)
                :orientation (:orientation piece)}
               variant-result))))

       (= :hand-card (:kind source))
       (let [card (or source-card
                      (core/player-hand-card state player-id (:card-id source))
                      (when source-card-already-discarded?
                        (discard-pile-card state (:card-id source))))]
         (cond
           (nil? card)
           (core/failure :invalid-hand-card
                         "Sword hand-card sources must reference a card in the player's hand."
                         {:card-id (:card-id source)
                          :player-id player-id})

           (and source-card
                (not= (:card-id source) (:id source-card)))
           (core/failure :invalid-hand-card
                         "Sword paid source cards must match the command source card."
                         {:card-id (:card-id source)
                          :source-card-id (:id source-card)})

           :else
           (let [variant-result (resolve-sword-variant card sword-variant source)]
             (if (:ok? variant-result)
               {:ok? true
                :source source
                :source-card card
                :sword-variant (:sword-variant variant-result)
                :discard-source-card? (not source-card-already-discarded?)
                :piece piece
                :piece-coordinate (coordinate-map piece-coordinate)
                :orientation (:orientation piece)}
               variant-result))))

       :else
       (core/failure :invalid-sword-command
                     "Sword move sources must be either :territory or :hand-card."
                     {:source source})))))

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
        target-coordinate (coordinate-map (core/piece-coordinate state target-piece))
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
                  {:target (target-summary target)})))

(defn- resolve-replacement-card-options
  [source-result target destroyed? replacement-card-source replacement-card-id]
  (if (= :territory (:kind target))
    (let [replacement-card-source (or replacement-card-source
                                      (when (some? replacement-card-id)
                                        :hand))]
      (cond
        (and destroyed?
             (or (some? replacement-card-source)
                 (some? replacement-card-id)))
        (core/failure :invalid-sword-replacement
                      "Destroyed territories do not use a replacement card."
                      {:target target
                       :replacement-card-source replacement-card-source
                       :replacement-card-id replacement-card-id})

        destroyed?
        {:ok? true}

        (and (not destroyed?)
             (nil? replacement-card-source))
        {:ok? true}

        (not (contains? sword-territory-card-sources replacement-card-source))
        (core/failure :invalid-sword-replacement-card-source
                      "Sword territory attacks require a supported replacement card source."
                      {:replacement-card-source replacement-card-source
                       :valid-sources sword-territory-card-sources})

        (and (= :discard-pile replacement-card-source)
             (not= :sword-from-discard (:sword-variant source-result)))
        (core/failure :sword-variant-option-unavailable
                      "Only Tower Sword can attack territory using a discard-pile replacement card."
                      {:sword-variant (:sword-variant source-result)
                       :replacement-card-source replacement-card-source})

        :else
        {:ok? true
         :replacement-card-source replacement-card-source
         :replacement-card-id replacement-card-id}))
    (if (or (some? replacement-card-source)
            (some? replacement-card-id))
      (core/failure :invalid-sword-replacement
                    "Sword piece attacks do not use a replacement card."
                    {:target target
                     :replacement-card-source replacement-card-source
                     :replacement-card-id replacement-card-id})
      {:ok? true})))

(defn- resolve-sword-source-command
  ([state command]
   (resolve-sword-source-command state command {}))
  ([state command source-opts]
   (let [{:keys [player-id source sword-variant]} command]
     (cond
       (not (map? command))
       (core/failure :invalid-sword-command
                     "Sword moves require a command map."
                     {:command command})

       (nil? (get-in state [:players-by-id player-id]))
       (core/failure :unknown-player
                     "Sword moves require a participating player."
                     {:player-id player-id})

       (not (core/current-player-id? state player-id))
       (core/failure :not-current-player
                     "Only the current player can resolve a Sword move."
                     {:player-id player-id
                      :current-player-id (get-in state [:turn :current-player-id])})

       :else
       (resolve-sword-source state
                             player-id
                             source
                             sword-variant
                             source-opts)))))

(defn- resolve-sword-command* [state command source-opts]
  (let [{:keys [player-id source sword-variant target damage orientation
                replacement-card-source replacement-card-id]} command]
    (cond
      (not (map? command))
      (core/failure :invalid-sword-command
                    "Sword moves require a command map."
                    {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (core/failure :unknown-player
                    "Sword moves require a participating player."
                    {:player-id player-id})

      (not (core/current-player-id? state player-id))
      (core/failure :not-current-player
                    "Only the current player can resolve a Sword move."
                    {:player-id player-id
                     :current-player-id (get-in state [:turn :current-player-id])})

      :else
      (let [source-result (resolve-sword-source state
                                                player-id
                                                source
                                                sword-variant
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

(defn resolve-sword-command [state command]
  (resolve-sword-command* state command {}))

(defn- piece-space [piece]
  (select-keys piece [:space-index :space]))

(defn- remove-card-from-discard [state card-id]
  (update state :discard-pile
          (fn [discard-pile]
            (vec (remove #(= card-id (:id %)) discard-pile)))))

(defn- replace-board-cell-card [state board-index replacement-card]
  (update state :board
          (fn [cells]
            (mapv (fn [cell]
                    (if (= board-index (:index cell))
                      (assoc cell :card replacement-card)
                      cell))
                  cells))))

(defn- remove-board-cell [state board-index]
  (update state :board
          (fn [cells]
            (vec (remove #(= board-index (:index %)) cells)))))

(defn- replacement-card-from-source
  [state player-id replacement-card-source replacement-card-id]
  (case replacement-card-source
    :hand (core/player-hand-card state player-id replacement-card-id)
    :discard-pile (discard-pile-card state replacement-card-id)
    nil))

(defn- remove-replacement-card
  [state player-id replacement-card-source replacement-card-id]
  (case replacement-card-source
    :hand (core/remove-card-from-hand state player-id replacement-card-id)
    :discard-pile (remove-card-from-discard state replacement-card-id)
    state))

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
          (let [replacement-card (replacement-card-from-source cost-state
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
                                   (remove-replacement-card player-id
                                                            replacement-card-source
                                                            replacement-card-id)
                                   (replace-board-cell-card (:index target-cell)
                                                            replacement-card)
                                   (core/discard-card original-card)
                                   (core/append-history event))]
                (core/success next-state [event])))))))))

(defn- apply-resolved-sword-action
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

(defn- apply-single-sword-move
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

(defn- source-card-id [source-result]
  (get-in source-result [:source-card :id]))

(defn- paid-sword-source-opts [source-result]
  {:source-card (:source-card source-result)
   :source-card-already-discarded? (:discard-source-card? source-result)
   :allow-major-minion? true})

(defn- resolve-specific-major-source [state command card-id error-code message]
  (let [source-result (major/resolve-major-source state command)]
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
      source-result)))

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
           :territory (= (:board-index left) (:board-index right))
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

(defn- apply-death-sword-move [state command]
  (let [source-result (resolve-specific-major-source
                       state
                       command
                       "death"
                       :death-actions-unavailable
                       "Only Death can apply multiple Sword actions.")]
    (if-not (:ok? source-result)
      source-result
      (let [actions-result (normalize-action-list command :sword-actions "Death" 2)]
        (if-not (:ok? actions-result)
          actions-result
          (let [player-id (:player-id command)
                cost-state (major/charge-source-once state source-result)
                source-opts (paid-sword-source-opts source-result)
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
                                              source-opts))))))))

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
        (let [target-coordinate (coordinate-map (core/piece-coordinate state target-piece))
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

(defn- apply-justice-sword-move [state command]
  (let [source-result (resolve-sword-source-command state command)]
    (if-not (:ok? source-result)
      source-result
      (cond
        (not= "justice" (source-card-id source-result))
        (core/failure :justice-hand-trade-unavailable
                      "Only Justice can trade hands before applying Sword."
                      {:card-id (source-card-id source-result)
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
                  sword-result (apply-single-sword-move
                                trade-state
                                (dissoc command
                                        :hand-trade-target
                                        :hand-trade-target-piece-id)
                                {:source-opts (paid-sword-source-opts source-result)
                                 :charge-source? false})]
              (if-not (:ok? sword-result)
                sword-result
                (core/success (:state sword-result)
                              (concat [event] (:events sword-result)))))))))))

(defn- apply-tower-sword-move [state command]
  (let [source-result (resolve-sword-source-command state command)]
    (if-not (:ok? source-result)
      source-result
      (if-not (= "tower" (source-card-id source-result))
        (core/failure :sword-orient-unavailable
                      "Only Tower Sword can orient a minion before applying Sword."
                      {:card-id (source-card-id source-result)
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
                                (dissoc command :minion-orientation))]
              (if-not (:ok? sword-result)
                sword-result
                (core/success (:state sword-result)
                              (concat (:events orient-result)
                                      (:events sword-result)))))))))))

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

(defn apply-moon-move [state command]
  (let [source-result (resolve-specific-major-source
                       state
                       command
                       "moon"
                       :moon-actions-unavailable
                       "Only Moon can apply Rod followed by Sword.")]
    (if-not (:ok? source-result)
      source-result
      (let [rod-action (:rod command)
            sword-action (:sword command)]
        (if (and (nil? rod-action)
                 (nil? sword-action))
          (core/failure :invalid-moon-command
                        "Moon moves require a :rod action, a :sword action, or both."
                        {:command command})
          (let [player-id (:player-id command)
                cost-state (major/charge-source-once state source-result)
                source-opts (paid-sword-source-opts source-result)
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
                        rod-result))))))))))))

(defn apply-sword-move-with-opts
  ([state command]
   (apply-sword-move-with-opts state command {}))
  ([state command {:keys [source-opts] :as opts
                   :or {source-opts {}}}]
  (cond
    (contains? command :sword-actions)
    (if (seq source-opts)
      (core/failure :sword-composite-source-opts-unavailable
                    "Death Sword source overrides are not supported."
                    {:command command})
      (apply-death-sword-move state command))

    (or (contains? command :hand-trade-target)
        (contains? command :hand-trade-target-piece-id))
    (if (seq source-opts)
      (core/failure :sword-composite-source-opts-unavailable
                    "Justice Sword source overrides are not supported."
                    {:command command})
      (apply-justice-sword-move state command))

    (contains? command :minion-orientation)
    (if (seq source-opts)
      (core/failure :sword-composite-source-opts-unavailable
                    "Tower Sword source overrides are not supported."
                    {:command command})
      (apply-tower-sword-move state command))

    (or (contains? command :rod)
        (contains? command :sword))
    (if (seq source-opts)
      (core/failure :sword-composite-source-opts-unavailable
                    "Moon source overrides are not supported."
                    {:command command})
      (apply-moon-move state command))

    :else
    (apply-single-sword-move state command opts))))

(defn apply-sword-move [state command]
  (apply-sword-move-with-opts state command {}))
