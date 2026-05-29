(ns gnostica.game-state.disc
  (:require [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.game-state.cup :as cup]
            [gnostica.game-state.core :as core]
            [gnostica.game-state.placement :as placement]
            [gnostica.pieces :as pieces]))

(def disc-territory-card-sources #{:hand :discard-pile})

(def disc-direction-offsets
  {:north [-1 0]
   :east [0 1]
   :south [1 0]
   :west [0 -1]})

(def disc-piece-size-ranks
  {:small 0
   :medium 1
   :large 2})

(def disc-piece-sizes-by-rank
  (into {} (map (fn [[size rank]] [rank size]) disc-piece-size-ranks)))

(defn- disc-piece-size-after [size action-count]
  (get disc-piece-sizes-by-rank
       (+ (get disc-piece-size-ranks size -100)
          action-count)))

(defn- discard-pile-card [state card-id]
  (some (fn [card]
          (when (= card-id (:id card))
            card))
        (:discard-pile state)))

(defn- card-worth-more? [replacement-card original-card action-count]
  (let [original-value (cards/card-point-value original-card)
        replacement-value (cards/card-point-value replacement-card)]
    (and (some? original-value)
         (= (+ original-value action-count) replacement-value))))

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

(defn disc-target-coordinate [coordinate orientation]
  (when-let [{:keys [row col]} (coordinate-map coordinate)]
    (cond
      (= :up orientation)
      {:row row
       :col col}

      :else
      (when-let [[row-offset col-offset] (get disc-direction-offsets orientation)]
        {:row (+ row row-offset)
         :col (+ col col-offset)}))))

(defn- same-coordinate? [left right]
  (= (coordinate-map left)
     (coordinate-map right)))

(defn- disc-targetable-coordinate? [actor-coordinate target-coordinate orientation target-self?]
  (or target-self?
      (same-coordinate? target-coordinate
                        (disc-target-coordinate actor-coordinate orientation))))

(defn- target-summary [target]
  (select-keys target [:kind :piece-id :board-index :row :col]))

(defn- territory-target-cell [state target]
  (cond
    (not (map? target))
    (core/failure :invalid-disc-target
             "Disc territory targets require a target map."
             {:target target})

    (not= :territory (:kind target))
    (core/failure :invalid-disc-target
             "Disc territory targets must use :kind :territory."
             {:target target})

    (some? (:board-index target))
    (if-let [cell (core/board-cell-by-index state (:board-index target))]
      {:ok? true
       :cell cell}
      (core/failure :invalid-target-territory
               "Disc territory targets must reference an existing board cell."
               {:target target}))

    (and (int? (:row target))
         (int? (:col target)))
    (if-let [cell (core/board-cell-at state (:row target) (:col target))]
      {:ok? true
       :cell cell}
      (core/failure :invalid-target-territory
               "Disc territory targets must reference an existing board cell."
               {:target target}))

    :else
    (core/failure :invalid-disc-target
             "Disc territory targets require a board index or row and column."
             {:target target})))

(defn- resolve-disc-variant [card requested-variant source]
  (let [variants (cards/disc-variants card)
        variant-set (set variants)]
    (cond
      (empty? variants)
      (core/failure :source-card-not-disc
               "The source card does not provide a Disc power."
               {:card-id (:id card)
                :source source})

      (nil? requested-variant)
      {:ok? true
       :disc-variant (first variants)}

      (not (contains? cards/disc-variant-ids requested-variant))
      (core/failure :invalid-disc-variant
               "Disc moves require a known Disc variant."
               {:disc-variant requested-variant
                :valid-variants cards/disc-variant-ids})

      (contains? variant-set requested-variant)
      {:ok? true
       :disc-variant requested-variant}

      :else
      (core/failure :disc-variant-unavailable
               "The source card does not provide the selected Disc variant."
               {:card-id (:id card)
                :disc-variant requested-variant
                :available-variants variants}))))

(defn- resolve-disc-source
  ([state player-id source disc-variant]
   (resolve-disc-source state player-id source disc-variant {}))
  ([state player-id source disc-variant
    {:keys [source-card-already-discarded? source-card power-card
            allow-major-minion?]}]
   (let [piece (core/piece-by-id state (:piece-id source))
         piece-coordinate (when piece
                            (core/piece-coordinate state piece))]
     (cond
       (not (map? source))
       (core/failure :invalid-disc-command
                "Disc moves require a source map."
                {:source source})

       (nil? piece)
       (core/failure :invalid-piece
                "Disc moves require one of the player's pieces as the acting minion."
                {:piece-id (:piece-id source)})

       (not= player-id (:player-id piece))
       (core/failure :invalid-piece
                "The acting minion must belong to the move's player."
                {:piece-id (:piece-id source)
                 :player-id player-id
                 :piece-player-id (:player-id piece)})

       (nil? piece-coordinate)
       (core/failure :invalid-piece-space
                "Disc moves require an acting minion with a board coordinate."
                {:piece-id (:piece-id source)
                 :space-index (:space-index piece)
                 :space (:space piece)})

       (not (contains? pieces/legal-orientations (:orientation piece)))
       (core/failure :invalid-disc-direction
                "Disc moves require the acting minion to have a legal orientation."
                {:piece-id (:id piece)
                 :orientation (:orientation piece)
                 :legal-orientations pieces/legal-orientations})

       (= :territory (:kind source))
       (let [cell (core/board-cell-by-index state (:board-index source))]
         (cond
           (nil? cell)
           (core/failure :invalid-source-territory
                    "Disc territory sources must reference an existing board cell."
                    {:board-index (:board-index source)})

           (and source-card
                (not= (get-in cell [:card :id]) (:id source-card)))
           (core/failure :invalid-source-territory
                    "Disc paid source cards must match the command source territory."
                    {:board-index (:board-index source)
                     :territory-card-id (get-in cell [:card :id])
                     :source-card-id (:id source-card)})

           (and (not allow-major-minion?)
                (not= (:board-index source) (:space-index piece)))
           (core/failure :source-piece-mismatch
                    "The acting minion must occupy the source territory."
                    {:piece-id (:piece-id source)
                     :piece-space-index (:space-index piece)
                     :source-board-index (:board-index source)})

           :else
           (let [paid-card (or source-card (:card cell))
                 power-card (or power-card paid-card)
                 variant-result (resolve-disc-variant power-card
                                                      disc-variant
                                                      source)]
             (if (:ok? variant-result)
               {:ok? true
                :source source
                :source-card paid-card
                :power-card power-card
                :disc-variant (:disc-variant variant-result)
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
                    "Disc hand-card sources must reference a card in the player's hand."
                    {:card-id (:card-id source)
                     :player-id player-id})

           (and source-card
                (not= (:card-id source) (:id source-card)))
           (core/failure :invalid-hand-card
                    "Disc paid source cards must match the command source card."
                    {:card-id (:card-id source)
                     :source-card-id (:id source-card)})

           :else
           (let [power-card (or power-card card)
                 variant-result (resolve-disc-variant power-card disc-variant source)]
             (if (:ok? variant-result)
               {:ok? true
                :source source
                :source-card card
                :power-card power-card
                :disc-variant (:disc-variant variant-result)
                :discard-source-card? (not source-card-already-discarded?)
                :piece piece
                :piece-coordinate (coordinate-map piece-coordinate)
                :orientation (:orientation piece)}
               variant-result))))

       :else
       (core/failure :invalid-disc-command
                "Disc move sources must be either :territory or :hand-card."
                {:source source})))))

(defn- resolve-disc-orientation [player-id target-piece orientation]
  (cond
    (nil? orientation)
    {:ok? true}

    (not= player-id (:player-id target-piece))
    (core/failure :invalid-orientation
             "Enemy pieces retain their original orientation when grown by a Disc."
             {:piece-id (:id target-piece)
              :piece-player-id (:player-id target-piece)
              :orientation orientation})

    (not (contains? pieces/legal-orientations orientation))
    (core/failure :invalid-orientation
             "Disc moves can only reorient current-player pieces to a legal orientation."
             {:piece-id (:id target-piece)
              :orientation orientation
              :legal-orientations pieces/legal-orientations})

    :else
    {:ok? true
     :orientation orientation}))

(defn- normalize-disc-piece-target [state player-id source-result target-piece requested-orientation]
  (let [{:keys [piece]
         source-orientation :orientation
         source-coordinate :piece-coordinate} source-result
        target-coordinate (coordinate-map (core/piece-coordinate state target-piece))
        target-self? (= (:id piece) (:id target-piece))]
    (cond
      (nil? target-coordinate)
      (core/failure :invalid-piece-space
               "Disc piece targets must have a board coordinate."
               {:piece-id (:id target-piece)
                :space-index (:space-index target-piece)
                :space (:space target-piece)})

      (not (disc-targetable-coordinate? source-coordinate
                                        target-coordinate
                                        source-orientation
                                        target-self?))
      (core/failure :invalid-disc-target
               "Disc piece targets must be the minion itself, occupy the current space for upright minions, or occupy the adjacent space in the minion direction."
               {:piece-id (:id target-piece)
                :orientation source-orientation
                :source-coordinate source-coordinate
                :target-coordinate target-coordinate
                :expected-coordinate (disc-target-coordinate source-coordinate source-orientation)})

      :else
      (let [orientation-result (resolve-disc-orientation player-id target-piece requested-orientation)
            target-cell (core/target-piece-territory-cell state target-piece)]
        (if (:ok? orientation-result)
          {:ok? true
           :target (cond-> {:kind :piece
                            :piece-id (:id target-piece)
                            :player-id (:player-id target-piece)
                            :row (:row target-coordinate)
                            :col (:col target-coordinate)}
                     target-cell
                     (assoc :board-index (:index target-cell))

                     (:orientation orientation-result)
                     (assoc :orientation (:orientation orientation-result)))
           :target-piece target-piece}
          orientation-result)))))

(defn- normalize-disc-territory-target [state player-id source-result target requested-orientation]
  (if (some? requested-orientation)
    (core/failure :invalid-orientation
             "Disc territory growth does not take a piece orientation."
             {:orientation requested-orientation
              :target target})
    (let [cell-result (territory-target-cell state target)]
      (if (:ok? cell-result)
        (let [cell (:cell cell-result)
              cell-coordinate (select-keys cell [:row :col])
              {:keys [piece-coordinate]
               source-orientation :orientation} source-result
              enemy-pieces (core/enemy-pieces-at-coordinate state
                                                            player-id
                                                            (:row cell)
                                                            (:col cell))]
          (cond
            (not (disc-targetable-coordinate? piece-coordinate
                                              cell-coordinate
                                              source-orientation
                                              false))
            (core/failure :invalid-disc-target
                     "Disc territory targets must be the current space for upright minions or the adjacent space in the minion direction."
                     {:target target
                      :orientation source-orientation
                      :source-coordinate piece-coordinate
                      :target-coordinate cell-coordinate
                      :expected-coordinate (disc-target-coordinate piece-coordinate source-orientation)})

            (seq enemy-pieces)
            (core/failure :target-territory-occupied-by-enemy
                     "Disc territory growth cannot target a territory occupied by enemy pieces."
                     {:target {:kind :territory
                               :board-index (:index cell)
                               :row (:row cell)
                               :col (:col cell)}
                      :enemy-piece-ids (mapv :id enemy-pieces)})

            :else
            {:ok? true
             :target {:kind :territory
                      :board-index (:index cell)
                      :row (:row cell)
                      :col (:col cell)}
             :target-cell cell}))
        cell-result))))

(defn- resolve-disc-target [state player-id source-result target orientation]
  (cond
    (not (map? target))
    (core/failure :invalid-disc-target
             "Disc moves require a target map."
             {:target target})

    (= :piece (:kind target))
    (cond
      (nil? (:piece-id target))
      (core/failure :invalid-disc-target
               "Disc piece targets require a target piece id."
               {:target target})

      :else
      (if-let [target-piece (core/piece-by-id state (:piece-id target))]
        (normalize-disc-piece-target state
                                     player-id
                                     source-result
                                     target-piece
                                     orientation)
        (core/failure :invalid-target-piece
                 "Disc piece targets must reference a piece on the board."
                 {:target target})))

    (= :territory (:kind target))
    (normalize-disc-territory-target state player-id source-result target orientation)

    :else
    (core/failure :invalid-disc-target
             "Disc move targets must be :piece or :territory."
             {:target (target-summary target)})))

(defn- resolve-replacement-card-options
  [source-result target replacement-card-source replacement-card-id]
  (if (= :territory (:kind target))
    (let [replacement-card-source (or replacement-card-source
                                      (when (some? replacement-card-id)
                                        :hand))]
      (cond
        (nil? replacement-card-source)
        {:ok? true}

        (not (contains? disc-territory-card-sources replacement-card-source))
        (core/failure :invalid-disc-replacement-card-source
                 "Disc territory growth requires a supported replacement card source."
                 {:replacement-card-source replacement-card-source
                  :valid-sources disc-territory-card-sources})

        (and (= :discard-pile replacement-card-source)
             (not= :disc-from-discard (:disc-variant source-result)))
        (core/failure :disc-variant-option-unavailable
                 "Only Star Disc can grow territory from a discard-pile replacement card."
                 {:disc-variant (:disc-variant source-result)
                  :replacement-card-source replacement-card-source})

        :else
        {:ok? true
         :replacement-card-source replacement-card-source
         :replacement-card-id replacement-card-id}))
    (if (or (some? replacement-card-source)
            (some? replacement-card-id))
      (core/failure :invalid-disc-replacement
               "Disc piece growth does not use a replacement card."
               {:target target
                :replacement-card-source replacement-card-source
                :replacement-card-id replacement-card-id})
      {:ok? true})))

(defn- resolve-disc-action
  [state player-id source-result action]
  (let [{:keys [target orientation replacement-card-source replacement-card-id]} action
        target-result (resolve-disc-target state
                                           player-id
                                           source-result
                                           target
                                           orientation)]
    (if-not (:ok? target-result)
      target-result
      (let [replacement-result (resolve-replacement-card-options
                                source-result
                                (:target target-result)
                                replacement-card-source
                                replacement-card-id)]
        (if-not (:ok? replacement-result)
          replacement-result
          (let [normalized-command (cond-> {:player-id player-id
                                            :source (core/source-summary
                                                     (:source source-result))
                                            :disc-variant (:disc-variant source-result)
                                            :target (:target target-result)}
                                     (:orientation (:target target-result))
                                     (assoc :orientation
                                            (:orientation (:target target-result)))

                                     (:replacement-card-source replacement-result)
                                     (assoc :replacement-card-source
                                            (:replacement-card-source replacement-result))

                                     (:replacement-card-id replacement-result)
                                     (assoc :replacement-card-id
                                            (:replacement-card-id replacement-result)))]
            (merge {:ok? true
                    :command normalized-command
                    :source-card (:source-card source-result)
                    :discard-source-card? (:discard-source-card? source-result)
                    :piece (:piece source-result)}
                   (select-keys target-result
                                [:target-piece :target-cell]))))))))

(defn- resolve-disc-command*
  [state command source-opts]
  (let [{:keys [player-id source disc-variant]} command]
    (cond
      (not (map? command))
      (core/failure :invalid-disc-command
               "Disc moves require a command map."
               {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (core/failure :unknown-player
               "Disc moves require a participating player."
               {:player-id player-id})

      (not (core/current-player-id? state player-id))
      (core/failure :not-current-player
               "Only the current player can resolve a Disc move."
               {:player-id player-id
                :current-player-id (get-in state [:turn :current-player-id])})

      :else
      (let [source-result (resolve-disc-source state
                                               player-id
                                               source
                                               disc-variant
                                               source-opts)]
        (if-not (:ok? source-result)
          source-result
          (resolve-disc-action state player-id source-result command))))))

(defn resolve-disc-command
  ([state command]
   (resolve-disc-command* state command {}))
  ([state command source-opts]
   (resolve-disc-command* state command source-opts)))

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

(defn- apply-disc-piece-growth
  ([state player-id result]
   (apply-disc-piece-growth state player-id result {}))
  ([state player-id {:keys [command source-card discard-source-card? target-piece]}
    {:keys [action-count charge-source? shortcut?]
     :or {action-count 1
          charge-source? true}}]
   (let [{:keys [source target disc-variant]} command
        owner-id (:player-id target-piece)
        old-size (:size target-piece)
        next-size (disc-piece-size-after old-size action-count)]
    (cond
      (nil? next-size)
      (core/failure :target-piece-max-size
               "Disc piece growth cannot grow the target by the requested number of actions."
               {:piece-id (:id target-piece)
                :size old-size
                :action-count action-count})

      (not (pos? (core/stash-count state owner-id next-size)))
      (core/failure :no-larger-piece-available
               "Disc piece growth requires a larger piece in the target owner's stash."
               {:player-id owner-id
                :piece-id (:id target-piece)
                :from-size old-size
                :to-size next-size})

      :else
      (let [grown-piece (merge {:id (core/next-piece-id state owner-id next-size)
                                :player-id owner-id
                                :size next-size
                                :orientation (or (:orientation target)
                                                 (:orientation target-piece))}
                               (piece-space target-piece))
            event (cond-> {:type :disc/piece-grown
                           :player-id player-id
                           :source source
                           :disc-variant disc-variant
                           :target (select-keys target [:kind :piece-id :player-id :board-index :row :col])
                           :from-size old-size
                           :to-size next-size
                           :replaced-piece target-piece
                           :piece grown-piece}
                    (> action-count 1)
                    (assoc :action-count action-count)

                    shortcut?
                    (assoc :shortcut? true))
            source-cost {:source-card source-card
                         :discard-source-card? discard-source-card?}
            cost-state (cond-> state
                         charge-source?
                         (core/apply-source-cost player-id source-cost))
            next-state (-> cost-state
                           (core/increment-stash owner-id old-size)
                           (core/decrement-stash owner-id next-size)
                           (core/replace-piece-by-id (:id target-piece) grown-piece)
                           (core/append-history event))]
        (core/success next-state [event]))))))

(defn- apply-disc-territory-growth
  ([state player-id result]
   (apply-disc-territory-growth state player-id result {}))
  ([state player-id {:keys [command source-card discard-source-card? target-cell]}
    {:keys [action-count charge-source? shortcut?]
     :or {action-count 1
          charge-source? true}}]
   (let [{:keys [source target disc-variant
                 replacement-card-source replacement-card-id]} command
        replacement-card-source (or replacement-card-source :hand)
        original-card (:card target-cell)]
    (cond
      (nil? replacement-card-id)
      (core/failure :invalid-disc-replacement
               "Disc territory growth requires a selected replacement card."
               {:target (select-keys target [:kind :board-index :row :col])
                :replacement-card-source replacement-card-source})

      (not (contains? disc-territory-card-sources replacement-card-source))
      (core/failure :invalid-disc-replacement-card-source
               "Disc territory growth requires a supported replacement card source."
               {:replacement-card-source replacement-card-source
                :valid-sources disc-territory-card-sources})

      (and (= :hand replacement-card-source)
           (= :hand-card (:kind source))
           (= (:id source-card) replacement-card-id))
      (core/failure :card-already-used
               "A played source card cannot also become the replacement territory."
               {:card-id replacement-card-id})

      :else
      (let [source-cost {:source-card source-card
                         :discard-source-card? discard-source-card?}
            cost-state (cond-> state
                         charge-source?
                         (core/apply-source-cost player-id source-cost))
            replacement-card (replacement-card-from-source cost-state
                                                           player-id
                                                           replacement-card-source
                                                           replacement-card-id)
            original-value (cards/card-point-value original-card)
            replacement-value (cards/card-point-value replacement-card)]
        (cond
          (nil? replacement-card)
          (core/failure :invalid-disc-replacement-card
                   "Disc territory growth requires a replacement card from the selected source."
                   {:card-id replacement-card-id
                    :player-id player-id
                    :replacement-card-source replacement-card-source})

          (not (card-worth-more? replacement-card original-card action-count))
          (core/failure :invalid-disc-replacement-card
                   "Disc territory growth requires a replacement card worth the requested number of Disc actions more than the original territory."
                   {:original-card-id (:id original-card)
                    :original-value original-value
                    :replacement-card-id (:id replacement-card)
                    :replacement-value replacement-value
                    :action-count action-count})

          :else
          (let [grown-cell (assoc target-cell :card replacement-card)
                event (cond-> {:type :disc/territory-grown
                               :player-id player-id
                               :source source
                               :disc-variant disc-variant
                               :target (select-keys target [:kind :board-index :row :col])
                               :replacement-card-source replacement-card-source
                               :original-card-id (:id original-card)
                               :replacement-card-id (:id replacement-card)
                               :from-value original-value
                               :to-value replacement-value
                               :territory grown-cell}
                        (> action-count 1)
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
            (core/success next-state [event]))))))))

(defn- resolve-disc-source-command
  ([state command]
   (resolve-disc-source-command state command {}))
  ([state command source-opts]
   (let [{:keys [player-id source disc-variant]} command]
     (cond
       (not (map? command))
       (core/failure :invalid-disc-command
                "Disc moves require a command map."
                {:command command})

       (nil? (get-in state [:players-by-id player-id]))
       (core/failure :unknown-player
                "Disc moves require a participating player."
                {:player-id player-id})

       (not (core/current-player-id? state player-id))
       (core/failure :not-current-player
                "Only the current player can resolve a Disc move."
                {:player-id player-id
                 :current-player-id (get-in state [:turn :current-player-id])})

       :else
       (resolve-disc-source state player-id source disc-variant source-opts)))))

(defn- source-cost-state [state player-id source-result]
  (core/apply-source-cost state
                          player-id
                          {:source-card (:source-card source-result)
                           :discard-source-card? (:discard-source-card?
                                                  source-result)}))

(defn- source-power-card [source-result]
  (or (:power-card source-result)
      (:source-card source-result)))

(defn- source-power-card-id [source-result]
  (:id (source-power-card source-result)))

(defn- paid-disc-source-opts [source-result]
  (cond-> {:source-card (:source-card source-result)
           :source-card-already-discarded? (:discard-source-card?
                                            source-result)}
    (:power-card source-result)
    (assoc :power-card (:power-card source-result))))

(defn- apply-resolved-disc-action [state player-id result opts]
  (case (get-in result [:command :target :kind])
    :piece
    (apply-disc-piece-growth state player-id result opts)

    :territory
    (apply-disc-territory-growth state player-id result opts)

    (core/failure :invalid-disc-target
             "Disc move targets must be :piece or :territory."
             {:target (get-in result [:command :target])})))

(defn- apply-single-disc-move
  ([state command]
   (apply-single-disc-move state command {}))
  ([state command {:keys [source-opts charge-source?]
                   :or {source-opts {}
                        charge-source? true}}]
   (let [result (resolve-disc-command* state command source-opts)]
     (if-not (:ok? result)
       result
       (apply-resolved-disc-action state
                                   (get-in result [:command :player-id])
                                   result
                                   {:charge-source? charge-source?})))))

(defn- disc-action-command [command action]
  (let [source (cond-> (:source command)
                 (:piece-id action)
                 (assoc :piece-id (:piece-id action)))]
    (merge (select-keys command [:player-id :disc-variant])
           (dissoc action :piece-id)
           {:source source})))

(defn- same-disc-target? [left-result right-result]
  (let [left (get-in left-result [:command :target])
        right (get-in right-result [:command :target])]
    (and (= (:kind left) (:kind right))
         (case (:kind left)
           :piece (= (:piece-id left) (:piece-id right))
           :territory (= (:board-index left) (:board-index right))
           false))))

(defn- strength-disc-actions [command]
  (let [actions (:disc-actions command)]
    (cond
      (not (sequential? actions))
      (core/failure :invalid-disc-actions
               "Strength Disc moves require a sequential :disc-actions collection."
               {:disc-actions actions})

      (not (<= 1 (count actions) 2))
      (core/failure :invalid-disc-actions
               "Strength can apply one or two Disc actions."
               {:action-count (count actions)
                :maximum 2})

      :else
      {:ok? true
       :actions (mapv #(disc-action-command command %) actions)})))

(defn- shortcut-result [left-result right-result]
  (let [right-command (:command right-result)]
    (cond-> (assoc left-result
                   :command (merge (:command left-result)
                                   (select-keys right-command
                                                [:replacement-card-source
                                                 :replacement-card-id
                                                 :orientation])))
      (:orientation right-command)
      (assoc-in [:command :target :orientation] (:orientation right-command)))))

(defn- apply-strength-shortcut [state player-id left-result right-result]
  (let [result (shortcut-result left-result right-result)]
    (case (get-in result [:command :target :kind])
      :piece
      (apply-disc-piece-growth state
                               player-id
                               result
                               {:action-count 2
                                :charge-source? false
                                :shortcut? true})

      :territory
      (apply-disc-territory-growth state
                                   player-id
                                   result
                                   {:action-count 2
                                    :charge-source? false
                                    :shortcut? true})

      (core/failure :invalid-disc-target
               "Strength Disc shortcuts require piece or territory targets."
               {:target (get-in result [:command :target])}))))

(defn- apply-strength-actions-sequential [state player-id actions source-opts]
  (loop [current-state state
         remaining actions
         events []]
    (if-let [action (first remaining)]
      (let [result (resolve-disc-command* current-state action source-opts)]
        (if-not (:ok? result)
          result
          (let [applied (apply-resolved-disc-action current-state
                                                    player-id
                                                    result
                                                    {:charge-source? false})]
            (if-not (:ok? applied)
              applied
              (recur (:state applied)
                     (rest remaining)
                     (into events (:events applied)))))))
      (core/success current-state events))))

(defn- apply-strength-disc-move
  ([state command]
   (apply-strength-disc-move state command {}))
  ([state command source-opts]
   (let [source-result (resolve-disc-source-command state command source-opts)]
    (if-not (:ok? source-result)
      source-result
      (let [actions-result (strength-disc-actions command)
            player-id (:player-id command)]
        (cond
          (not= "strength" (source-power-card-id source-result))
          (core/failure :disc-actions-unavailable
                   "Only Strength can apply multiple Disc actions."
                   {:card-id (source-power-card-id source-result)
                    :source (:source command)})

          (not (:ok? actions-result))
          actions-result

          :else
          (let [cost-state (source-cost-state state player-id source-result)
                actions (:actions actions-result)
                source-opts (paid-disc-source-opts source-result)]
            (if (= 2 (count actions))
              (let [left-result (resolve-disc-command* cost-state
                                                       (first actions)
                                                       source-opts)
                    right-result (when (:ok? left-result)
                                   (resolve-disc-command* cost-state
                                                          (second actions)
                                                          source-opts))]
                (cond
                  (not (:ok? left-result))
                  left-result

                  (not (:ok? right-result))
                  right-result

                  (same-disc-target? left-result right-result)
                  (apply-strength-shortcut cost-state
                                           player-id
                                           left-result
                                           right-result)

                  :else
                  (apply-strength-actions-sequential cost-state
                                                     player-id
                                                     actions
                                                     source-opts)))
              (apply-strength-actions-sequential cost-state
                                                 player-id
                                                 actions
                                                 source-opts)))))))))

(defn- apply-star-disc-move
  ([state command]
   (apply-star-disc-move state command {}))
  ([state command source-opts]
   (let [source-result (resolve-disc-source-command state command source-opts)]
    (if-not (:ok? source-result)
      source-result
      (if-not (= "star" (source-power-card-id source-result))
        (core/failure :disc-orient-unavailable
                 "Only Star Disc can orient a minion before applying Disc."
                 {:card-id (source-power-card-id source-result)
                  :source (:source command)})
        (let [orientation (:minion-orientation command)
              orient-result (placement/apply-orient-move
                             state
                             {:player-id (:player-id command)
                              :piece-id (get-in command [:source :piece-id])
                              :orientation orientation})]
          (if-not (:ok? orient-result)
            orient-result
            (let [disc-result (apply-single-disc-move
                               (:state orient-result)
                               (dissoc command :minion-orientation)
                               {:source-opts source-opts})]
              (if-not (:ok? disc-result)
                disc-result
                (core/success (:state disc-result)
                              (concat (:events orient-result)
                                      (:events disc-result))))))))))))

(defn- cup-created-piece-id [cup-result]
  (some (fn [event]
          (when (contains? #{:cup/small-piece-created}
                           (:type event))
            (get-in event [:piece :id])))
        (:events cup-result)))

(defn- cup-created-territory-index [cup-result]
  (some (fn [event]
          (when (= :cup/territory-created (:type event))
            (:board-index event)))
        (:events cup-result)))

(defn- sun-cup-command [command]
  (merge {:player-id (:player-id command)
          :source (:source command)
          :cup-variant :cup}
         (:cup command)))

(defn- sun-disc-target [disc-action cup-result]
  (case (get-in disc-action [:target :kind])
    :created-piece
    (when-let [piece-id (cup-created-piece-id cup-result)]
      {:kind :piece
       :piece-id piece-id})

    :created-territory
    (when-let [board-index (cup-created-territory-index cup-result)]
      {:kind :territory
       :board-index board-index})

    (:target disc-action)))

(defn- sun-disc-source [command disc-action cup-result]
  (let [created-piece-id (cup-created-piece-id cup-result)
        piece-id (or (:piece-id disc-action)
                     (when (= :created-piece (get-in disc-action [:target :kind]))
                       created-piece-id)
                     (get-in command [:source :piece-id]))]
    (assoc (:source command) :piece-id piece-id)))

(defn- sun-disc-command [command cup-result]
  (let [disc-action (:disc command)
        disc-target (sun-disc-target disc-action cup-result)]
    (when disc-target
      (-> (merge {:player-id (:player-id command)
                  :disc-variant :disc}
                 (dissoc disc-action :piece-id :target))
          (assoc :source (sun-disc-source command
                                          disc-action
                                          cup-result)
                 :target disc-target)))))

(defn- sun-created-piece-shortcut? [command]
  (and (= :territory (get-in command [:cup :target :kind]))
       (= :created-piece (get-in command [:disc :target :kind]))))

(defn- sun-created-territory-shortcut? [command]
  (and (= :wasteland (get-in command [:cup :target :kind]))
       (= :created-territory (get-in command [:disc :target :kind]))))

(defn- apply-sun-created-piece-shortcut [state command source-result]
  (let [player-id (:player-id command)
        target (get-in command [:cup :target])
        board-index (:board-index target)
        cell (core/board-cell-by-index state board-index)
        orientation (or (get-in command [:disc :orientation])
                        (get-in command [:cup :orientation]))
        target-result (when cell
                        (cup/validate-target-coordinate
                         state
                         source-result
                         target
                         (select-keys cell [:row :col])))
        target-pieces (when cell
                        (core/pieces-at-board-index state board-index))]
    (cond
      (nil? cell)
      (core/failure :invalid-target-territory
               "Sun piece shortcuts must target an existing territory."
               {:target target})

      (not (:ok? target-result))
      target-result

      (not (contains? pieces/legal-orientations orientation))
      (core/failure :invalid-orientation
               "Sun piece shortcuts require a legal final orientation."
               {:orientation orientation
                :legal-orientations pieces/legal-orientations})

      (<= pieces/max-pieces-per-space (count target-pieces))
      (core/failure :target-territory-full
               "Sun piece shortcuts require fewer than three pieces on the target territory."
               {:board-index board-index
                :maximum pieces/max-pieces-per-space})

      (not (pos? (core/stash-count state player-id :medium)))
      (core/failure :no-larger-piece-available
               "Sun piece shortcuts require a medium piece in the player's stash."
               {:player-id player-id
                :to-size :medium})

      :else
      (let [piece {:id (core/next-piece-id state player-id :medium)
                   :player-id player-id
                   :space-index board-index
                   :size :medium
                   :orientation orientation}
            event {:type :sun/piece-created-and-grown
                   :player-id player-id
                   :source (core/source-summary (:source source-result))
                   :cup-target {:kind :territory
                                :board-index board-index}
                   :disc-target {:kind :created-piece}
                   :shortcut? true
                   :piece piece}
            cost-state (source-cost-state state player-id source-result)
            next-state (-> cost-state
                           (core/decrement-stash player-id :medium)
                           (update-in [:pieces :on-board] conj piece)
                           (core/append-history event))]
        (core/success next-state [event])))))

(defn- apply-sun-created-territory-shortcut [state command source-result]
  (let [player-id (:player-id command)
        target (get-in command [:cup :target])
        {:keys [row col] :as normalized-target} (core/wasteland-target target)
        replacement-card-source (or (get-in command [:disc :replacement-card-source])
                                    :hand)
        replacement-card-id (get-in command [:disc :replacement-card-id])
        source-card (:source-card source-result)
        target-result (when normalized-target
                        (cup/validate-target-coordinate
                         state
                         source-result
                         normalized-target
                         normalized-target))]
    (cond
      (nil? normalized-target)
      (core/failure :invalid-cup-target
               "Sun territory shortcuts target an explicit wasteland coordinate."
               {:target target})

      (some? (core/board-cell-at state row col))
      (core/failure :target-not-wasteland
               "Sun territory shortcuts must target an empty wasteland space."
               {:target normalized-target})

      (not (core/wasteland-target? state normalized-target))
      (core/failure :target-not-wasteland
               "Sun territory shortcuts cannot target the void."
               {:target normalized-target})

      (not (:ok? target-result))
      target-result

      (seq (core/enemy-pieces-at-coordinate state player-id row col))
      (core/failure :wasteland-occupied-by-enemy
               "Sun territory shortcuts cannot target a wasteland occupied by enemy pieces."
               {:target normalized-target
                :enemy-piece-ids (mapv :id (core/enemy-pieces-at-coordinate
                                            state
                                            player-id
                                            row
                                            col))})

      (not= :hand replacement-card-source)
      (core/failure :invalid-disc-replacement-card-source
               "Sun territory shortcuts currently require a hand replacement card."
               {:replacement-card-source replacement-card-source
                :valid-sources #{:hand}})

      (= (:id source-card) replacement-card-id)
      (core/failure :card-already-used
               "A played source card cannot also become the shortcut territory."
               {:card-id replacement-card-id})

      :else
      (let [cost-state (source-cost-state state player-id source-result)
            replacement-card (core/player-hand-card cost-state
                                                    player-id
                                                    replacement-card-id)
            replacement-value (cards/card-point-value replacement-card)]
        (cond
          (nil? replacement-card)
          (core/failure :invalid-disc-replacement-card
                   "Sun territory shortcuts require a replacement card from the player's hand."
                   {:card-id replacement-card-id
                    :player-id player-id})

          (not= 2 replacement-value)
          (core/failure :invalid-disc-replacement-card
                   "Sun territory shortcuts require a royalty replacement card."
                   {:replacement-card-id (:id replacement-card)
                    :replacement-value replacement-value})

          :else
          (let [board-index (core/next-board-index state)
                cell {:index board-index
                      :row row
                      :col col
                      :orientation (board/orientation-for row col)
                      :face :up
                      :card replacement-card}
                event {:type :sun/territory-created-and-grown
                       :player-id player-id
                       :source (core/source-summary (:source source-result))
                       :target normalized-target
                       :board-index board-index
                       :replacement-card-id (:id replacement-card)
                       :to-value replacement-value
                       :shortcut? true
                       :territory cell}
                next-state (-> cost-state
                               (core/remove-card-from-hand player-id replacement-card-id)
                               (update :board conj cell)
                               (core/move-wasteland-pieces-to-board-index row col board-index)
                               (core/append-history event))]
            (core/success next-state [event])))))))

(defn- apply-sun-shortcut [state command source-result]
  (cond
    (sun-created-piece-shortcut? command)
    (apply-sun-created-piece-shortcut state command source-result)

    (sun-created-territory-shortcut? command)
    (apply-sun-created-territory-shortcut state command source-result)

    :else
    nil))

(defn apply-sun-move-with-opts
  ([state command]
   (apply-sun-move-with-opts state command {}))
  ([state command {:keys [source-opts]
                   :or {source-opts {}}}]
   (let [source-result (resolve-disc-source-command state command source-opts)]
    (if-not (:ok? source-result)
      source-result
      (cond
        (not= "sun" (source-power-card-id source-result))
        (core/failure :sun-actions-unavailable
                 "Only Sun can apply Cup followed by Disc."
                 {:card-id (source-power-card-id source-result)
                  :source (:source command)})

        (nil? (:cup command))
        (core/failure :invalid-sun-command
                 "Sun moves require a :cup action map."
                 {:command command})

        :else
        (if-let [shortcut-result (apply-sun-shortcut state command source-result)]
          shortcut-result
          (let [cup-result (cup/apply-cup-move-with-opts
                            state
                            (sun-cup-command command)
                            {:source-opts source-opts})]
            (if-not (:ok? cup-result)
              cup-result
              (if-not (:disc command)
                cup-result
                (if-let [disc-command (sun-disc-command command cup-result)]
                  (let [disc-result (apply-single-disc-move
                                     (:state cup-result)
                                     disc-command
                                     {:source-opts (paid-disc-source-opts
                                                    source-result)
                                      :charge-source? false})]
                    (if-not (:ok? disc-result)
                      disc-result
                      (core/success (:state disc-result)
                                    (concat (:events cup-result)
                                            (:events disc-result)))))
                  (core/failure :invalid-sun-disc-target
                           "Sun Disc targets that refer to a newly-created Cup target require that Cup action to create that target."
                           {:disc (:disc command)
                            :cup-events (:events cup-result)})))))))))))

(defn apply-sun-move [state command]
  (apply-sun-move-with-opts state command {}))

(defn apply-disc-move-with-opts
  ([state command]
   (apply-disc-move-with-opts state command {}))
  ([state command {:keys [source-opts] :as opts
                   :or {source-opts {}}}]
  (cond
    (contains? command :disc-actions)
    (apply-strength-disc-move state command source-opts)

    (contains? command :minion-orientation)
    (apply-star-disc-move state command source-opts)

    :else
    (apply-single-disc-move state command opts))))

(defn apply-disc-move [state command]
  (apply-disc-move-with-opts state command {}))
