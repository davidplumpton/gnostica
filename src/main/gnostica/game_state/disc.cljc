(ns gnostica.game-state.disc
  (:require [gnostica.cards :as cards]
            [gnostica.game-state.core :as core]
            [gnostica.pieces :as pieces]))

(def disc-territory-card-sources #{:hand :discard-pile})

(def disc-direction-offsets
  {:north [-1 0]
   :east [0 1]
   :south [1 0]
   :west [0 -1]})

(def next-disc-piece-size
  {:small :medium
   :medium :large})

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

(defn- resolve-disc-source [state player-id source disc-variant]
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

          (not= (:board-index source) (:space-index piece))
          (core/failure :source-piece-mismatch
                   "The acting minion must occupy the source territory."
                   {:piece-id (:piece-id source)
                    :piece-space-index (:space-index piece)
                    :source-board-index (:board-index source)})

          :else
          (let [variant-result (resolve-disc-variant (:card cell)
                                                     disc-variant
                                                     source)]
            (if (:ok? variant-result)
              {:ok? true
               :source source
               :source-card (:card cell)
               :disc-variant (:disc-variant variant-result)
               :piece piece
               :piece-coordinate (coordinate-map piece-coordinate)
               :orientation (:orientation piece)}
              variant-result))))

      (= :hand-card (:kind source))
      (let [card (core/player-hand-card state player-id (:card-id source))]
        (cond
          (nil? card)
          (core/failure :invalid-hand-card
                   "Disc hand-card sources must reference a card in the player's hand."
                   {:card-id (:card-id source)
                    :player-id player-id})

          :else
          (let [variant-result (resolve-disc-variant card disc-variant source)]
            (if (:ok? variant-result)
              {:ok? true
               :source source
               :source-card card
               :disc-variant (:disc-variant variant-result)
               :discard-source-card? true
               :piece piece
               :piece-coordinate (coordinate-map piece-coordinate)
               :orientation (:orientation piece)}
              variant-result))))

      :else
      (core/failure :invalid-disc-command
               "Disc move sources must be either :territory or :hand-card."
               {:source source}))))

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

(defn resolve-disc-command [state command]
  (let [{:keys [player-id source target orientation disc-variant
                replacement-card-source replacement-card-id]} command]
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
      (let [source-result (resolve-disc-source state player-id source disc-variant)]
        (if-not (:ok? source-result)
          source-result
          (let [target-result (resolve-disc-target state
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
                                                    :source (core/source-summary source)
                                                    :disc-variant (:disc-variant source-result)
                                                    :target (:target target-result)}
                                             (:orientation (:target target-result))
                                             (assoc :orientation (:orientation (:target target-result)))

                                             (:replacement-card-source replacement-result)
                                             (assoc :replacement-card-source
                                                    (:replacement-card-source replacement-result))

                                             (:replacement-card-id replacement-result)
                                             (assoc :replacement-card-id
                                                    (:replacement-card-id replacement-result)))]
	                    (merge {:ok? true
	                            :command normalized-command
	                            :source-card (:source-card source-result)
	                            :piece (:piece source-result)}
	                           (select-keys target-result
	                                        [:target-piece :target-cell]))))))))))))

(defn- piece-space [piece]
  (select-keys piece [:space-index :space]))

(defn- discard-pile-card [state card-id]
  (some (fn [card]
          (when (= card-id (:id card))
            card))
        (:discard-pile state)))

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

(defn- apply-disc-piece-growth [state player-id {:keys [command source-card target-piece]}]
  (let [{:keys [source target disc-variant]} command
        owner-id (:player-id target-piece)
        old-size (:size target-piece)
        next-size (get next-disc-piece-size old-size)]
    (cond
      (nil? next-size)
      (core/failure :target-piece-max-size
               "Disc piece growth cannot grow a large piece."
               {:piece-id (:id target-piece)
                :size old-size})

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
            event {:type :disc/piece-grown
                   :player-id player-id
                   :source source
                   :disc-variant disc-variant
                   :target (select-keys target [:kind :piece-id :player-id :board-index :row :col])
                   :from-size old-size
                   :to-size next-size
                   :replaced-piece target-piece
                   :piece grown-piece}
            source-cost {:source-card source-card
                         :discard-source-card? (= :hand-card (:kind source))}
            next-state (-> state
                           (core/apply-source-cost player-id source-cost)
                           (core/increment-stash owner-id old-size)
                           (core/decrement-stash owner-id next-size)
                           (core/replace-piece-by-id (:id target-piece) grown-piece)
                           (core/append-history event))]
        (core/success next-state [event])))))

(defn- apply-disc-territory-growth [state player-id {:keys [command source-card target-cell]}]
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
                         :discard-source-card? (= :hand-card (:kind source))}
            cost-state (core/apply-source-cost state player-id source-cost)
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

          (not (cards/card-worth-one-more? replacement-card original-card))
          (core/failure :invalid-disc-replacement-card
                   "Disc territory growth requires a replacement card worth exactly one more point than the original territory."
                   {:original-card-id (:id original-card)
                    :original-value original-value
                    :replacement-card-id (:id replacement-card)
                    :replacement-value replacement-value})

          :else
          (let [grown-cell (assoc target-cell :card replacement-card)
                event {:type :disc/territory-grown
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
                next-state (-> cost-state
                               (remove-replacement-card player-id
                                                        replacement-card-source
                                                        replacement-card-id)
                               (replace-board-cell-card (:index target-cell)
                                                        replacement-card)
                               (core/discard-card original-card)
                               (core/append-history event))]
            (core/success next-state [event])))))))

(defn apply-disc-move [state command]
  (let [result (resolve-disc-command state command)]
    (if-not (:ok? result)
      result
      (let [normalized-command (:command result)
            player-id (:player-id normalized-command)]
        (case (get-in normalized-command [:target :kind])
          :piece
          (apply-disc-piece-growth state player-id result)

          :territory
          (apply-disc-territory-growth state player-id result)

          (core/failure :invalid-disc-target
                   "Disc move targets must be :piece or :territory."
                   {:target (:target normalized-command)}))))))
