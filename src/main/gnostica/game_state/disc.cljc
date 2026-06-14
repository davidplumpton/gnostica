(ns gnostica.game-state.disc
  (:require [gnostica.cards :as cards]
            [gnostica.game-state.card-source :as card-source]
            [gnostica.game-state.core :as core]
            [gnostica.game-state.suit-target :as suit-target]
            [gnostica.pieces :as pieces]))

(def disc-territory-card-sources #{:hand :discard-pile})

(def disc-source-config
  {:suit-label "Disc"
   :variant-key :disc-variant
   :variants-fn cards/disc-variants
   :variant-ids cards/disc-variant-ids
   :source-card-not-suit-code :source-card-not-disc
   :invalid-variant-code :invalid-disc-variant
   :variant-unavailable-code :disc-variant-unavailable
   :command-error-code :invalid-disc-command
   :direction-error-code :invalid-disc-direction
   :invalid-target-code :invalid-disc-target})

(def disc-replacement-card-config
  {:valid-sources disc-territory-card-sources
   :source-error-code :invalid-disc-replacement-card-source
   :source-error-message
   "Disc territory growth requires a supported replacement card source."
   :piece-error-code :invalid-disc-replacement
   :piece-error-message "Disc piece growth does not use a replacement card."
   :variant-key :disc-variant
   :discard-variant :disc-from-discard
   :discard-unavailable-code :disc-variant-option-unavailable
   :discard-unavailable-message
   "Only Star Disc can grow territory from a discard-pile replacement card."})

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

(defn- card-worth-more? [replacement-card original-card action-count]
  (let [original-value (cards/card-point-value original-card)
        replacement-value (cards/card-point-value replacement-card)]
    (and (some? original-value)
         (= (+ original-value action-count) replacement-value))))

(defn disc-target-coordinate [coordinate orientation]
  (suit-target/target-coordinate coordinate orientation))

(defn- resolve-disc-source
  ([state player-id source disc-variant]
   (resolve-disc-source state player-id source disc-variant {}))
  ([state player-id source disc-variant source-opts]
   (card-source/resolve-suit-source state
                                    player-id
                                    source
                                    disc-variant
                                    source-opts
                                    disc-source-config)))

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

(def disc-target-config
  (merge
   (select-keys disc-source-config [:suit-label :invalid-target-code])
   {:target-coordinate-fn disc-target-coordinate
    :target-map-error-message "Disc moves require a target map."
    :piece-id-error-message "Disc piece targets require a target piece id."
    :piece-missing-error-message
    "Disc piece targets must reference a piece on the board."
    :piece-space-error-message
    "Disc piece targets must have a board coordinate."
    :piece-coordinate-error-message
    "Disc piece targets must be the minion itself, occupy the current space for upright minions, or occupy the adjacent space in the minion direction."
    :territory-orientation-error-message
    "Disc territory growth does not take a piece orientation."
    :territory-coordinate-error-message
    "Disc territory targets must be the current space for upright minions or the adjacent space in the minion direction."
    :territory-occupied-error-message
    "Disc territory growth cannot target a territory occupied by enemy pieces."
    :target-kind-error-message "Disc move targets must be :piece or :territory."
    :piece-success-fn
    (fn [{:keys [player-id target-piece target-map target-options]}]
      (let [orientation-result (resolve-disc-orientation
                                player-id
                                target-piece
                                (:orientation target-options))]
        (if (:ok? orientation-result)
          {:ok? true
           :target (cond-> target-map
                     (:orientation orientation-result)
                     (assoc :orientation (:orientation orientation-result)))
           :target-piece target-piece}
          orientation-result)))
    :territory-success-fn
    (fn [{:keys [target-cell target-map]}]
      {:ok? true
       :target target-map
       :target-cell target-cell})}))

(defn- resolve-disc-target [state player-id source-result target orientation]
  (suit-target/resolve-target state
                              player-id
                              source-result
                              target
                              {:orientation orientation}
                              disc-target-config))

(defn- resolve-replacement-card-options
  [source-result target replacement-card-source replacement-card-id]
  (card-source/resolve-replacement-card-options
   source-result
   target
   {:replacement-card-source replacement-card-source
    :replacement-card-id replacement-card-id}
   disc-replacement-card-config))

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

(defn resolve-disc-command*
  [state command source-opts]
  (let [player-result (card-source/resolve-current-player-command
                       state
                       command
                       disc-source-config)]
    (if-not (:ok? player-result)
      player-result
      (let [player-id (:player-id player-result)
            source-result (resolve-disc-source state
                                               player-id
                                               (:source command)
                                               (:disc-variant command)
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
             replacement-card (card-source/replacement-card-from-source
                               cost-state
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
                                (card-source/remove-replacement-card
                                 player-id
                                 replacement-card-source
                                 replacement-card-id)
                                (card-source/replace-board-cell-card
                                 (:index target-cell)
                                 replacement-card)
                                (core/discard-card original-card)
                                (core/append-history event))]
             (core/success next-state [event]))))))))

(defn resolve-disc-source-command
  ([state command]
   (resolve-disc-source-command state command {}))
  ([state command source-opts]
   (let [player-result (card-source/resolve-current-player-command
                        state
                        command
                        disc-source-config)]
     (if-not (:ok? player-result)
       player-result
       (resolve-disc-source state
                            (:player-id player-result)
                            (:source command)
                            (:disc-variant command)
                            source-opts)))))

(defn source-cost-state [state player-id source-result]
  (core/apply-source-cost state
                          player-id
                          {:source-card (:source-card source-result)
                           :discard-source-card? (:discard-source-card?
                                                  source-result)}))

(defn source-power-card [source-result]
  (card-source/source-power-card source-result))

(defn source-power-card-id [source-result]
  (card-source/source-power-card-id source-result))

(defn paid-disc-source-opts [source-result]
  (card-source/paid-source-opts source-result))

(defn apply-resolved-disc-action [state player-id result opts]
  (case (get-in result [:command :target :kind])
    :piece
    (apply-disc-piece-growth state player-id result opts)

    :territory
    (apply-disc-territory-growth state player-id result opts)

    (core/failure :invalid-disc-target
                  "Disc move targets must be :piece or :territory."
                  {:target (get-in result [:command :target])})))

(defn apply-single-disc-move
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

(defn apply-disc-move-with-opts
  ([state command]
   (apply-disc-move-with-opts state command {}))
  ([state command opts]
   (apply-single-disc-move state command opts)))

(defn apply-disc-move [state command]
  (apply-disc-move-with-opts state command {}))
