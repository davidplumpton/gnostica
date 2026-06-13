(ns gnostica.game-state.card-source
  (:require [gnostica.game-state.core :as core]
            [gnostica.game-state.spatial :as spatial]
            [gnostica.pieces :as pieces]))

(defn discard-pile-card [state card-id]
  (some (fn [card]
          (when (= card-id (:id card))
            card))
        (:discard-pile state)))

(defn resolve-current-player-command
  [state command {:keys [suit-label command-error-code]}]
  (let [player-id (:player-id command)]
    (cond
      (not (map? command))
      (core/failure command-error-code
                    (str suit-label " moves require a command map.")
                    {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (core/failure :unknown-player
                    (str suit-label " moves require a participating player.")
                    {:player-id player-id})

      (not (core/current-player-id? state player-id))
      (core/failure :not-current-player
                    (str "Only the current player can resolve a "
                         suit-label
                         " move.")
                    {:player-id player-id
                     :current-player-id (get-in state
                                                [:turn :current-player-id])})

      :else
      {:ok? true
       :player-id player-id})))

(defn territory-target-cell
  [state target {:keys [suit-label invalid-target-code]}]
  (cond
    (not (map? target))
    (core/failure invalid-target-code
                  (str suit-label " territory targets require a target map.")
                  {:target target})

    (not= :territory (:kind target))
    (core/failure invalid-target-code
                  (str suit-label
                       " territory targets must use :kind :territory.")
                  {:target target})

    (some? (:board-index target))
    (if-let [cell (core/board-cell-by-index state (:board-index target))]
      {:ok? true
       :cell cell}
      (core/failure :invalid-target-territory
                    (str suit-label
                         " territory targets must reference an existing "
                         "board cell.")
                    {:target target}))

    (and (int? (:row target))
         (int? (:col target)))
    (if-let [cell (core/board-cell-at state (:row target) (:col target))]
      {:ok? true
       :cell cell}
      (core/failure :invalid-target-territory
                    (str suit-label
                         " territory targets must reference an existing "
                         "board cell.")
                    {:target target}))

    :else
    (core/failure invalid-target-code
                  (str suit-label
                       " territory targets require a board index or row "
                       "and column.")
                  {:target target})))

(defn- resolve-suit-variant
  [card requested-variant source
   {:keys [suit-label variant-key variants-fn variant-ids
           source-card-not-suit-code invalid-variant-code
           variant-unavailable-code]}]
  (let [variants (variants-fn card)
        variant-set (set variants)]
    (cond
      (empty? variants)
      (core/failure source-card-not-suit-code
                    (str "The source card does not provide a "
                         suit-label
                         " power.")
                    {:card-id (:id card)
                     :source source})

      (nil? requested-variant)
      {:ok? true
       variant-key (first variants)}

      (not (contains? variant-ids requested-variant))
      (core/failure invalid-variant-code
                    (str suit-label " moves require a known " suit-label
                         " variant.")
                    {variant-key requested-variant
                     :valid-variants variant-ids})

      (contains? variant-set requested-variant)
      {:ok? true
       variant-key requested-variant}

      :else
      (core/failure variant-unavailable-code
                    (str "The source card does not provide the selected "
                         suit-label
                         " variant.")
                    {:card-id (:id card)
                     variant-key requested-variant
                     :available-variants variants}))))

(defn resolve-suit-source
  ([state player-id source requested-variant config]
   (resolve-suit-source state player-id source requested-variant {} config))
  ([state player-id source requested-variant
    {:keys [source-card-already-discarded? source-card power-card
            allow-major-minion?]}
    {:keys [suit-label variant-key command-error-code direction-error-code
            piece-check-fn]
     :as config}]
   (let [piece (core/piece-by-id state (:piece-id source))
         piece-coordinate (when piece
                            (core/piece-coordinate state piece))]
     (cond
       (not (map? source))
       (core/failure command-error-code
                     (str suit-label " moves require a source map.")
                     {:source source})

       (nil? piece)
       (core/failure :invalid-piece
                     (str suit-label
                          " moves require one of the player's pieces as "
                          "the acting minion.")
                     {:piece-id (:piece-id source)})

       (not= player-id (:player-id piece))
       (core/failure :invalid-piece
                     "The acting minion must belong to the move's player."
                     {:piece-id (:piece-id source)
                      :player-id player-id
                      :piece-player-id (:player-id piece)})

       (nil? piece-coordinate)
       (core/failure :invalid-piece-space
                     (str suit-label
                          " moves require an acting minion with a board "
                          "coordinate.")
                     {:piece-id (:piece-id source)
                      :space-index (:space-index piece)
                      :space (:space piece)})

       (not (contains? pieces/legal-orientations (:orientation piece)))
       (core/failure direction-error-code
                     (str suit-label
                          " moves require the acting minion to have a legal "
                          "orientation.")
                     {:piece-id (:id piece)
                      :orientation (:orientation piece)
                      :legal-orientations pieces/legal-orientations})

       :else
       (if-let [piece-error (when piece-check-fn
                              (piece-check-fn piece))]
         piece-error
         (case (:kind source)
           :territory
           (let [cell (core/board-cell-by-index state (:board-index source))]
             (cond
               (nil? cell)
               (core/failure :invalid-source-territory
                             (str suit-label
                                  " territory sources must reference an "
                                  "existing board cell.")
                             {:board-index (:board-index source)})

               (and source-card
                    (not= (get-in cell [:card :id]) (:id source-card)))
               (core/failure :invalid-source-territory
                             (str suit-label
                                  " paid source cards must match the command "
                                  "source territory.")
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
                     variant-result (resolve-suit-variant power-card
                                                          requested-variant
                                                          source
                                                          config)]
                 (if (:ok? variant-result)
                   {:ok? true
                    :source source
                    :source-card paid-card
                    :power-card power-card
                    variant-key (get variant-result variant-key)
                    :piece piece
                    :piece-coordinate (spatial/coordinate-map piece-coordinate)
                    :orientation (:orientation piece)}
                   variant-result))))

           :hand-card
           (let [card (or source-card
                          (core/player-hand-card state
                                                 player-id
                                                 (:card-id source))
                          (when source-card-already-discarded?
                            (discard-pile-card state (:card-id source))))]
             (cond
               (nil? card)
               (core/failure :invalid-hand-card
                             (str suit-label
                                  " hand-card sources must reference a card "
                                  "in the player's hand.")
                             {:card-id (:card-id source)
                              :player-id player-id})

               (and source-card
                    (not= (:card-id source) (:id source-card)))
               (core/failure :invalid-hand-card
                             (str suit-label
                                  " paid source cards must match the command "
                                  "source card.")
                             {:card-id (:card-id source)
                              :source-card-id (:id source-card)})

               :else
               (let [power-card (or power-card card)
                     variant-result (resolve-suit-variant power-card
                                                          requested-variant
                                                          source
                                                          config)]
                 (if (:ok? variant-result)
                   {:ok? true
                    :source source
                    :source-card card
                    :power-card power-card
                    variant-key (get variant-result variant-key)
                    :discard-source-card? (not source-card-already-discarded?)
                    :piece piece
                    :piece-coordinate (spatial/coordinate-map piece-coordinate)
                    :orientation (:orientation piece)}
                   variant-result))))

           (core/failure command-error-code
                         (str suit-label
                              " move sources must be either :territory or "
                              ":hand-card.")
                         {:source source})))))))

(defn resolve-replacement-card-options
  [source-result target
   {:keys [destroyed? replacement-card-source replacement-card-id]}
   {:keys [valid-sources source-error-code source-error-message
           piece-error-code piece-error-message destroyed-error-code
           destroyed-error-message variant-key discard-variant
           discard-unavailable-code discard-unavailable-message]}]
  (if (= :territory (:kind target))
    (let [replacement-card-source (or replacement-card-source
                                      (when (some? replacement-card-id)
                                        :hand))]
      (cond
        (and destroyed?
             (or (some? replacement-card-source)
                 (some? replacement-card-id)))
        (core/failure destroyed-error-code
                      destroyed-error-message
                      {:target target
                       :replacement-card-source replacement-card-source
                       :replacement-card-id replacement-card-id})

        destroyed?
        {:ok? true}

        (nil? replacement-card-source)
        {:ok? true}

        (not (contains? valid-sources replacement-card-source))
        (core/failure source-error-code
                      source-error-message
                      {:replacement-card-source replacement-card-source
                       :valid-sources valid-sources})

        (and (= :discard-pile replacement-card-source)
             (not= discard-variant (get source-result variant-key)))
        (core/failure discard-unavailable-code
                      discard-unavailable-message
                      {variant-key (get source-result variant-key)
                       :replacement-card-source replacement-card-source})

        :else
        {:ok? true
         :replacement-card-source replacement-card-source
         :replacement-card-id replacement-card-id}))
    (if (or (some? replacement-card-source)
            (some? replacement-card-id))
      (core/failure piece-error-code
                    piece-error-message
                    {:target target
                     :replacement-card-source replacement-card-source
                     :replacement-card-id replacement-card-id})
      {:ok? true})))

(defn remove-card-from-discard [state card-id]
  (update state :discard-pile
          (fn [discard-pile]
            (vec (remove #(= card-id (:id %)) discard-pile)))))

(defn replace-board-cell-card [state board-index replacement-card]
  (update state :board
          (fn [cells]
            (mapv (fn [cell]
                    (if (= board-index (:index cell))
                      (assoc cell :card replacement-card)
                      cell))
                  cells))))

(defn replacement-card-from-source
  [state player-id replacement-card-source replacement-card-id]
  (case replacement-card-source
    :hand (core/player-hand-card state player-id replacement-card-id)
    :discard-pile (discard-pile-card state replacement-card-id)
    nil))

(defn remove-replacement-card
  [state player-id replacement-card-source replacement-card-id]
  (case replacement-card-source
    :hand (core/remove-card-from-hand state player-id replacement-card-id)
    :discard-pile (remove-card-from-discard state replacement-card-id)
    state))

(defn source-card-id [source-result]
  (get-in source-result [:source-card :id]))

(defn source-power-card [source-result]
  (or (:power-card source-result)
      (:source-card source-result)))

(defn source-power-card-id [source-result]
  (:id (source-power-card source-result)))

(defn paid-source-opts
  ([source-result]
   (paid-source-opts source-result (:power-card source-result)))
  ([source-result power-card]
   (cond-> {:source-card (:source-card source-result)
            :source-card-already-discarded? (= :hand-card
                                               (get-in source-result
                                                       [:source :kind]))
            :allow-major-minion? true}
     power-card
     (assoc :power-card power-card))))
