(ns gnostica.game-state.rod
  (:require [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.game-state.core :as core]
            [gnostica.pieces :as pieces]))

(def rod-modes #{:move-minion
                 :push-piece
                 :push-territory})

(def rod-direction-offsets
  {:north [-1 0]
   :east [0 1]
   :south [1 0]
   :west [0 -1]})

(defn- discard-pile-card [state card-id]
  (some (fn [card]
          (when (= card-id (:id card))
            card))
        (:discard-pile state)))

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

(defn rod-destination-coordinate [coordinate direction distance]
  (when-let [[row-offset col-offset] (get rod-direction-offsets direction)]
    (when-let [{:keys [row col]} (coordinate-map coordinate)]
      (when (int? distance)
        {:row (+ row (* row-offset distance))
         :col (+ col (* col-offset distance))}))))

(defn- same-coordinate? [left right]
  (= (coordinate-map left)
     (coordinate-map right)))

(defn- rod-targetable-coordinate? [actor-coordinate target-coordinate direction target-self?]
  (or target-self?
      (same-coordinate? target-coordinate
                        (rod-destination-coordinate actor-coordinate direction 1))))

(defn- target-summary [target]
  (select-keys target [:kind :piece-id :board-index :row :col]))

(defn- territory-target-cell [state target]
  (cond
    (not (map? target))
    (core/failure :invalid-rod-target
             "Rod territory targets require a target map."
             {:target target})

    (not= :territory (:kind target))
    (core/failure :invalid-rod-target
             "Rod territory targets must use :kind :territory."
             {:target target})

    (some? (:board-index target))
    (if-let [cell (core/board-cell-by-index state (:board-index target))]
      {:ok? true
       :cell cell}
      (core/failure :invalid-target-territory
               "Rod territory targets must reference an existing board cell."
               {:target target}))

    (and (int? (:row target))
         (int? (:col target)))
    (if-let [cell (core/board-cell-at state (:row target) (:col target))]
      {:ok? true
       :cell cell}
      (core/failure :invalid-target-territory
               "Rod territory targets must reference an existing board cell."
               {:target target}))

    :else
    (core/failure :invalid-rod-target
             "Rod territory targets require a board index or row and column."
             {:target target})))

(defn- resolve-rod-variant [card requested-variant source]
  (let [variants (cards/rod-variants card)
        variant-set (set variants)]
    (cond
      (empty? variants)
      (core/failure :source-card-not-rod
               "The source card does not provide a Rod power."
               {:card-id (:id card)
                :source source})

      (nil? requested-variant)
      {:ok? true
       :rod-variant (first variants)}

      (not (contains? cards/rod-variant-ids requested-variant))
      (core/failure :invalid-rod-variant
               "Rod moves require a known Rod variant."
               {:rod-variant requested-variant
                :valid-variants cards/rod-variant-ids})

      (contains? variant-set requested-variant)
      {:ok? true
       :rod-variant requested-variant}

      :else
      (core/failure :rod-variant-unavailable
               "The source card does not provide the selected Rod variant."
               {:card-id (:id card)
                :rod-variant requested-variant
                :available-variants variants}))))

(defn- resolve-rod-source
  ([state player-id source rod-variant]
   (resolve-rod-source state player-id source rod-variant {}))
  ([state player-id source rod-variant
    {:keys [source-card source-card-already-discarded? allow-major-minion?]}]
   (let [piece (core/piece-by-id state (:piece-id source))
         piece-coordinate (when piece
                            (core/piece-coordinate state piece))]
     (cond
       (not (map? source))
       (core/failure :invalid-rod-command
                "Rod moves require a source map."
                {:source source})

       (nil? piece)
       (core/failure :invalid-piece
                "Rod moves require one of the player's pieces as the acting minion."
                {:piece-id (:piece-id source)})

       (not= player-id (:player-id piece))
       (core/failure :invalid-piece
                "The acting minion must belong to the move's player."
                {:piece-id (:piece-id source)
                 :player-id player-id
                 :piece-player-id (:player-id piece)})

       (nil? piece-coordinate)
       (core/failure :invalid-piece-space
                "Rod moves require an acting minion with a board coordinate."
                {:piece-id (:piece-id source)
                 :space-index (:space-index piece)
                 :space (:space piece)})

       (= :up (:orientation piece))
       (core/failure :rod-minion-upright
                "A piece standing upright may not use a Rod."
                {:piece-id (:id piece)
                 :orientation (:orientation piece)})

       (not (contains? pieces/cardinal-orientations (:orientation piece)))
       (core/failure :invalid-rod-direction
                "Rod moves require the acting minion to point in a cardinal direction."
                {:piece-id (:id piece)
                 :orientation (:orientation piece)
                 :legal-directions pieces/cardinal-orientations})

       (= :territory (:kind source))
       (let [cell (core/board-cell-by-index state (:board-index source))]
         (cond
           (nil? cell)
           (core/failure :invalid-source-territory
                    "Rod territory sources must reference an existing board cell."
                    {:board-index (:board-index source)})

           (and (not allow-major-minion?)
                (not= (:board-index source) (:space-index piece)))
           (core/failure :source-piece-mismatch
                    "The acting minion must occupy the source territory."
                    {:piece-id (:piece-id source)
                     :piece-space-index (:space-index piece)
                     :source-board-index (:board-index source)})

           :else
           (let [variant-result (resolve-rod-variant (:card cell)
                                                     rod-variant
                                                     source)]
             (if (:ok? variant-result)
               {:ok? true
                :source source
                :source-card (:card cell)
                :rod-variant (:rod-variant variant-result)
                :piece piece
                :piece-coordinate (coordinate-map piece-coordinate)
                :direction (:orientation piece)}
               variant-result))))

       (= :hand-card (:kind source))
       (let [card (or source-card
                      (core/player-hand-card state player-id (:card-id source))
                      (when source-card-already-discarded?
                        (discard-pile-card state (:card-id source))))]
         (cond
           (nil? card)
           (core/failure :invalid-hand-card
                    "Rod hand-card sources must reference a card in the player's hand."
                    {:card-id (:card-id source)
                     :player-id player-id})

           (and source-card
                (not= (:card-id source) (:id source-card)))
           (core/failure :invalid-hand-card
                    "Rod paid source cards must match the command source card."
                    {:card-id (:card-id source)
                     :source-card-id (:id source-card)})

           :else
           (let [variant-result (resolve-rod-variant card rod-variant source)]
             (if (:ok? variant-result)
               {:ok? true
                :source source
                :source-card card
                :rod-variant (:rod-variant variant-result)
                :discard-source-card? (not source-card-already-discarded?)
                :piece piece
                :piece-coordinate (coordinate-map piece-coordinate)
                :direction (:orientation piece)}
               variant-result))))

       :else
       (core/failure :invalid-rod-command
                "Rod move sources must be either :territory or :hand-card."
                {:source source})))))

(defn- resolve-rod-distance [piece distance]
  (let [maximum (or (pieces/pips piece) 0)]
    (cond
      (not (int? distance))
      (core/failure :invalid-rod-distance
               "Rod moves require an integer distance."
               {:distance distance
                :maximum maximum})

      (not (pos? distance))
      (core/failure :invalid-rod-distance
               "Rod moves cannot move zero spaces."
               {:distance distance
                :maximum maximum})

      (< maximum distance)
      (core/failure :invalid-rod-distance
               "Rod moves cannot exceed the acting minion's pip count."
               {:distance distance
                :maximum maximum})

      :else
      {:ok? true
       :distance distance
       :maximum maximum})))

(defn- resolve-rod-orientation [player-id moved-piece orientation]
  (cond
    (nil? orientation)
    {:ok? true}

    (not= player-id (:player-id moved-piece))
    (core/failure :invalid-orientation
             "Enemy pieces retain their original orientation when moved by a Rod."
             {:piece-id (:id moved-piece)
              :piece-player-id (:player-id moved-piece)
              :orientation orientation})

    (not (contains? pieces/legal-orientations orientation))
    (core/failure :invalid-orientation
             "Rod moves can only reorient current-player pieces to a legal orientation."
             {:piece-id (:id moved-piece)
              :orientation orientation
              :legal-orientations pieces/legal-orientations})

    :else
    {:ok? true
     :orientation orientation}))

(defn- normalize-rod-piece-target [state player-id source-result target-piece distance orientation]
  (let [{:keys [piece direction]
         source-coordinate :piece-coordinate} source-result
        target-coordinate (coordinate-map (core/piece-coordinate state target-piece))
        target-self? (= (:id piece) (:id target-piece))]
    (cond
      (nil? target-coordinate)
      (core/failure :invalid-piece-space
               "Rod piece targets must have a board coordinate."
               {:piece-id (:id target-piece)
                :space-index (:space-index target-piece)
                :space (:space target-piece)})

      (not (rod-targetable-coordinate? source-coordinate target-coordinate direction target-self?))
      (core/failure :invalid-rod-target
               "Rod piece targets must be the minion itself or occupy the adjacent space in the minion direction."
               {:piece-id (:id target-piece)
                :direction direction
                :source-coordinate source-coordinate
                :target-coordinate target-coordinate
                :expected-coordinate (rod-destination-coordinate source-coordinate direction 1)})

      :else
      (let [orientation-result (resolve-rod-orientation player-id target-piece orientation)]
        (if (:ok? orientation-result)
          {:ok? true
           :target (cond-> {:kind :piece
                            :piece-id (:id target-piece)
                            :player-id (:player-id target-piece)
                            :row (:row target-coordinate)
                            :col (:col target-coordinate)
                            :destination (rod-destination-coordinate target-coordinate
                                                                     direction
                                                                     distance)}
                     (:orientation orientation-result)
                     (assoc :orientation (:orientation orientation-result)))
           :target-piece target-piece}
          orientation-result)))))

(defn- resolve-rod-target [state player-id source-result mode target distance orientation]
  (case mode
    :move-minion
    (let [piece (:piece source-result)]
      (if (and (some? target)
               (not= {:kind :piece
                      :piece-id (:id piece)}
                     (target-summary target)))
        (core/failure :invalid-rod-target
                 "Move-minion Rod commands move the acting minion and do not need another target."
                 {:target target
                  :piece-id (:id piece)})
        (normalize-rod-piece-target state
                                    player-id
                                    source-result
                                    piece
                                    distance
                                    orientation)))

    :push-piece
    (cond
      (not (map? target))
      (core/failure :invalid-rod-target
               "Push-piece Rod commands require a target piece map."
               {:target target})

      (not= :piece (:kind target))
      (core/failure :invalid-rod-target
               "Push-piece Rod commands target :kind :piece."
               {:target target})

      (nil? (:piece-id target))
      (core/failure :invalid-rod-target
               "Push-piece Rod commands require a target piece id."
               {:target target})

      :else
      (if-let [target-piece (core/piece-by-id state (:piece-id target))]
        (normalize-rod-piece-target state
                                    player-id
                                    source-result
                                    target-piece
                                    distance
                                    orientation)
        (core/failure :invalid-target-piece
                 "Push-piece Rod commands must reference a piece on the board."
                 {:target target})))

    :push-territory
    (if (some? orientation)
      (core/failure :invalid-orientation
               "Rod territory pushes do not take a piece orientation."
               {:orientation orientation
                :target target})
      (let [cell-result (territory-target-cell state target)]
        (if (:ok? cell-result)
          (let [cell (:cell cell-result)
                cell-coordinate (select-keys cell [:row :col])
                {:keys [piece-coordinate direction]} source-result]
            (if (rod-targetable-coordinate? piece-coordinate
                                            cell-coordinate
                                            direction
                                            false)
              {:ok? true
               :target {:kind :territory
                        :board-index (:index cell)
                        :row (:row cell)
                        :col (:col cell)
                        :destination (rod-destination-coordinate cell-coordinate
                                                                 direction
                                                                 distance)}
               :target-cell cell}
              (core/failure :invalid-rod-target
                       "Rod territory targets must occupy the adjacent space in the minion direction."
                       {:target target
                        :direction direction
                        :source-coordinate piece-coordinate
                        :target-coordinate cell-coordinate
                        :expected-coordinate (rod-destination-coordinate piece-coordinate direction 1)})))
          cell-result)))))

(defn- resolve-rod-command* [state command source-opts]
  (let [{:keys [player-id source mode target distance orientation direction rod-variant]} command]
    (cond
      (not (map? command))
      (core/failure :invalid-rod-command
               "Rod moves require a command map."
               {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (core/failure :unknown-player
               "Rod moves require a participating player."
               {:player-id player-id})

      (not (core/current-player-id? state player-id))
      (core/failure :not-current-player
               "Only the current player can resolve a Rod move."
               {:player-id player-id
                :current-player-id (get-in state [:turn :current-player-id])})

      (not (contains? rod-modes mode))
      (core/failure :invalid-rod-mode
               "Rod moves require a supported mode."
               {:mode mode
                :supported-modes rod-modes})

      :else
      (let [source-result (resolve-rod-source state player-id source rod-variant source-opts)]
        (if-not (:ok? source-result)
          source-result
          (let [minion-direction (:direction source-result)]
            (cond
              (and (some? direction)
                   (not= direction minion-direction))
              (core/failure :invalid-rod-direction
                       "Rod command direction must match the acting minion orientation."
                       {:direction direction
                        :minion-direction minion-direction
                        :piece-id (get-in source-result [:piece :id])})

              :else
              (let [distance-result (resolve-rod-distance (:piece source-result)
                                                          distance)]
                (if-not (:ok? distance-result)
                  distance-result
                  (let [target-result (resolve-rod-target state
                                                          player-id
                                                          source-result
                                                          mode
                                                          target
                                                          (:distance distance-result)
                                                          orientation)]
                    (if-not (:ok? target-result)
                      target-result
                      (let [normalized-command (cond-> {:player-id player-id
                                                        :source (core/source-summary source)
                                                        :rod-variant (:rod-variant source-result)
                                                        :mode mode
                                                        :target (:target target-result)
                                                        :distance (:distance distance-result)
                                                        :direction minion-direction}
                                                 (:orientation (:target target-result))
                                                 (assoc :orientation (:orientation (:target target-result))))]
                        (merge {:ok? true
                                :command normalized-command
                                :source-card (:source-card source-result)
                                :discard-source-card? (:discard-source-card?
                                                       source-result)
                                :piece (:piece source-result)}
                               (select-keys target-result
                                            [:target-piece :target-cell]))))))))))))))

(defn resolve-rod-command [state command]
  (resolve-rod-command* state command {}))

(defn- rod-unbounded? [rod-variant]
  (= :rod-unbounded rod-variant))

(defn- rod-destination-space
  [state moved-piece rod-variant {:keys [row col] :as destination}
   {:keys [allow-full-destination?] :as _opts}]
  (cond
    (not (and (int? row) (int? col)))
    (core/failure :invalid-rod-destination
             "Rod piece movement requires a destination coordinate."
             {:piece-id (:id moved-piece)
              :destination destination})

    :else
    (if-let [cell (core/board-cell-at state row col)]
      (let [space-pieces (remove #(= (:id moved-piece) (:id %))
                                 (core/pieces-at-coordinate state row col))]
        (if (and (not allow-full-destination?)
                 (not (rod-unbounded? rod-variant))
                 (<= pieces/max-pieces-per-space (count space-pieces)))
          (core/failure :target-territory-full
                   "Rod piece movement requires fewer than three pieces on the destination territory."
                   {:piece-id (:id moved-piece)
                    :board-index (:index cell)
                    :row row
                    :col col
                    :piece-ids (mapv :id space-pieces)
                    :maximum pieces/max-pieces-per-space})
          {:ok? true
           :piece-space {:space-index (:index cell)}
           :destination {:kind :territory
                         :board-index (:index cell)
                         :row row
                         :col col}}))
      (let [target {:kind :wasteland
                    :row row
                    :col col}]
        (if (core/wasteland-target? state target)
          {:ok? true
           :piece-space {:space target}
           :destination target}
          (core/failure :rod-destination-void
                   "Rod piece movement cannot end in the void."
                   {:piece-id (:id moved-piece)
                    :destination destination}))))))

(defn- rod-territory-destination-space [state player-id {:keys [row col] :as destination}]
  (cond
    (not (and (int? row) (int? col)))
    (core/failure :invalid-rod-destination
             "Rod territory pushing requires a destination coordinate."
             {:destination destination})

    (some? (core/board-cell-at state row col))
    (core/failure :target-not-wasteland
             "Rod territory pushing must land in an empty wasteland space."
             {:destination destination})

    (not (core/wasteland-target? state {:kind :wasteland
                                   :row row
                                   :col col}))
    (core/failure :rod-destination-void
             "Rod territory pushing cannot land in the void."
             {:destination destination})

    (seq (core/enemy-pieces-at-coordinate state player-id row col))
    (core/failure :wasteland-occupied-by-enemy
             "Rod territory pushing cannot land on a wasteland occupied by enemy pieces."
             {:destination destination
              :enemy-piece-ids (mapv :id (core/enemy-pieces-at-coordinate state player-id row col))})

    :else
    {:ok? true
     :destination {:kind :wasteland
                   :row row
                   :col col}}))

(defn- move-piece-to-space [piece piece-space orientation]
  (let [piece (cond-> piece
                orientation (assoc :orientation orientation))]
    (if-let [space-index (:space-index piece-space)]
      (-> piece
          (dissoc :space)
          (assoc :space-index space-index))
      (-> piece
          (dissoc :space-index)
          (assoc :space (:space piece-space))))))

(defn- apply-rod-piece-move
  [state player-id {:keys [command source-card discard-source-card? target-piece]} opts]
  (let [{:keys [mode source target distance direction rod-variant]} command
        destination-result (rod-destination-space state
                                                  target-piece
                                                  rod-variant
                                                  (:destination target)
                                                  opts)]
    (if-not (:ok? destination-result)
      destination-result
      (let [moved-piece (move-piece-to-space target-piece
                                             (:piece-space destination-result)
                                             (:orientation target))
            event {:type (case mode
                           :move-minion :rod/minion-moved
                           :push-piece :rod/piece-pushed)
                   :player-id player-id
                   :source source
                   :rod-variant rod-variant
                   :target (select-keys target [:kind :piece-id :player-id :row :col])
                   :destination (:destination destination-result)
                   :distance distance
                   :direction direction
                   :piece moved-piece}
            source-cost {:source-card source-card
                         :discard-source-card? discard-source-card?}
            next-state (-> state
                           (core/apply-source-cost player-id source-cost)
                           (core/replace-piece moved-piece)
                           (core/append-history event))]
        (core/success next-state [event])))))

(defn- apply-rod-territory-push
  [state player-id {:keys [command source-card discard-source-card? target-cell]}]
  (let [{:keys [source target distance direction rod-variant]} command
        {:keys [row col]} target-cell
        destination-result (rod-territory-destination-space state
                                                           player-id
                                                           (:destination target))
        enemy-pieces (core/enemy-pieces-at-coordinate state player-id row col)]
    (cond
      (seq enemy-pieces)
      (core/failure :target-territory-occupied-by-enemy
               "Rod territory pushing cannot target a territory occupied by enemy pieces."
               {:target (select-keys target [:kind :board-index :row :col])
                :enemy-piece-ids (mapv :id enemy-pieces)})

      (not (:ok? destination-result))
      destination-result

      :else
      (let [destination (:destination destination-result)
            moved-territory (assoc target-cell
                                   :row (:row destination)
                                   :col (:col destination)
                                   :orientation (board/orientation-for (:row destination)
                                                                       (:col destination)))
            event {:type :rod/territory-pushed
                   :player-id player-id
                   :source source
                   :rod-variant rod-variant
                   :target (select-keys target [:kind :board-index :row :col])
                   :destination destination
                   :distance distance
                   :direction direction
                   :territory moved-territory}
            source-cost {:source-card source-card
                         :discard-source-card? discard-source-card?}
            next-state (-> state
                           (core/apply-source-cost player-id source-cost)
                           (core/move-board-index-pieces-to-wasteland (:index target-cell) row col)
                           (core/move-territory-cell (:index target-cell)
                                                (:row destination)
                                                (:col destination))
                           (core/move-wasteland-pieces-to-board-index (:row destination)
                                                                 (:col destination)
                                                                 (:index target-cell))
                           (core/append-history event))]
        (core/success next-state [event])))))

(defn apply-rod-move-with-opts
  ([state command]
   (apply-rod-move-with-opts state command {}))
  ([state command {:keys [source-opts allow-full-destination?]
                   :or {source-opts {}}}]
   (let [result (resolve-rod-command* state command source-opts)]
     (if-not (:ok? result)
       result
       (let [normalized-command (:command result)
             player-id (:player-id normalized-command)
             opts {:allow-full-destination? allow-full-destination?}]
         (case (:mode normalized-command)
           (:move-minion :push-piece)
           (apply-rod-piece-move state player-id result opts)

           :push-territory
           (apply-rod-territory-push state player-id result)))))))

(defn apply-rod-move [state command]
  (apply-rod-move-with-opts state command {}))
