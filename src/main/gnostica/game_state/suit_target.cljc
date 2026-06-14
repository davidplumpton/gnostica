(ns gnostica.game-state.suit-target
  (:require [gnostica.game-state.card-source :as card-source]
            [gnostica.game-state.core :as core]
            [gnostica.game-state.spatial :as spatial]))

(defn target-coordinate [coordinate orientation]
  (spatial/target-coordinate coordinate orientation))

(defn targetable-coordinate?
  ([actor-coordinate candidate-coordinate orientation target-self?]
   (targetable-coordinate? actor-coordinate
                           candidate-coordinate
                           orientation
                           target-self?
                           target-coordinate))
  ([actor-coordinate candidate-coordinate orientation target-self?
    target-coordinate-fn]
   (or target-self?
       (spatial/same-coordinate? candidate-coordinate
                                 (target-coordinate-fn actor-coordinate
                                                       orientation)))))

(defn- expected-coordinate
  [source-coordinate source-orientation {:keys [target-coordinate-fn]}]
  ((or target-coordinate-fn target-coordinate) source-coordinate source-orientation))

(defn- piece-target-map [state target-piece target-coordinate]
  (let [target-cell (core/target-piece-territory-cell state target-piece)]
    {:target-cell target-cell
     :target-map (cond-> {:kind :piece
                          :piece-id (:id target-piece)
                          :player-id (:player-id target-piece)
                          :row (:row target-coordinate)
                          :col (:col target-coordinate)}
                   target-cell
                   (assoc :board-index (:index target-cell)))}))

(defn- territory-target-map [cell]
  {:kind :territory
   :board-index (:index cell)
   :row (:row cell)
   :col (:col cell)})

(defn- normalize-piece-target
  [state player-id source-result target-piece target-options
   {:keys [invalid-target-code piece-space-error-code
           piece-space-error-message piece-coordinate-error-message
           piece-error-fn piece-success-fn]
    :or {piece-space-error-code :invalid-piece-space}
    :as config}]
  (let [{:keys [piece]
         source-orientation :orientation
         source-coordinate :piece-coordinate} source-result
        target-coord (spatial/coordinate-map
                      (core/piece-coordinate state target-piece))
        target-self? (= (:id piece) (:id target-piece))]
    (if (nil? target-coord)
      (core/failure piece-space-error-code
                    piece-space-error-message
                    {:piece-id (:id target-piece)
                     :space-index (:space-index target-piece)
                     :space (:space target-piece)})
      (let [{:keys [target-cell target-map]} (piece-target-map state
                                                               target-piece
                                                               target-coord)
            context {:state state
                     :player-id player-id
                     :source-result source-result
                     :source-piece piece
                     :source-coordinate source-coordinate
                     :source-orientation source-orientation
                     :target-piece target-piece
                     :target-coordinate target-coord
                     :target-self? target-self?
                     :target-cell target-cell
                     :target-map target-map
                     :target-options target-options}]
        (if-let [piece-error (when piece-error-fn
                               (piece-error-fn context))]
          piece-error
          (if (not (targetable-coordinate? source-coordinate
                                           target-coord
                                           source-orientation
                                           target-self?
                                           (or (:target-coordinate-fn config)
                                               target-coordinate)))
            (core/failure invalid-target-code
                          piece-coordinate-error-message
                          {:piece-id (:id target-piece)
                           :orientation source-orientation
                           :source-coordinate source-coordinate
                           :target-coordinate target-coord
                           :expected-coordinate (expected-coordinate
                                                 source-coordinate
                                                 source-orientation
                                                 config)})
            (piece-success-fn context)))))))

(defn- normalize-territory-target
  [state player-id source-result target target-options
   {:keys [invalid-target-code territory-orientation-error-message
           territory-coordinate-error-message territory-occupied-error-message
           territory-success-fn]
    :as config}]
  (if (some? (:orientation target-options))
    (core/failure :invalid-orientation
                  territory-orientation-error-message
                  {:orientation (:orientation target-options)
                   :target target})
    (let [cell-result (card-source/territory-target-cell state target config)]
      (if (:ok? cell-result)
        (let [cell (:cell cell-result)
              cell-coordinate (select-keys cell [:row :col])
              {:keys [piece-coordinate]
               source-orientation :orientation
               source-piece :piece} source-result
              enemy-pieces (core/enemy-pieces-at-coordinate state
                                                            player-id
                                                            (:row cell)
                                                            (:col cell))
              target-map (territory-target-map cell)
              context {:state state
                       :player-id player-id
                       :source-result source-result
                       :source-piece source-piece
                       :source-coordinate piece-coordinate
                       :source-orientation source-orientation
                       :target target
                       :target-cell cell
                       :target-coordinate cell-coordinate
                       :target-map target-map
                       :target-options target-options}]
          (cond
            (not (targetable-coordinate? piece-coordinate
                                         cell-coordinate
                                         source-orientation
                                         false
                                         (or (:target-coordinate-fn config)
                                             target-coordinate)))
            (core/failure invalid-target-code
                          territory-coordinate-error-message
                          {:target target
                           :orientation source-orientation
                           :source-coordinate piece-coordinate
                           :target-coordinate cell-coordinate
                           :expected-coordinate (expected-coordinate
                                                 piece-coordinate
                                                 source-orientation
                                                 config)})

            (seq enemy-pieces)
            (core/failure :target-territory-occupied-by-enemy
                          territory-occupied-error-message
                          {:target target-map
                           :enemy-piece-ids (mapv :id enemy-pieces)})

            :else
            (territory-success-fn context)))
        cell-result))))

(defn resolve-target
  [state player-id source-result target target-options
   {:keys [invalid-target-code target-map-error-message
           piece-id-error-message piece-missing-error-message
           target-kind-error-message]
    :as config}]
  (cond
    (not (map? target))
    (core/failure invalid-target-code
                  target-map-error-message
                  {:target target})

    (= :piece (:kind target))
    (cond
      (nil? (:piece-id target))
      (core/failure invalid-target-code
                    piece-id-error-message
                    {:target target})

      :else
      (if-let [target-piece (core/piece-by-id state (:piece-id target))]
        (normalize-piece-target state
                                player-id
                                source-result
                                target-piece
                                target-options
                                config)
        (core/failure :invalid-target-piece
                      piece-missing-error-message
                      {:target target})))

    (= :territory (:kind target))
    (normalize-territory-target state
                                player-id
                                source-result
                                target
                                target-options
                                config)

    :else
    (core/failure invalid-target-code
                  target-kind-error-message
                  {:target (spatial/target-summary target)})))
