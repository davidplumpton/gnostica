(ns gnostica.move-selection.target-options
  (:require [gnostica.move-selection.context :as context]
            [gnostica.move-selection.options :as options]
            [gnostica.pieces :as pieces]))

(def required-context-keys
  #{:board
    :cup-move?
    :cup-target-db
    :cup-target-territory?
    :current-player-hand
    :current-player-pieces
    :current-player-pieces-on-space
    :current-player-territory-source-options
    :disc-move?
    :disc-replacement-card-options-for
    :disc-replacement-card-source-option-ids
    :disc-territory-target?
    :empty-board-target?
    :hermit-move?
    :hermit-piece-target-selected?
    :hermit-target-territory?
    :hermit-territory-target-selected?
    :move-params
    :move-selection
    :move-source
    :move-world-copy-options
    :one-point-card-options-for
    :rod-move?
    :rod-territory-target?
    :sun-cup-needs-one-point-card?
    :sun-disc-replacement-card-options-for
    :sun-disc-territory-target?
    :sun-disc-territory-target-stage?
    :sun-move?
    :sword-damage-options-for
    :sword-move?
    :sword-replacement-card-options-for
    :sword-replacement-card-source-option-ids
    :sword-territory-target?
    :territory-card-source-option-ids
    :valid-board-index?
    :world-copy-board-cell
    :world-move?})

(defn make-context [deps]
  (context/make "gnostica.move-selection.target-options" required-context-keys deps))

(defn- call [ctx key & args]
  (apply context/call ctx key args))

(defn move-damage-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (call ctx :sword-damage-options-for db source params)))

(defn move-piece-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (case source
      :activate-territory
      (if (call ctx :valid-board-index? db (:source-board-index params))
        (call ctx :current-player-pieces-on-space db (:source-board-index params))
        [])

      (:play-hand-card :orient-piece)
      (call ctx :current-player-pieces db)

      [])))

(defn move-hand-card-options [ctx db]
  (if (= :play-hand-card (call ctx :move-source db))
    (call ctx :current-player-hand db)
    []))

(defn move-discard-card-options [ctx db]
  (if (= :draw-cards (call ctx :move-source db))
    (call ctx :current-player-hand db)
    []))

(defn move-source-board-options [ctx db]
  (call ctx :current-player-territory-source-options db))

(defn move-target-board-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (cond
      (= :place-initial-small source)
      (filterv #(call ctx :empty-board-target? db (:index %))
               (call ctx :board db))

      (and (call ctx :world-move? db source params)
           (nil? (call ctx :world-copy-board-cell db
                       (:copied-board-index params))))
      (call ctx :move-world-copy-options db)

      (call ctx :sun-disc-territory-target-stage? db source params)
      (filterv #(call ctx :sun-disc-territory-target? db source params %)
               (call ctx :board db))

      (or (call ctx :cup-move? db source params)
          (call ctx :sun-move? db source params))
      (let [target-db (call ctx :cup-target-db db source params)]
        (filterv #(call ctx :cup-target-territory? db source params %)
                 (call ctx :board target-db)))

      (and (call ctx :disc-move? db source params)
           (= :territory (:disc-target-kind params)))
      (filterv #(call ctx :disc-territory-target? db source params %)
               (call ctx :board db))

      (and (call ctx :sword-move? db source params)
           (= :territory (:sword-target-kind params)))
      (filterv #(call ctx :sword-territory-target? db source params %)
               (call ctx :board db))

      (and (call ctx :rod-move? db source params)
           (= :push-territory (:rod-mode params)))
      (filterv #(call ctx :rod-territory-target? db source params %)
               (call ctx :board db))

      (call ctx :hermit-move? db source params)
      (cond
        (call ctx :hermit-piece-target-selected? params)
        (filterv #(call ctx :empty-board-target? db (:index %))
                 (call ctx :board db))

        (call ctx :hermit-territory-target-selected? params)
        []

        :else
        (filterv #(call ctx :hermit-target-territory? db params %)
                 (call ctx :board db)))

      :else
      (call ctx :board db))))

(defn move-one-point-card-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (if (or (and (call ctx :cup-move? db source params)
                 (not= :draw-pile-top (:territory-card-source params)))
            (call ctx :sun-cup-needs-one-point-card? db source params))
      (call ctx :one-point-card-options-for db source params)
      [])))

(defn move-territory-card-source-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (cond
      (call ctx :cup-move? db source params)
      (mapv options/territory-card-source-definitions
            (call ctx :territory-card-source-option-ids db source params))

      (call ctx :disc-move? db source params)
      (mapv options/disc-replacement-card-source-definitions
            (call ctx :disc-replacement-card-source-option-ids db source params))

      (call ctx :sword-move? db source params)
      (mapv options/disc-replacement-card-source-definitions
            (call ctx :sword-replacement-card-source-option-ids db source params))

      :else
      [])))

(defn move-disc-target-kind-options [ctx db]
  (if (call ctx :disc-move?
            db
            (call ctx :move-source db)
            (call ctx :move-params db))
    (mapv options/disc-target-kind-definitions options/disc-target-kind-order)
    []))

(defn move-sword-target-kind-options [ctx db]
  (if (call ctx :sword-move?
            db
            (call ctx :move-source db)
            (call ctx :move-params db))
    (mapv options/sword-target-kind-definitions options/sword-target-kind-order)
    []))

(defn move-replacement-card-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (cond
      (call ctx :sun-move? db source params)
      (call ctx :sun-disc-replacement-card-options-for db source params)

      (call ctx :disc-move? db source params)
      (call ctx :disc-replacement-card-options-for db source params)

      (call ctx :sword-move? db source params)
      (call ctx :sword-replacement-card-options-for db source params)

      :else
      [])))

(defn move-orientation-options [_ctx _db]
  (mapv (fn [orientation]
          {:id orientation
           :label (pieces/orientation-label orientation)})
        [:up :north :east :south :west]))
