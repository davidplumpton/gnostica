(ns gnostica.move-selection.preview
  (:require [gnostica.board-layout :as layout]
            [gnostica.cards :as cards]
            [gnostica.move-selection.context :as context]
            [gnostica.move-selection.options :as options]
            [gnostica.pieces :as pieces]))

(def required-context-keys
  #{:active-power
    :board
    :board-cell-by-index
    :cup-move?
    :current-player-id
    :devil-move?
    :disc-move?
    :hermit-move?
    :hierophant-move?
    :major-orient-step?
    :move-disc-orientation-available?
    :move-hermit-orientation-required?
    :move-orientation-options
    :move-preview-result
    :move-ready?
    :move-rod-orientation-required?
    :move-selection
    :move-sun-disc-orientation-available?
    :move-sword-orientation-available?
    :piece-by-id
    :piece-coordinate
    :replacement-card-expected-value
    :replacement-card-options-for-source
    :rod-move?
    :selected-disc-action-count
    :selected-replacement-card-source
    :selected-sun-disc-mode
    :sun-disc-target-cell
    :sword-move?
    :target-board-cell})

(defn make-context [deps]
  (context/make "gnostica.move-selection.preview" required-context-keys deps))

(defn- call [ctx key & args]
  (apply context/call ctx key args))

(def ^:private direction-deltas
  {:north [-1 0]
   :east [0 1]
   :south [1 0]
   :west [0 -1]})

(defn- add-coordinates [[row col] [row-delta col-delta] distance]
  [(+ row (* row-delta distance))
   (+ col (* col-delta distance))])

(defn- path-coordinates [from orientation distance]
  (when-let [delta (get direction-deltas orientation)]
    (mapv #(add-coordinates from delta %)
          (range 1 (inc (or distance 0))))))

(defn- board-cell-at-coordinate [ctx db [row col]]
  (some #(when (and (= row (:row %))
                    (= col (:col %)))
           %)
        (call ctx :board db)))

(defn- wasteland-space-at-coordinate [ctx db [row col]]
  (some #(when (and (= row (:row %))
                    (= col (:col %)))
           %)
        (layout/wasteland-spaces (call ctx :board db))))

(defn- territory-label [ctx db board-index]
  (if-let [cell (call ctx :board-cell-by-index db board-index)]
    (str (:title (:card cell)) " [" board-index "]")
    (str "Territory [" board-index "]")))

(defn- wasteland-label [{:keys [row col]}]
  (str "Wasteland row " (inc row) ", column " (inc col)))

(defn- coordinate-space [ctx db [row col]]
  (cond
    (nil? row)
    nil

    (board-cell-at-coordinate ctx db [row col])
    (let [{:keys [index orientation]} (board-cell-at-coordinate ctx db [row col])]
      {:kind :territory
       :board-index index
       :row row
       :col col
       :orientation orientation
       :label (territory-label ctx db index)})

    (wasteland-space-at-coordinate ctx db [row col])
    (let [{:keys [orientation] :as space} (wasteland-space-at-coordinate
                                           ctx
                                           db
                                           [row col])]
      {:kind :wasteland
       :row row
       :col col
       :orientation orientation
       :label (wasteland-label space)})

    :else
    {:kind :void
     :row row
     :col col
     :orientation :portrait
     :label (str "Void row " (inc row) ", column " (inc col))}))

(defn- coordinate-for-board-index [ctx db board-index]
  (when-let [{:keys [row col]} (call ctx :board-cell-by-index db board-index)]
    [row col]))

(defn- coordinate-for-wasteland [{:keys [row col]}]
  (when (and (int? row) (int? col))
    [row col]))

(defn- piece-coordinate [ctx db piece]
  (call ctx :piece-coordinate db piece))

(defn- target-coordinate-from-params [ctx db params]
  (cond
    (some? (:sun-disc-target-piece-id params))
    (some->> (:sun-disc-target-piece-id params)
             (call ctx :piece-by-id db)
             (piece-coordinate ctx db))

    (some? (:target-piece-id params))
    (some->> (:target-piece-id params)
             (call ctx :piece-by-id db)
             (piece-coordinate ctx db))

    (some? (:sun-disc-target-board-index params))
    (coordinate-for-board-index ctx db (:sun-disc-target-board-index params))

    (some? (:target-board-index params))
    (coordinate-for-board-index ctx db (:target-board-index params))

    (:hermit-destination-wasteland params)
    (coordinate-for-wasteland (:hermit-destination-wasteland params))

    (some? (:hermit-destination-board-index params))
    (coordinate-for-board-index ctx db (:hermit-destination-board-index params))

    (:target-wasteland params)
    (coordinate-for-wasteland (:target-wasteland params))))

(defn- rod-preview-moved-coordinate [ctx db params]
  (case (:rod-mode params)
    :move-minion
    (some->> (:piece-id params)
             (call ctx :piece-by-id db)
             (piece-coordinate ctx db))

    :push-piece
    (some->> (:target-piece-id params)
             (call ctx :piece-by-id db)
             (piece-coordinate ctx db))

    :push-territory
    (coordinate-for-board-index ctx db (:target-board-index params))

    nil))

(defn- rod-preview [ctx db _source params preview-status preview-error]
  (let [minion (call ctx :piece-by-id db (:piece-id params))
        from (rod-preview-moved-coordinate ctx db params)
        distance (:distance params)
        coordinates (when (and from
                               (:orientation minion)
                               (pos-int? distance))
                      (path-coordinates from (:orientation minion) distance))]
    (when (or from (seq coordinates))
      {:kind :movement
       :power :rod
       :mode (:rod-mode params)
       :status preview-status
       :error preview-error
       :source-space (coordinate-space ctx db from)
       :path (mapv #(coordinate-space ctx db %) coordinates)
       :destination-space (coordinate-space ctx db (last coordinates))
       :distance distance
       :summary (str "Rod "
                     (get-in options/rod-mode-definitions [(:rod-mode params) :label]
                             "movement")
                     (when distance
                       (str " " distance)))})))

(defn- hermit-preview [ctx db _source params preview-status preview-error]
  (let [from (cond
               (:target-piece-id params)
               (some->> (:target-piece-id params)
                        (call ctx :piece-by-id db)
                        (piece-coordinate ctx db))

               (some? (:target-board-index params))
               (coordinate-for-board-index ctx db (:target-board-index params)))
        to (or (coordinate-for-wasteland (:hermit-destination-wasteland params))
               (coordinate-for-board-index ctx db (:hermit-destination-board-index params)))]
    (when (or from to)
      {:kind :movement
       :power :hermit
       :status preview-status
       :error preview-error
       :source-space (coordinate-space ctx db from)
       :path (cond-> []
               from (conj (coordinate-space ctx db from))
               to (conj (coordinate-space ctx db to)))
       :destination-space (coordinate-space ctx db to)
       :summary "Hermit relocation"})))

(defn- piece-size-after-growth [size steps]
  (nth (iterate {:small :medium
                 :medium :large
                 :large :large}
                size)
       (or steps 1)
       size))

(defn- piece-size-for-pips [pips]
  (case pips
    1 :small
    2 :medium
    3 :large
    nil))

(defn- selected-replacement-card [ctx db source params card-id]
  (some #(when (= card-id (:id %)) %)
        (call ctx
              :replacement-card-options-for-source
              db
              source
              params
              (call ctx :selected-replacement-card-source db source params))))

(defn- disc-mutation-preview [ctx db source params preview-status preview-error]
  (case (:disc-target-kind params)
    :piece
    (when-let [piece (call ctx :piece-by-id db (:target-piece-id params))]
      (let [action-count (call ctx :selected-disc-action-count db source params)]
        {:kind :mutation
         :power :disc
         :status preview-status
         :error preview-error
         :target-space (coordinate-space ctx db (piece-coordinate ctx db piece))
         :target {:kind :piece
                  :piece-id (:id piece)}
         :summary (str "Grow "
                       (pieces/size-label (:size piece))
                       " to "
                       (pieces/size-label (piece-size-after-growth (:size piece)
                                                                  action-count)))}))

    :territory
    (when-let [cell (call ctx :target-board-cell db params)]
      (let [old-value (cards/card-point-value (:card cell))
            replacement (selected-replacement-card ctx
                                                   db
                                                   source
                                                   params
                                                   (:replacement-card-id params))
            new-value (or (some-> replacement cards/card-point-value)
                          (call ctx :replacement-card-expected-value db source params))]
        {:kind :mutation
         :power :disc
         :status preview-status
         :error preview-error
         :target-space (coordinate-space ctx db [(:row cell) (:col cell)])
         :target {:kind :territory
                  :board-index (:index cell)}
         :summary (str "Grow territory "
                       (or old-value "?")
                       " to "
                       (or new-value "?"))}))

    nil))

(defn- sword-mutation-preview [ctx db _source params preview-status preview-error]
  (case (:sword-target-kind params)
    :piece
    (when-let [piece (call ctx :piece-by-id db (:target-piece-id params))]
      (let [damage (:damage params)
            remaining (when (int? damage)
                        (- (pieces/pips piece) damage))]
        {:kind :mutation
         :power :sword
         :status preview-status
         :error preview-error
         :target-space (coordinate-space ctx db (piece-coordinate ctx db piece))
         :target {:kind :piece
                  :piece-id (:id piece)}
         :summary (if (and (int? remaining) (pos? remaining))
                    (str "Shrink "
                         (pieces/size-label (:size piece))
                         " to "
                         (pieces/size-label (piece-size-for-pips remaining)))
                    "Destroy piece")}))

    :territory
    (when-let [cell (call ctx :target-board-cell db params)]
      (let [old-value (cards/card-point-value (:card cell))
            damage (:damage params)
            new-value (when (and old-value damage)
                        (- old-value damage))]
        {:kind :mutation
         :power :sword
         :status preview-status
         :error preview-error
         :target-space (coordinate-space ctx db [(:row cell) (:col cell)])
         :target {:kind :territory
                  :board-index (:index cell)}
         :summary (if (and (int? new-value) (pos? new-value))
                    (str "Reduce territory "
                         old-value
                         " to "
                         new-value)
                    "Destroy territory")}))

    nil))

(defn- sun-mutation-preview [ctx db source params preview-status preview-error]
  (case (call ctx :selected-sun-disc-mode db source params)
    :created-piece
    {:kind :mutation
     :power :sun
     :status preview-status
     :error preview-error
     :target-space (coordinate-space ctx db (target-coordinate-from-params ctx db params))
     :target {:kind :created-piece}
     :summary "Create a medium piece"}

    :created-territory
    {:kind :mutation
     :power :sun
     :status preview-status
     :error preview-error
     :target-space (coordinate-space ctx db (target-coordinate-from-params ctx db params))
     :target {:kind :created-territory}
     :summary "Create a two-point territory"}

    :piece
    (when-let [piece (call ctx :piece-by-id db (:sun-disc-target-piece-id params))]
      {:kind :mutation
       :power :sun
       :status preview-status
       :error preview-error
       :target-space (coordinate-space ctx db (piece-coordinate ctx db piece))
       :target {:kind :piece
                :piece-id (:id piece)}
       :summary (str "Grow "
                     (pieces/size-label (:size piece))
                     " to "
                     (pieces/size-label (piece-size-after-growth (:size piece) 1)))})

    :territory
    (when-let [cell (call ctx :sun-disc-target-cell db params)]
      {:kind :mutation
       :power :sun
       :status preview-status
       :error preview-error
       :target-space (coordinate-space ctx db [(:row cell) (:col cell)])
       :target {:kind :territory
                :board-index (:index cell)}
       :summary (str "Grow territory "
                     (or (cards/card-point-value (:card cell)) "?")
                     " to "
                     (or (call ctx :replacement-card-expected-value db source params) "?"))})

    nil))

(defn- mutation-preview [ctx db source params preview-status preview-error]
  (cond
    (call ctx :disc-move? db source params)
    (disc-mutation-preview ctx db source params preview-status preview-error)

    (call ctx :sword-move? db source params)
    (sword-mutation-preview ctx db source params preview-status preview-error)

    (call ctx :sun-move? db source params)
    (sun-mutation-preview ctx db source params preview-status preview-error)

    :else
    nil))

(defn- initial-placement-preview [ctx db source params preview-status preview-error]
  (when (= :place-initial-small source)
    (when-let [coordinate (target-coordinate-from-params ctx db params)]
      {:kind :placement
       :status preview-status
       :error preview-error
       :target-space (coordinate-space ctx db coordinate)
       :player-id (call ctx :current-player-id db)
       :piece-size :small
       :orientation (:orientation params)
       :summary "Place small piece"})))

(defn- compass-field [ctx db source params]
  (let [stage (:stage (call ctx :move-selection db))]
    (cond
      (or (= :minion-orientation stage)
          (and (= :confirm stage)
               (call ctx :major-orient-step? db source params)))
      :minion-orientation

      (and (= :sun (call ctx :active-power db source params))
           (call ctx :move-sun-disc-orientation-available? db))
      :sun-disc-orientation

      (or (= :orientation stage)
          (= :orient-piece source)
          (= :place-initial-small source)
          (and (call ctx :rod-move? db source params)
               (call ctx :move-rod-orientation-required? db)
               (:distance params))
          (call ctx :move-hermit-orientation-required? db)
          (call ctx :move-disc-orientation-available? db)
          (call ctx :move-sword-orientation-available? db)
          (call ctx :hierophant-move? db source params)
          (call ctx :devil-move? db source params)
          (and (or (call ctx :cup-move? db source params)
                   (call ctx :sun-move? db source params))
               (some? (:target-board-index params))
               (nil? (:target-piece-id params))))
      :orientation)))

(defn- compass-coordinate [ctx db source params field movement]
  (case field
    :minion-orientation
    (some->> (:piece-id params)
             (call ctx :piece-by-id db)
             (piece-coordinate ctx db))

    :sun-disc-orientation
    (some->> (:sun-disc-target-piece-id params)
             (call ctx :piece-by-id db)
             (piece-coordinate ctx db))

    :orientation
    (or (some-> movement :destination-space ((juxt :row :col)))
        (cond
          (= :orient-piece source)
          (some->> (:piece-id params)
                   (call ctx :piece-by-id db)
                   (piece-coordinate ctx db))

          (= :place-initial-small source)
          (target-coordinate-from-params ctx db params)

          (call ctx :hermit-move? db source params)
          (or (coordinate-for-wasteland (:hermit-destination-wasteland params))
              (coordinate-for-board-index ctx db (:hermit-destination-board-index params)))

          :else
          (target-coordinate-from-params ctx db params)))))

(defn- orientation-compass [ctx db source params movement]
  (when-let [field (compass-field ctx db source params)]
    (when-let [coordinate (compass-coordinate ctx db source params field movement)]
      {:active? true
       :field field
       :space (coordinate-space ctx db coordinate)
       :selected-orientation (case field
                               :minion-orientation (:minion-orientation params)
                               :sun-disc-orientation (:sun-disc-orientation params)
                               (:orientation params))
       :options (call ctx :move-orientation-options db)})))

(defn move-preview [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)
        preview-result (when (call ctx :move-ready? db)
                         (call ctx :move-preview-result db))
        preview-error (when (false? (:ok? preview-result))
                        (:error preview-result))
        preview-status (cond
                         (false? (:ok? preview-result)) :disabled
                         (:ok? preview-result) :legal
                         :else :pending)
        movement (cond
                   (call ctx :rod-move? db source params)
                   (rod-preview ctx db source params preview-status preview-error)

                   (call ctx :hermit-move? db source params)
                   (hermit-preview ctx db source params preview-status preview-error)

                   :else
                   nil)
        mutation (mutation-preview ctx db source params preview-status preview-error)
        placement (initial-placement-preview ctx db source params preview-status preview-error)
        compass (orientation-compass ctx db source params movement)]
    (cond-> {:active? (boolean (or movement mutation placement compass))
             :status preview-status
             :error preview-error}
      movement
      (assoc :movement movement)

      mutation
      (assoc :mutation mutation)

      placement
      (assoc :placement placement)

      compass
      (assoc :orientation-compass compass)

      (or movement mutation placement compass)
      (assoc :summary (or (:summary mutation)
                          (:summary placement)
                          (:summary movement)
                          "Choose orientation")))))
