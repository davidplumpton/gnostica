(ns gnostica.app-state.gestures
  (:require [gnostica.app-state.view-models :as views]
            [gnostica.gesture-input :as gesture-input]
            [gnostica.gesture-intent :as gesture-intent]
            [gnostica.move-selection :as move-selection]))

(def empty-keyboard-placement-targeting
  gesture-intent/empty-keyboard-placement-targeting)

(defn gesture-intent [app-db]
  (gesture-intent/gesture-intent app-db))

(defn cancel-gesture-intent [app-db]
  (-> app-db
      move-selection/cancel-move
      gesture-intent/cancel-gesture-intent))

(defn open-gesture-detailed-entry [app-db]
  (if (:detailed-entry-available? (views/direct-manipulation app-db))
    (-> app-db
        gesture-intent/open-detailed-entry
        (views/set-panel-open :move true))
    app-db))

(defn start-gesture-intent [app-db input]
  (let [gesture-db (gesture-intent/start-gesture-intent app-db input)
        {:keys [detailed-entry-available? detailed-entry-default?]}
        (views/direct-manipulation gesture-db)]
    (if (and detailed-entry-available? detailed-entry-default?)
      (open-gesture-detailed-entry gesture-db)
      gesture-db)))

(defn pending-move-tray-view [app-db]
  (assoc (gesture-intent/pending-move-tray-view app-db)
         :action-ribbon (move-selection/move-action-ribbon app-db)
         :detailed-entry-available? (:detailed-entry-available?
                                     (views/direct-manipulation app-db))))

(defn keyboard-placement-targeting [app-db]
  (:keyboard-placement-targeting app-db empty-keyboard-placement-targeting))

(defn keyboard-placement-targeting-active? [app-db]
  (true? (:active? (keyboard-placement-targeting app-db))))

(defn keyboard-placement-targeting-mode [app-db]
  (:mode (keyboard-placement-targeting app-db)))

(defn cancel-move [app-db]
  (cancel-gesture-intent app-db))

(defn select-board-card [app-db index]
  (if (views/board-cell-by-index app-db index)
    (move-selection/select-board-for-active-move
     (-> app-db
         (assoc :selected-board-index index)
         (views/set-panel-open :territory true))
     index)
    app-db))

(defn- target->key [{:keys [kind board-index row col]}]
  (case kind
    :territory [:territory board-index]
    :wasteland [:wasteland row col]
    nil))

(defn- target-sort-key [{:keys [kind board-index row col]}]
  [row col (case kind :territory 0 :wasteland 1 2) (or board-index 0)])

(defn- abs-number [n]
  (if (neg? n) (- n) n))

(defn- center-coordinate [targets coordinate-key]
  (when (seq targets)
    (/ (+ (apply min (map coordinate-key targets))
          (apply max (map coordinate-key targets)))
       2)))

(defn- center-target [targets]
  (when (seq targets)
    (let [center-row (center-coordinate targets :row)
          center-col (center-coordinate targets :col)]
      (first (sort-by (fn [target]
                        [(+ (abs-number (- (:row target) center-row))
                            (abs-number (- (:col target) center-col)))
                         (target-sort-key target)])
                      targets)))))

(defn- board-targets [app-db]
  (mapv (fn [{:keys [index row col]}]
          {:kind :territory
           :board-index index
           :row row
           :col col})
        (move-selection/move-target-board-options app-db)))

(defn- wasteland-targets [app-db]
  (mapv (fn [{:keys [row col]}]
          {:kind :wasteland
           :row row
           :col col})
        (move-selection/move-target-wasteland-options app-db)))

(defn- placement-targets [app-db]
  (sort-by target-sort-key
           (concat (board-targets app-db)
                   (wasteland-targets app-db))))

(defn- target-by-key [targets target]
  (let [key (target->key target)]
    (some #(when (= key (target->key %)) %)
          targets)))

(defn- current-placement-target [app-db]
  (let [{:keys [target-board-index target-wasteland]}
        (move-selection/move-params app-db)]
    (cond
      (some? target-board-index)
      (some #(when (= target-board-index (:index %))
               {:kind :territory
                :board-index (:index %)
                :row (:row %)
                :col (:col %)})
            (move-selection/move-target-board-options app-db))

      target-wasteland
      (some #(when (and (= (:row target-wasteland) (:row %))
                        (= (:col target-wasteland) (:col %)))
               {:kind :wasteland
                :row (:row %)
                :col (:col %)})
            (move-selection/move-target-wasteland-options app-db)))))

(defn- default-placement-target [app-db]
  (let [targets (placement-targets app-db)
        selected-index (:selected-board-index app-db)]
    (or (target-by-key targets (current-placement-target app-db))
        (center-target targets)
        (when (some? selected-index)
          (target-by-key targets {:kind :territory
                                  :board-index selected-index}))
        (first targets))))

(defn- select-placement-target [app-db {:keys [kind board-index row col]}]
  (case kind
    :territory (move-selection/select-board-for-active-move app-db board-index)
    :wasteland (move-selection/select-move-wasteland-target app-db row col)
    app-db))

(defn- abs-int [n]
  (if (neg? n) (- n) n))

(defn- direction-delta [direction [row col] target]
  (case direction
    :north (- row (:row target))
    :east (- (:col target) col)
    :south (- (:row target) row)
    :west (- col (:col target))
    nil))

(defn- direction-secondary-distance [direction [row col] target]
  (case direction
    (:north :south) (abs-int (- (:col target) col))
    (:east :west) (abs-int (- (:row target) row))
    0))

(defn- next-placement-target [app-db direction]
  (let [targets (placement-targets app-db)
        current (or (target-by-key targets (current-placement-target app-db))
                    (first targets))]
    (if-not (and current direction)
      current
      (let [coordinate [(:row current) (:col current)]]
        (or (first
             (sort-by (fn [target]
                        [(direction-secondary-distance direction coordinate target)
                         (direction-delta direction coordinate target)
                         (target-sort-key target)])
                      (filter (fn [target]
                                (let [delta (direction-delta direction coordinate target)]
                                  (and delta (pos? delta))))
                              targets)))
            current)))))

(defn start-keyboard-placement-targeting [app-db]
  (let [player-id (move-selection/current-player-id app-db)
        input (gesture-input/stash-piece-source-input player-id)
        staged-db (gesture-intent/start-gesture-intent app-db input)
        target (default-placement-target staged-db)
        targeted-db (cond-> staged-db
                      target (select-placement-target target))]
    (-> targeted-db
        gesture-intent/refresh-gesture-intent
        (assoc :keyboard-placement-targeting {:active? true
                                              :mode :target}))))

(defn move-keyboard-placement-target [app-db direction]
  (if (and (keyboard-placement-targeting-active? app-db)
           (= :target (keyboard-placement-targeting-mode app-db)))
    (if-let [target (next-placement-target app-db direction)]
      (-> app-db
          (select-placement-target target)
          gesture-intent/refresh-gesture-intent)
      app-db)
    app-db))

(defn accept-keyboard-placement-target [app-db]
  (if (keyboard-placement-targeting-active? app-db)
    (assoc app-db :keyboard-placement-targeting {:active? true
                                                 :mode :orientation})
    app-db))

(defn- gesture-drag-orientation-error [input]
  {:code :drag-orientation-unavailable
   :message "This drag cannot choose a piece orientation."
   :data {:source (:source input)
          :fields (:fields input)}})

(defn- current-player-piece-id? [app-db piece-id]
  (move-selection/current-player-piece?
   app-db
   (move-selection/piece-by-id app-db piece-id)))

(defn- hermit-drag-orientation-available? [app-db params]
  (and (= :hermit (:power params))
       (current-player-piece-id? app-db (:target-piece-id params))))

(defn- gesture-drag-orientation-available? [app-db input]
  (let [{:keys [source params]} (move-selection/move-selection app-db)]
    (and (:active? (gesture-intent app-db))
         (gesture-input/orientation-drag-input? input)
         (or (= :place-initial-small source)
             (= :orient-piece source)
             (move-selection/move-rod-orientation-required? app-db)
             (move-selection/move-hermit-orientation-required? app-db)
             (hermit-drag-orientation-available? app-db params)
             (move-selection/move-disc-orientation-available? app-db)
             (move-selection/move-sun-disc-orientation-available? app-db)
             (move-selection/move-sword-orientation-available? app-db)))))

(defn gesture-drag-orientation-result [app-db input request]
  (when (and request
             (:active? (gesture-intent app-db))
             (gesture-input/orientation-drag-input? input))
    (if (gesture-drag-orientation-available? app-db input)
      (if-let [orientation (gesture-input/orientation-request->orientation
                            (or (get-in app-db
                                        [:move-selection :params :orientation])
                                (gesture-input/drag-orientation input))
                            request)]
        {:handled? true
         :accepted? true
         :orientation orientation}
        {:handled? true
         :accepted? false
         :error (gesture-drag-orientation-error input)})
      {:handled? true
       :accepted? false
       :error (gesture-drag-orientation-error input)})))

(defn- pending-placement-orientation-available? [app-db]
  (let [intent (gesture-intent app-db)
        {:keys [source params]} (move-selection/move-selection app-db)]
    (and (true? (:active? intent))
         (= :place-initial-small (:move-source intent))
         (= :place-initial-small source)
         (not (move-selection/turn-action-consumed? app-db))
         (or (some? (:target-board-index params))
             (some? (:target-wasteland params))))))

(defn pending-placement-orientation-result [app-db request]
  (when (and request
             (pending-placement-orientation-available? app-db))
    (when-let [orientation (gesture-input/orientation-request->orientation
                            (get-in app-db
                                    [:move-selection :params :orientation])
                            request)]
      {:handled? true
       :accepted? true
       :orientation orientation})))

(defn set-gesture-drag-orientation [app-db {:keys [orientation error]}]
  (cond
    orientation
    (-> app-db
        (move-selection/set-move-orientation orientation)
        gesture-intent/refresh-gesture-intent)

    error
    (assoc-in app-db [:gesture-intent :error] error)

    :else
    app-db))

(defn set-pending-placement-orientation [app-db {:keys [orientation]}]
  (if (and orientation
           (pending-placement-orientation-available? app-db))
    (-> app-db
        (move-selection/set-move-orientation orientation)
        gesture-intent/refresh-gesture-intent)
    app-db))
