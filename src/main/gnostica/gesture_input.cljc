(ns gnostica.gesture-input
  #?(:clj (:require [clojure.edn :as reader]
                    [clojure.string :as str])
     :cljs (:require [cljs.reader :as reader]
                     [clojure.string :as str])))

(def mime-type "application/gnostica-gesture")
(def fallback-mime-type "text/plain")
(def fallback-text-prefix "gnostica-gesture:")
(def pointer-drag-move-event "gnostica-gesture-pointer-move")
(def pointer-drag-drop-event "gnostica-gesture-pointer-drop")
(def pointer-drag-cancel-event "gnostica-gesture-pointer-cancel")
(def orientation-change-event "gnostica-gesture-orientation-change")

#?(:cljs
   (defonce active-gesture-input* (atom nil)))

(def orientation-cycle-order
  [:up :north :east :south :west])

(def orientation-key-requests
  {"arrowup" :up
   "arrowright" :east
   "arrowdown" :south
   "arrowleft" :west})

(def orientation-cycle-key "o")

(defn gesture-input-string [input]
  (pr-str input))

(defn gesture-input-fallback-string [input]
  (str fallback-text-prefix (gesture-input-string input)))

(def source-kinds
  #{:territory :hand-card :draw-pile :discard-pile :piece :stash-piece})

(def target-kinds
  #{:territory :piece :wasteland})

(def valid-orientations
  #{:up :north :east :south :west})

(def valid-piece-sizes
  #{:small :medium :large})

(def gesture-field-keys
  #{:source-board-index
    :hand-card-id
    :piece-id
    :power
    :copied-board-index
    :copied-power
    :rod-mode
    :disc-target-kind
    :sword-target-kind
    :disc-action-count
    :major-action-count
    :sword-action-count
    :devil-action-count
    :fool-reveal-count
    :high-priestess-redraw-count
    :minion-orientation
    :sun-disc-mode
    :target-piece-id
    :sun-disc-target-piece-id
    :target-board-index
    :sun-disc-target-board-index
    :territory-card-source
    :one-point-card-id
    :damage
    :distance
    :replacement-card-source
    :replacement-card-id
    :sun-disc-replacement-card-id
    :hermit-destination-board-index
    :target-wasteland
    :hermit-destination-wasteland
    :orientation
    :draw-count
    :discard-card-ids})

(def gesture-top-level-keys
  #{:source :source-id :preserve-selection? :fields :target})

(defn- id? [value]
  (or (keyword? value)
      (string? value)))

(defn- card-id? [value]
  (and (string? value)
       (not (str/blank? value))))

(defn- board-index? [value]
  (and (int? value)
       (not (neg? value))))

(defn- coordinate? [value]
  (int? value))

(defn- closed-map? [value allowed-keys]
  (and (map? value)
       (every? allowed-keys (keys value))))

(declare valid-target?)

(defn- valid-source? [source]
  (and (closed-map? source #{:kind :board-index :card-id :piece-id
                             :player-id :size :orientation})
       (contains? source-kinds (:kind source))
       (case (:kind source)
         :territory
         (and (board-index? (:board-index source))
              (not (contains? source :card-id))
              (not (contains? source :piece-id))
              (not (contains? source :player-id))
              (not (contains? source :size))
              (not (contains? source :orientation)))

         :hand-card
         (and (card-id? (:card-id source))
              (not (contains? source :board-index))
              (not (contains? source :piece-id))
              (not (contains? source :player-id))
              (not (contains? source :size))
              (not (contains? source :orientation)))

         :draw-pile
         (= #{:kind} (set (keys source)))

         :discard-pile
         (= #{:kind} (set (keys source)))

         :piece
         (and (id? (:piece-id source))
              (not (contains? source :board-index))
              (not (contains? source :card-id))
              (not (contains? source :player-id))
              (not (contains? source :size))
              (not (contains? source :orientation)))

         :stash-piece
         (and (id? (:player-id source))
              (contains? valid-piece-sizes (:size source))
              (or (not (contains? source :orientation))
                  (contains? valid-orientations (:orientation source)))
              (not (contains? source :board-index))
              (not (contains? source :card-id))
              (not (contains? source :piece-id))))))

(defn- valid-wasteland-field? [value]
  (and (map? value)
       (= :wasteland (:kind value))
       (coordinate? (:row value))
       (coordinate? (:col value))))

(defn- valid-gesture-field? [field value]
  (case field
    (:source-board-index :copied-board-index :target-board-index
     :sun-disc-target-board-index :hermit-destination-board-index)
    (board-index? value)

    (:hand-card-id :one-point-card-id :replacement-card-id
     :sun-disc-replacement-card-id)
    (card-id? value)

    (:piece-id :target-piece-id :sun-disc-target-piece-id)
    (id? value)

    (:power :copied-power :rod-mode :disc-target-kind :sword-target-kind
     :territory-card-source :replacement-card-source :sun-disc-mode)
    (keyword? value)

    (:disc-action-count :major-action-count :sword-action-count
     :devil-action-count :fool-reveal-count :high-priestess-redraw-count
     :damage :distance :draw-count)
    (and (int? value)
         (not (neg? value)))

    (:minion-orientation :orientation)
    (contains? valid-orientations value)

    (:target-wasteland :hermit-destination-wasteland)
    (valid-wasteland-field? value)

    :discard-card-ids
    (and (sequential? value)
         (every? card-id? value))

    false))

(defn- valid-fields? [fields]
  (and (closed-map? fields gesture-field-keys)
       (every? (fn [[field value]]
                 (valid-gesture-field? field value))
               fields)))

(defn- valid-target? [target]
  (and (closed-map? target #{:kind :board-index :piece-id :row :col})
       (contains? target-kinds (:kind target))
       (case (:kind target)
         :territory
         (and (board-index? (:board-index target))
              (not (contains? target :piece-id))
              (not (contains? target :row))
              (not (contains? target :col)))

         :piece
         (and (id? (:piece-id target))
              (not (contains? target :board-index))
              (not (contains? target :row))
              (not (contains? target :col)))

         :wasteland
         (and (coordinate? (:row target))
              (coordinate? (:col target))
              (not (contains? target :board-index))
              (not (contains? target :piece-id))))))

(defn gesture-input? [input]
  (and (closed-map? input gesture-top-level-keys)
       (or (not (contains? input :preserve-selection?))
           (boolean? (:preserve-selection? input)))
       (or (not (contains? input :source-id))
           (keyword? (:source-id input)))
       (or (not (contains? input :source))
           (valid-source? (:source input)))
       (or (not (contains? input :fields))
           (valid-fields? (:fields input)))
       (or (not (contains? input :target))
           (valid-target? (:target input)))
       (or (contains? input :source)
           (contains? input :fields))))

(defn- valid-gesture-input [input]
  (when (gesture-input? input)
    input))

(defn fallback-gesture-payload? [payload]
  (and (string? payload)
       (str/starts-with? payload fallback-text-prefix)))

(defn- read-gesture-input [payload]
  (when (and (string? payload)
             (seq payload))
    (try
      (valid-gesture-input (reader/read-string payload))
      (catch #?(:clj Exception :cljs :default) _
        nil))))

(defn parse-gesture-input-string [payload]
  (read-gesture-input payload))

(defn parse-gesture-input-fallback-string [payload]
  (when (fallback-gesture-payload? payload)
    (read-gesture-input (subs payload (count fallback-text-prefix)))))

(defn gesture-data-transfer-types? [types fallback-payload]
  (boolean
   (or (some #(= mime-type %) types)
       (and (some #(= fallback-mime-type %) types)
            (fallback-gesture-payload? fallback-payload)))))

(defn- comparable-drag-input [input]
  (let [input (cond-> input
                (contains? input :source)
                (update :source dissoc :orientation)

                (contains? input :fields)
                (update :fields #(not-empty (dissoc % :orientation))))]
    (cond-> input
      (nil? (:fields input))
      (dissoc :fields))))

(defn- compatible-active-input [active-input payload-input payload]
  (when-let [active-input (valid-gesture-input active-input)]
    (when (or (and payload-input
                   (= (comparable-drag-input active-input)
                      (comparable-drag-input payload-input)))
              (str/blank? (or payload "")))
      active-input)))

(defn gesture-input-from-drag-data
  [{:keys [types custom-payload fallback-payload active-input]}]
  (when (gesture-data-transfer-types? types fallback-payload)
    (let [custom-type? (some #(= mime-type %) types)
          custom-input (parse-gesture-input-string custom-payload)
          fallback-input (parse-gesture-input-fallback-string fallback-payload)]
      (or (when custom-type?
            (compatible-active-input active-input custom-input custom-payload))
          custom-input
          fallback-input))))

#?(:cljs
   (defn gesture-mime-type? [data-transfer]
     (when data-transfer
       (boolean
        (some #(= mime-type %)
              (array-seq (.-types data-transfer)))))))

#?(:cljs
   (defn gesture-data-transfer? [data-transfer]
     (when data-transfer
       (let [types (array-seq (.-types data-transfer))]
         (gesture-data-transfer-types?
          types
          (when (some #(= fallback-mime-type %) types)
            (.getData data-transfer fallback-mime-type)))))))

#?(:cljs
   (defn active-gesture-input []
     @active-gesture-input*))

#?(:cljs
   (defn set-active-gesture-input! [input]
     (reset! active-gesture-input* input)))

#?(:cljs
   (defn clear-active-gesture-input! []
     (reset! active-gesture-input* nil)))

#?(:cljs
   (defn gesture-input-from-data-transfer [data-transfer]
     (when data-transfer
       (let [types (array-seq (.-types data-transfer))]
         (gesture-input-from-drag-data
          {:types types
           :custom-payload (when (some #(= mime-type %) types)
                             (.getData data-transfer mime-type))
           :fallback-payload (when (some #(= fallback-mime-type %) types)
                               (.getData data-transfer fallback-mime-type))
           :active-input (active-gesture-input)})))))

#?(:cljs
   (defn set-gesture-data! [data-transfer input]
     (when data-transfer
       (set-active-gesture-input! input)
       (let [payload (gesture-input-string input)]
         (.setData data-transfer mime-type payload)
         (.setData data-transfer fallback-mime-type
                   (str fallback-text-prefix payload))
         (set! (.-effectAllowed data-transfer) "move")))))

(defn- id-value [value id-key]
  (if (map? value)
    (get value id-key)
    value))

(defn territory-source-input [cell-or-index]
  {:source {:kind :territory
            :board-index (id-value cell-or-index :index)}})

(defn territory-target [cell-or-index]
  {:kind :territory
   :board-index (id-value cell-or-index :index)})

(defn hand-card-source-input [card-or-id]
  {:source {:kind :hand-card
            :card-id (id-value card-or-id :id)}})

(defn draw-pile-source-input []
  {:source {:kind :draw-pile}})

(defn piece-source-input [piece-or-id]
  {:source {:kind :piece
            :piece-id (id-value piece-or-id :id)}})

(defn piece-target [piece-or-id]
  {:kind :piece
   :piece-id (id-value piece-or-id :id)})

(defn stash-piece-source-input [player-or-id]
  {:source {:kind :stash-piece
            :player-id (id-value player-or-id :id)
            :size :small}})

(defn wasteland-target
  ([space]
   {:kind :wasteland
    :row (:row space)
    :col (:col space)})
  ([row col]
   {:kind :wasteland
    :row row
    :col col}))

(def board-space-drag-source-kinds #{:piece :stash-piece})

(defn source-kind [input-or-source]
  (or (:kind input-or-source)
      (get-in input-or-source [:source :kind])))

(defn board-space-drag-source? [input-or-source]
  (contains? board-space-drag-source-kinds
             (source-kind input-or-source)))

(defn orientation-drag-input? [input]
  (boolean
   (or (board-space-drag-source? input)
       (get-in input [:fields :piece-id])
       (get-in input [:fields :target-piece-id])
       (get-in input [:fields :sun-disc-target-piece-id]))))

(defn drag-orientation [input]
  (or (get-in input [:fields :orientation])
      (get-in input [:source :orientation])))

(defn with-drag-orientation [input orientation]
  (let [source-kind (get-in input [:source :kind])]
    (cond-> (assoc-in input [:fields :orientation] orientation)
      (contains? board-space-drag-source-kinds source-kind)
      (assoc-in [:source :orientation] orientation))))

(defn- orientation-cycle-index [orientation]
  (first
   (keep-indexed (fn [index candidate]
                   (when (= orientation candidate)
                     index))
                 orientation-cycle-order)))

(defn next-orientation [orientation]
  (let [index (orientation-cycle-index orientation)]
    (nth orientation-cycle-order
         (if (some? index)
           (mod (inc index) (count orientation-cycle-order))
           0))))

(defn orientation-request->orientation [current-orientation request]
  (cond
    (= :cycle request)
    (next-orientation current-orientation)

    (some #{request} orientation-cycle-order)
    request

    :else
    nil))

(defn orientation-key-request [{:keys [key code alt? ctrl? meta?]}]
  (when-not (or alt? ctrl? meta?)
    (let [key (some-> key str/lower-case)
          code (some-> code str/lower-case)]
      (or (get orientation-key-requests key)
          (when (or (= orientation-cycle-key key)
                    (= "keyo" code))
            :cycle)))))

(defn target-key [{:keys [kind board-index row col piece-id]}]
  (case kind
    :territory [:territory board-index]
    :wasteland [:wasteland row col]
    :piece [:piece piece-id]
    nil))

(defn same-target? [left right]
  (and (some? left)
       (some? right)
       (= (target-key left)
          (target-key right))))

(defn show-target-highlight? [drag-state target]
  (or (not (board-space-drag-source? (:source drag-state)))
      (same-target? (:target drag-state) target)))

(defn replacement-card-choice-input [card-or-id descriptor]
  {:preserve-selection? true
   :fields (cond-> {:replacement-card-id (id-value card-or-id :id)}
             (:replacement-card-source descriptor)
             (assoc :replacement-card-source (:replacement-card-source descriptor)))})

(defn draggable-card-source? [descriptor]
  (not (contains? #{:discard :territory-card :replacement-card}
                  (:role descriptor))))

(defn hand-card-drag-input [card descriptor]
  (cond
    (and (= :replacement-card (:role descriptor))
         (:enabled? descriptor))
    (replacement-card-choice-input card descriptor)

    (draggable-card-source? descriptor)
    (hand-card-source-input card)

    :else
    nil))

(defn discard-card-drag-input [card descriptor]
  (when (and card
             (= :replacement-card (:role descriptor))
             (:enabled? descriptor))
    (replacement-card-choice-input card descriptor)))

(defn territory-drag-input [cell descriptor]
  (if (and (:active? descriptor)
           (= :target (:role descriptor)))
    {:preserve-selection? true
     :fields {:target-board-index (:index cell)}}
    (territory-source-input cell)))

(defn piece-drag-input [piece descriptor]
  (case (:role descriptor)
    :minion
    (when (:enabled? descriptor)
      {:preserve-selection? true
       :fields {:piece-id (:id piece)}})

    :target
    (when (:enabled? descriptor)
      {:preserve-selection? true
       :fields {:target-piece-id (:id piece)}})

    (when (:source-enabled? descriptor)
      (piece-source-input piece))))
