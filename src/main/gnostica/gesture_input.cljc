(ns gnostica.gesture-input
  #?(:clj (:require [clojure.edn :as reader]
                    [clojure.string :as str])
     :cljs (:require [cljs.reader :as reader]
                     [clojure.string :as str])))

(def mime-type "application/gnostica-gesture")
(def fallback-mime-type "text/plain")
(def fallback-text-prefix "gnostica-gesture:")

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

(defn parse-gesture-input-string [payload]
  (when (seq payload)
    (let [payload (if (str/starts-with? payload fallback-text-prefix)
                    (subs payload (count fallback-text-prefix))
                    payload)]
      (try
        (reader/read-string payload)
        (catch #?(:clj Exception :cljs :default) _
          nil)))))

#?(:cljs
   (defn gesture-mime-type? [data-transfer]
     (when data-transfer
       (boolean
        (some #(= mime-type %)
              (array-seq (.-types data-transfer)))))))

#?(:cljs
   (defn gesture-data-transfer? [data-transfer]
     (when data-transfer
       (boolean
        (some #(or (= mime-type %)
                   (= fallback-mime-type %))
              (array-seq (.-types data-transfer)))))))

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
       (or (when (gesture-mime-type? data-transfer)
             (active-gesture-input))
           (parse-gesture-input-string (.getData data-transfer mime-type))
           (parse-gesture-input-string (.getData data-transfer fallback-mime-type))
           (when (gesture-mime-type? data-transfer)
             (active-gesture-input))))))

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
