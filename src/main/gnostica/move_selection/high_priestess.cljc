(ns gnostica.move-selection.high-priestess
  (:require [gnostica.game-state :as game-state]
            [gnostica.move-selection.options :as options]
            [gnostica.move-selection.state :as selection]))

(defn source-card [db source-id params]
  (when (= :play-hand-card source-id)
    (selection/source-card db source-id params)))

(defn redraw-pass-offset [pass-index]
  (when (and (int? pass-index)
             (<= 1 pass-index 2))
    (dec pass-index)))

(defn initial-redraw-state [db source-id params]
  (let [source-card (source-card db source-id params)
        source-card-id (:id source-card)]
    {:hand (if source-card-id
             (vec (remove #(= source-card-id (:id %))
                          (selection/current-player-hand db)))
             (selection/current-player-hand db))
     :unknown-hand-count 0
     :draw-pile (vec (get-in db [:game :draw-pile] []))
     :unknown-draw-count 0
     :discard-pile (cond-> (selection/discard-pile db)
                     source-card
                     (conj source-card))}))

(defn hand-count [redraw-state]
  (+ (count (:hand redraw-state))
     (:unknown-hand-count redraw-state 0)))

(defn valid-discard-card-ids-in-state [redraw-state discard-card-ids]
  (let [hand-card-ids (set (map :id (:hand redraw-state)))]
    (vec (filter hand-card-ids (distinct (or discard-card-ids []))))))

(defn draw-count-options-in-state [redraw-state discard-card-ids]
  (let [discard-count (count (valid-discard-card-ids-in-state redraw-state
                                                              discard-card-ids))
        post-discard-hand-count (- (hand-count redraw-state) discard-count)
        hand-slots (- game-state/starting-hand-size post-discard-hand-count)
        available-cards (+ (count (:draw-pile redraw-state))
                           (:unknown-draw-count redraw-state 0)
                           (count (:discard-pile redraw-state))
                           discard-count)
        max-draw-count (max 0 (min hand-slots available-cards))]
    (vec (range 0 (inc max-draw-count)))))

(defn discard-staged-cards [redraw-state discard-card-ids]
  (let [valid-discard-card-ids (valid-discard-card-ids-in-state redraw-state
                                                                discard-card-ids)
        discard-card-id-set (set valid-discard-card-ids)
        cards-to-discard (filterv #(contains? discard-card-id-set (:id %))
                                  (:hand redraw-state))]
    (-> redraw-state
        (update :hand
                (fn [hand]
                  (vec (remove #(contains? discard-card-id-set (:id %)) hand))))
        (update :discard-pile into cards-to-discard))))

(defn refresh-staged-draw-pile [redraw-state]
  (if (and (empty? (:draw-pile redraw-state))
           (zero? (:unknown-draw-count redraw-state 0))
           (seq (:discard-pile redraw-state)))
    (let [discard-count (count (:discard-pile redraw-state))]
      (-> redraw-state
          (assoc :discard-pile [])
          (assoc :unknown-draw-count discard-count)))
    redraw-state))

(defn stage-draw-one [redraw-state]
  (let [redraw-state (refresh-staged-draw-pile redraw-state)]
    (cond
      (seq (:draw-pile redraw-state))
      (let [card (first (:draw-pile redraw-state))]
        (-> redraw-state
            (update :draw-pile #(vec (rest %)))
            (update :hand conj card)))

      (pos? (:unknown-draw-count redraw-state 0))
      (-> redraw-state
          (update :unknown-draw-count dec)
          (update :unknown-hand-count inc))

      :else
      redraw-state)))

(defn stage-draw [redraw-state draw-count]
  (let [drawn-state (nth (iterate stage-draw-one redraw-state) draw-count)]
    (if (pos? draw-count)
      (refresh-staged-draw-pile drawn-state)
      drawn-state)))

(defn apply-staged-redraw-pass [redraw-state pass]
  (let [discard-card-ids (valid-discard-card-ids-in-state
                          redraw-state
                          (:discard-card-ids pass))
        options (set (draw-count-options-in-state redraw-state discard-card-ids))
        draw-count (if (contains? options (:draw-count pass))
                     (:draw-count pass)
                     0)]
    (-> redraw-state
        (discard-staged-cards discard-card-ids)
        (stage-draw draw-count))))

(defn redraw-state-before-pass [db source-id params pass-index]
  (when-let [target-offset (redraw-pass-offset pass-index)]
    (loop [redraw-state (initial-redraw-state db source-id params)
           offset 0]
      (if (>= offset target-offset)
        redraw-state
        (recur (apply-staged-redraw-pass
                redraw-state
                (get (vec (:redraws params)) offset {}))
               (inc offset))))))

(defn hand-card-options
  ([db source-id params]
   (:hand (initial-redraw-state db source-id params)))
  ([db source-id params pass-index]
   (if-let [redraw-state (redraw-state-before-pass db
                                                   source-id
                                                   params
                                                   pass-index)]
     (:hand redraw-state)
     [])))

(defn valid-discard-card-ids
  ([db source-id params discard-card-ids]
   (valid-discard-card-ids-in-state
    (initial-redraw-state db source-id params)
    discard-card-ids))
  ([db source-id params pass-index discard-card-ids]
   (if-let [redraw-state (redraw-state-before-pass db
                                                   source-id
                                                   params
                                                   pass-index)]
     (valid-discard-card-ids-in-state redraw-state discard-card-ids)
     [])))

(defn draw-count-options
  ([db source-id params discard-card-ids]
   (draw-count-options-in-state
    (initial-redraw-state db source-id params)
    discard-card-ids))
  ([db source-id params pass-index discard-card-ids]
   (if-let [redraw-state (redraw-state-before-pass db
                                                   source-id
                                                   params
                                                   pass-index)]
     (draw-count-options-in-state redraw-state discard-card-ids)
     [])))

(defn selected-redraw-count [params]
  (when (some #{(:high-priestess-redraw-count params)}
              options/high-priestess-redraw-count-order)
    (:high-priestess-redraw-count params)))

(defn redraw-pass [params pass-index]
  (let [offset (redraw-pass-offset pass-index)]
    (when (some? offset)
      (get (vec (:redraws params)) offset {:discard-card-ids []}))))

(defn normalize-redraws [db source-id params]
  (if-let [redraw-count (selected-redraw-count params)]
    (let [redraws (vec (:redraws params))]
      (loop [redraw-state (initial-redraw-state db source-id params)
             offset 0
             normalized-redraws []]
        (if (= offset redraw-count)
          (assoc params :redraws normalized-redraws)
          (let [pass (get redraws offset {})
                discard-card-ids (valid-discard-card-ids-in-state
                                  redraw-state
                                  (:discard-card-ids pass))
                options (set (draw-count-options-in-state redraw-state
                                                          discard-card-ids))
                normalized-pass (cond-> (assoc pass
                                               :discard-card-ids discard-card-ids)
                                  (not (contains? options (:draw-count pass)))
                                  (dissoc :draw-count))]
            (recur (apply-staged-redraw-pass redraw-state normalized-pass)
                   (inc offset)
                   (conj normalized-redraws normalized-pass))))))
    (dissoc params :redraws)))

(defn redraws-complete? [db source-id params]
  (if-let [redraw-count (selected-redraw-count params)]
    (let [redraws (vec (:redraws params))]
      (loop [redraw-state (initial-redraw-state db source-id params)
             offset 0]
        (if (= offset redraw-count)
          true
          (let [pass (get redraws offset)
                discard-card-ids (valid-discard-card-ids-in-state
                                  redraw-state
                                  (:discard-card-ids pass))
                options (set (draw-count-options-in-state redraw-state
                                                          discard-card-ids))
                normalized-pass (cond-> (assoc pass
                                               :discard-card-ids discard-card-ids)
                                  (not (contains? options (:draw-count pass)))
                                  (dissoc :draw-count))]
            (if (and (= (vec (:discard-card-ids pass)) discard-card-ids)
                     (contains? options (:draw-count pass)))
              (recur (apply-staged-redraw-pass redraw-state normalized-pass)
                     (inc offset))
              false)))))
    false))
