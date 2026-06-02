(ns gnostica.move-selection.commands
  (:require [gnostica.move-selection.context :as context]
            [gnostica.move-selection.registry :as registry]))

(def required-context-keys
  #{:active-card
    :active-power
    :completed-major-actions
    :composite-current-action
    :current-player-id
    :devil-current-action
    :fool-command-map
    :move-selection
    :normalize-high-priestess-redraws
    :selected-cup-variant
    :selected-disc-action-count
    :selected-disc-replacement-card-source
    :selected-disc-variant
    :selected-power
    :selected-rod-variant
    :selected-sword-replacement-card-source
    :selected-sword-variant
    :selected-world-copied-power
    :source-command
    :star-disc-source?
    :stored-fool-reveal-actions
    :strength-disc-source?
    :sword-major-current-action
    :sword-orientation-available?
    :sword-replacement-card-source-option-ids
    :valid-discard-card-ids
    :valid-judgement-card-ids
    :world-move?})

(defn make-context [deps]
  (context/make "gnostica.move-selection.commands" required-context-keys deps))

(defn- call [ctx key & args]
  (apply context/call ctx key args))

(defn- value [ctx key]
  (context/value ctx key))

(defn cup-target-command [params]
  (cond
    (:target-wasteland params)
    (let [territory-card-source (or (:territory-card-source params) :hand)]
      (cond-> {:target (select-keys (:target-wasteland params) [:kind :row :col])
               :territory-card-source territory-card-source}
        (= :hand territory-card-source)
        (assoc :one-point-card-id (:one-point-card-id params))))

    (:target-piece-id params)
    {:target {:kind :piece
              :piece-id (:target-piece-id params)}}

    :else
    {:target {:kind :territory
              :board-index (:target-board-index params)}
     :orientation (:orientation params)}))

(defn- cup-command [ctx db source params]
  (assoc (cup-target-command params)
         :cup-variant (call ctx :selected-cup-variant db source params)))

(defn- sun-cup-command [params]
  (let [cup-target (cup-target-command params)]
    (if (= :created-territory (:sun-disc-mode params))
      (select-keys cup-target [:target])
      cup-target)))

(defn sun-disc-command [params]
  (case (:sun-disc-mode params)
    :skip nil

    :created-piece
    (cond-> {:target {:kind :created-piece}}
      (:orientation params)
      (assoc :orientation (:orientation params)))

    :created-territory
    {:target {:kind :created-territory}
     :replacement-card-source :hand
     :replacement-card-id (:sun-disc-replacement-card-id params)}

    :piece
    (cond-> {:target {:kind :piece
                      :piece-id (:sun-disc-target-piece-id params)}}
      (:sun-disc-orientation params)
      (assoc :orientation (:sun-disc-orientation params)))

    :territory
    {:target {:kind :territory
              :board-index (:sun-disc-target-board-index params)}
     :replacement-card-source :hand
     :replacement-card-id (:sun-disc-replacement-card-id params)}

    nil))

(defn- sun-command [_ctx _db _source params]
  (let [disc-command (sun-disc-command params)]
    (cond-> {:cup (sun-cup-command params)}
      disc-command
      (assoc :disc disc-command))))

(defn- piece-orientation-command [params]
  {:target {:kind :piece
            :piece-id (:target-piece-id params)}
   :orientation (:orientation params)})

(defn- hermit-destination-command [params]
  (if-let [destination-wasteland (:hermit-destination-wasteland params)]
    (select-keys destination-wasteland [:kind :row :col])
    {:kind :territory
     :board-index (:hermit-destination-board-index params)}))

(defn- hermit-command [_ctx _db _source params]
  (cond-> {:target (if (:target-piece-id params)
                     {:kind :piece
                      :piece-id (:target-piece-id params)}
                     {:kind :territory
                      :board-index (:target-board-index params)})
           :destination (hermit-destination-command params)}
    (:orientation params)
    (assoc :orientation (:orientation params))))

(defn- initial-placement-target-command [params]
  (if-let [target-wasteland (:target-wasteland params)]
    (select-keys target-wasteland [:kind :row :col])
    {:kind :territory
     :board-index (:target-board-index params)}))

(defn- rod-target-command [params]
  (case (:rod-mode params)
    :move-minion {}
    :push-piece {:target {:kind :piece
                          :piece-id (:target-piece-id params)}}
    :push-territory {:target {:kind :territory
                              :board-index (:target-board-index params)}}))

(defn rod-command [ctx db source params]
  (let [rod-variant (call ctx :selected-rod-variant db source params)]
    (cond-> (merge {:mode (:rod-mode params)
                    :distance (:distance params)}
                   (rod-target-command params))
      rod-variant
      (assoc :rod-variant rod-variant)

      (:orientation params)
      (assoc :orientation (:orientation params)))))

(defn disc-target-command [ctx db source params]
  (case (:disc-target-kind params)
    :piece
    (cond-> {:target {:kind :piece
                      :piece-id (:target-piece-id params)}}
      (:orientation params)
      (assoc :orientation (:orientation params)))

    :territory
    (cond-> {:target {:kind :territory
                      :board-index (:target-board-index params)}
             :replacement-card-source (call ctx
                                            :selected-disc-replacement-card-source
                                            db
                                            source
                                            params)
             :replacement-card-id (:replacement-card-id params)}
      (nil? (:replacement-card-id params))
      (dissoc :replacement-card-id))

    {}))

(defn- strength-disc-command [ctx db source params]
  (let [action (disc-target-command ctx db source params)
        action-count (call ctx :selected-disc-action-count db source params)]
    {:disc-variant (call ctx :selected-disc-variant db source params)
     :disc-actions (vec (repeat action-count action))}))

(defn- disc-command [ctx db source params]
  (if (call ctx :strength-disc-source? db source params)
    (strength-disc-command ctx db source params)
    (cond-> (assoc (disc-target-command ctx db source params)
                   :disc-variant (call ctx :selected-disc-variant db source params))
      (and (call ctx :star-disc-source? db source params)
           (:minion-orientation params))
      (assoc :minion-orientation (:minion-orientation params)))))

(defn sword-target-command [ctx db source params]
  (case (:sword-target-kind params)
    :piece
    (cond-> {:target {:kind :piece
                      :piece-id (:target-piece-id params)}
             :damage (:damage params)}
      (and (:orientation params)
           (call ctx :sword-orientation-available? db source params))
      (assoc :orientation (:orientation params)))

    :territory
    (cond-> {:target {:kind :territory
                      :board-index (:target-board-index params)}
             :damage (:damage params)}
      (seq (call ctx :sword-replacement-card-source-option-ids db source params))
      (assoc :replacement-card-source
             (call ctx :selected-sword-replacement-card-source db source params))

      (:replacement-card-id params)
      (assoc :replacement-card-id (:replacement-card-id params)))

    {}))

(defn sword-command [ctx db source params]
  (let [sword-variant (call ctx :selected-sword-variant db source params)]
    (cond-> (sword-target-command ctx db source params)
      sword-variant
      (assoc :sword-variant sword-variant))))

(defn- sword-major-command [ctx db source params]
  (let [actions (vec (remove nil?
                             (conj (call ctx :completed-major-actions params)
                                   (call ctx :sword-major-current-action db source params))))
        action-by-power (fn [power]
                          (some #(when (= power (:power %)) %) actions))]
    (case (call ctx :active-power db source params)
      :justice
      (cond-> {:hand-trade-target (:target (action-by-power :trade-hand))}
        (action-by-power :sword)
        (merge (sword-command ctx db source params)))

      :death
      {:sword-actions (mapv #(dissoc % :power) actions)}

      :tower
      (merge {:minion-orientation (:orientation (action-by-power :orient-minion))}
             (sword-command ctx db source params))

      :moon
      {:rod (dissoc (action-by-power :rod) :power)
       :sword (dissoc (call ctx :sword-major-current-action db source params) :power)}

      {})))

(defn- fool-command [ctx _db _source params]
  ((value ctx :fool-command-map) params
                                 (call ctx :stored-fool-reveal-actions params)))

(defn- high-priestess-command [ctx db source params]
  {:redraws (vec (:redraws (call ctx :normalize-high-priestess-redraws
                                 db
                                 source
                                 params)))})

(defn- judgement-command [ctx db source params]
  {:piece-id (:piece-id params)
   :card-ids (call ctx :valid-judgement-card-ids db source params (:judgement-card-ids params))})

(defn- devil-command [ctx db source params]
  (let [actions (vec (remove nil?
                             (conj (call ctx :completed-major-actions params)
                                   (call ctx :devil-current-action db source params))))]
    (if (< 1 (count actions))
      {:actions actions}
      (piece-orientation-command params))))

(defn- unavailable-power-command [ctx db _source params power]
  (let [card (call ctx :active-card db _source params)]
    (cond-> {:power power}
      card
      (assoc :card-id (:id card)))))

(defn- composite-major-command [ctx db source params]
  {:actions (vec (remove nil?
                         (conj (call ctx :completed-major-actions params)
                               (call ctx :composite-current-action db source params))))})

(def ^:private power-command-builders
  {:rod rod-command
   :disc disc-command
   :cup cup-command
   :sun sun-command
   :sword sword-command
   :sword-major sword-major-command
   :fool fool-command
   :high-priestess high-priestess-command
   :judgement judgement-command
   :piece-orientation (fn [_ctx _db _source params]
                        (piece-orientation-command params))
   :hermit hermit-command
   :devil devil-command
   :composite-major composite-major-command})

(defn gameplay-power-command-for-power [ctx db source params power]
  (if-let [builder (get power-command-builders
                        (registry/power-command-builder power))]
    (builder ctx db source params)
    (unavailable-power-command ctx db source params power)))

(defn- world-command-for-power [ctx db source params power]
  (let [power (or power
                  (call ctx :selected-world-copied-power db source params))
        command (gameplay-power-command-for-power ctx db source params power)]
    (cond-> (assoc command :copied-board-index (:copied-board-index params))
      power
      (assoc :copied-power power))))

(defn- world-command [ctx db source params]
  (world-command-for-power ctx db source params nil))

(defn- gameplay-power-command [ctx db source params]
  (if (call ctx :world-move? db source params)
    (world-command ctx db source params)
    (gameplay-power-command-for-power ctx
                                      db
                                      source
                                      params
                                      (call ctx :selected-power db source params))))

(defn gameplay-command-for-power [ctx db source params power]
  (when source
    (merge {:player-id (call ctx :current-player-id db)
            :source (call ctx :source-command source params)}
           (if (call ctx :world-move? db source params)
             (world-command-for-power ctx db source params power)
             (gameplay-power-command-for-power ctx db source params power)))))

(defn gameplay-command [ctx db source params]
  (when source
    (merge {:player-id (call ctx :current-player-id db)
            :source (call ctx :source-command source params)}
           (gameplay-power-command ctx db source params))))

(defn rod-resolver-command [ctx db source params]
  (gameplay-command-for-power ctx db source params :rod))

(defn disc-resolver-command [ctx db source params]
  (let [command (gameplay-command-for-power ctx db source params :disc)]
    (if-let [action (first (:disc-actions command))]
      (merge (select-keys command [:player-id
                                   :source
                                   :copied-board-index
                                   :copied-power
                                   :disc-variant])
             action)
      command)))

(defn sword-resolver-command [ctx db source params]
  (let [power (call ctx :active-power db source params)
        command (gameplay-command-for-power
                 ctx
                 db
                 source
                 params
                 (if (contains? #{:justice :death :tower :moon} power)
                   power
                   :sword))]
    (if-let [action (last (:sword-actions command))]
      (merge (select-keys command [:player-id
                                   :source
                                   :copied-board-index
                                   :copied-power
                                   :sword-variant])
             action)
      command)))

(defn sun-disc-resolver-command [ctx db source params]
  (when-let [disc-command (sun-disc-command params)]
    (merge {:player-id (call ctx :current-player-id db)
            :source (call ctx :source-command source params)
            :disc-variant :disc}
           disc-command)))

(defn move-command [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (when source
      (case source
        :activate-territory
        (gameplay-command ctx db source params)

        :play-hand-card
        (gameplay-command ctx db source params)

        :draw-cards
        {:source :draw-cards
         :player-id (call ctx :current-player-id db)
         :discard-card-ids (call ctx :valid-discard-card-ids db (:discard-card-ids params))
         :draw-count (:draw-count params)}

        :orient-piece
        {:source :orient-piece
         :player-id (call ctx :current-player-id db)
         :piece-id (:piece-id params)
         :orientation (:orientation params)}

        :place-initial-small
        {:source :place-initial-small
         :player-id (call ctx :current-player-id db)
         :target (initial-placement-target-command params)
         :orientation (:orientation params)}

        {:source source
         :player-id (call ctx :current-player-id db)
         :params params}))))
