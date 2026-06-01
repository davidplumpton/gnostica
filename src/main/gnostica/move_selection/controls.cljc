(ns gnostica.move-selection.controls
  (:require [gnostica.move-selection.context :as context]
            [gnostica.move-selection.registry :as registry]))

(def required-context-keys
  #{:active-composite-action-power
    :active-power
    :active-sword-major-action-power
    :board
    :death-sword-action-count-option-values
    :devil-action-count-option-values
    :disc-action-count-option-values
    :fool-active-play?
    :fool-active-reveal
    :fool-active-reveal-card
    :fool-completed-reveal-count
    :fool-completed-reveals
    :fool-move?
    :fool-play-power-options
    :fool-reveal-count-order
    :hand-trade-major-action-count-definitions
    :hand-trade-major-action-count-option-values
    :hand-trade-major-action-count-source?
    :high-priestess-draw-count-options
    :high-priestess-hand-card-options
    :high-priestess-move?
    :high-priestess-redraw-count-order
    :high-priestess-redraw-pass
    :high-priestess-valid-discard-card-ids
    :judgement-card-maximum
    :judgement-discard-card-options
    :judgement-move?
    :move-power-definitions
    :move-power-ids-for-card
    :move-power-order
    :move-selection
    :move-source-definitions
    :move-source-order
    :rod-mode-definitions
    :rod-mode-order
    :selected-fool-play-power
    :selected-fool-reveal-count
    :selected-hand-trade-major-action-count
    :selected-high-priestess-redraw-count
    :selected-power
    :selected-world-copied-power
    :source-card
    :source-unavailable-reason
    :sun-disc-mode-definitions
    :sun-disc-mode-option-ids
    :world-copied-card
    :world-copied-power-ids-for-card
    :world-copy-board-indexes
    :world-move?})

(defn make-context [deps]
  (context/make "gnostica.move-selection.controls" required-context-keys deps))

(defn- call [ctx key & args]
  (apply context/call ctx key args))

(defn- value [ctx key]
  (context/value ctx key))

(defn move-source-options [ctx db]
  (mapv (fn [source-id]
          (let [definition (get (value ctx :move-source-definitions) source-id)
                reason (call ctx :source-unavailable-reason db source-id)]
            (assoc definition
                   :enabled? (nil? reason)
                   :reason reason)))
        (value ctx :move-source-order)))

(defn move-power-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)
        options (call ctx :move-power-ids-for-card
                      (call ctx :source-card db source params))]
    (mapv (value ctx :move-power-definitions)
          (filter #(contains? (set options) %)
                  (value ctx :move-power-order)))))

(defn move-power [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (call ctx :selected-power db source params)))

(defn move-world-copy-options [ctx db]
  (let [board-indexes (set (call ctx :world-copy-board-indexes db))]
    (filterv #(contains? board-indexes (:index %))
             (call ctx :board db))))

(defn move-world-copied-power-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)
        options (call ctx :world-copied-power-ids-for-card
                      (call ctx :world-copied-card db params))]
    (if (call ctx :world-move? db source params)
      (mapv (value ctx :move-power-definitions)
            (filter #(contains? (set options) %)
                    (value ctx :move-power-order)))
      [])))

(defn move-world-copied-power [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (call ctx :selected-world-copied-power db source params)))

(defn move-rod-mode-options [ctx _db]
  (mapv (value ctx :rod-mode-definitions)
        (value ctx :rod-mode-order)))

(defn move-disc-action-count-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (call ctx :disc-action-count-option-values db source params)))

(defn move-major-action-count-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (mapv (value ctx :hand-trade-major-action-count-definitions)
          (call ctx :hand-trade-major-action-count-option-values db source params))))

(defn move-major-action-count [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (when (call ctx :hand-trade-major-action-count-source? db source params)
      (call ctx :selected-hand-trade-major-action-count db source params))))

(defn move-sword-action-count-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (call ctx :death-sword-action-count-option-values db source params)))

(defn move-devil-action-count-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (call ctx :devil-action-count-option-values db source params)))

(defn move-sun-disc-mode-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (mapv (value ctx :sun-disc-mode-definitions)
          (call ctx :sun-disc-mode-option-ids db source params))))

(defn move-fool-reveal-count-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (if (call ctx :fool-move? db source params)
      (value ctx :fool-reveal-count-order)
      [])))

(defn move-fool-play-power-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)
        options (call ctx :fool-play-power-options params)]
    (if (and (call ctx :fool-active-play? db source params)
             (seq options))
      (mapv (value ctx :move-power-definitions)
            (filter #(contains? (set options) %)
                    (value ctx :move-power-order)))
      [])))

(defn move-fool-play-power [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (call ctx :selected-fool-play-power db source params)))

(defn move-fool-reveal-state [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)
        reveal-count (call ctx :selected-fool-reveal-count params)
        completed (call ctx :fool-completed-reveals params)
        active-reveal (call ctx :fool-active-reveal params)
        active-card (call ctx :fool-active-reveal-card params)]
    (when (call ctx :fool-move? db source params)
      (cond-> {:selected-count reveal-count
               :completed-count (count completed)
               :remaining-count (when reveal-count
                                  (max 0 (- reveal-count
                                            (count completed)
                                            (if active-reveal 1 0))))
               :completed-reveals completed
               :active-reveal active-reveal
               :active-card active-card
               :play-power-options (move-fool-play-power-options ctx db)
               :play-power (move-fool-play-power ctx db)
               :can-reveal? (and reveal-count
                                 (not active-reveal)
                                 (< (count completed) reveal-count))}
        active-card
        (assoc :active-card-id (:id active-card)
               :active-card-title (:title active-card)
               :active-card-image (:image active-card))))))

(defn move-high-priestess-redraw-count-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (if (call ctx :high-priestess-move? db source params)
      (value ctx :high-priestess-redraw-count-order)
      [])))

(defn move-high-priestess-redraw-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (if-let [redraw-count (and (call ctx :high-priestess-move? db source params)
                               (call ctx :selected-high-priestess-redraw-count params))]
      (mapv (fn [pass-index]
              (let [pass (call ctx :high-priestess-redraw-pass params pass-index)
                    discard-card-ids (:discard-card-ids pass)
                    draw-count-options (call ctx
                                             :high-priestess-draw-count-options
                                             db
                                             source
                                             params
                                             pass-index
                                             discard-card-ids)]
                {:pass-index pass-index
                 :discard-card-options (call ctx
                                             :high-priestess-hand-card-options
                                             db
                                             source
                                             params
                                             pass-index)
                 :selected-discard-card-ids (call ctx
                                                  :high-priestess-valid-discard-card-ids
                                                  db
                                                  source
                                                  params
                                                  pass-index
                                                  discard-card-ids)
                 :draw-count-options draw-count-options
                 :selected-draw-count (when (some #{(:draw-count pass)}
                                                  draw-count-options)
                                        (:draw-count pass))}))
            (range 1 (inc redraw-count)))
      [])))

(defn move-judgement-card-options [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (if (call ctx :judgement-move? db source params)
      (call ctx :judgement-discard-card-options db source params)
      [])))

(defn move-judgement-card-maximum [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (if (call ctx :judgement-move? db source params)
      (call ctx :judgement-card-maximum db source params)
      0)))

(declare power-control-groups)

(defn- control-group
  ([type]
   {:type type})
  ([type attrs]
   (assoc attrs :type type)))

(defn- power-control-group [power type]
  (control-group type {:power power}))

(defn- major-action-control-group [power action-power type]
  (control-group type {:power power
                       :action-power action-power}))

(defn- composite-major-control-groups [ctx db source-id params]
  (let [power (call ctx :active-power db source-id params)
        action-power (call ctx :active-composite-action-power db source-id params)]
    (vec
     (concat
      (when (seq (call ctx :hand-trade-major-action-count-option-values db source-id params))
        [(power-control-group power :major-action-count)])
      (case action-power
        :orient-minion [(major-action-control-group power action-power :minion-orientation)]
        :rod [(major-action-control-group power action-power :rod)]
        :cup [(major-action-control-group power action-power :cup)]
        :trade-hand [(major-action-control-group power action-power :target-piece)]
        [])))))

(defn- sword-major-control-groups [ctx db source-id params]
  (let [power (call ctx :active-power db source-id params)
        action-power (call ctx :active-sword-major-action-power db source-id params)]
    (vec
     (concat
      (when (seq (call ctx :hand-trade-major-action-count-option-values db source-id params))
        [(power-control-group power :major-action-count)])
      (when (= :death power)
        [(power-control-group power :sword-action-count)])
      (when (or (not= :death power)
                (:sword-action-count params))
        (case action-power
          :trade-hand [(major-action-control-group power action-power :target-piece)]
          :orient-minion [(major-action-control-group power action-power :minion-orientation)]
          :rod [(major-action-control-group power action-power :rod)]
          :sword [(major-action-control-group power action-power :sword)]
          []))))))

(defn- fool-play-power-choice-needed? [ctx params]
  (< 1 (count (call ctx :fool-play-power-options params))))

(defn- fool-control-groups [ctx db source-id params]
  (let [reveal-count (call ctx :selected-fool-reveal-count params)
        completed-count (call ctx :fool-completed-reveal-count params)
        active-reveal (call ctx :fool-active-reveal params)
        child-power (call ctx :selected-fool-play-power db source-id params)]
    (vec
     (concat
      [(power-control-group :fool :fool-reveal-count)]
      (cond
        (nil? reveal-count)
        []

        (zero? reveal-count)
        []

        active-reveal
        (concat
         [(power-control-group :fool :fool-reveal-decision)]
         (when (= :play (:choice active-reveal))
           (concat
            (when (and (fool-play-power-choice-needed? ctx params)
                       (nil? child-power))
              [(power-control-group :fool :fool-play-power)])
            (when child-power
              (power-control-groups ctx db source-id params child-power)))))

        (< completed-count reveal-count)
        [(power-control-group :fool :fool-reveal-card)]

        :else
        [])))))

(defn- world-control-groups [ctx db source-id params]
  (vec
   (concat
    [(power-control-group :world :world-copy)]
    (when (some? (:copied-board-index params))
      [(power-control-group :world :world-copied-power)])
    (when-let [copied-power (call ctx :selected-world-copied-power db source-id params)]
      (power-control-groups ctx db source-id params copied-power)))))

(defn- power-control-groups [ctx db source-id params power]
  (case (registry/power-control-kind power)
    :static
    (mapv #(power-control-group power %)
          (registry/power-control-groups power))

    :fool
    (fool-control-groups ctx db source-id params)

    :world
    (world-control-groups ctx db source-id params)

    :composite-major
    (composite-major-control-groups ctx db source-id params)

    :sword-major
    (sword-major-control-groups ctx db source-id params)

    []))

(defn- gameplay-control-groups [ctx db source-id params]
  (let [power (call ctx :selected-power db source-id params)]
    (vec
     (concat
      [(control-group :power)]
      (power-control-groups ctx db source-id params power)))))

(defn move-control-groups [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (case source
      :activate-territory
      (vec
       (concat
        [(control-group :source-board)]
        (when (some? (:source-board-index params))
          [(control-group :piece)])
        (when (:piece-id params)
          (gameplay-control-groups ctx db source params))))

      :play-hand-card
      (vec
       (concat
        [(control-group :hand-card)]
        (when (:hand-card-id params)
          [(control-group :piece)])
        (when (:piece-id params)
          (gameplay-control-groups ctx db source params))))

      :draw-cards
      [(control-group :discard-cards)
       (control-group :draw-count)]

      :orient-piece
      (vec
       (concat
        [(control-group :piece)]
        (when (:piece-id params)
          [(control-group :orientation)])))

      :place-initial-small
      (vec
       (concat
        [(control-group :target-space)]
        (when (or (some? (:target-board-index params))
                  (:target-wasteland params))
          [(control-group :orientation)])))

      [])))
