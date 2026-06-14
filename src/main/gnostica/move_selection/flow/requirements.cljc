(ns gnostica.move-selection.flow.requirements
  (:require [gnostica.move-selection.context :as context]
            [gnostica.move-selection.options :as options]
            [gnostica.move-selection.power-context :as power]
            [gnostica.move-selection.registry :as registry]
            [gnostica.move-selection.state :as selection]
            [gnostica.pieces :as pieces]))

(def required-context-keys
  #{:disc-replacement-card-by-id
    :disc-replacement-card-source-option-ids
    :disc-target-piece
    :disc-territory-target?
    :draw-count-options
    :hermit-destination-complete?
    :hermit-orientation-required?
    :hermit-target-piece?
    :hermit-target-selected?
    :hermit-target-territory?
    :hierophant-target-piece?
    :high-priestess-redraws-complete?
    :judgement-card-selection-complete?
    :move-distance-options
    :move-rod-orientation-required?
    :move-target-piece-options
    :selected-disc-replacement-card-source
    :selected-high-priestess-redraw-count
    :selected-sun-disc-mode
    :selected-sword-replacement-card-source
    :sun-cup-needs-one-point-card?
    :sun-cup-target-kind
    :sun-disc-replacement-card-by-id
    :sun-disc-target-piece
    :sun-disc-territory-target?
    :sun-disc-territory-target-stage?
    :sword-damage-options-for
    :sword-orientation-available?
    :sword-replacement-card-by-id
    :sword-replacement-card-source-option-ids
    :sword-target-piece
    :sword-territory-target?
    :target-resolution-complete?
    :target-space-complete?})

(defn make-context [deps]
  (context/make "gnostica.move-selection.flow.requirements"
                required-context-keys
                deps))

(defn- call [ctx key & args]
  (apply context/call ctx key args))

(defn requirement-complete? [ctx db source-id params requirement]
  (case requirement
    :source-board-index
    (let [index (:source-board-index params)]
      (and (selection/valid-board-index? db index)
           (seq (selection/current-player-pieces-on-space db index))))

    :hand-card-id
    (some? (selection/hand-card-by-id db (:hand-card-id params)))

    :piece-id
    (let [piece (selection/current-player-piece-by-id db (:piece-id params))]
      (case source-id
        :activate-territory
        (and piece
             (= (:source-board-index params) (:space-index piece)))

        (:play-hand-card :orient-piece)
        (some? piece)

        false))

    :power
    (some? (power/selected-power db source-id params))

    :copied-board-index
    (some? (power/world-copy-board-cell db (:copied-board-index params)))

    :copied-power
    (some? (power/selected-world-copied-power db source-id params))

    :rod-mode
    (and (power/rod-move? db source-id params)
         (contains? options/rod-modes (:rod-mode params)))

    :disc-action-count
    (some #{(:disc-action-count params)}
          (power/disc-action-count-option-values db source-id params))

    :sword-action-count
    (some #{(:sword-action-count params)}
          (power/death-sword-action-count-option-values db source-id params))

    :devil-action-count
    (some #{(:devil-action-count params)}
          (power/devil-action-count-option-values db source-id params))

    :minion-orientation
    (contains? pieces/legal-orientations (:minion-orientation params))

    :sun-disc-mode
    (some? (call ctx :selected-sun-disc-mode db source-id params))

    :sun-disc-target-piece-id
    (some? (call ctx :sun-disc-target-piece db source-id params))

    :sun-disc-target-board-index
    (some #(= (:sun-disc-target-board-index params) (:index %))
          (filterv #(call ctx :sun-disc-territory-target? db source-id params %)
                   (selection/board db)))

    :sun-disc-replacement-card-id
    (some? (call ctx
                 :sun-disc-replacement-card-by-id
                 db
                 source-id
                 params
                 (:sun-disc-replacement-card-id params)))

    :disc-target-kind
    (and (power/disc-move? db source-id params)
         (contains? options/disc-target-kinds (:disc-target-kind params)))

    :sword-target-kind
    (and (power/sword-move? db source-id params)
         (contains? options/sword-target-kinds (:sword-target-kind params)))

    :fool-reveal-count
    (and (power/fool-move? db source-id params)
         (some #{(:fool-reveal-count params)}
               options/fool-reveal-count-order))

    :fool-reveal-card
    (and (power/fool-move? db source-id params)
         (some? (power/fool-active-reveal params)))

    :fool-reveal-choice
    (and (power/fool-move? db source-id params)
         (contains? #{:skip :play}
                    (:choice (power/fool-active-reveal params))))

    :fool-play-power
    (and (power/fool-active-play? db source-id params)
         (some? (power/selected-fool-play-power db source-id params))
         (not= :unavailable
               (power/selected-fool-play-power db source-id params)))

    :high-priestess-redraw-count
    (and (power/high-priestess-move? db source-id params)
         (some #{(:high-priestess-redraw-count params)}
               options/high-priestess-redraw-count-order))

    :high-priestess-redraws
    (and (power/high-priestess-move? db source-id params)
         (call ctx :high-priestess-redraws-complete? db source-id params))

    :judgement-card-selection
    (and (power/judgement-move? db source-id params)
         (call ctx :judgement-card-selection-complete? db source-id params))

    :target-piece-id
    (cond
      (power/rod-move? db source-id params)
      (some? (selection/piece-by-id db (:target-piece-id params)))

      (power/sun-move? db source-id params)
      (some? (call ctx :sun-disc-target-piece db source-id params))

      (power/disc-move? db source-id params)
      (some? (call ctx :disc-target-piece db params))

      (power/sword-move? db source-id params)
      (some? (call ctx :sword-target-piece db source-id params))

      (power/manipulation-piece-power? db source-id params)
      (some? (some #(when (= (:target-piece-id params) (:id %)) %)
                   (if (power/devil-move? db source-id params)
                     (call ctx :move-target-piece-options db)
                     (filterv #(call ctx :hierophant-target-piece? db params %)
                              (selection/board-pieces db)))))

      (power/hanged-man-trade-stage? db source-id params)
      (some? (selection/piece-by-id db (:target-piece-id params)))

      (power/justice-trade-stage? db source-id params)
      (some? (selection/piece-by-id db (:target-piece-id params)))

      :else
      false)

    :target-board-index
    (cond
      (call ctx :sun-disc-territory-target-stage? db source-id params)
      (some #(= (:sun-disc-target-board-index params) (:index %))
            (filterv #(call ctx :sun-disc-territory-target? db source-id params %)
                     (selection/board db)))

      (and (power/disc-move? db source-id params)
           (= :territory (:disc-target-kind params)))
      (some #(= (:target-board-index params) (:index %))
            (filterv #(call ctx :disc-territory-target? db source-id params %)
                     (selection/board db)))

      (and (power/sword-move? db source-id params)
           (= :territory (:sword-target-kind params)))
      (some #(= (:target-board-index params) (:index %))
            (filterv #(call ctx :sword-territory-target? db source-id params %)
                     (selection/board db)))

      :else
      (selection/valid-board-index? db (:target-board-index params)))

    :target-space
    (case source-id
      :place-initial-small
      (call ctx :target-space-complete? db source-id params)

      (and (or (power/cup-move? db source-id params)
               (power/sun-move? db source-id params))
           (call ctx :target-space-complete? db source-id params)))

    :target-resolution
    (and (power/cup-move? db source-id params)
         (call ctx :target-resolution-complete? db source-id params))

    :hermit-target-space
    (or (some? (some #(when (= (:target-piece-id params) (:id %)) %)
                     (filterv #(call ctx :hermit-target-piece? db params %)
                              (selection/board-pieces db))))
        (some? (some #(when (= (:target-board-index params) (:index %)) %)
                     (filterv #(call ctx :hermit-target-territory? db params %)
                              (selection/board db)))))

    :hermit-destination-space
    (call ctx :hermit-destination-complete? db params)

    :replacement-card-source
    (cond
      (power/disc-move? db source-id params)
      (some? (call ctx :selected-disc-replacement-card-source db source-id params))

      (power/sword-move? db source-id params)
      (some? (call ctx :selected-sword-replacement-card-source db source-id params))

      :else
      false)

    :replacement-card-id
    (cond
      (power/sun-move? db source-id params)
      (some? (call ctx
                   :sun-disc-replacement-card-by-id
                   db
                   source-id
                   params
                   (:sun-disc-replacement-card-id params)))

      (power/disc-move? db source-id params)
      (some? (call ctx
                   :disc-replacement-card-by-id
                   db
                   source-id
                   params
                   (:replacement-card-id params)))

      (power/sword-move? db source-id params)
      (some? (call ctx
                   :sword-replacement-card-by-id
                   db
                   source-id
                   params
                   (:replacement-card-id params)))

      :else
      false)

    :one-point-card-id
    (some? (selection/one-point-card-by-id db
                                           source-id
                                           params
                                           (:one-point-card-id params)))

    :orientation
    (contains? pieces/legal-orientations (:orientation params))

    :draw-count
    (some #{(:draw-count params)}
          (call ctx :draw-count-options db (:discard-card-ids params)))

    :distance
    (some #{(:distance params)} (call ctx :move-distance-options db))

    :damage
    (some #{(:damage params)}
          (call ctx :sword-damage-options-for db source-id params))

    false))

(defn rod-requirements [ctx db params]
  (let [mode (:rod-mode params)]
    (vec
     (concat [:rod-mode]
             (case mode
               :move-minion [:distance]
               :push-piece [:target-piece-id :distance]
               :push-territory [:target-board-index :distance]
               [])
             (when (and (contains? options/rod-modes mode)
                        (call ctx :move-rod-orientation-required? db))
               [:orientation])))))

(defn- disc-requirements [ctx db source-id params]
  (vec
   (concat (when (power/strength-disc-source? db source-id params)
             [:disc-action-count])
           (when (power/star-disc-source? db source-id params)
             [:minion-orientation])
           [:disc-target-kind]
           (case (:disc-target-kind params)
             :piece [:target-piece-id]
             :territory
             (concat [:target-board-index]
                     (when (< 1 (count (call ctx
                                             :disc-replacement-card-source-option-ids
                                             db
                                             source-id
                                             params)))
                       [:replacement-card-source])
                     [:replacement-card-id])
             []))))

(defn- sun-requirements [ctx db source-id params]
  (let [mode (call ctx :selected-sun-disc-mode db source-id params)]
    (vec
     (concat [:target-space]
             (when (= :territory (call ctx :sun-cup-target-kind params))
               [:orientation])
             [:sun-disc-mode]
             (when (call ctx :sun-cup-needs-one-point-card? db source-id params)
               [:one-point-card-id])
             (case mode
               :piece [:sun-disc-target-piece-id]
               :territory [:sun-disc-target-board-index
                           :sun-disc-replacement-card-id]
               :created-territory [:sun-disc-replacement-card-id]
               [])))))

(defn sword-requirements [ctx db source-id params]
  (vec
   (concat [:sword-target-kind]
           (case (:sword-target-kind params)
             :piece
             (concat [:target-piece-id :damage]
                     (when (and (some? (:damage params))
                                (call ctx :sword-orientation-available?
                                      db
                                      source-id
                                      params))
                       [:orientation]))

             :territory
             (concat [:target-board-index :damage]
                     (when (and (some? (:damage params))
                                (< 1 (count (call ctx
                                                  :sword-replacement-card-source-option-ids
                                                  db
                                                  source-id
                                                  params))))
                       [:replacement-card-source])
                     (when (seq (call ctx
                                      :sword-replacement-card-source-option-ids
                                      db
                                      source-id
                                      params))
                       [:replacement-card-id]))

             []))))

(declare power-requirements)

(defn- sword-major-requirements [ctx db source-id params]
  (case (power/active-sword-major-action-power db source-id params)
    :trade-hand [:target-piece-id]
    :orient-minion [:minion-orientation]
    :rod (rod-requirements ctx db params)
    :sword (vec
            (concat (when (power/death-sword-source? db source-id params)
                      [:sword-action-count])
                    (sword-requirements ctx db source-id params)))
    []))

(defn- fool-play-power-choice-needed? [_ctx params]
  (< 1 (count (power/fool-play-power-options params))))

(defn fool-play-requirements [ctx db source-id params]
  (let [child-power (power/selected-fool-play-power db source-id params)]
    (vec
     (concat (when (or (fool-play-power-choice-needed? ctx params)
                       (nil? child-power)
                       (= :unavailable child-power))
               [:fool-play-power])
             (when (and child-power
                        (not= :unavailable child-power))
               (power-requirements ctx db source-id params child-power))))))

(defn- fool-requirements [ctx db source-id params]
  (let [reveal-count (power/selected-fool-reveal-count params)
        completed-count (power/fool-completed-reveal-count params)
        active-reveal (power/fool-active-reveal params)]
    (vec
     (concat [:fool-reveal-count]
             (when (some? reveal-count)
               (cond
                 (zero? reveal-count)
                 []

                 active-reveal
                 (concat [:fool-reveal-choice]
                         (when (= :play (:choice active-reveal))
                           (fool-play-requirements ctx db source-id params)))

                 (< completed-count reveal-count)
                 [:fool-reveal-card]

                 :else
                 []))))))

(defn- high-priestess-requirements [ctx _db _source-id params]
  (vec
   (concat [:high-priestess-redraw-count]
           (when (some? (call ctx :selected-high-priestess-redraw-count params))
             [:high-priestess-redraws]))))

(defn- judgement-requirements [_ctx _db _source-id _params]
  [:judgement-card-selection])

(defn- hierophant-requirements [_ctx _db _source-id _params]
  [:target-piece-id :orientation])

(defn- hermit-requirements [ctx db _source-id params]
  (vec
   (concat [:hermit-target-space]
           (when (call ctx :hermit-target-selected? params)
             [:hermit-destination-space])
           (when (and (call ctx :hermit-destination-complete? db params)
                      (call ctx :hermit-orientation-required? db params))
             [:orientation]))))

(defn- devil-requirements [_ctx _db _source-id _params]
  [:devil-action-count :target-piece-id :orientation])

(defn- cup-requirements [_ctx _db _source-id _params]
  [:target-space :target-resolution])

(defn- composite-major-requirements [ctx db source-id params]
  (case (power/active-composite-action-power db source-id params)
    :orient-minion [:minion-orientation]
    :cup (cup-requirements ctx db source-id params)
    :rod (rod-requirements ctx db params)
    :trade-hand [:target-piece-id]
    []))

(defn- power-requirements [ctx db source-id params power]
  (case power
    :cup [:target-space :target-resolution]
    :rod (rod-requirements ctx db params)
    :disc (disc-requirements ctx db source-id params)
    :sun (sun-requirements ctx db source-id params)
    :sword (sword-requirements ctx db source-id params)
    (:empress :emperor :lovers :chariot :hanged-man :temperance)
    (composite-major-requirements ctx db source-id params)
    (:justice :death :tower :moon)
    (sword-major-requirements ctx db source-id params)
    :fool (fool-requirements ctx db source-id params)
    :high-priestess (high-priestess-requirements ctx db source-id params)
    :judgement (judgement-requirements ctx db source-id params)
    :hierophant (hierophant-requirements ctx db source-id params)
    :hermit (hermit-requirements ctx db source-id params)
    :devil (devil-requirements ctx db source-id params)
    []))

(defn- world-requirements [ctx db source-id params]
  (let [copy-selected? (some? (power/world-copy-board-cell
                               db
                               (:copied-board-index params)))
        copied-power (power/selected-world-copied-power db source-id params)]
    (vec
     (concat [:copied-board-index]
             (when (and copy-selected? (nil? copied-power))
               [:copied-power])
             (when copied-power
               (power-requirements ctx db source-id params copied-power))))))

(defn- gameplay-source-requirements [ctx db source-id params]
  (let [base (case source-id
               :activate-territory [:source-board-index :piece-id]
               :play-hand-card [:hand-card-id :piece-id])
        card (selection/source-card db source-id params)
        power (power/selected-power db source-id params)]
    (vec
     (concat base
             (when (and card (nil? power))
               [:power])
             (if (= :world power)
               (world-requirements ctx db source-id params)
               (power-requirements ctx db source-id params power))))))

(defn move-requirements [ctx db source-id params]
  (case source-id
    (:activate-territory :play-hand-card)
    (gameplay-source-requirements ctx db source-id params)

    (:draw-cards :orient-piece :place-initial-small)
    (:requirements (get registry/move-source-definitions source-id))

    []))

(defn first-missing-requirement [ctx db source-id params]
  (some (fn [requirement]
          (when-not (requirement-complete? ctx db source-id params requirement)
            requirement))
        (move-requirements ctx db source-id params)))

(defn move-missing-fields [ctx db]
  (let [{:keys [source params]} (selection/move-selection db)]
    (if source
      (->> (move-requirements ctx db source params)
           (remove #(requirement-complete? ctx db source params %))
           vec)
      [:source])))
