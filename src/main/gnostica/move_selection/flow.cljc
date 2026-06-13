(ns gnostica.move-selection.flow
  (:require [gnostica.move-selection.commands :as commands]
            [gnostica.move-selection.context :as context]
            [gnostica.move-selection.staging :as staging]
            [gnostica.pieces :as pieces]))

(def required-context-keys
  #{:active-composite-action-power
    :active-power
    :active-sword-major-action-power
    :board
    :board-pieces
    :completed-major-action-count
    :composite-major-move?
    :copied-suit-powers
    :cup-move?
    :current-player-piece-by-id
    :current-player-pieces-on-space
    :death-sword-action-count-option-values
    :death-sword-source?
    :devil-action-count-option-values
    :devil-move?
    :disc-action-count-option-values
    :disc-move?
    :disc-replacement-card-by-id
    :disc-replacement-card-source-option-ids
    :disc-target-kinds
    :disc-target-piece
    :disc-territory-target?
    :draw-count-options
    :fool-active-play?
    :fool-active-reveal
    :fool-active-reveal-card
    :fool-completed-reveal-count
    :fool-move?
    :fool-play-power-options
    :fool-reveal-count-order
    :gameplay-power-command-for-power
    :hand-card-by-id
    :hanged-man-trade-stage?
    :hermit-destination-complete?
    :hermit-orientation-required?
    :hermit-target-piece?
    :hermit-target-selected?
    :hermit-target-territory?
    :hierophant-target-piece?
    :high-priestess-move?
    :high-priestess-redraw-count-order
    :high-priestess-redraws-complete?
    :judgement-card-selection-complete?
    :judgement-move?
    :justice-trade-stage?
    :manipulation-piece-power?
    :move-distance-options
    :move-rod-orientation-required?
    :move-selection
    :move-source-definitions
    :move-target-piece-options
    :one-point-card-by-id
    :piece-by-id
    :rod-modes
    :rod-move?
    :selected-cup-variant
    :selected-death-sword-action-count
    :selected-devil-action-count
    :selected-disc-replacement-card-source
    :selected-fool-play-power
    :selected-fool-reveal-count
    :selected-high-priestess-redraw-count
    :selected-power
    :selected-rod-variant
    :selected-sun-disc-mode
    :selected-sword-replacement-card-source
    :selected-world-copied-power
    :source-card
    :star-disc-source?
    :strength-disc-source?
    :sun-cup-needs-one-point-card?
    :sun-cup-target-kind
    :sun-disc-replacement-card-by-id
    :sun-disc-target-piece
    :sun-disc-territory-target?
    :sun-disc-territory-target-stage?
    :sun-move?
    :sword-damage-options-for
    :sword-major-move?
    :sword-move?
    :sword-orientation-available?
    :sword-replacement-card-by-id
    :sword-replacement-card-source-option-ids
    :sword-target-kinds
    :sword-target-piece
    :sword-territory-target?
    :target-resolution-complete?
    :target-space-complete?
    :valid-board-index?
    :valid-wasteland-target?
    :world-copy-board-cell})

(defn make-context [deps]
  (context/make "gnostica.move-selection.flow" required-context-keys deps))

(defn- call [ctx key & args]
  (apply context/call ctx key args))

(defn- value [ctx key]
  (context/value ctx key))

(defn requirement-complete? [ctx db source-id params requirement]
  (case requirement
    :source-board-index
    (let [index (:source-board-index params)]
      (and (call ctx :valid-board-index? db index)
           (seq (call ctx :current-player-pieces-on-space db index))))

    :hand-card-id
    (some? (call ctx :hand-card-by-id db (:hand-card-id params)))

    :piece-id
    (let [piece (call ctx :current-player-piece-by-id db (:piece-id params))]
      (case source-id
        :activate-territory
        (and piece
             (= (:source-board-index params) (:space-index piece)))

        (:play-hand-card :orient-piece)
        (some? piece)

        false))

    :power
    (some? (call ctx :selected-power db source-id params))

    :copied-board-index
    (some? (call ctx :world-copy-board-cell db (:copied-board-index params)))

    :copied-power
    (some? (call ctx :selected-world-copied-power db source-id params))

    :rod-mode
    (and (call ctx :rod-move? db source-id params)
         (contains? (value ctx :rod-modes) (:rod-mode params)))

    :disc-action-count
    (some #{(:disc-action-count params)}
          (call ctx :disc-action-count-option-values db source-id params))

    :sword-action-count
    (some #{(:sword-action-count params)}
          (call ctx :death-sword-action-count-option-values db source-id params))

    :devil-action-count
    (some #{(:devil-action-count params)}
          (call ctx :devil-action-count-option-values db source-id params))

    :minion-orientation
    (contains? pieces/legal-orientations (:minion-orientation params))

    :sun-disc-mode
    (some? (call ctx :selected-sun-disc-mode db source-id params))

    :sun-disc-target-piece-id
    (some? (call ctx :sun-disc-target-piece db source-id params))

    :sun-disc-target-board-index
    (some #(= (:sun-disc-target-board-index params) (:index %))
          (filterv #(call ctx :sun-disc-territory-target? db source-id params %)
                   (call ctx :board db)))

    :sun-disc-replacement-card-id
    (some? (call ctx
                 :sun-disc-replacement-card-by-id
                 db
                 source-id
                 params
                 (:sun-disc-replacement-card-id params)))

    :disc-target-kind
    (and (call ctx :disc-move? db source-id params)
         (contains? (value ctx :disc-target-kinds) (:disc-target-kind params)))

    :sword-target-kind
    (and (call ctx :sword-move? db source-id params)
         (contains? (value ctx :sword-target-kinds) (:sword-target-kind params)))

    :fool-reveal-count
    (and (call ctx :fool-move? db source-id params)
         (some #{(:fool-reveal-count params)}
               (value ctx :fool-reveal-count-order)))

    :fool-reveal-card
    (and (call ctx :fool-move? db source-id params)
         (some? (call ctx :fool-active-reveal params)))

    :fool-reveal-choice
    (and (call ctx :fool-move? db source-id params)
         (contains? #{:skip :play}
                    (:choice (call ctx :fool-active-reveal params))))

    :fool-play-power
    (and (call ctx :fool-active-play? db source-id params)
         (some? (call ctx :selected-fool-play-power db source-id params))
         (not= :unavailable
               (call ctx :selected-fool-play-power db source-id params)))

    :high-priestess-redraw-count
    (and (call ctx :high-priestess-move? db source-id params)
         (some #{(:high-priestess-redraw-count params)}
               (value ctx :high-priestess-redraw-count-order)))

    :high-priestess-redraws
    (and (call ctx :high-priestess-move? db source-id params)
         (call ctx :high-priestess-redraws-complete? db source-id params))

    :judgement-card-selection
    (and (call ctx :judgement-move? db source-id params)
         (call ctx :judgement-card-selection-complete? db source-id params))

    :target-piece-id
    (cond
      (call ctx :rod-move? db source-id params)
      (some? (call ctx :piece-by-id db (:target-piece-id params)))

      (call ctx :sun-move? db source-id params)
      (some? (call ctx :sun-disc-target-piece db source-id params))

      (call ctx :disc-move? db source-id params)
      (some? (call ctx :disc-target-piece db params))

      (call ctx :sword-move? db source-id params)
      (some? (call ctx :sword-target-piece db source-id params))

      (call ctx :manipulation-piece-power? db source-id params)
      (some? (some #(when (= (:target-piece-id params) (:id %)) %)
                   (if (call ctx :devil-move? db source-id params)
                     (call ctx :move-target-piece-options db)
                     (filterv #(call ctx :hierophant-target-piece? db params %)
                              (call ctx :board-pieces db)))))

      (call ctx :hanged-man-trade-stage? db source-id params)
      (some? (call ctx :piece-by-id db (:target-piece-id params)))

      (call ctx :justice-trade-stage? db source-id params)
      (some? (call ctx :piece-by-id db (:target-piece-id params)))

      :else
      false)

    :target-board-index
    (cond
      (call ctx :sun-disc-territory-target-stage? db source-id params)
      (some #(= (:sun-disc-target-board-index params) (:index %))
            (filterv #(call ctx :sun-disc-territory-target? db source-id params %)
                     (call ctx :board db)))

      (and (call ctx :disc-move? db source-id params)
           (= :territory (:disc-target-kind params)))
      (some #(= (:target-board-index params) (:index %))
            (filterv #(call ctx :disc-territory-target? db source-id params %)
                     (call ctx :board db)))

      (and (call ctx :sword-move? db source-id params)
           (= :territory (:sword-target-kind params)))
      (some #(= (:target-board-index params) (:index %))
            (filterv #(call ctx :sword-territory-target? db source-id params %)
                     (call ctx :board db)))

      :else
      (call ctx :valid-board-index? db (:target-board-index params)))

    :target-space
    (case source-id
      :place-initial-small
      (call ctx :target-space-complete? db source-id params)

      (and (or (call ctx :cup-move? db source-id params)
               (call ctx :sun-move? db source-id params))
           (call ctx :target-space-complete? db source-id params)))

    :target-resolution
    (and (call ctx :cup-move? db source-id params)
         (call ctx :target-resolution-complete? db source-id params))

    :hermit-target-space
    (or (some? (some #(when (= (:target-piece-id params) (:id %)) %)
                     (filterv #(call ctx :hermit-target-piece? db params %)
                              (call ctx :board-pieces db))))
        (some? (some #(when (= (:target-board-index params) (:index %)) %)
                     (filterv #(call ctx :hermit-target-territory? db params %)
                              (call ctx :board db)))))

    :hermit-destination-space
    (call ctx :hermit-destination-complete? db params)

    :replacement-card-source
    (cond
      (call ctx :disc-move? db source-id params)
      (some? (call ctx :selected-disc-replacement-card-source db source-id params))

      (call ctx :sword-move? db source-id params)
      (some? (call ctx :selected-sword-replacement-card-source db source-id params))

      :else
      false)

    :replacement-card-id
    (cond
      (call ctx :sun-move? db source-id params)
      (some? (call ctx
                   :sun-disc-replacement-card-by-id
                   db
                   source-id
                   params
                   (:sun-disc-replacement-card-id params)))

      (call ctx :disc-move? db source-id params)
      (some? (call ctx
                   :disc-replacement-card-by-id
                   db
                   source-id
                   params
                   (:replacement-card-id params)))

      (call ctx :sword-move? db source-id params)
      (some? (call ctx
                   :sword-replacement-card-by-id
                   db
                   source-id
                   params
                   (:replacement-card-id params)))

      :else
      false)

    :one-point-card-id
    (some? (call ctx
                 :one-point-card-by-id
                 db
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

(defn- rod-requirements [ctx db params]
  (let [mode (:rod-mode params)]
    (vec
     (concat [:rod-mode]
             (case mode
               :move-minion [:distance]
               :push-piece [:target-piece-id :distance]
               :push-territory [:target-board-index :distance]
               [])
             (when (and (contains? (value ctx :rod-modes) mode)
                        (call ctx :move-rod-orientation-required? db))
               [:orientation])))))

(defn- disc-requirements [ctx db source-id params]
  (vec
   (concat (when (call ctx :strength-disc-source? db source-id params)
             [:disc-action-count])
           (when (call ctx :star-disc-source? db source-id params)
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

(defn- sword-requirements [ctx db source-id params]
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
  (case (call ctx :active-sword-major-action-power db source-id params)
    :trade-hand [:target-piece-id]
    :orient-minion [:minion-orientation]
    :rod (rod-requirements ctx db params)
    :sword (vec
            (concat (when (call ctx :death-sword-source? db source-id params)
                      [:sword-action-count])
                    (sword-requirements ctx db source-id params)))
    []))

(defn- fool-play-power-choice-needed? [ctx params]
  (< 1 (count (call ctx :fool-play-power-options params))))

(defn- fool-play-requirements [ctx db source-id params]
  (let [child-power (call ctx :selected-fool-play-power db source-id params)]
    (vec
     (concat (when (or (fool-play-power-choice-needed? ctx params)
                       (nil? child-power)
                       (= :unavailable child-power))
               [:fool-play-power])
             (when (and child-power
                        (not= :unavailable child-power))
               (power-requirements ctx db source-id params child-power))))))

(defn- fool-requirements [ctx db source-id params]
  (let [reveal-count (call ctx :selected-fool-reveal-count params)
        completed-count (call ctx :fool-completed-reveal-count params)
        active-reveal (call ctx :fool-active-reveal params)]
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
  (case (call ctx :active-composite-action-power db source-id params)
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
  (let [copy-selected? (some? (call ctx :world-copy-board-cell
                                    db
                                    (:copied-board-index params)))
        copied-power (call ctx :selected-world-copied-power db source-id params)]
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
        card (call ctx :source-card db source-id params)
        power (call ctx :selected-power db source-id params)]
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
    (:requirements (get (value ctx :move-source-definitions) source-id))

    []))

(defn first-missing-requirement [ctx db source-id params]
  (some (fn [requirement]
          (when-not (requirement-complete? ctx db source-id params requirement)
            requirement))
        (move-requirements ctx db source-id params)))

(defn move-missing-fields [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (if source
      (->> (move-requirements ctx db source params)
           (remove #(requirement-complete? ctx db source params %))
           vec)
      [:source])))

(defn stage-for-requirement [ctx db source-id params requirement]
  (case requirement
    :source-board-index :source-territory
    :hand-card-id :hand-card
    :power :power
    :copied-board-index :world-copy
    :copied-power :copied-power
    :piece-id :piece
    :rod-mode :rod-mode
    :disc-action-count :disc-action-count
    :sword-action-count :sword-action-count
    :devil-action-count :devil-action-count
    :minion-orientation :minion-orientation
    :sun-disc-mode :sun-disc-mode
    :disc-target-kind :disc-target-kind
    :sword-target-kind :sword-target-kind
    :fool-reveal-count :fool-reveal-count
    :fool-reveal-card :fool-reveal-card
    :fool-reveal-choice :fool-reveal-choice
    :fool-play-power :fool-play-power
    :high-priestess-redraw-count :high-priestess-redraw-count
    :high-priestess-redraws :high-priestess-redraw
    :judgement-card-selection :judgement-card-selection
    :target-piece-id :target-piece
    :sun-disc-target-piece-id :target-piece
    :target-board-index :target
    :sun-disc-target-board-index :target
    :target-space :target
    :hermit-target-space :target
    :hermit-destination-space :hermit-destination
    :target-resolution (cond
                         (and (call ctx
                                    :valid-wasteland-target?
                                    db
                                    (:target-wasteland params))
                              (= :wheel-cup
                                 (call ctx :selected-cup-variant db source-id params))
                              (nil? (:territory-card-source params)))
                         :territory-card-source

                         (call ctx :valid-wasteland-target?
                               db
                               (:target-wasteland params))
                         :one-point-card

                         :else
                         :orientation)
    :one-point-card-id :one-point-card
    :territory-card-source :territory-card-source
    :replacement-card-source :replacement-card-source
    :replacement-card-id :replacement-card
    :sun-disc-replacement-card-id :replacement-card
    :orientation :orientation
    :distance :distance
    :damage :damage
    :draw-count :draw-count
    :confirm))

(defn- composite-final-action? [ctx _power params]
  (pos? (call ctx :completed-major-action-count params)))

(defn composite-current-action-complete? [ctx db source-id params]
  (case (call ctx :active-composite-action-power db source-id params)
    :orient-minion
    (requirement-complete? ctx db source-id params :minion-orientation)

    :cup
    (and (requirement-complete? ctx db source-id params :target-space)
         (requirement-complete? ctx db source-id params :target-resolution))

    :rod
    (every? #(requirement-complete? ctx db source-id params %)
            (rod-requirements ctx db params))

    :trade-hand
    (requirement-complete? ctx db source-id params :target-piece-id)

    false))

(defn composite-current-action [ctx db source-id params]
  (case (call ctx :active-composite-action-power db source-id params)
    :orient-minion
    {:power :orient-minion
     :piece-id (:piece-id params)
     :orientation (:minion-orientation params)}

    :cup
    (assoc (commands/cup-target-command params)
           :power :cup
           :piece-id (:piece-id params))

    :rod
    (assoc (dissoc (commands/rod-command ctx db source-id params) :rod-variant)
           :power :rod
           :piece-id (:piece-id params))

    :trade-hand
    {:power :trade-hand
     :piece-id (:piece-id params)
     :target {:kind :piece
              :piece-id (:target-piece-id params)}}

    nil))

(defn- sword-major-final-action? [ctx db source-id params]
  (case (call ctx :active-power db source-id params)
    :justice (pos? (call ctx :completed-major-action-count params))
    :tower (pos? (call ctx :completed-major-action-count params))
    :moon (pos? (call ctx :completed-major-action-count params))
    :death (<= (dec (call ctx :selected-death-sword-action-count params))
               (call ctx :completed-major-action-count params))
    false))

(defn sword-major-current-action-complete? [ctx db source-id params]
  (case (call ctx :active-sword-major-action-power db source-id params)
    :trade-hand
    (requirement-complete? ctx db source-id params :target-piece-id)

    :orient-minion
    (requirement-complete? ctx db source-id params :minion-orientation)

    :rod
    (every? #(requirement-complete? ctx db source-id params %)
            (rod-requirements ctx db params))

    :sword
    (every? #(requirement-complete? ctx db source-id params %)
            (sword-requirements ctx db source-id params))

    false))

(defn sword-major-current-action [ctx db source-id params]
  (case (call ctx :active-sword-major-action-power db source-id params)
    :trade-hand
    {:power :trade-hand
     :piece-id (:piece-id params)
     :target {:kind :piece
              :piece-id (:target-piece-id params)}}

    :orient-minion
    {:power :orient-minion
     :piece-id (:piece-id params)
     :orientation (:minion-orientation params)}

    :rod
    (assoc (dissoc (commands/rod-command ctx db source-id params) :rod-variant)
           :power :rod
           :piece-id (:piece-id params))

    :sword
    (assoc (commands/sword-target-command ctx db source-id params)
           :power :sword
           :piece-id (:piece-id params))

    nil))

(defn- devil-final-action? [ctx params]
  (<= (dec (call ctx :selected-devil-action-count params))
      (call ctx :completed-major-action-count params)))

(defn devil-current-action-complete? [ctx db source-id params]
  (and (requirement-complete? ctx db source-id params :target-piece-id)
       (requirement-complete? ctx db source-id params :orientation)))

(defn devil-current-action [_ctx _db _source-id params]
  {:power :orient-target
   :piece-id (:piece-id params)
   :target {:kind :piece
            :piece-id (:target-piece-id params)}
   :orientation (:orientation params)})

(defn- reveal-action-command [reveal]
  (:action reveal {}))

(defn stored-fool-reveal-actions [params]
  (mapv reveal-action-command (:fool-reveals params)))

(defn- fool-current-play-complete? [ctx db source-id params]
  (and (call ctx :fool-active-play? db source-id params)
       (every? #(requirement-complete? ctx db source-id params %)
               (fool-play-requirements ctx db source-id params))))

(defn- fool-current-play-reveal [ctx db source-id params]
  (when (fool-current-play-complete? ctx db source-id params)
    (let [power (call ctx :selected-fool-play-power db source-id params)
          card (call ctx :fool-active-reveal-card params)
          action-power (if (contains? (value ctx :copied-suit-powers) power)
                         power
                         (keyword (:id card)))]
      {:card-id (:card-id (call ctx :fool-active-reveal params))
       :choice :play
       :action {:power action-power
                :piece-id (:piece-id params)
                :play-command (call ctx
                                    :gameplay-power-command-for-power
                                    db
                                    source-id
                                    params
                                    power)}})))

(defn- append-major-action [params action]
  (-> params
      staging/clear-current-major-action-params
      (update :major-actions (fnil conj []) action)))

(defn- advance-composite-steps [ctx db]
  (loop [db db]
    (let [{:keys [source params]} (call ctx :move-selection db)
          power (call ctx :selected-power db source params)]
      (if (and (call ctx :composite-major-move? db source params)
               (not (composite-final-action? ctx power params))
               (composite-current-action-complete? ctx db source params))
        (let [action (composite-current-action ctx db source params)]
          (recur (update-in db
                            [:move-selection :params]
                            append-major-action
                            action)))
        db))))

(defn- advance-sword-major-steps [ctx db]
  (loop [db db]
    (let [{:keys [source params]} (call ctx :move-selection db)]
      (if (and (call ctx :sword-major-move? db source params)
               (not (sword-major-final-action? ctx db source params))
               (sword-major-current-action-complete? ctx db source params))
        (let [action (sword-major-current-action ctx db source params)]
          (recur (update-in db
                            [:move-selection :params]
                            append-major-action
                            action)))
        db))))

(defn- advance-devil-steps [ctx db]
  (loop [db db]
    (let [{:keys [source params]} (call ctx :move-selection db)]
      (if (and (call ctx :devil-move? db source params)
               (requirement-complete? ctx db source params :devil-action-count)
               (not (devil-final-action? ctx params))
               (devil-current-action-complete? ctx db source params))
        (let [action (devil-current-action ctx db source params)]
          (recur (update-in db
                            [:move-selection :params]
                            append-major-action
                            action)))
        db))))

(defn- advance-fool-steps [ctx db]
  (loop [db db]
    (let [{:keys [source params]} (call ctx :move-selection db)]
      (if-let [reveal (and (call ctx :fool-move? db source params)
                           (fool-current-play-reveal ctx db source params))]
        (recur (update-in db
                          [:move-selection :params]
                          staging/commit-fool-active-reveal
                          reveal))
        db))))

(defn advance-move-selection-steps [ctx db]
  (advance-fool-steps
   ctx
   (advance-devil-steps
    ctx
    (advance-sword-major-steps
     ctx
     (advance-composite-steps ctx db)))))

(defn refresh-move-selection [ctx db]
  (let [{:keys [source params] :as selection} (call ctx :move-selection db)]
    (assoc db :move-selection
           (if source
             (let [missing (first-missing-requirement ctx db source params)]
               (assoc selection
                      :stage (if missing
                               (stage-for-requirement ctx db source params missing)
                               :confirm)))
             (assoc selection :stage :source)))))
