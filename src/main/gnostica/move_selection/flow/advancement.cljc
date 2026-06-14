(ns gnostica.move-selection.flow.advancement
  (:require [gnostica.move-selection.commands :as commands]
            [gnostica.move-selection.context :as context]
            [gnostica.move-selection.flow.requirements :as requirements]
            [gnostica.move-selection.power-context :as power]
            [gnostica.move-selection.registry :as registry]
            [gnostica.move-selection.staging :as staging]
            [gnostica.move-selection.state :as selection]
            [gnostica.power-taxonomy :as taxonomy]))

(def required-context-keys
  (conj requirements/required-context-keys
        :gameplay-power-command-for-power))

(defn make-context [deps]
  (context/make "gnostica.move-selection.flow.advancement"
                required-context-keys
                deps))

(defn- call [ctx key & args]
  (apply context/call ctx key args))

(defn- requirement-complete? [ctx db source-id params requirement]
  (requirements/requirement-complete? ctx db source-id params requirement))

(defn- composite-final-action? [_ctx _power params]
  (pos? (power/completed-major-action-count params)))

(defn composite-current-action-complete? [ctx db source-id params]
  (case (power/active-composite-action-power db source-id params)
    :orient-minion
    (requirement-complete? ctx db source-id params :minion-orientation)

    :cup
    (and (requirement-complete? ctx db source-id params :target-space)
         (requirement-complete? ctx db source-id params :target-resolution))

    :rod
    (every? #(requirement-complete? ctx db source-id params %)
            (requirements/rod-requirements ctx db params))

    :trade-hand
    (requirement-complete? ctx db source-id params :target-piece-id)

    false))

(defn composite-current-action [ctx db source-id params]
  (case (power/active-composite-action-power db source-id params)
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

(defn- sword-major-final-action? [_ctx db source-id params]
  (case (power/active-power db source-id params)
    :justice (pos? (power/completed-major-action-count params))
    :tower (pos? (power/completed-major-action-count params))
    :moon (pos? (power/completed-major-action-count params))
    :death (<= (dec (power/selected-death-sword-action-count params))
               (power/completed-major-action-count params))
    false))

(defn sword-major-current-action-complete? [ctx db source-id params]
  (case (power/active-sword-major-action-power db source-id params)
    :trade-hand
    (requirement-complete? ctx db source-id params :target-piece-id)

    :orient-minion
    (requirement-complete? ctx db source-id params :minion-orientation)

    :rod
    (every? #(requirement-complete? ctx db source-id params %)
            (requirements/rod-requirements ctx db params))

    :sword
    (every? #(requirement-complete? ctx db source-id params %)
            (requirements/sword-requirements ctx db source-id params))

    false))

(defn sword-major-current-action [ctx db source-id params]
  (case (power/active-sword-major-action-power db source-id params)
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

(defn- devil-final-action? [_ctx params]
  (<= (dec (power/selected-devil-action-count params))
      (power/completed-major-action-count params)))

(defn devil-current-action-complete? [ctx db source-id params]
  (and (requirement-complete? ctx db source-id params :target-piece-id)
       (requirement-complete? ctx db source-id params :orientation)))

(defn devil-current-action [_ctx _db _source-id params]
  {:power :orient-target
   :piece-id (:piece-id params)
   :target {:kind :piece
            :piece-id (:target-piece-id params)}
   :orientation (:orientation params)})

(defn- fool-current-play-complete? [ctx db source-id params]
  (and (power/fool-active-play? db source-id params)
       (every? #(requirement-complete? ctx db source-id params %)
               (requirements/fool-play-requirements ctx db source-id params))))

(defn- fool-current-play-reveal [ctx db source-id params]
  (when (fool-current-play-complete? ctx db source-id params)
    (let [power (power/selected-fool-play-power db source-id params)
          card (power/fool-active-reveal-card params)
          action-power (if (contains? registry/copied-suit-powers power)
                         power
                         (or (taxonomy/full-card-power card)
                             (keyword (:id card))))]
      {:card-id (:card-id (power/fool-active-reveal params))
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
    (let [{:keys [source params]} (selection/move-selection db)
          power (power/selected-power db source params)]
      (if (and (power/composite-major-move? db source params)
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
    (let [{:keys [source params]} (selection/move-selection db)]
      (if (and (power/sword-major-move? db source params)
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
    (let [{:keys [source params]} (selection/move-selection db)]
      (if (and (power/devil-move? db source params)
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
    (let [{:keys [source params]} (selection/move-selection db)]
      (if-let [reveal (and (power/fool-move? db source params)
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
