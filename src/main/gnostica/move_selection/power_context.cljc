(ns gnostica.move-selection.power-context
  (:require [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]
            [gnostica.move-selection.options :as options]
            [gnostica.move-selection.registry :as registry]
            [gnostica.move-selection.state :as state]))

(defn move-power-ids-for-card [card]
  (registry/power-ids-for-card card))

(defn selected-power-for-card [card selected]
  (let [power-options (move-power-ids-for-card card)]
    (cond
      (contains? (set power-options) selected)
      selected

      (= 1 (count power-options))
      (first power-options)

      (and card (empty? power-options))
      :unavailable

      :else
      nil)))

(defn selected-power [db source-id params]
  (when (state/gameplay-move-source? source-id)
    (let [card (state/source-card db source-id params)
          selected (:power params)]
      (selected-power-for-card card selected))))

(defn world-move? [db source-id params]
  (= :world (selected-power db source-id params)))

(defn world-copy-board-indexes [db]
  (set (map :board-index (game-state/world-major-territories (state/game db)))))

(defn world-copy-board-cell [db board-index]
  (when (contains? (world-copy-board-indexes db) board-index)
    (state/board-cell-by-index db board-index)))

(defn world-copied-card [db params]
  (:card (world-copy-board-cell db (:copied-board-index params))))

(defn world-copied-power-ids-for-card [card]
  (registry/copied-power-ids-for-card card))

(defn selected-world-copied-power [db source-id params]
  (when (world-move? db source-id params)
    (let [card (world-copied-card db params)
          power-options (world-copied-power-ids-for-card card)
          selected (:copied-power params)]
      (cond
        (contains? (set power-options) selected)
        selected

        (= 1 (count power-options))
        (first power-options)

        (and card (empty? power-options))
        :unavailable

        :else
        nil))))

(defn parent-fool-move? [db source-id params]
  (or (= :fool (selected-power db source-id params))
      (and (world-move? db source-id params)
           (= :fool (selected-world-copied-power db source-id params)))))

(defn fool-completed-reveals [params]
  (vec (:fool-reveals params)))

(defn fool-completed-reveal-count [params]
  (count (fool-completed-reveals params)))

(defn selected-fool-reveal-count [params]
  (when (some #{(:fool-reveal-count params)} options/fool-reveal-count-order)
    (:fool-reveal-count params)))

(defn fool-active-reveal [params]
  (:fool-active-reveal params))

(defn fool-active-reveal-card [params]
  (some-> (fool-active-reveal params)
          :card-id
          cards/card-by-id))

(defn fool-play-power-options [params]
  (registry/fool-play-power-ids-for-card (fool-active-reveal-card params)))

(defn fool-active-play? [db source-id params]
  (and (parent-fool-move? db source-id params)
       (= :play (:choice (fool-active-reveal params)))))

(defn selected-fool-play-power [db source-id params]
  (when (fool-active-play? db source-id params)
    (let [power-options (fool-play-power-options params)
          selected (:fool-play-power params)]
      (cond
        (contains? (set power-options) selected)
        selected

        (= 1 (count power-options))
        (first power-options)

        (and (fool-active-reveal-card params)
             (empty? power-options))
        :unavailable

        :else
        nil))))

(defn active-power [db source-id params]
  (if-let [fool-power (selected-fool-play-power db source-id params)]
    fool-power
    (if (world-move? db source-id params)
      (selected-world-copied-power db source-id params)
      (selected-power db source-id params))))

(defn active-card [db source-id params]
  (if-let [card (and (fool-active-play? db source-id params)
                     (fool-active-reveal-card params))]
    card
    (if (world-move? db source-id params)
      (world-copied-card db params)
      (state/source-card db source-id params))))

(defn world-source-opts [db source-id params]
  (when (world-move? db source-id params)
    (let [source-result (game-state/resolve-major-source
                         (state/game db)
                         {:player-id (state/current-player-id db)
                          :source (state/source-command source-id params)})
          copied-card (world-copied-card db params)]
      (when (and (:ok? source-result) copied-card)
        (assoc (game-state/major-paid-source-opts source-result)
               :power-card copied-card
               :allow-major-minion? true)))))

(defn completed-major-actions [params]
  (vec (:major-actions params)))

(defn completed-major-action-count [params]
  (count (completed-major-actions params)))

(defn hand-trade-major-action-count-source? [db source-id params]
  (contains? #{:hanged-man :justice}
             (active-power db source-id params)))

(defn hand-trade-major-action-count-option-values [db source-id params]
  (if (hand-trade-major-action-count-source? db source-id params)
    options/hand-trade-major-action-count-order
    []))

(defn selected-hand-trade-major-action-count [db source-id params]
  (if (some #{(:major-action-count params)}
            (hand-trade-major-action-count-option-values db source-id params))
    (:major-action-count params)
    2))

(defn ordered-major-action-count-source? [db source-id params]
  (contains? #{:chariot :temperance}
             (active-power db source-id params)))

(defn moon-action-choice-source? [db source-id params]
  (= :moon (active-power db source-id params)))

(defn major-action-count-option-values [db source-id params]
  (cond
    (hand-trade-major-action-count-source? db source-id params)
    options/hand-trade-major-action-count-order

    (ordered-major-action-count-source? db source-id params)
    options/ordered-major-action-count-order

    (moon-action-choice-source? db source-id params)
    options/moon-action-choice-order

    :else
    []))

(defn major-action-count-option-definitions [db source-id params]
  (cond
    (hand-trade-major-action-count-source? db source-id params)
    options/hand-trade-major-action-count-definitions

    (ordered-major-action-count-source? db source-id params)
    options/ordered-major-action-count-definitions

    (moon-action-choice-source? db source-id params)
    options/moon-action-choice-definitions

    :else
    {}))

(defn selected-major-action-count [db source-id params]
  (let [selected (:major-action-count params)]
    (cond
      (some #{selected} (major-action-count-option-values db source-id params))
      selected

      (hand-trade-major-action-count-source? db source-id params)
      2

      (ordered-major-action-count-source? db source-id params)
      2

      (moon-action-choice-source? db source-id params)
      :both

      :else
      nil)))

(defn composite-major-move? [db source-id params]
  (contains? registry/composite-major-powers (active-power db source-id params)))

(defn active-composite-action-power [db source-id params]
  (case (active-power db source-id params)
    :empress (if (zero? (completed-major-action-count params))
               :orient-minion
               :cup)
    :emperor (if (zero? (completed-major-action-count params))
               :orient-minion
               :rod)
    :lovers (if (zero? (completed-major-action-count params))
              :rod
              :cup)
    :chariot (when (< (completed-major-action-count params)
                      (selected-major-action-count db source-id params))
               :rod)
    :hanged-man (let [action-count (selected-hand-trade-major-action-count
                                    db
                                    source-id
                                    params)
                      completed-count (completed-major-action-count params)]
                  (when (< completed-count action-count)
                    (if (= 1 action-count)
                      :trade-hand
                      (if (zero? completed-count)
                        :rod
                        :trade-hand))))
    :temperance (when (< (completed-major-action-count params)
                         (selected-major-action-count db source-id params))
                  :cup)
    nil))

(defn composite-action-power? [db source-id params power]
  (= power (active-composite-action-power db source-id params)))

(defn selected-death-sword-action-count [params]
  (if (some #{(:sword-action-count params)}
            options/death-sword-action-count-order)
    (:sword-action-count params)
    1))

(defn sword-major-move? [db source-id params]
  (contains? registry/sword-major-powers (active-power db source-id params)))

(defn active-sword-major-action-power [db source-id params]
  (case (active-power db source-id params)
    :justice (let [action-count (selected-hand-trade-major-action-count
                                 db
                                 source-id
                                 params)
                   completed-count (completed-major-action-count params)]
               (when (< completed-count action-count)
                 (if (zero? completed-count)
                   :trade-hand
                   :sword)))
    :death :sword
    :tower (if (zero? (completed-major-action-count params))
             :orient-minion
             :sword)
    :moon (let [choice (selected-major-action-count db source-id params)
                completed-count (completed-major-action-count params)]
            (case choice
              :rod-only (when (zero? completed-count)
                          :rod)
              :sword-only (when (zero? completed-count)
                            :sword)
              :both (case completed-count
                      0 :rod
                      1 :sword
                      nil)
              nil))
    nil))

(defn sword-major-action-power? [db source-id params power]
  (= power (active-sword-major-action-power db source-id params)))

(defn hanged-man-trade-stage? [db source-id params]
  (composite-action-power? db source-id params :trade-hand))

(defn justice-trade-stage? [db source-id params]
  (sword-major-action-power? db source-id params :trade-hand))

(defn major-orient-step? [db source-id params]
  (or (composite-action-power? db source-id params :orient-minion)
      (sword-major-action-power? db source-id params :orient-minion)))

(defn cup-move? [db source-id params]
  (or (= :cup (active-power db source-id params))
      (composite-action-power? db source-id params :cup)))

(defn sun-move? [db source-id params]
  (= :sun (active-power db source-id params)))

(defn selected-cup-variant [db source-id params]
  (when (cup-move? db source-id params)
    (cards/cup-variant (active-card db source-id params))))

(defn territory-card-source-option-ids [db source-id params]
  (when (cup-move? db source-id params)
    (if (= :wheel-cup (selected-cup-variant db source-id params))
      options/territory-card-source-order
      [:hand])))

(defn rod-move? [db source-id params]
  (or (= :rod (active-power db source-id params))
      (composite-action-power? db source-id params :rod)
      (sword-major-action-power? db source-id params :rod)))

(defn selected-rod-variant [db source-id params]
  (when (rod-move? db source-id params)
    (cards/rod-variant (active-card db source-id params))))

(defn disc-move? [db source-id params]
  (= :disc (active-power db source-id params)))

(defn selected-disc-variant [db source-id params]
  (when (disc-move? db source-id params)
    (cards/disc-variant (active-card db source-id params))))

(defn strength-disc-source? [db source-id params]
  (and (disc-move? db source-id params)
       (= "strength" (:id (active-card db source-id params)))))

(defn star-disc-source? [db source-id params]
  (and (disc-move? db source-id params)
       (= "star" (:id (active-card db source-id params)))))

(defn disc-action-count-option-values [db source-id params]
  (if (strength-disc-source? db source-id params)
    options/strength-disc-action-count-order
    []))

(defn selected-disc-action-count [db source-id params]
  (if (strength-disc-source? db source-id params)
    (if (some #{(:disc-action-count params)}
              options/strength-disc-action-count-order)
      (:disc-action-count params)
      1)
    1))

(defn card-worth-disc-actions-more? [replacement-card original-card action-count]
  (let [original-value (cards/card-point-value original-card)
        replacement-value (cards/card-point-value replacement-card)]
    (and (some? original-value)
         (= (+ original-value action-count)
            replacement-value))))

(defn sword-move? [db source-id params]
  (or (= :sword (active-power db source-id params))
      (sword-major-action-power? db source-id params :sword)))

(defn death-sword-source? [db source-id params]
  (= :death (active-power db source-id params)))

(defn death-sword-action-count-option-values [db source-id params]
  (if (death-sword-source? db source-id params)
    options/death-sword-action-count-order
    []))

(defn fool-move? [db source-id params]
  (parent-fool-move? db source-id params))

(defn high-priestess-move? [db source-id params]
  (= :high-priestess (active-power db source-id params)))

(defn judgement-move? [db source-id params]
  (= :judgement (active-power db source-id params)))

(defn hierophant-move? [db source-id params]
  (= :hierophant (active-power db source-id params)))

(defn hermit-move? [db source-id params]
  (= :hermit (active-power db source-id params)))

(defn devil-move? [db source-id params]
  (= :devil (active-power db source-id params)))

(defn devil-action-count-option-values [db source-id params]
  (if (devil-move? db source-id params)
    options/devil-action-count-order
    []))

(defn selected-devil-action-count [params]
  (if (some #{(:devil-action-count params)}
            options/devil-action-count-order)
    (:devil-action-count params)
    1))

(defn manipulation-piece-power? [db source-id params]
  (or (hierophant-move? db source-id params)
      (devil-move? db source-id params)))

(defn selected-sword-variant [db source-id params]
  (when (sword-move? db source-id params)
    (cards/sword-variant (active-card db source-id params))))

(defn card-worth-sword-damage-less? [replacement-card original-card damage]
  (let [original-value (cards/card-point-value original-card)
        replacement-value (cards/card-point-value replacement-card)]
    (and (some? original-value)
         (pos? (- original-value damage))
         (= (- original-value damage)
            replacement-value))))

(defn- reveal-action-command [reveal]
  (:action reveal {}))

(defn stored-fool-reveal-actions [params]
  (mapv reveal-action-command (:fool-reveals params)))
