(ns gnostica.move-selection.ribbon
  (:require [gnostica.cards :as cards]
            [gnostica.move-selection.context :as context]
            [gnostica.move-selection.options :as options]
            [gnostica.move-selection.registry :as registry]
            [gnostica.pieces :as pieces]))

(def required-context-keys
  #{:active-power
    :board-cell-by-index
    :completed-major-actions
    :composite-current-action
    :composite-current-action-complete?
    :composite-major-move?
    :devil-current-action
    :devil-current-action-complete?
    :devil-move?
    :fool-active-reveal
    :fool-completed-reveals
    :high-priestess-draw-count-options-in-state
    :high-priestess-redraw-pass
    :high-priestess-redraw-state-before-pass
    :high-priestess-valid-discard-card-ids-in-state
    :judgement-card-maximum
    :move-prompt
    :move-ready?
    :move-selection
    :piece-by-id
    :requirement-complete?
    :selected-death-sword-action-count
    :selected-devil-action-count
    :selected-fool-reveal-count
    :selected-hand-trade-major-action-count
    :selected-high-priestess-redraw-count
    :selected-major-action-count
    :selected-power
    :selected-sun-disc-mode
    :selected-world-copied-power
    :sun-cup-needs-one-point-card?
    :sun-cup-target-ready?
    :sword-major-current-action
    :sword-major-current-action-complete?
    :sword-major-move?})

(defn make-context [deps]
  (context/make "gnostica.move-selection.ribbon" required-context-keys deps))

(defn- call [ctx key & args]
  (apply context/call ctx key args))

(def ^:private action-ribbon-powers
  #{:empress :emperor :lovers :chariot :hanged-man :temperance
    :justice :death :tower :moon
    :sun :fool :high-priestess :judgement :devil :world})

(def ^:private action-step-labels
  {:orient-minion "Orient minion"
   :orient-target "Orient target"
   :cup "Cup"
   :rod "Rod"
   :trade-hand "Trade hands"
   :sword "Sword"
   :sun-cup "Cup"
   :sun-disc "Disc"
   :fool-reveal "Reveal"
   :high-priestess-redraw "Redraw"
   :judgement-draw "Draw discards"
   :world-copy "World copy"
   :world-power "Copied power"})

(defn- power-label [power]
  (registry/power-label power))

(defn- player-label [player-id]
  (get-in pieces/players-by-id [player-id :name] (name player-id)))

(defn- piece-label [ctx db piece-id]
  (if-let [piece (call ctx :piece-by-id db piece-id)]
    (str (player-label (:player-id piece))
         " "
         (pieces/size-label (:size piece)))
    (str piece-id)))

(defn- territory-label [ctx db board-index]
  (if-let [cell (call ctx :board-cell-by-index db board-index)]
    (str (:title (:card cell)) " [" board-index "]")
    (str "Territory [" board-index "]")))

(defn- wasteland-label [{:keys [row col]}]
  (str "Wasteland row " (inc row) ", column " (inc col)))

(defn- target-label [ctx db target]
  (case (:kind target)
    :territory (territory-label ctx db (:board-index target))
    :piece (piece-label ctx db (:piece-id target))
    :wasteland (wasteland-label target)
    "Target"))

(defn- action-detail [ctx db action]
  (case (:power action)
    :orient-minion
    (str (piece-label ctx db (:piece-id action))
         " to "
         (pieces/orientation-label (:orientation action)))

    :orient-target
    (str (target-label ctx db (:target action))
         " to "
         (pieces/orientation-label (:orientation action)))

    :cup
    (cond-> (target-label ctx db (:target action))
      (:orientation action)
      (str " facing " (pieces/orientation-label (:orientation action))))

    :rod
    (let [mode-label (get-in options/rod-mode-definitions [(:mode action) :label] "Rod")
          distance (:distance action)
          target (:target action)]
      (str mode-label
           (when distance
             (str " " distance))
           (when target
             (str " to " (target-label ctx db target)))
           (when (:orientation action)
             (str ", " (pieces/orientation-label (:orientation action))))))

    :trade-hand
    (str "With " (target-label ctx db (:target action)))

    :sword
    (str (target-label ctx db (:target action))
         (when-let [damage (:damage action)]
           (str ", " damage " damage"))
         (when (:orientation action)
           (str ", " (pieces/orientation-label (:orientation action)))))

    "Ready"))

(defn- indexed-action-plan [items]
  (first
   (reduce (fn [[result index] item]
             (if (:skipped? item)
               [(conj result item) index]
               [(conj result (assoc item :action-index index)) (inc index)]))
           [[] 0]
           items)))

(defn- optional-paired-action-plan [power selected detail]
  (if (= 1 selected)
    [{:power power}
     {:power power
      :skipped? true
      :detail detail}]
    [{:power power}
     {:power power}]))

(defn- composite-action-plan [ctx db source-id params]
  (indexed-action-plan
   (case (call ctx :active-power db source-id params)
     :empress [{:power :orient-minion}
               {:power :cup}]
     :emperor [{:power :orient-minion}
               {:power :rod}]
     :lovers [{:power :rod}
              {:power :cup}]
     :chariot (optional-paired-action-plan
               :rod
               (call ctx :selected-major-action-count db source-id params)
               "Use one")
     :hanged-man (if (= 1 (call ctx
                                :selected-hand-trade-major-action-count
                                db
                                source-id
                                params))
                   [{:power :rod
                     :skipped? true
                     :detail "Trade only"}
                    {:power :trade-hand}]
                   [{:power :rod}
                    {:power :trade-hand}])
     :temperance (optional-paired-action-plan
                  :cup
                  (call ctx :selected-major-action-count db source-id params)
                  "Use one")
     [])))

(defn- moon-action-plan [ctx db source-id params]
  (case (call ctx :selected-major-action-count db source-id params)
    :rod-only [{:power :rod}
               {:power :sword
                :skipped? true
                :detail "Move only"}]
    :sword-only [{:power :rod
                  :skipped? true
                  :detail "Attack only"}
                 {:power :sword}]
    :both [{:power :rod}
           {:power :sword}]
    [{:power :rod}
     {:power :sword}]))

(defn- sword-major-action-plan [ctx db source-id params]
  (indexed-action-plan
   (case (call ctx :active-power db source-id params)
     :justice (if (= 1 (call ctx
                             :selected-hand-trade-major-action-count
                             db
                             source-id
                             params))
                [{:power :trade-hand}
                 {:power :sword
                  :skipped? true
                  :detail "Trade only"}]
                [{:power :trade-hand}
                 {:power :sword}])
     :death (vec (repeat (call ctx :selected-death-sword-action-count params)
                         {:power :sword}))
     :tower [{:power :orient-minion}
             {:power :sword}]
     :moon (moon-action-plan ctx db source-id params)
     [])))

(defn- devil-action-plan [ctx params]
  (indexed-action-plan
   (vec (repeat (or (:devil-action-count params)
                    (call ctx :selected-devil-action-count params))
                {:power :orient-target}))))

(defn- current-major-action [ctx db source-id params]
  (cond
    (call ctx :composite-major-move? db source-id params)
    (call ctx :composite-current-action db source-id params)

    (call ctx :sword-major-move? db source-id params)
    (call ctx :sword-major-current-action db source-id params)

    (call ctx :devil-move? db source-id params)
    (call ctx :devil-current-action db source-id params)))

(defn- current-major-action-complete? [ctx db source-id params]
  (cond
    (call ctx :composite-major-move? db source-id params)
    (call ctx :composite-current-action-complete? db source-id params)

    (call ctx :sword-major-move? db source-id params)
    (call ctx :sword-major-current-action-complete? db source-id params)

    (call ctx :devil-move? db source-id params)
    (call ctx :devil-current-action-complete? db source-id params)

    :else
    false))

(defn- action-step-key [action]
  [(:power action)
   (:mode action)
   (:target action)
   (:piece-id action)])

(defn- compact-compound-steps [steps]
  (loop [remaining steps
         result []]
    (if-let [step (first remaining)]
      (let [same-run (take-while #(and (#{:done :ready} (:status %))
                                       (= (action-step-key (:action step))
                                          (action-step-key (:action %))))
                                 remaining)
            run-count (count same-run)]
        (if (< 1 run-count)
          (recur (drop run-count remaining)
                 (conj result
                       (-> step
                           (assoc :id (str (:id step) "-compound")
                                  :label (str (:label step) " x" run-count)
                                  :compound? true
                                  :detail "Same target shortcut"))))
          (recur (rest remaining)
                 (conj result step))))
      (vec result))))

(defn- sequence-action-steps [ctx db source-id params plan]
  (let [completed-actions (call ctx :completed-major-actions params)
        completed-count (count completed-actions)
        current-complete? (current-major-action-complete? ctx db source-id params)
        current-action (when current-complete?
                         (current-major-action ctx db source-id params))]
    (compact-compound-steps
     (mapv (fn [{:keys [power skipped? action-index detail] :as item} position]
             (let [label (or (:label item)
                             (get action-step-labels power (power-label power)))
                   action (when (int? action-index)
                            (cond
                              (< action-index completed-count)
                              (nth completed-actions action-index)

                              (and (= action-index completed-count)
                                   current-complete?)
                              current-action))]
               (cond
                 skipped?
                 (assoc item
                        :id (str "skipped-" position "-" (name power))
                        :label label
                        :status :skipped
                        :detail detail)

                 (< action-index completed-count)
                 (assoc item
                        :id (str "done-" action-index "-" (name power))
                        :label label
                        :status :done
                        :action action
                        :detail (action-detail ctx db action))

                 (= action-index completed-count)
                 (assoc item
                        :id (str "active-" action-index "-" (name power))
                        :label label
                        :status (if current-complete? :ready :active)
                        :action action
                        :detail (if current-complete?
                                  (action-detail ctx db action)
                                  (call ctx :move-prompt db)))

                 :else
                 (assoc item
                        :id (str "pending-" action-index "-" (name power))
                        :label label
                        :status :pending
                        :detail "Pending"))))
           plan
           (range)))))

(defn- sun-cup-ribbon-complete? [ctx db source-id params]
  (and (call ctx :sun-cup-target-ready? db source-id params)
       (or (not (call ctx :sun-cup-needs-one-point-card? db source-id params))
           (call ctx :requirement-complete? db source-id params :one-point-card-id))))

(defn- sun-disc-ribbon-complete? [ctx db source-id params]
  (case (call ctx :selected-sun-disc-mode db source-id params)
    :skip true
    :created-piece true
    :created-territory (call ctx
                             :requirement-complete?
                             db
                             source-id
                             params
                             :sun-disc-replacement-card-id)
    :piece (call ctx
                 :requirement-complete?
                 db
                 source-id
                 params
                 :sun-disc-target-piece-id)
    :territory (and (call ctx
                          :requirement-complete?
                          db
                          source-id
                          params
                          :sun-disc-target-board-index)
                    (call ctx
                          :requirement-complete?
                          db
                          source-id
                          params
                          :sun-disc-replacement-card-id))
    false))

(defn- sun-action-steps [ctx db source-id params]
  (let [cup-complete? (sun-cup-ribbon-complete? ctx db source-id params)
        disc-mode (call ctx :selected-sun-disc-mode db source-id params)
        disc-complete? (sun-disc-ribbon-complete? ctx db source-id params)]
    [{:id "sun-cup"
      :power :sun-cup
      :label "Cup"
      :status (cond
                cup-complete? :done
                :else :active)
      :detail (if cup-complete?
                "Cup target selected"
                (call ctx :move-prompt db))}
     {:id "sun-disc"
      :power :sun-disc
      :label "Disc"
      :status (cond
                (not cup-complete?) :pending
                (= :skip disc-mode) :skipped
                disc-complete? :ready
                :else :active)
      :detail (cond
                (not cup-complete?) "Pending Cup"
                (= :skip disc-mode) "Cup only"
                disc-mode (get-in options/sun-disc-mode-definitions [disc-mode :label])
                :else "Optional Disc step")}]))

(defn- fool-reveal-detail [reveal]
  (let [card (some-> (:card-id reveal) cards/card-by-id)
        title (:title card)]
    (case (:choice reveal)
      :skip (str (or title "Revealed card") " skipped")
      :play (str (or title "Revealed card")
                 " played"
                 (when-let [power (:power (:action reveal))]
                   (str " as " (power-label power))))
      (or title "Choose skip or play"))))

(defn- fool-action-steps [ctx params]
  (let [selected (call ctx :selected-fool-reveal-count params)
        completed (call ctx :fool-completed-reveals params)
        active-reveal (call ctx :fool-active-reveal params)]
    (mapv (fn [n]
            (let [completed-reveal (nth completed (dec n) nil)
                  active? (= n (:index active-reveal))]
              {:id (str "fool-reveal-" n)
               :power :fool-reveal
               :label (str "Reveal " n)
               :status (cond
                         (nil? selected) (if (= 1 n) :active :pending)
                         (< selected n) :skipped
                         completed-reveal :done
                         active? :active
                         (= n (inc (count completed))) :active
                         :else :pending)
               :detail (cond
                         (nil? selected) "Choose reveal count"
                         (< selected n) "Skipped"
                         completed-reveal (fool-reveal-detail completed-reveal)
                         active? (fool-reveal-detail active-reveal)
                         (= n (inc (count completed))) "Ready to reveal"
                         :else "Pending")}))
          [1 2])))

(defn- high-priestess-redraw-pass-complete? [ctx db source-id params pass-index]
  (when-let [redraw-state (call ctx
                                :high-priestess-redraw-state-before-pass
                                db
                                source-id
                                params
                                pass-index)]
    (let [pass (call ctx :high-priestess-redraw-pass params pass-index)
          discard-card-ids (call ctx
                                 :high-priestess-valid-discard-card-ids-in-state
                                 redraw-state
                                 (:discard-card-ids pass))
          options (set (call ctx
                             :high-priestess-draw-count-options-in-state
                             redraw-state
                             discard-card-ids))]
      (and (= (vec (:discard-card-ids pass)) discard-card-ids)
           (contains? options (:draw-count pass))))))

(defn- high-priestess-action-steps [ctx db source-id params]
  (let [selected (call ctx :selected-high-priestess-redraw-count params)]
    (mapv (fn [n]
            (let [complete? (high-priestess-redraw-pass-complete?
                             ctx
                             db
                             source-id
                             params
                             n)]
              {:id (str "high-priestess-redraw-" n)
               :power :high-priestess-redraw
               :label (str "Redraw " n)
               :status (cond
                         (nil? selected) (if (= 1 n) :active :pending)
                         (< selected n) :skipped
                         complete? :ready
                         :else :active)
               :detail (cond
                         (nil? selected) "Choose redraw count"
                         (< selected n) "Skipped"
                         complete? "Discard/draw selected"
                         :else "Choose discards and draw count")}))
          [1 2])))

(defn- judgement-action-steps [ctx db source-id params]
  [{:id "judgement-draw"
    :power :judgement-draw
    :label "Draw discards"
    :status (if (call ctx
                      :requirement-complete?
                      db
                      source-id
                      params
                      :judgement-card-selection)
              :ready
              :active)
    :detail (let [selected-count (count (:judgement-card-ids params))]
              (str selected-count
                   "/"
                   (call ctx :judgement-card-maximum db source-id params)
                   " cards"))}])

(defn- non-world-action-steps [ctx db source-id params power]
  (cond
    (= :fool power)
    (fool-action-steps ctx params)

    (call ctx :composite-major-move? db source-id params)
    (sequence-action-steps ctx
                           db
                           source-id
                           params
                           (composite-action-plan ctx db source-id params))

    (call ctx :sword-major-move? db source-id params)
    (sequence-action-steps ctx
                           db
                           source-id
                           params
                           (sword-major-action-plan ctx db source-id params))

    (call ctx :devil-move? db source-id params)
    (if (call ctx :requirement-complete? db source-id params :devil-action-count)
      (sequence-action-steps ctx
                             db
                             source-id
                             params
                             (devil-action-plan ctx params))
      [{:id "devil-action-count"
        :power :orient-target
        :label "Orientations"
        :status :active
        :detail "Choose orientation count"}])

    (= :sun power)
    (sun-action-steps ctx db source-id params)

    (= :high-priestess power)
    (high-priestess-action-steps ctx db source-id params)

    (= :judgement power)
    (judgement-action-steps ctx db source-id params)

    :else
    []))

(defn- world-copy-detail [ctx db params]
  (when-let [board-index (:copied-board-index params)]
    (territory-label ctx db board-index)))

(defn- world-action-steps [ctx db source-id params]
  (let [copy-complete? (call ctx :requirement-complete?
                             db
                             source-id
                             params
                             :copied-board-index)
        copied-power (call ctx :selected-world-copied-power db source-id params)
        copy-step {:id "world-copy"
                   :power :world-copy
                   :label "World copy"
                   :status (if copy-complete? :done :active)
                   :board-index (:copied-board-index params)
                   :detail (or (world-copy-detail ctx db params)
                               "Choose a non-World major territory")}
        power-step {:id "world-power"
                    :power :world-power
                    :label "Copied power"
                    :status (cond
                              (not copy-complete?) :pending
                              copied-power :done
                              :else :active)
                    :detail (if copied-power
                              (power-label copied-power)
                              "Choose copied power")}]
    (vec
     (concat [copy-step power-step]
             (when copied-power
               (non-world-action-steps ctx db source-id params copied-power))))))

(defn move-action-ribbon [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)
        selected (call ctx :selected-power db source params)
        power (call ctx :active-power db source params)]
    (when (and source
               (contains? action-ribbon-powers selected)
               (not= :unavailable selected))
      (let [world? (= :world selected)
            steps (if world?
                    (world-action-steps ctx db source params)
                    (non-world-action-steps ctx db source params selected))]
        {:visible? true
         :power selected
         :power-label (power-label selected)
         :copied-power (when world? power)
         :copied-power-label (when (and world? power)
                               (power-label power))
         :summary (if (and world? power)
                    (str "World copies " (power-label power))
                    (power-label selected))
         :prompt (call ctx :move-prompt db)
         :ready? (call ctx :move-ready? db)
         :steps steps}))))
