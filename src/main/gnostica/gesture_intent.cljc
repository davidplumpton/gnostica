(ns gnostica.gesture-intent
  (:require [gnostica.board-layout :as layout]
            [gnostica.move-selection :as move-selection]))

(def empty-gesture-intent
  {:active? false
   :input nil
   :source nil
   :target nil
   :alternatives []
   :preview-command nil
   :missing-fields []
   :error nil
   :detailed? false})

(defn- gesture-error [code message data]
  {:code code
   :message message
   :data data})

(defn gesture-intent [db]
  (:gesture-intent db empty-gesture-intent))

(defn cancel-gesture-intent [db]
  (assoc db :gesture-intent empty-gesture-intent))

(defn- selection-error [db]
  (get-in db [:move-selection :error]))

(defn- apply-while-valid [db f & args]
  (if (selection-error db)
    db
    (apply f db args)))

(defn- inferred-source-from-selection [db]
  (let [{:keys [source params]} (move-selection/move-selection db)]
    (case source
      :activate-territory
      (when-let [board-index (:source-board-index params)]
        {:move-source source
         :source {:kind :territory
                  :board-index board-index}})

      :play-hand-card
      (when-let [card-id (:hand-card-id params)]
        {:move-source source
         :source {:kind :hand-card
                  :card-id card-id}})

      :draw-cards
      {:move-source source
       :source {:kind :draw-pile}}

      :orient-piece
      (when-let [piece-id (:piece-id params)]
        {:move-source source
         :source {:kind :piece
                  :piece-id piece-id}})

      :place-initial-small
      {:move-source source
       :source {:kind :stash-piece
                :player-id (move-selection/current-player-id db)
                :size :small}}

      nil)))

(defn- infer-source [db {:keys [source source-id preserve-selection?]}]
  (cond
    preserve-selection?
    (inferred-source-from-selection db)

    source-id
    {:move-source source-id
     :source source}

    :else
    (case (:kind source)
      :hand-card
      {:move-source :play-hand-card
       :source source}

      :territory
      {:move-source :activate-territory
       :source source}

      :draw-pile
      {:move-source :draw-cards
       :source source}

      :discard-pile
      {:move-source :draw-cards
       :source source}

      :piece
      {:move-source :orient-piece
       :source source}

      :stash-piece
      {:move-source :place-initial-small
       :source source}

      nil)))

(defn- select-wasteland [db {:keys [row col]}]
  (if (and (int? row) (int? col))
    (move-selection/select-move-wasteland-target db row col)
    db))

(defn- start-source [db {:keys [move-source source]}]
  (let [db (move-selection/select-move-source db move-source)]
    (case move-source
      :play-hand-card
      (apply-while-valid db move-selection/select-move-hand-card (:card-id source))

      :activate-territory
      (apply-while-valid db move-selection/select-board-for-active-move
                         (:board-index source))

      :orient-piece
      (apply-while-valid db move-selection/select-move-piece (:piece-id source))

      :place-initial-small
      (case (:kind source)
        :territory
        (apply-while-valid db move-selection/select-board-for-active-move
                           (:board-index source))

        :wasteland
        (apply-while-valid db move-selection/select-move-wasteland-target
                           (:row source)
                           (:col source))

        db)

      db)))

(defn- toggle-card-ids [db card-ids toggle-fn]
  (reduce (fn [db card-id]
            (apply-while-valid db toggle-fn card-id))
          db
          (or card-ids [])))

(defn- apply-field [db field value]
  (case field
    :source-board-index
    (move-selection/select-board-for-active-move db value)

    :hand-card-id
    (move-selection/select-move-hand-card db value)

    :piece-id
    (move-selection/select-move-piece db value)

    :power
    (move-selection/select-move-power db value)

    :copied-board-index
    (move-selection/select-move-world-copy db value)

    :copied-power
    (move-selection/select-move-power db value)

    :rod-mode
    (move-selection/select-move-rod-mode db value)

    :disc-target-kind
    (move-selection/select-move-disc-target-kind db value)

    :sword-target-kind
    (move-selection/select-move-sword-target-kind db value)

    :disc-action-count
    (move-selection/set-move-disc-action-count db value)

    :major-action-count
    (move-selection/set-move-major-action-count db value)

    :sword-action-count
    (move-selection/set-move-sword-action-count db value)

    :devil-action-count
    (move-selection/set-move-devil-action-count db value)

    :fool-reveal-count
    (move-selection/set-move-fool-reveal-count db value)

    :high-priestess-redraw-count
    (move-selection/set-move-high-priestess-redraw-count db value)

    :minion-orientation
    (move-selection/set-move-minion-orientation db value)

    :sun-disc-mode
    (move-selection/select-move-sun-disc-mode db value)

    :target-piece-id
    (move-selection/select-move-target-piece db value)

    :target-board-index
    (move-selection/select-board-for-active-move db value)

    :sun-disc-target-board-index
    (move-selection/select-board-for-active-move db value)

    :hermit-destination-board-index
    (move-selection/select-board-for-active-move db value)

    :target-wasteland
    (select-wasteland db value)

    :hermit-destination-wasteland
    (select-wasteland db value)

    :territory-card-source
    (move-selection/select-move-territory-card-source db value)

    :replacement-card-source
    (move-selection/select-move-territory-card-source db value)

    :one-point-card-id
    (move-selection/select-move-one-point-card db value)

    :replacement-card-id
    (move-selection/select-move-replacement-card db value)

    :orientation
    (move-selection/set-move-orientation db value)

    :distance
    (move-selection/set-move-distance db value)

    :damage
    (move-selection/set-move-damage db value)

    :draw-count
    (move-selection/set-move-draw-count db value)

    db))

(def ^:private pre-target-field-order
  [:source-board-index
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
   :target-board-index])

(def ^:private post-target-field-order
  [:territory-card-source
   :one-point-card-id
   :damage
   :distance
   :replacement-card-source
   :replacement-card-id
   :hermit-destination-board-index
   :target-wasteland
   :hermit-destination-wasteland
   :orientation
   :draw-count])

(defn- apply-fields [db fields field-order]
  (reduce (fn [db field]
            (if (contains? fields field)
              (apply-while-valid db apply-field field (get fields field))
              db))
          db
          field-order))

(defn- board-cell-by-index [db index]
  (layout/cell-by-index (move-selection/board db) index))

(defn- piece-coordinate [db piece]
  (if-let [{:keys [row col]} (:space piece)]
    [row col]
    (when-let [{:keys [row col]} (board-cell-by-index db (:space-index piece))]
      [row col])))

(defn- target-coordinate [db target]
  (case (:kind target)
    :territory
    (when-let [{:keys [row col]} (board-cell-by-index db (:board-index target))]
      [row col])

    :wasteland
    [(:row target) (:col target)]

    nil))

(defn- cardinal-distance [[row col] orientation [target-row target-col]]
  (case orientation
    :north (when (and (= col target-col) (< target-row row))
             (- row target-row))
    :east (when (and (= row target-row) (< col target-col))
            (- target-col col))
    :south (when (and (= col target-col) (< row target-row))
             (- target-row row))
    :west (when (and (= row target-row) (< target-col col))
            (- col target-col))
    nil))

(defn- rod-moved-coordinate [db params]
  (case (:rod-mode params)
    :move-minion
    (some->> (:piece-id params)
             (move-selection/piece-by-id db)
             (piece-coordinate db))

    :push-piece
    (some->> (:target-piece-id params)
             (move-selection/piece-by-id db)
             (piece-coordinate db))

    :push-territory
    (when-let [{:keys [row col]} (board-cell-by-index db (:target-board-index params))]
      [row col])

    nil))

(defn- rod-drop-distance [db target]
  (let [{:keys [params]} (move-selection/move-selection db)
        minion (move-selection/piece-by-id db (:piece-id params))]
    (when (and (:rod-mode params) minion)
      (when-let [from (rod-moved-coordinate db params)]
        (when-let [to (target-coordinate db target)]
          (cardinal-distance from (:orientation minion) to))))))

(defn- infer-target-kind [db target]
  (let [stage (:stage (move-selection/move-selection db))
        target-kind (case (:kind target)
                      :territory :territory
                      :piece :piece
                      nil)]
    (case stage
      :disc-target-kind
      (if target-kind
        (move-selection/select-move-disc-target-kind db target-kind)
        db)

      :sword-target-kind
      (if target-kind
        (move-selection/select-move-sword-target-kind db target-kind)
        db)

      db)))

(defn- apply-target [db target]
  (let [db (infer-target-kind db target)]
    (if-let [distance (rod-drop-distance db target)]
      (move-selection/set-move-distance db distance)
      (case (:kind target)
        :territory
        (move-selection/select-board-for-active-move db (:board-index target))

        :piece
        (move-selection/select-move-target-piece db (:piece-id target))

        :wasteland
        (move-selection/select-move-wasteland-target db (:row target) (:col target))

        db))))

(defn- stage-gesture [db {:keys [fields target preserve-selection?] :as input} inferred-source]
  (let [fields (or fields {})
        db (cond-> db
             (not preserve-selection?)
             (assoc :move-selection (move-selection/empty-move-selection))

             (not preserve-selection?)
             (start-source inferred-source))
        db (-> db
               (toggle-card-ids (:discard-card-ids fields)
                                move-selection/toggle-move-discard-card)
               (apply-fields fields pre-target-field-order))
        db (if target
             (apply-while-valid db apply-target target)
             db)]
    (apply-fields db fields post-target-field-order)))

(defn- alternative-options [db field]
  (case field
    :source
    (move-selection/move-source-options db)

    :source-board-index
    (move-selection/move-source-board-options db)

    :hand-card-id
    (move-selection/move-hand-card-options db)

    :piece-id
    (move-selection/move-piece-options db)

    :power
    (move-selection/move-power-options db)

    :copied-board-index
    (move-selection/move-world-copy-options db)

    :copied-power
    (move-selection/move-world-copied-power-options db)

    :rod-mode
    (move-selection/move-rod-mode-options db)

    :disc-action-count
    (move-selection/move-disc-action-count-options db)

    :sword-action-count
    (move-selection/move-sword-action-count-options db)

    :devil-action-count
    (move-selection/move-devil-action-count-options db)

    :sun-disc-mode
    (move-selection/move-sun-disc-mode-options db)

    :disc-target-kind
    (move-selection/move-disc-target-kind-options db)

    :sword-target-kind
    (move-selection/move-sword-target-kind-options db)

    :target-piece-id
    (move-selection/move-target-piece-options db)

    :target-board-index
    (move-selection/move-target-board-options db)

    :target-space
    {:territories (move-selection/move-target-board-options db)
     :pieces (move-selection/move-target-piece-options db)
     :wastelands (move-selection/move-target-wasteland-options db)}

    :hermit-target-space
    {:territories (move-selection/move-target-board-options db)
     :pieces (move-selection/move-target-piece-options db)}

    :hermit-destination-space
    {:territories (move-selection/move-target-board-options db)
     :wastelands (move-selection/move-target-wasteland-options db)}

    :territory-card-source
    (move-selection/move-territory-card-source-options db)

    :replacement-card-source
    (move-selection/move-territory-card-source-options db)

    :one-point-card-id
    (move-selection/move-one-point-card-options db)

    :replacement-card-id
    (move-selection/move-replacement-card-options db)

    :orientation
    (move-selection/move-orientation-options db)

    :minion-orientation
    (move-selection/move-orientation-options db)

    :distance
    (move-selection/move-distance-options db)

    :damage
    (move-selection/move-damage-options db)

    :target-resolution
    (move-selection/move-orientation-options db)

    :draw-count
    (move-selection/draw-count-options db)

    []))

(defn- alternatives [db missing-fields]
  (mapv (fn [field]
          {:field field
           :prompt (get move-selection/requirement-prompts
                        field
                        "Complete this choice.")
           :options (alternative-options db field)})
        missing-fields))

(defn- pending-record [db input inferred-source detailed?]
  (let [selection (move-selection/move-selection db)
        missing-fields (move-selection/move-missing-fields db)
        ready? (move-selection/move-ready? db)
        preview-result (when ready?
                         (move-selection/move-preview-result db))
        error (:error selection)]
    {:active? true
     :input input
     :source (:source inferred-source)
     :move-source (:move-source inferred-source)
     :target (:target input)
     :alternatives (alternatives db missing-fields)
     :preview-command (when ready?
                        (move-selection/move-command db))
     :preview-result preview-result
     :missing-fields missing-fields
     :error (or error
                (when (false? (:ok? preview-result))
                  (:error preview-result)))
     :detailed? (true? detailed?)}))

(defn start-gesture-intent [db input]
  (if-let [inferred-source (infer-source db input)]
    (let [staged-db (stage-gesture db input inferred-source)]
      (assoc staged-db
             :gesture-intent
             (pending-record staged-db input inferred-source false)))
    (assoc db
           :gesture-intent
           (assoc empty-gesture-intent
                  :active? true
                  :input input
                  :error (gesture-error :unknown-gesture-source
                                        "Choose a hand card, territory, draw pile, piece, or stash piece gesture source."
                                        {:source (:source input)})))))

(defn refresh-gesture-intent [db]
  (let [intent (gesture-intent db)]
    (if (:active? intent)
      (assoc db
             :gesture-intent
             (pending-record db
                             (:input intent)
                             {:source (:source intent)
                              :move-source (:move-source intent)}
                             (:detailed? intent)))
      db)))

(defn open-detailed-entry [db]
  (if (:active? (gesture-intent db))
    (assoc-in db [:gesture-intent :detailed?] true)
    db))

(defn pending-move-tray-view [db]
  (let [intent (gesture-intent db)
        active? (true? (:active? intent))
        selection (move-selection/move-selection db)
        missing-fields (when active?
                         (move-selection/move-missing-fields db))
        ready? (and active? (move-selection/move-ready? db))
        command (when ready?
                  (move-selection/move-command db))
        preview-result (when ready?
                         (move-selection/move-preview-result db))
        preview-error (when (false? (:ok? preview-result))
                        (:error preview-result))
        error (or (:error intent)
                  (when active?
                    (:error selection))
                  preview-error)]
    {:active? active?
     :source (:source intent)
     :target (:target intent)
     :move-source (:move-source intent)
     :selection selection
     :summary (cond
                command "Pending move is ready to confirm."
                error (:message error)
                active? (move-selection/move-prompt db)
                :else "No pending gesture move.")
     :missing-fields (or missing-fields (:missing-fields intent))
     :alternatives (if active?
                     (alternatives db missing-fields)
                     (:alternatives intent))
     :preview-command command
     :preview-result preview-result
     :error error
     :ready? ready?
     :can-confirm? (and ready?
                        (nil? preview-error))
     :can-cancel? (true? (:active? intent))
     :detailed-entry-label "Detailed entry"
     :detailed-open? (true? (:detailed? intent))}))
