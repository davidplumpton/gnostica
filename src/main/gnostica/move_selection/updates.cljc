(ns gnostica.move-selection.updates
  (:require [gnostica.game-state :as game-state]
            [gnostica.move-selection.context :as context]
            [gnostica.move-selection.flow :as flow]
            [gnostica.move-selection.high-priestess :as high-priestess]
            [gnostica.move-selection.options :as options]
            [gnostica.move-selection.staging :as staging]
            [gnostica.move-selection.power-context :as power]
            [gnostica.move-selection.state :as selection]
            [gnostica.pieces :as pieces]))

(def required-context-keys
  #{:cup-move?
    :cup-target-territory?
    :default-initial-placement-target-index
    :disc-move?
    :disc-replacement-card-source-option-ids
    :disc-territory-target?
    :empty-board-target?
    :hermit-move?
    :hermit-piece-target-selected?
    :hermit-target-selected?
    :hermit-target-territory?
    :hermit-territory-target-selected?
    :move-damage-options
    :move-distance-options
    :move-fool-play-power-options
    :move-power-options
    :move-target-piece-options
    :move-target-wasteland-options
    :move-world-copied-power-options
    :replacement-card-source-for-card
    :rod-move?
    :rod-territory-target?
    :selected-sun-disc-mode
    :source-unavailable-reason
    :sun-cup-needs-one-point-card?
    :sun-disc-mode-option-ids
    :sun-disc-orientation-available?
    :sun-disc-replacement-card-by-id
    :sun-disc-territory-target?
    :sun-disc-territory-target-stage?
    :sun-move?
    :sword-move?
    :sword-replacement-card-source-option-ids
    :sword-territory-target?
    :territory-card-source-option-ids
    :valid-board-index?
    :world-copy-board-cell
    :world-move?})

(defn make-context [deps]
  (context/make "gnostica.move-selection.updates" required-context-keys deps))

(def ^:dynamic *context* nil)

(defn- require-context []
  (or *context*
      (throw (ex-info "gnostica.move-selection.updates requires a bound context"
                      {:code :move-selection-updates/missing-context}))))

(defn with-context [ctx f & args]
  (binding [*context* ctx]
    (apply f args)))

(defn- ctx-call [key & args]
  (apply context/call (require-context) key args))

(def empty-move-selection staging/empty-selection)
(def board-cell-by-index selection/board-cell-by-index)
(def current-player-hand selection/current-player-hand)
(def current-player-id selection/current-player-id)
(def current-player-pieces-on-space selection/current-player-pieces-on-space)
(def current-player-piece? selection/current-player-piece?)
(def current-player-piece-by-id selection/current-player-piece-by-id)
(def discard-pile selection/discard-pile)
(def game selection/game)
(def hand-card-by-id selection/hand-card-by-id)
(def piece-by-id selection/piece-by-id)
(def selected-board-index selection/selected-board-index)
(def source-card selection/source-card)
(def source-command selection/source-command)
(def move-selection selection/move-selection)
(def move-source selection/move-source)
(def move-params selection/move-params)
(def active-power power/active-power)
(def cup-move? power/cup-move?)
(def death-sword-action-count-option-values power/death-sword-action-count-option-values)
(def devil-action-count-option-values power/devil-action-count-option-values)
(def devil-move? power/devil-move?)
(def disc-action-count-option-values power/disc-action-count-option-values)
(def disc-move? power/disc-move?)
(def fool-active-play? power/fool-active-play?)
(def fool-active-reveal power/fool-active-reveal)
(def fool-move? power/fool-move?)
(def hand-trade-major-action-count-option-values power/hand-trade-major-action-count-option-values)
(def high-priestess-move? power/high-priestess-move?)
(def hanged-man-trade-stage? power/hanged-man-trade-stage?)
(def hermit-move? power/hermit-move?)
(def hierophant-move? power/hierophant-move?)
(def judgement-move? power/judgement-move?)
(def justice-trade-stage? power/justice-trade-stage?)
(def major-action-count-option-values power/major-action-count-option-values)
(def major-orient-step? power/major-orient-step?)
(def one-point-card-by-id selection/one-point-card-by-id)
(def rod-move? power/rod-move?)
(def selected-death-sword-action-count power/selected-death-sword-action-count)
(def selected-devil-action-count power/selected-devil-action-count)
(def selected-disc-action-count power/selected-disc-action-count)
(def selected-fool-play-power power/selected-fool-play-power)
(def selected-fool-reveal-count power/selected-fool-reveal-count)
(def selected-hand-trade-major-action-count power/selected-hand-trade-major-action-count)
(def star-disc-source? power/star-disc-source?)
(def sun-move? power/sun-move?)
(def sword-move? power/sword-move?)
(def valid-board-index? selection/valid-board-index?)
(def world-copy-board-cell power/world-copy-board-cell)
(def world-move? power/world-move?)

(def disc-target-kinds options/disc-target-kinds)
(def disc-target-kind-order options/disc-target-kind-order)
(def fool-reveal-count-order options/fool-reveal-count-order)
(def high-priestess-redraw-count-order options/high-priestess-redraw-count-order)
(def rod-modes options/rod-modes)
(def rod-mode-order options/rod-mode-order)
(def sword-target-kinds options/sword-target-kinds)
(def sword-target-kind-order options/sword-target-kind-order)

(def redraw-pass-offset high-priestess/redraw-pass-offset)
(def high-priestess-draw-count-options high-priestess/draw-count-options)
(def high-priestess-hand-card-options high-priestess/hand-card-options)
(def high-priestess-redraw-pass high-priestess/redraw-pass)
(def normalize-high-priestess-redraws high-priestess/normalize-redraws)
(def selected-high-priestess-redraw-count high-priestess/selected-redraw-count)

(defn- default-initial-placement-target-index [& args]
  (apply ctx-call :default-initial-placement-target-index args))

(defn- empty-board-target? [& args]
  (apply ctx-call :empty-board-target? args))

(defn- hermit-piece-target-selected? [& args]
  (apply ctx-call :hermit-piece-target-selected? args))

(defn- hermit-target-selected? [& args]
  (apply ctx-call :hermit-target-selected? args))

(defn- hermit-target-territory? [& args]
  (apply ctx-call :hermit-target-territory? args))

(defn- hermit-territory-target-selected? [& args]
  (apply ctx-call :hermit-territory-target-selected? args))

(defn- move-fool-play-power-options [& args]
  (apply ctx-call :move-fool-play-power-options args))

(defn- move-power-options [& args]
  (apply ctx-call :move-power-options args))

(defn- move-target-piece-options [& args]
  (apply ctx-call :move-target-piece-options args))

(defn- move-world-copied-power-options [& args]
  (apply ctx-call :move-world-copied-power-options args))

(defn- source-unavailable-reason [& args]
  (apply ctx-call :source-unavailable-reason args))

(defn- sun-disc-territory-target-stage? [& args]
  (apply ctx-call :sun-disc-territory-target-stage? args))

(defn- sun-disc-territory-target? [& args]
  (apply ctx-call :sun-disc-territory-target? args))

(defn- cup-target-territory? [& args]
  (apply ctx-call :cup-target-territory? args))

(defn- disc-territory-target? [& args]
  (apply ctx-call :disc-territory-target? args))

(defn- sword-territory-target? [& args]
  (apply ctx-call :sword-territory-target? args))

(defn- rod-territory-target? [& args]
  (apply ctx-call :rod-territory-target? args))

(defn- move-target-wasteland-options [& args]
  (apply ctx-call :move-target-wasteland-options args))

(defn- selected-sun-disc-mode [& args]
  (apply ctx-call :selected-sun-disc-mode args))

(defn- sun-disc-mode-option-ids [& args]
  (apply ctx-call :sun-disc-mode-option-ids args))

(defn- sun-cup-needs-one-point-card? [& args]
  (apply ctx-call :sun-cup-needs-one-point-card? args))

(defn- sun-disc-orientation-available? [& args]
  (apply ctx-call :sun-disc-orientation-available? args))

(defn- sun-disc-replacement-card-by-id [& args]
  (apply ctx-call :sun-disc-replacement-card-by-id args))

(defn- disc-replacement-card-source-option-ids [& args]
  (apply ctx-call :disc-replacement-card-source-option-ids args))

(defn- sword-replacement-card-source-option-ids [& args]
  (apply ctx-call :sword-replacement-card-source-option-ids args))

(defn- territory-card-source-option-ids [& args]
  (apply ctx-call :territory-card-source-option-ids args))

(defn- replacement-card-source-for-card [& args]
  (apply ctx-call :replacement-card-source-for-card args))

(defn- move-distance-options [& args]
  (apply ctx-call :move-distance-options args))

(defn- wasteland-space-by-coordinate [db row col]
  (some #(when (and (= row (:row %))
                    (= col (:col %)))
           %)
        (move-target-wasteland-options db)))

(defn valid-discard-card-ids [db discard-card-ids]
  (let [hand-card-ids (set (map :id (current-player-hand db)))]
    (vec (filter hand-card-ids (distinct (or discard-card-ids []))))))

(defn max-draw-count
  ([db]
   (max-draw-count db (get-in db [:move-selection :params :discard-card-ids])))
  ([db discard-card-ids]
   (let [discard-count (count (valid-discard-card-ids db discard-card-ids))
         post-discard-hand-count (- (count (current-player-hand db))
                                    discard-count)
         hand-slots (- game-state/starting-hand-size
                       post-discard-hand-count)
         available-cards (+ (count (get-in db [:game :draw-pile] []))
                            (count (get-in db [:game :discard-pile] []))
                            discard-count)]
     (max 0 (min hand-slots available-cards)))))

(defn draw-count-options
  ([db]
   (draw-count-options db (get-in db [:move-selection :params :discard-card-ids])))
  ([db discard-card-ids]
   (let [discard-count (count (valid-discard-card-ids db discard-card-ids))
         min-draw-count (if (pos? discard-count) 0 1)
         max-draw-count (max-draw-count db discard-card-ids)]
     (if (< max-draw-count min-draw-count)
       []
       (vec (range min-draw-count (inc max-draw-count)))))))

(defn- default-draw-count [options]
  (or (some #{1} options)
      (first options)))

(defn- normalize-draw-selection-params [db params]
  (let [discard-card-ids (valid-discard-card-ids db (:discard-card-ids params))
        params (assoc params :discard-card-ids discard-card-ids)
        options (draw-count-options db discard-card-ids)
        draw-count (:draw-count params)]
    (cond
      (some #{draw-count} options)
      params

      (seq options)
      (assoc params :draw-count (default-draw-count options))

      :else
      (dissoc params :draw-count))))

(defn judgement-discard-card-options [db source-id params]
  (let [source-card (source-card db source-id params)]
    (cond-> (discard-pile db)
      (and (= :play-hand-card source-id)
           (judgement-move? db source-id params)
           source-card)
      (conj source-card))))

(defn judgement-card-maximum [db source-id params]
  (let [source-cost-count (if (and (= :play-hand-card source-id)
                                   (judgement-move? db source-id params)
                                   (source-card db source-id params))
                            1
                            0)
        hand-count (- (count (current-player-hand db)) source-cost-count)
        hand-slots (max 0 (- game-state/starting-hand-size hand-count))
        minion-pips (or (some-> (piece-by-id db (:piece-id params)) pieces/pips) 0)]
    (min minion-pips hand-slots)))

(defn valid-judgement-card-ids [db source-id params card-ids]
  (let [selected-card-ids (set (or card-ids []))]
    (->> (judgement-discard-card-options db source-id params)
         (map :id)
         (filter selected-card-ids)
         (take (judgement-card-maximum db source-id params))
         vec)))

(defn judgement-card-selection-complete? [db source-id params]
  (= (vec (:judgement-card-ids params))
     (valid-judgement-card-ids db source-id params (:judgement-card-ids params))))

(defn move-error [code message data]
  {:code code
   :message message
   :data data})

(defn- flow-context []
  (flow/make-context (require-context)))

(defn- requirement-complete? [db source-id params requirement]
  (flow/requirement-complete? (flow-context) db source-id params requirement))

(defn move-missing-fields [db]
  (flow/move-missing-fields (flow-context) db))

(declare select-move-world-copy)

(defn- stored-fool-reveal-actions [params]
  (flow/stored-fool-reveal-actions params))

(defn- refresh-move-selection [db]
  (flow/refresh-move-selection (flow-context) db))

(defn update-move-selection [db f & args]
  (refresh-move-selection
   (update db :move-selection
           (fnil (fn [selection]
                   (apply f selection args))
                 (empty-move-selection)))))

(defn- update-move-selection-success [db f & args]
  (refresh-move-selection
   (flow/advance-move-selection-steps
    (flow-context)
    (update db :move-selection
            (fnil (fn [selection]
                    (assoc (apply f selection args)
                           :error nil
                           :last-result nil))
                  (empty-move-selection))))))

(defn move-ready? [db]
  (= :confirm (:stage (move-selection db))))

(defn select-move-source [db source-id]
  (let [reason (source-unavailable-reason db source-id)]
    (if reason
      (assoc db :move-selection
             (assoc (empty-move-selection)
                    :error (move-error :move-source-unavailable
                                       reason
                                       {:source source-id})))
      (let [selected-index (selected-board-index db)
            placement-target-index (when (= source-id :place-initial-small)
                                     (default-initial-placement-target-index
                                      db
                                      selected-index))
            base-params (cond-> (if (= source-id :draw-cards)
                                  (normalize-draw-selection-params db
                                                                   {:discard-card-ids []})
                                  {})

                          (and (= source-id :activate-territory)
                               (seq (current-player-pieces-on-space db selected-index)))
                          (assoc :source-board-index selected-index)

                          placement-target-index
                          (assoc :target-board-index placement-target-index))]
        (refresh-move-selection
         (assoc db :move-selection
                {:stage :source
                 :source source-id
                 :params base-params
                 :error nil
                 :last-result nil}))))))

(defn cancel-move [db]
  (assoc db :move-selection (empty-move-selection)))

(defn- set-high-priestess-redraw-count-param [params db source redraw-count]
  (normalize-high-priestess-redraws
   db
   source
   (assoc params :high-priestess-redraw-count redraw-count)))

(defn- update-high-priestess-redraw-pass [params pass-index f & args]
  (if-let [offset (redraw-pass-offset pass-index)]
    (let [redraw-count (or (selected-high-priestess-redraw-count params) 0)
          redraws (vec (concat (:redraws params)
                               (repeat (max 0 (- redraw-count
                                                 (count (:redraws params))))
                                       {:discard-card-ids []})))]
      (assoc params
             :redraws
             (update redraws offset #(apply f (or % {:discard-card-ids []}) args))))
    params))

(defn- select-hermit-board-target [db params index]
  (let [cell (board-cell-by-index db index)]
    (cond
      (hermit-piece-target-selected? params)
      (if (empty-board-target? db index)
        (update-move-selection-success db
                                       update
                                       :params
                                       staging/set-hermit-destination-territory
                                       index)
        (update-move-selection db assoc
                               :error
                               (move-error :hermit-destination-occupied
                                           "Choose an empty destination territory."
                                           {:board-index index})))

      (hermit-territory-target-selected? params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-hermit-destination
                                         "Hermit territory moves must choose a wasteland destination."
                                         {:board-index index}))

      (hermit-target-territory? db params cell)
      (update-move-selection-success db update :params staging/set-territory-target index)

      :else
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-hermit-target
                                         "Choose a Hermit-targetable territory."
                                         {:board-index index})))))

(defn select-board-for-active-move [db index]
  (if-not (valid-board-index? db index)
    db
    (let [{:keys [source params]} (move-selection db)
          cell (board-cell-by-index db index)]
      (case source
        :activate-territory
        (let [has-source? (requirement-complete? db source params :source-board-index)
              has-piece? (requirement-complete? db source params :piece-id)
              current-pieces (current-player-pieces-on-space db index)]
          (cond
            (and has-source?
                 has-piece?
                 (= :world-copy (:stage (move-selection db))))
            (select-move-world-copy db index)

            (and has-source?
                 has-piece?
                 (sun-disc-territory-target-stage? db source params)
                 (sun-disc-territory-target? db source params cell))
            (update-move-selection-success db
                                           update
                                           :params
                                           staging/set-sun-disc-target-territory
                                           index)

            (and has-source?
                 has-piece?
                 (sun-disc-territory-target-stage? db source params))
            (update-move-selection db assoc
                                   :error
                                   (move-error :invalid-sun-disc-target
                                               "Choose a Sun Disc-targetable territory."
                                               {:board-index index}))

            (and has-source?
                 has-piece?
                 (hermit-move? db source params))
            (select-hermit-board-target db params index)

            (and has-source?
                 has-piece?
                 (or (cup-move? db source params)
                     (sun-move? db source params))
                 (cup-target-territory? db source params cell))
            (update-move-selection-success db update :params staging/set-territory-target index)

            (and has-source?
                 has-piece?
                 (or (cup-move? db source params)
                     (sun-move? db source params)))
            (update-move-selection db assoc
                                   :error
                                   (move-error :invalid-cup-target
                                               "Choose a Cup-targetable territory."
                                               {:board-index index}))

            (and has-source?
                 has-piece?
                 (or (and (disc-move? db source params)
                          (= :territory (:disc-target-kind params))
                          (disc-territory-target? db source params cell))
                     (and (sword-move? db source params)
                          (= :territory (:sword-target-kind params))
                          (sword-territory-target? db source params cell))))
            (update-move-selection-success db update :params staging/set-territory-target index)

            (and has-source?
                 has-piece?
                 (rod-move? db source params)
                 (= :push-territory (:rod-mode params))
                 (rod-territory-target? db source params cell))
            (update-move-selection-success db update :params staging/set-territory-target index)

            (and has-source?
                 has-piece?
                 (rod-move? db source params)
                 (= :push-territory (:rod-mode params)))
            (update-move-selection db assoc
                                   :error
                                   (move-error :invalid-rod-target
                                               "Choose a Rod-targetable territory."
                                               {:board-index index}))

            (and has-source?
                 has-piece?
                 (rod-move? db source params))
            db

            (and has-source?
                 has-piece?
                 (or (and (disc-move? db source params)
                          (not= :territory (:disc-target-kind params)))
                     (and (sword-move? db source params)
                          (not= :territory (:sword-target-kind params)))))
            db

            (and has-source?
                 has-piece?
                 (not (or (disc-move? db source params)
                          (sword-move? db source params))))
            (update-move-selection-success db update :params staging/set-territory-target index)

            (and has-source?
                 has-piece?
                 (disc-move? db source params))
            (update-move-selection db assoc
                                   :error
                                   (move-error :invalid-disc-target
                                               "Choose a Disc-targetable territory."
                                               {:board-index index}))

            (and has-source?
                 has-piece?
                 (sword-move? db source params))
            (update-move-selection db assoc
                                   :error
                                   (move-error :invalid-sword-target
                                               "Choose a Sword-targetable territory."
                                               {:board-index index}))

            (seq current-pieces)
            (update-move-selection-success db update :params staging/set-source-board-index index)

            :else
            (update-move-selection db assoc
                                   :error
                                   (move-error :invalid-source-territory
                                               "Choose a territory with one of the current player's pieces."
                                               {:board-index index}))))

        :play-hand-card
        (cond
          (= :world-copy (:stage (move-selection db)))
          (select-move-world-copy db index)

          (and (sun-disc-territory-target-stage? db source params)
               (sun-disc-territory-target? db source params cell))
          (update-move-selection-success db
                                         update
                                         :params
                                         staging/set-sun-disc-target-territory
                                         index)

          (sun-disc-territory-target-stage? db source params)
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-sun-disc-target
                                             "Choose a Sun Disc-targetable territory."
                                             {:board-index index}))

          (hermit-move? db source params)
          (select-hermit-board-target db params index)

          (and (disc-move? db source params)
               (not= :territory (:disc-target-kind params)))
          db

          (and (sword-move? db source params)
               (not= :territory (:sword-target-kind params)))
          db

          (and (disc-move? db source params)
               (not (disc-territory-target? db source params cell)))
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-disc-target
                                             "Choose a Disc-targetable territory."
                                             {:board-index index}))

          (and (sword-move? db source params)
               (not (sword-territory-target? db source params cell)))
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-sword-target
                                             "Choose a Sword-targetable territory."
                                             {:board-index index}))

          (and (rod-move? db source params)
               (= :push-territory (:rod-mode params))
               (rod-territory-target? db source params cell))
          (update-move-selection-success db update :params staging/set-territory-target index)

          (and (rod-move? db source params)
               (= :push-territory (:rod-mode params)))
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-rod-target
                                             "Choose a Rod-targetable territory."
                                             {:board-index index}))

          (rod-move? db source params)
          db

          (and (or (cup-move? db source params)
                   (sun-move? db source params))
               (cup-target-territory? db source params cell))
          (update-move-selection-success db update :params staging/set-territory-target index)

          (or (cup-move? db source params)
              (sun-move? db source params))
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-cup-target
                                             "Choose a Cup-targetable territory."
                                             {:board-index index}))

          :else
          (update-move-selection-success db update :params staging/set-territory-target index))

        :place-initial-small
        (if (empty-board-target? db index)
          (update-move-selection-success db update :params staging/set-territory-target index)
          (update-move-selection db assoc
                                 :error
                                 (move-error :target-space-occupied
                                             "Choose an empty territory or wasteland."
                                             {:board-index index})))

        db))))

(defn select-move-wasteland-target [db row col]
  (let [source (move-source db)
        params (move-params db)
        space (wasteland-space-by-coordinate db row col)]
    (cond
      (not (or (cup-move? db source params)
               (sun-move? db source params)
               (hermit-move? db source params)
               (= :place-initial-small source)))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-wasteland-target
                                         "Wasteland targets are only available for Cup, Sun, Hermit, or initial placement moves."
                                         {:row row
                                          :col col
                                          :source source}))

      (nil? space)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-wasteland-target
                                         "Choose an available wasteland space."
                                         {:row row
                                          :col col}))

      (hermit-move? db source params)
      (if (hermit-target-selected? params)
        (update-move-selection-success db
                                       update
                                       :params
                                       staging/set-hermit-destination-wasteland
                                       space)
        (update-move-selection db assoc
                               :error
                               (move-error :invalid-hermit-destination
                                           "Choose a Hermit target before choosing a destination."
                                           {:row row
                                            :col col})))

      :else
      (update-move-selection-success db update :params staging/set-wasteland-target space))))

(defn select-move-piece [db piece-id]
  (let [{:keys [source params]} (move-selection db)
        piece (current-player-piece-by-id db piece-id)]
    (cond
      (nil? source)
      db

      (nil? piece)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-piece
                                         "Choose one of the current player's pieces."
                                         {:piece-id piece-id}))

      (= :activate-territory source)
      (let [source-index (:source-board-index params)]
        (cond
          (nil? (:space-index piece))
          (update-move-selection db assoc
                                 :error
                                 (move-error :piece-outside-source-territory
                                             "Choose a minion on a territory."
                                             {:piece-id piece-id}))

          (nil? source-index)
          (-> db
              (update-move-selection-success assoc-in [:params :source-board-index] (:space-index piece))
              (update-move-selection-success update :params staging/set-acting-piece piece-id))

          (= source-index (:space-index piece))
          (update-move-selection-success db update :params staging/set-acting-piece piece-id)

          :else
          (update-move-selection db assoc
                                 :error
                                 (move-error :piece-outside-source-territory
                                             "Choose a minion on the selected source territory."
                                             {:piece-id piece-id
                                              :source-board-index source-index}))))

      (contains? #{:play-hand-card :orient-piece} source)
      (update-move-selection-success db update :params staging/set-acting-piece piece-id)

      :else
      db)))

(defn select-move-hand-card [db card-id]
  (if (and (= :play-hand-card (move-source db))
           (hand-card-by-id db card-id))
    (update-move-selection-success db update :params staging/set-hand-card-source card-id)
    (update-move-selection db assoc
                           :error
                           (move-error :invalid-hand-card
                                       "Choose a card from the current player's hand."
                                       {:card-id card-id}))))

(defn select-move-world-copy [db board-index]
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (not (world-move? db source params))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-world-copy
                                         "World copy choices are only available for World moves."
                                         {:board-index board-index
                                          :source source}))

      (world-copy-board-cell db board-index)
      (update-move-selection-success db update :params staging/set-world-copy board-index)

      :else
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-world-copy
                                         "Choose a non-World major territory for World to copy."
                                         {:board-index board-index})))))

(defn select-move-power [db power]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (world-move? db source params)
             (world-copy-board-cell db (:copied-board-index params)))
      (let [power-ids (set (map :id (move-world-copied-power-options db)))]
        (if (contains? power-ids power)
          (update-move-selection-success db
                                         update
                                         :params
                                         staging/set-world-copied-power
                                         power)
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-world-copied-power
                                             "Choose a power provided by the copied major territory."
                                             {:power power
                                              :options (vec power-ids)}))))
      (let [power-ids (set (map :id (move-power-options db)))]
        (if (contains? power-ids power)
          (update-move-selection-success db update :params staging/set-power power)
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-move-power
                                             "Choose a power provided by the selected card."
                                             {:power power
                                              :options (vec power-ids)})))))))

(defn select-move-rod-mode [db mode]
  (if (contains? rod-modes mode)
    (update-move-selection-success db update :params staging/set-rod-mode mode)
    (update-move-selection db assoc
                           :error
                           (move-error :invalid-rod-mode
                                       "Choose a supported Rod move."
                                       {:mode mode
                                        :options rod-mode-order}))))

(defn select-move-disc-target-kind [db target-kind]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (disc-move? db source params)
             (contains? disc-target-kinds target-kind))
      (update-move-selection-success db update :params staging/set-disc-target-kind target-kind)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-disc-target-kind
                                         "Choose a supported Disc growth target."
                                         {:target-kind target-kind
                                          :options disc-target-kind-order})))))

(defn select-move-sword-target-kind [db target-kind]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (sword-move? db source params)
             (contains? sword-target-kinds target-kind))
      (update-move-selection-success db update :params staging/set-sword-target-kind target-kind)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sword-target-kind
                                         "Choose a supported Sword attack target."
                                         {:target-kind target-kind
                                          :options sword-target-kind-order})))))

(defn set-move-disc-action-count [db action-count]
  (let [{:keys [source params]} (move-selection db)
        options (disc-action-count-option-values db source params)]
    (if (some #{action-count} options)
      (update-move-selection-success db update :params staging/set-disc-action-count action-count)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-disc-action-count
                                         "Choose a supported Disc action count."
                                         {:action-count action-count
                                          :options options})))))

(defn set-move-sword-action-count [db action-count]
  (let [{:keys [source params]} (move-selection db)
        options (death-sword-action-count-option-values db source params)]
    (if (some #{action-count} options)
      (update-move-selection-success db update :params staging/set-sword-action-count action-count)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sword-action-count
                                         "Choose a supported Sword action count."
                                         {:action-count action-count
                                          :options options})))))

(defn set-move-major-action-count [db action-count]
  (let [{:keys [source params]} (move-selection db)
        options (major-action-count-option-values db source params)]
    (if (some #{action-count} options)
      (update-move-selection-success db update :params staging/set-major-action-count action-count)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-major-action-count
                                         "Choose a supported major action count."
                                         {:action-count action-count
                                          :options options})))))

(defn set-move-devil-action-count [db action-count]
  (let [{:keys [source params]} (move-selection db)
        options (devil-action-count-option-values db source params)]
    (if (some #{action-count} options)
      (update-move-selection-success db update :params staging/set-devil-action-count action-count)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-devil-action-count
                                         "Choose a supported Devil orientation count."
                                         {:action-count action-count
                                          :options options})))))

(defn fool-command-map [params reveal-actions]
  (cond-> {:reveals (vec reveal-actions)}
    (:fool-shuffle-fn params)
    (assoc :shuffle-fn (:fool-shuffle-fn params))))

(defn- fool-preview-command [db source params reveal-actions]
  (cond-> (merge {:player-id (current-player-id db)
                  :source (source-command source params)}
                 (fool-command-map params reveal-actions))
    (world-move? db source params)
    (assoc :copied-board-index (:copied-board-index params))))

(defn- fool-preview-result [db source params reveal-actions transition-options]
  (let [command (cond-> (fool-preview-command db source params reveal-actions)
                  (and (:shuffle-fn transition-options)
                       (not (contains? params :fool-shuffle-fn)))
                  (assoc :shuffle-fn (:shuffle-fn transition-options)))]
    (if (world-move? db source params)
      (game-state/apply-world-move (game db) command)
      (game-state/apply-fool-move (game db) command))))

(defn- fool-revealed-card-id [result reveal-index]
  (some #(when (and (= :fool/card-revealed (:type %))
                    (= reveal-index (:reveal-index %)))
           (:card-id %))
        (:events result)))

(defn set-move-fool-reveal-count [db reveal-count]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (fool-move? db source params)
             (some #{reveal-count} fool-reveal-count-order))
      (update-move-selection-success db update :params staging/set-fool-reveal-count reveal-count)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-fool-reveal-count
                                         "Choose a supported Fool reveal count."
                                         {:reveal-count reveal-count
                                          :options fool-reveal-count-order})))))

(defn reveal-move-fool-card
  ([db]
   (reveal-move-fool-card db {}))
  ([db transition-options]
   (let [{:keys [source params]} (move-selection db)
         reveal-count (selected-fool-reveal-count params)
         completed-actions (stored-fool-reveal-actions params)
         reveal-index (inc (count completed-actions))]
     (cond
       (not (fool-move? db source params))
       (update-move-selection db assoc
                              :error
                              (move-error :invalid-fool-reveal
                                          "Fool reveal choices are only available for Fool moves."
                                          {:source source}))

       (nil? reveal-count)
       (update-move-selection db assoc
                              :error
                              (move-error :missing-fool-reveal-count
                                          "Choose how many cards Fool reveals first."
                                          {}))

       (or (fool-active-reveal params)
           (<= reveal-count (count completed-actions)))
       (update-move-selection db assoc
                              :error
                              (move-error :invalid-fool-reveal
                                          "There is no pending Fool reveal to turn over."
                                          {:reveal-count reveal-count
                                           :completed-count (count completed-actions)}))

       :else
       (let [result (fool-preview-result db
                                         source
                                         params
                                         (conj completed-actions {})
                                         transition-options)]
         (if-not (:ok? result)
           (assoc db :move-selection
                  (assoc (move-selection db)
                         :stage :rejected
                         :error (:error result)
                         :last-result result))
           (let [card-id (fool-revealed-card-id result reveal-index)]
             (if-not card-id
               (update-move-selection db assoc
                                      :error
                                      (move-error :missing-fool-reveal-card
                                                  "Fool reveal preview did not identify a card."
                                                  {:reveal-index reveal-index}))
               (update-move-selection-success
                db
                update
                :params
                (fn [params]
                  (cond-> (assoc params
                                 :fool-active-reveal {:index reveal-index
                                                      :card-id card-id})
                    (and (:shuffle-fn transition-options)
                         (not (:fool-shuffle-fn params)))
                    (assoc :fool-shuffle-fn (:shuffle-fn transition-options)))))))))))))

(defn skip-move-fool-reveal [db]
  (let [{:keys [source params]} (move-selection db)
        active-reveal (fool-active-reveal params)]
    (cond
      (not (fool-move? db source params))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-fool-reveal
                                         "Fool reveal choices are only available for Fool moves."
                                         {:source source}))

      (nil? active-reveal)
      (update-move-selection db assoc
                             :error
                             (move-error :missing-fool-reveal
                                         "Reveal a Fool card before choosing skip."
                                         {}))

      :else
      (update-move-selection-success db
                                     update
                                     :params
                                     staging/commit-fool-active-reveal
                                     {:card-id (:card-id active-reveal)
                                      :choice :skip
                                      :action {}}))))

(defn play-move-fool-reveal [db]
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (not (fool-move? db source params))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-fool-reveal
                                         "Fool reveal choices are only available for Fool moves."
                                         {:source source}))

      (nil? (fool-active-reveal params))
      (update-move-selection db assoc
                             :error
                             (move-error :missing-fool-reveal
                                         "Reveal a Fool card before choosing play."
                                         {}))

      (= :unavailable (selected-fool-play-power db source (assoc-in params [:fool-active-reveal :choice] :play)))
      (update-move-selection db assoc
                             :error
                             (move-error :fool-play-unavailable
                                         "The revealed card does not have a browser-playable power."
                                         {:card-id (:card-id (fool-active-reveal params))}))

      :else
      (update-move-selection-success db
                                     update
                                     :params
                                     (fn [params]
                                       (-> params
                                           staging/clear-fool-play-params
                                           (assoc-in [:fool-active-reveal :choice] :play)))))))

(defn select-move-fool-play-power [db power]
  (let [{:keys [source params]} (move-selection db)
        power-ids (set (map :id (move-fool-play-power-options db)))]
    (if (and (fool-active-play? db source params)
             (contains? power-ids power))
      (update-move-selection-success db
                                     update
                                     :params
                                     staging/set-fool-play-power
                                     power)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-fool-play-power
                                         "Choose a power provided by the revealed card."
                                         {:power power
                                          :options (vec power-ids)})))))

(defn set-move-high-priestess-redraw-count [db redraw-count]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (high-priestess-move? db source params)
             (some #{redraw-count} high-priestess-redraw-count-order))
      (update-move-selection-success db
                                     update
                                     :params
                                     set-high-priestess-redraw-count-param
                                     db
                                     source
                                     redraw-count)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-high-priestess-redraw-count
                                         "Choose a supported High Priestess redraw count."
                                         {:redraw-count redraw-count
                                          :options high-priestess-redraw-count-order})))))

(defn toggle-move-high-priestess-discard-card [db pass-index card-id]
  (let [{:keys [source params]} (move-selection db)
        pass (high-priestess-redraw-pass params pass-index)
        card-options (high-priestess-hand-card-options db source params pass-index)
        card-option-ids (set (map :id card-options))]
    (cond
      (not (high-priestess-move? db source params))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-high-priestess-discard-card
                                         "High Priestess discard choices are only available for High Priestess moves."
                                         {:card-id card-id
                                          :source source}))

      (or (nil? pass)
          (not (<= 1 pass-index (or (selected-high-priestess-redraw-count params) 0))))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-high-priestess-pass
                                         "Choose an available High Priestess redraw pass."
                                         {:pass-index pass-index}))

      (not (contains? card-option-ids card-id))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-high-priestess-discard-card
                                         "Choose a card from the current player's redraw hand."
                                         {:card-id card-id}))

      :else
      (let [selected-card-ids (set (:discard-card-ids pass))
            next-selected-card-ids (if (contains? selected-card-ids card-id)
                                     (disj selected-card-ids card-id)
                                     (conj selected-card-ids card-id))
            next-discard-card-ids (->> card-options
                                       (map :id)
                                       (filter next-selected-card-ids)
                                       vec)]
        (update-move-selection-success
         db
         update
         :params
         (fn [params]
           (normalize-high-priestess-redraws
            db
            source
            (update-high-priestess-redraw-pass
             params
             pass-index
             assoc
             :discard-card-ids
             next-discard-card-ids))))))))

(defn set-move-high-priestess-draw-count [db pass-index draw-count]
  (let [{:keys [source params]} (move-selection db)
        pass (high-priestess-redraw-pass params pass-index)
        options (high-priestess-draw-count-options db
                                                   source
                                                   params
                                                   pass-index
                                                   (:discard-card-ids pass))]
    (if (and (high-priestess-move? db source params)
             (int? pass-index)
             (<= 1 pass-index (or (selected-high-priestess-redraw-count params) 0))
             (some #{draw-count} options))
      (update-move-selection-success
       db
       update
       :params
       (fn [params]
         (normalize-high-priestess-redraws
          db
          source
          (update-high-priestess-redraw-pass params
                                             pass-index
                                             assoc
                                             :draw-count
                                             draw-count))))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-high-priestess-draw-count
                                         "Choose a draw count for this High Priestess redraw pass."
                                         {:pass-index pass-index
                                          :draw-count draw-count
                                          :options options})))))

(defn toggle-move-judgement-card [db card-id]
  (let [{:keys [source params]} (move-selection db)
        options (judgement-discard-card-options db source params)
        option-ids (set (map :id options))
        selected-card-ids (set (:judgement-card-ids params))
        selected? (contains? selected-card-ids card-id)
        maximum (judgement-card-maximum db source params)]
    (cond
      (not (judgement-move? db source params))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-judgement-card
                                         "Judgement card choices are only available for Judgement moves."
                                         {:card-id card-id
                                          :source source}))

      (not (contains? option-ids card-id))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-judgement-card
                                         "Choose a card from the discard pile."
                                         {:card-id card-id}))

      (and (not selected?)
           (<= maximum (count selected-card-ids)))
      (update-move-selection db assoc
                             :error
                             (move-error :judgement-card-limit
                                         "Judgement cannot draw more cards than the minion's pips or hand limit allow."
                                         {:card-id card-id
                                          :maximum maximum}))

      :else
      (let [next-selected-card-ids (if selected?
                                     (disj selected-card-ids card-id)
                                     (conj selected-card-ids card-id))]
        (update-move-selection-success
         db
         assoc-in
         [:params :judgement-card-ids]
         (->> options
              (map :id)
              (filter next-selected-card-ids)
              vec))))))

(defn set-move-minion-orientation [db orientation]
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (not (or (star-disc-source? db source params)
               (major-orient-step? db source params)))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-minion-orientation
                                         "Minion orientation is only available for Star Disc or ordered major moves."
                                         {:orientation orientation
                                          :source source}))

      (not (contains? pieces/legal-orientations orientation))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-orientation
                                         "Choose a legal piece orientation."
                                         {:orientation orientation}))

      :else
      (update-move-selection-success db update :params staging/set-minion-orientation orientation))))

(defn select-move-sun-disc-mode [db mode]
  (let [{:keys [source params]} (move-selection db)
        options (set (sun-disc-mode-option-ids db source params))]
    (if (contains? options mode)
      (update-move-selection-success db update :params staging/set-sun-disc-mode mode)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sun-disc-mode
                                         "Choose a supported Sun Disc option."
                                         {:mode mode
                                          :options (vec options)})))))

(defn set-move-sun-disc-orientation [db orientation]
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (not (sun-disc-orientation-available? db source params))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sun-disc-orientation
                                         "Sun Disc orientation is only available for current-player piece targets."
                                         {:orientation orientation
                                          :source source}))

      (not (contains? pieces/legal-orientations orientation))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-orientation
                                         "Choose a legal piece orientation."
                                         {:orientation orientation}))

      :else
      (update-move-selection-success db assoc-in
                                     [:params :sun-disc-orientation]
                                     orientation))))

(defn select-move-target-piece [db piece-id]
  (let [source (move-source db)
        params (move-params db)
        piece (piece-by-id db piece-id)
        selectable-piece? (some #(= piece-id (:id %))
                                (move-target-piece-options db))]
    (cond
      (nil? piece)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a piece on the board."
                                         {:piece-id piece-id}))

      (and (rod-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params staging/set-target-piece (:id piece))

      (rod-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Rod-targetable piece."
                                         {:piece-id piece-id}))

      (and (= :piece (selected-sun-disc-mode db source params))
           selectable-piece?)
      (update-move-selection-success db
                                     update
                                     :params
                                     staging/set-sun-disc-target-piece
                                     (:id piece))

      (= :piece (selected-sun-disc-mode db source params))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Sun Disc-targetable piece."
                                         {:piece-id piece-id}))

      (and (sun-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params staging/set-target-piece (:id piece))

      (sun-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose an enemy piece targeted by the minion."
                                         {:piece-id piece-id}))

      (and (cup-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params staging/set-target-piece (:id piece))

      (cup-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose an enemy piece targeted by the minion."
                                         {:piece-id piece-id}))

      (and (hierophant-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params staging/set-target-piece (:id piece))

      (hierophant-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Hierophant-targetable piece with a same-size stash piece available."
                                         {:piece-id piece-id}))

      (and (hermit-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params staging/set-target-piece (:id piece))

      (hermit-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Hermit-targetable piece."
                                         {:piece-id piece-id}))

      (and (devil-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params staging/set-target-piece (:id piece))

      (devil-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Devil-targetable piece."
                                         {:piece-id piece-id}))

      (and (hanged-man-trade-stage? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params staging/set-target-piece (:id piece))

      (hanged-man-trade-stage? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Hanged Man hand-trade target piece."
                                         {:piece-id piece-id}))

      (and (justice-trade-stage? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params staging/set-target-piece (:id piece))

      (justice-trade-stage? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Justice hand-trade target piece."
                                         {:piece-id piece-id}))

      (and (disc-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params staging/set-target-piece (:id piece))

      (disc-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Disc-targetable piece."
                                         {:piece-id piece-id}))

      (and (sword-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params staging/set-target-piece (:id piece))

      (sword-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Sword-targetable piece."
                                         {:piece-id piece-id}))

      :else
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Target pieces are only available for Cup, Rod, Disc, Sword, Justice, Hierophant, Hermit, or Devil moves."
                                         {:piece-id piece-id})))))

(defn select-move-territory-card-source [db territory-card-source]
  (let [{:keys [source params]} (move-selection db)
        replacement-source? (or (disc-move? db source params)
                                (sword-move? db source params))
        sun-replacement-source? (sun-move? db source params)
        option-ids (set (cond
                          (disc-move? db source params)
                          (disc-replacement-card-source-option-ids db source params)

                          (sword-move? db source params)
                          (sword-replacement-card-source-option-ids db source params)

                          sun-replacement-source?
                          [:hand]

                          :else
                          (territory-card-source-option-ids db source params)))]
    (if (contains? option-ids territory-card-source)
      (update-move-selection-success db
                                     update
                                     :params
                                     (cond
                                       replacement-source?
                                       (fn [params card-source]
                                         (-> params
                                             (assoc :replacement-card-source card-source)
                                             (dissoc :replacement-card-id)))

                                       sun-replacement-source?
                                       (fn [params _card-source] params)

                                       :else
                                       staging/set-territory-card-source)
                                     territory-card-source)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-territory-card-source
                                         "Choose an available card source."
                                         {:territory-card-source territory-card-source
                                          :options (vec option-ids)})))))

(defn select-move-one-point-card [db card-id]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (or (cup-move? db source params)
                 (sun-cup-needs-one-point-card? db source params))
             (one-point-card-by-id db source params card-id))
      (update-move-selection-success db assoc-in [:params :one-point-card-id] card-id)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-one-point-card
                                         "Choose a one-point card from the current player's hand."
                                         {:card-id card-id})))))

(defn select-move-replacement-card [db card-id]
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (and (sun-move? db source params)
           (sun-disc-replacement-card-by-id db source params card-id))
      (update-move-selection-success db assoc-in
                                     [:params :sun-disc-replacement-card-id]
                                     card-id)

      (sun-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sun-disc-replacement-card
                                         "Choose a replacement card for Sun's Disc action."
                                         {:card-id card-id}))

      (and (disc-move? db source params)
           (replacement-card-source-for-card db source params card-id))
      (let [card-source (replacement-card-source-for-card db source params card-id)]
        (update-move-selection-success db
                                       update
                                       :params
                                       assoc
                                       :replacement-card-source card-source
                                       :replacement-card-id card-id))

      (and (sword-move? db source params)
           (replacement-card-source-for-card db source params card-id))
      (let [card-source (replacement-card-source-for-card db source params card-id)]
        (update-move-selection-success db
                                       update
                                       :params
                                       assoc
                                       :replacement-card-source card-source
                                       :replacement-card-id card-id))

      (disc-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-disc-replacement-card
                                         "Choose a replacement card worth exactly one more point."
                                         {:card-id card-id}))

      (sword-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sword-replacement-card
                                         "Choose a replacement card worth the selected damage less."
                                         {:card-id card-id}))

      :else
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-replacement-card
                                         "Replacement cards are not available for this move."
                                         {:card-id card-id})))))

(defn set-move-orientation [db orientation]
  (if (contains? pieces/legal-orientations orientation)
    (update-move-selection-success db assoc-in [:params :orientation] orientation)
    (update-move-selection db assoc
                           :error
                           (move-error :invalid-orientation
                                       "Choose a legal piece orientation."
                                       {:orientation orientation}))))

(defn set-move-draw-count [db draw-count]
  (if (some #{draw-count} (draw-count-options db))
    (update-move-selection-success db assoc-in [:params :draw-count] draw-count)
    (update-move-selection db assoc
                           :error
                           (move-error :invalid-draw-count
                                       "Choose a draw count the current player can take."
                                       {:draw-count draw-count
                                        :options (draw-count-options db)}))))

(defn toggle-move-discard-card [db card-id]
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (not= :draw-cards source)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-discard-card
                                         "Discard choices are only available for draw moves."
                                         {:card-id card-id
                                          :source source}))

      (nil? (hand-card-by-id db card-id))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-discard-card
                                         "Choose a card from the current player's hand."
                                         {:card-id card-id}))

      :else
      (let [selected-card-ids (set (:discard-card-ids params))
            next-selected-card-ids (if (contains? selected-card-ids card-id)
                                     (disj selected-card-ids card-id)
                                     (conj selected-card-ids card-id))
            next-discard-card-ids (->> (current-player-hand db)
                                       (map :id)
                                       (filter next-selected-card-ids)
                                       vec)]
        (update-move-selection-success
         db
         assoc
         :params
         (normalize-draw-selection-params
          db
          (assoc params :discard-card-ids next-discard-card-ids)))))))

(defn set-move-distance [db distance]
  (if (some #{distance} (move-distance-options db))
    (update-move-selection-success db assoc-in [:params :distance] distance)
    (update-move-selection db assoc
                           :error
                           (move-error :invalid-distance
                                       "Choose a distance the acting minion can move."
                                       {:distance distance
                                        :options (move-distance-options db)}))))

(defn- move-damage-options [db]
  (ctx-call :move-damage-options db))

(defn set-move-damage [db damage]
  (let [options (move-damage-options db)]
    (if (some #{damage} options)
      (update-move-selection-success db update :params staging/set-damage damage)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sword-damage
                                         "Choose damage the acting minion can deal."
                                         {:damage damage
                                          :options options})))))
