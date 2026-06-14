(ns gnostica.move-selection.targeting
  (:require [gnostica.board-layout :as layout]
            [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]
            [gnostica.move-selection.commands :as commands]
            [gnostica.move-selection.context :as context]
            [gnostica.move-selection.options :as options]
            [gnostica.move-selection.registry :as registry]
            [gnostica.move-selection.power-context :as power]
            [gnostica.move-selection.state :as selection]
            [gnostica.pieces :as pieces]))

(def required-context-keys commands/required-context-keys)

(defn make-context [deps]
  (context/make "gnostica.move-selection.targeting" required-context-keys deps))

(def ^:dynamic *context* nil)

(defn- require-context []
  (or *context*
      (throw (ex-info "gnostica.move-selection.targeting requires a bound context"
                      {:code :move-selection-targeting/missing-context}))))

(defn with-context [ctx f & args]
  (binding [*context* ctx]
    (apply f args)))

(defn- command-context []
  (commands/make-context (require-context)))

(def game selection/game)
(def board selection/board)
(def board-cell-by-index selection/board-cell-by-index)
(def board-pieces selection/board-pieces)
(def current-player selection/current-player)
(def current-player-hand selection/current-player-hand)
(def current-player-id selection/current-player-id)
(def current-player-piece? selection/current-player-piece?)
(def discard-pile selection/discard-pile)
(def move-selection selection/move-selection)
(def move-source selection/move-source)
(def move-params selection/move-params)
(def piece-by-id selection/piece-by-id)
(def piece-coordinate selection/piece-coordinate)
(def pieces-at-coordinate selection/pieces-at-coordinate)
(def minion-target-coordinate selection/minion-target-coordinate)
(def targetable-piece? selection/targetable-piece?)
(def targetable-territory? selection/targetable-territory?)
(def valid-board-index? selection/valid-board-index?)
(def source-hand-card-id selection/source-hand-card-id)
(def source-card selection/source-card)
(def source-command selection/source-command)
(def target-board-cell selection/target-board-cell)
(def world-move? power/world-move?)
(def world-source-opts power/world-source-opts)
(def active-power power/active-power)
(def completed-major-actions power/completed-major-actions)
(def composite-action-power? power/composite-action-power?)
(def cup-move? power/cup-move?)
(def selected-cup-variant power/selected-cup-variant)
(def territory-card-source-option-ids power/territory-card-source-option-ids)
(def rod-move? power/rod-move?)
(def disc-move? power/disc-move?)
(def selected-disc-variant power/selected-disc-variant)
(def selected-disc-action-count power/selected-disc-action-count)
(def card-worth-disc-actions-more? power/card-worth-disc-actions-more?)
(def sword-move? power/sword-move?)
(def selected-sword-variant power/selected-sword-variant)
(def card-worth-sword-damage-less? power/card-worth-sword-damage-less?)
(def one-point-card-by-id selection/one-point-card-by-id)
(def sun-move? power/sun-move?)
(def devil-move? power/devil-move?)
(def hierophant-move? power/hierophant-move?)
(def hermit-move? power/hermit-move?)
(def hanged-man-trade-stage? power/hanged-man-trade-stage?)
(def justice-trade-stage? power/justice-trade-stage?)
(def star-disc-source? power/star-disc-source?)
(def composite-major-powers registry/composite-major-powers)
(def disc-replacement-card-source-order options/disc-replacement-card-source-order)

(defn- rod-command-resolves? [db source-id params command]
  (boolean
   (when (and (game db) command)
     (:ok? (if (world-move? db source-id params)
             (game-state/apply-world-move (game db) command)
             (game-state/resolve-rod-command (game db) command))))))

(defn- rod-piece-target? [db source-id params piece]
  (and piece
       (= :push-piece (:rod-mode params))
       (let [candidate-params (assoc params
                                     :rod-mode :push-piece
                                     :target-piece-id (:id piece)
                                     :distance 1)]
         (rod-command-resolves?
          db
          source-id
          candidate-params
          (commands/rod-resolver-command
           (command-context)
           db
           source-id
           candidate-params)))))

(defn rod-territory-target? [db source-id params cell]
  (and cell
       (= :push-territory (:rod-mode params))
       (let [candidate-params (assoc params
                                     :rod-mode :push-territory
                                     :target-board-index (:index cell)
                                     :distance 1)]
         (rod-command-resolves?
          db
          source-id
          candidate-params
          (commands/rod-resolver-command
           (command-context)
           db
           source-id
           candidate-params)))))

(defn- disc-resolution-state [state command]
  (if-let [orientation (:minion-orientation command)]
    (let [orient-result (game-state/apply-orient-move
                         state
                         {:player-id (:player-id command)
                          :piece-id (get-in command [:source :piece-id])
                          :orientation orientation})]
      (when (:ok? orient-result)
        (:state orient-result)))
    state))

(defn- disc-command-resolves? [db source-id params command]
  (let [state (game db)]
    (boolean
     (and state
          command
          (when-let [resolution-state (disc-resolution-state state command)]
            (:ok? (game-state/resolve-disc-command
                   resolution-state
                   (dissoc command :minion-orientation)
                   (or (world-source-opts db source-id params) {}))))))))

(defn- disc-piece-target? [db source-id params piece]
  (and piece
       (= :piece (:disc-target-kind params))
       (let [candidate-params (assoc params
                                     :disc-target-kind :piece
                                     :target-piece-id (:id piece))]
         (disc-command-resolves?
          db
          source-id
          candidate-params
          (commands/disc-resolver-command
           (command-context)
           db
           source-id
           candidate-params)))))

(defn disc-territory-target? [db source-id params cell]
  (and cell
       (= :territory (:disc-target-kind params))
       (let [candidate-params (assoc params
                                     :disc-target-kind :territory
                                     :target-board-index (:index cell))]
         (disc-command-resolves?
          db
          source-id
          candidate-params
          (commands/disc-resolver-command
           (command-context)
           db
           source-id
           candidate-params)))))

(defn disc-target-piece [db params]
  (let [source (get-in db [:move-selection :source])
        piece (piece-by-id db (:target-piece-id params))]
    (when (disc-piece-target? db source params piece)
      piece)))

(defn disc-replacement-card-source-option-ids [db source-id params]
  (when (and (disc-move? db source-id params)
             (= :territory (:disc-target-kind params)))
    (if (= :disc-from-discard (selected-disc-variant db source-id params))
      disc-replacement-card-source-order
      [:hand])))

(defn selected-disc-replacement-card-source [db source-id params]
  (let [options (set (disc-replacement-card-source-option-ids db source-id params))
        selected (:replacement-card-source params)]
    (cond
      (contains? options selected)
      selected

      (= 1 (count options))
      (first options)

      :else
      nil)))

(defn- discard-replacement-options [db source-id params]
  (let [source-card (source-card db source-id params)]
    (cond-> (discard-pile db)
      (and (= :play-hand-card source-id)
           source-card)
      (conj source-card))))

(defn disc-replacement-card-options-for-source [db source-id params replacement-source]
  (let [target-cell (target-board-cell db params)
        original-card (:card target-cell)
        source-card-id (source-hand-card-id source-id params)]
    (if-not original-card
      []
      (->> (case replacement-source
             :hand (current-player-hand db)
             :discard-pile (discard-replacement-options db source-id params)
             [])
           (filter #(and (card-worth-disc-actions-more?
                          %
                          original-card
                          (selected-disc-action-count db source-id params))
                         (or (not= :hand replacement-source)
                             (not= source-card-id (:id %)))))
           vec))))

(defn disc-replacement-card-options-for [db source-id params]
  (disc-replacement-card-options-for-source
   db
   source-id
   params
   (selected-disc-replacement-card-source db source-id params)))

(defn disc-replacement-card-by-id [db source-id params card-id]
  (some #(when (= card-id (:id %)) %)
        (disc-replacement-card-options-for db source-id params)))

(defn- completed-major-action-by-power [params power]
  (some #(when (= power (:power %)) %)
        (completed-major-actions params)))

(defn- tower-minion-orientation [params]
  (or (:minion-orientation params)
      (:orientation (completed-major-action-by-power params :orient-minion))))

(defn- sword-resolution-state [db source-id params command]
  (let [state (game db)]
    (cond
      (nil? state)
      nil

      (and (= :tower (active-power db source-id params))
           (tower-minion-orientation params))
      (let [orient-result (game-state/apply-orient-move
                           state
                           {:player-id (:player-id command)
                            :piece-id (get-in command [:source :piece-id])
                            :orientation (tower-minion-orientation params)})]
        (when (:ok? orient-result)
          (:state orient-result)))

      :else
      state)))

(defn- moon-command-resolves? [db source-id params]
  (let [moon-command (commands/gameplay-command-for-power
                      (command-context)
                      db
                      source-id
                      params
                      :moon)]
    (boolean
     (and (or (:rod moon-command)
              (:sword moon-command))
          (let [result (if (world-move? db source-id params)
                         (game-state/apply-world-move (game db) moon-command)
                         (game-state/apply-moon-move (game db) moon-command))]
            (:ok? result))))))

(defn- sword-command-resolves? [db source-id params command]
  (let [state (game db)]
    (boolean
     (and state
          command
          (if (= :moon (active-power db source-id params))
            (moon-command-resolves? db source-id params)
            (when-let [resolution-state (sword-resolution-state
                                         db
                                         source-id
                                         params
                                         command)]
              (:ok? (game-state/resolve-sword-command
                     resolution-state
                     command
                     (or (world-source-opts db source-id params) {})))))))))

(defn- sword-piece-target? [db source-id params piece]
  (and piece
       (= :piece (:sword-target-kind params))
       (let [candidate-params (assoc params
                                     :sword-target-kind :piece
                                     :target-piece-id (:id piece)
                                     :damage 1)]
         (sword-command-resolves?
          db
          source-id
          candidate-params
          (commands/sword-resolver-command
           (command-context)
           db
           source-id
           candidate-params)))))

(defn sword-territory-target? [db source-id params cell]
  (and cell
       (= :territory (:sword-target-kind params))
       (let [candidate-params (assoc params
                                     :sword-target-kind :territory
                                     :target-board-index (:index cell)
                                     :damage 1)]
         (sword-command-resolves?
          db
          source-id
          candidate-params
          (commands/sword-resolver-command
           (command-context)
           db
           source-id
           candidate-params)))))

(defn sword-target-piece [db source-id params]
  (let [piece (piece-by-id db (:target-piece-id params))]
    (when (sword-piece-target? db source-id params piece)
      piece)))

(defn- sword-territory-destroyed? [db params]
  (let [target-value (some-> (target-board-cell db params)
                             :card
                             cards/card-point-value)]
    (and (some? target-value)
         (= target-value (:damage params)))))

(defn sword-replacement-card-source-option-ids [db source-id params]
  (when (and (sword-move? db source-id params)
             (= :territory (:sword-target-kind params))
             (some? (:target-board-index params))
             (some? (:damage params))
             (not (sword-territory-destroyed? db params)))
    (if (= :sword-from-discard (selected-sword-variant db source-id params))
      disc-replacement-card-source-order
      [:hand])))

(defn selected-sword-replacement-card-source [db source-id params]
  (let [options (set (sword-replacement-card-source-option-ids db source-id params))
        selected (:replacement-card-source params)]
    (cond
      (contains? options selected)
      selected

      (= 1 (count options))
      (first options)

      :else
      nil)))

(defn sword-replacement-card-options-for-source [db source-id params replacement-source]
  (let [target-cell (target-board-cell db params)
        original-card (:card target-cell)
        damage (:damage params)
        source-card-id (source-hand-card-id source-id params)]
    (if-not (and original-card damage replacement-source)
      []
      (->> (case replacement-source
             :hand (current-player-hand db)
             :discard-pile (discard-replacement-options db source-id params)
             [])
           (filter #(and (card-worth-sword-damage-less?
                          %
                          original-card
                          damage)
                         (or (not= :hand replacement-source)
                             (not= source-card-id (:id %)))))
           vec))))

(defn sword-replacement-card-options-for [db source-id params]
  (sword-replacement-card-options-for-source
   db
   source-id
   params
   (selected-sword-replacement-card-source db source-id params)))

(defn sword-replacement-card-by-id [db source-id params card-id]
  (some #(when (= card-id (:id %)) %)
        (sword-replacement-card-options-for db source-id params)))

(defn sword-damage-options-for [db source-id params]
  (if-not (sword-move? db source-id params)
    []
    (let [piece (piece-by-id db (:piece-id params))
          max-damage (pieces/pips piece)
          target-pips (case (:sword-target-kind params)
                        :piece (some-> (sword-target-piece db source-id params)
                                       pieces/pips)
                        :territory (when (sword-territory-target?
                                          db
                                          source-id
                                          params
                                          (target-board-cell db params))
                                     (some-> (target-board-cell db params)
                                             :card
                                             cards/card-point-value))
                        nil)]
      (if (and (int? max-damage)
               (pos? max-damage)
               (int? target-pips)
               (pos? target-pips))
        (vec (range 1 (inc (min max-damage target-pips))))
        []))))

(defn sword-orientation-available? [db source-id params]
  (let [target-piece (piece-by-id db (:target-piece-id params))
        target-pips (pieces/pips target-piece)
        damage (:damage params)]
    (and (sword-move? db source-id params)
         (= :piece (:sword-target-kind params))
         (current-player-piece? db target-piece)
         (int? target-pips)
         (int? damage)
         (< damage target-pips))))

(defn- apply-composite-preview [state power command]
  (when (contains? composite-major-powers power)
    (when-let [transition-fn (registry/power-transition-fn power)]
      (transition-fn state command))))

(defn- cup-target-state [db source-id params]
  (let [state (game db)
        actions (completed-major-actions params)]
    (if (and state
             (seq actions)
             (composite-action-power? db source-id params :cup))
      (let [command (cond-> {:player-id (current-player-id db)
                             :source (source-command source-id params)
                             :actions actions}
                      (world-move? db source-id params)
                      (assoc :copied-board-index (:copied-board-index params)))
            result (if (world-move? db source-id params)
                     (game-state/apply-world-move state command)
                     (apply-composite-preview state
                                              (active-power db source-id params)
                                              command))]
        (if (:ok? result)
          (:state result)
          state))
      state)))

(defn cup-target-db [db source-id params]
  (if-let [state (cup-target-state db source-id params)]
    (assoc db :game state)
    db))

(defn- cup-target-piece? [db source-id params piece]
  (let [target-db (cup-target-db db source-id params)
        target-piece (piece-by-id target-db (:id piece))
        minion (piece-by-id target-db (:piece-id params))]
    (and target-piece
         (not (current-player-piece? target-db target-piece))
         (valid-board-index? target-db (:space-index target-piece))
         (targetable-piece? target-db minion target-piece))))

(defn cup-target-territory? [db source-id params cell]
  (let [target-db (cup-target-db db source-id params)
        target-cell (board-cell-by-index target-db (:index cell))
        minion (piece-by-id target-db (:piece-id params))]
    (and target-cell
         (targetable-territory? target-db minion target-cell))))

(defn- cup-target-piece [db params]
  (let [source (get-in db [:move-selection :source])
        piece (piece-by-id (cup-target-db db source params)
                           (:target-piece-id params))]
    (when (cup-target-piece? db source params piece)
      piece)))

(defn empty-board-target? [db index]
  (when-let [{:keys [row col]} (board-cell-by-index db index)]
    (empty? (pieces-at-coordinate db row col))))

(defn- empty-wasteland-target? [db {:keys [row col]}]
  (empty? (pieces-at-coordinate db row col)))

(def default-initial-placement-board-index 4)

(defn default-initial-placement-target-index [db selected-index]
  (or (when (empty-board-target? db default-initial-placement-board-index)
        default-initial-placement-board-index)
      (when (empty-board-target? db selected-index)
        selected-index)
      (some (fn [{:keys [index]}]
              (when (empty-board-target? db index)
                index))
            (board db))))

(declare valid-wasteland-target? enemy-pieces-at-coordinate)

(defn- cup-target-wasteland? [db source-id params {:keys [row col] :as space}]
  (let [target-db (cup-target-db db source-id params)
        minion (piece-by-id target-db (:piece-id params))]
    (and space
         (= [row col] (minion-target-coordinate target-db minion))
         (empty? (enemy-pieces-at-coordinate target-db row col)))))

(defn- enemy-pieces-at-coordinate [db row col]
  (let [player-id (current-player-id db)]
    (filterv #(and (not= player-id (:player-id %))
                   (= [row col] (piece-coordinate db %)))
             (board-pieces db))))

(defn- current-player-stash-count [db size]
  (or (get-in (current-player db) [:stash size])
      (get-in db [:game :pieces :stashes (current-player-id db) size])
      0))

(defn- major-target-piece? [db params piece]
  (let [minion (piece-by-id db (:piece-id params))]
    (and piece
         (targetable-piece? db minion piece))))

(defn- devil-target-state [db source-id params]
  (let [state (game db)
        actions (completed-major-actions params)]
    (if (and state
             (seq actions)
             (devil-move? db source-id params))
      (let [command (cond-> {:player-id (current-player-id db)
                             :source (source-command source-id params)
                             :actions actions}
                      (world-move? db source-id params)
                      (assoc :copied-board-index (:copied-board-index params)))
            result (if (world-move? db source-id params)
                     (game-state/apply-world-move state command)
                     (game-state/apply-devil-move state command))]
        (if (:ok? result)
          (:state result)
          state))
      state)))

(defn- devil-target-db [db source-id params]
  (if-let [state (devil-target-state db source-id params)]
    (assoc db :game state)
    db))

(defn- devil-target-piece? [db source-id params piece]
  (let [target-db (devil-target-db db source-id params)
        target-piece (piece-by-id target-db (:id piece))
        minion (piece-by-id target-db (:piece-id params))]
    (and target-piece
         (targetable-piece? target-db minion target-piece))))

(defn hierophant-target-piece? [db params piece]
  (and (major-target-piece? db params piece)
       (pos? (current-player-stash-count db (:size piece)))))

(defn hermit-target-piece? [db params piece]
  (and (nil? (:target-board-index params))
       (major-target-piece? db params piece)))

(defn hermit-target-territory? [db params cell]
  (let [minion (piece-by-id db (:piece-id params))]
    (and (nil? (:target-piece-id params))
         (targetable-territory? db minion cell)
         (empty? (enemy-pieces-at-coordinate db (:row cell) (:col cell))))))

(defn hermit-target-selected? [params]
  (or (:target-piece-id params)
      (some? (:target-board-index params))))

(defn hermit-piece-target-selected? [params]
  (some? (:target-piece-id params)))

(defn hermit-territory-target-selected? [params]
  (some? (:target-board-index params)))

(defn- hermit-empty-board-destination? [db index]
  (and (some? index)
       (empty-board-target? db index)))

(defn- hermit-empty-wasteland-destination? [db target]
  (and (valid-wasteland-target? db target)
       (empty-wasteland-target? db target)))

(defn- hermit-territory-wasteland-destination? [db target]
  (and (valid-wasteland-target? db target)
       (empty? (enemy-pieces-at-coordinate db (:row target) (:col target)))))

(defn hermit-destination-complete? [db params]
  (cond
    (hermit-piece-target-selected? params)
    (or (hermit-empty-board-destination? db (:hermit-destination-board-index params))
        (hermit-empty-wasteland-destination? db (:hermit-destination-wasteland params)))

    (hermit-territory-target-selected? params)
    (hermit-territory-wasteland-destination? db (:hermit-destination-wasteland params))

    :else
    false))

(defn hermit-orientation-required? [db params]
  (when-let [piece (piece-by-id db (:target-piece-id params))]
    (current-player-piece? db piece)))

(defn move-target-wasteland-options [db]
  (let [source (get-in db [:move-selection :source])
        params (get-in db [:move-selection :params])
        target-db (if (or (cup-move? db source params)
                          (sun-move? db source params))
                    (cup-target-db db source params)
                    db)
        spaces (layout/wasteland-spaces (board target-db))]
    (cond
      (= :place-initial-small source)
      (filterv #(empty-wasteland-target? db %) spaces)

      (or (cup-move? db source params)
          (sun-move? db source params))
      (filterv #(cup-target-wasteland? db source params %) spaces)

      (hermit-move? db source params)
      (cond
        (hermit-piece-target-selected? params)
        (filterv #(empty-wasteland-target? db %) spaces)

        (hermit-territory-target-selected? params)
        (filterv #(empty? (enemy-pieces-at-coordinate db (:row %) (:col %)))
                 spaces)

        :else
        [])

      :else
      spaces)))

(defn- wasteland-space-by-coordinate [db row col]
  (some #(when (and (= row (:row %))
                    (= col (:col %)))
           %)
        (move-target-wasteland-options db)))

(defn valid-wasteland-target? [db target]
  (and (= :wasteland (:kind target))
       (int? (:row target))
       (int? (:col target))
       (some? (wasteland-space-by-coordinate db (:row target) (:col target)))))

(defn target-space-complete? [db source-id params]
  (case source-id
    :place-initial-small
    (or (empty-board-target? db (:target-board-index params))
        (valid-wasteland-target? db (:target-wasteland params)))

    (or (some? (some #(when (= (:target-board-index params) (:index %)) %)
                     (filterv #(cup-target-territory? db source-id params %)
                              (board (cup-target-db db source-id params)))))
        (some? (cup-target-piece db params))
        (valid-wasteland-target? db (:target-wasteland params)))))

(defn target-resolution-complete? [db source-id params]
  (cond
    (some? (cup-target-piece db params))
    true

    (some? (some #(when (= (:target-board-index params) (:index %)) %)
                 (filterv #(cup-target-territory? db source-id params %)
                          (board (cup-target-db db source-id params)))))
    (contains? pieces/legal-orientations (:orientation params))

    (valid-wasteland-target? db (:target-wasteland params))
    (let [cup-variant (selected-cup-variant db source-id params)
          selected-source (:territory-card-source params)
          territory-card-source (or selected-source :hand)
          source-options (set (territory-card-source-option-ids db source-id params))]
      (cond
        (and (= :wheel-cup cup-variant)
             (nil? selected-source))
        false

        (not (contains? source-options territory-card-source))
        false

        (= :draw-pile-top territory-card-source)
        true

        :else
        (some? (one-point-card-by-id db source-id params (:one-point-card-id params)))))

    :else
    false))

(defn sun-cup-target-kind [params]
  (cond
    (:target-wasteland params) :wasteland
    (:target-piece-id params) :piece
    (some? (:target-board-index params)) :territory
    :else nil))

(defn sun-cup-target-ready? [db source-id params]
  (and (sun-move? db source-id params)
       (target-space-complete? db source-id params)
       (case (sun-cup-target-kind params)
         :territory (contains? pieces/legal-orientations (:orientation params))
         (:piece :wasteland) true
         false)))

(defn sun-disc-mode-option-ids [db source-id params]
  (when (sun-cup-target-ready? db source-id params)
    (vec
     (concat [:skip]
             (case (sun-cup-target-kind params)
               :territory [:created-piece]
               :wasteland [:created-territory]
               [])
             [:piece :territory]))))

(defn selected-sun-disc-mode [db source-id params]
  (let [options (set (sun-disc-mode-option-ids db source-id params))
        selected (:sun-disc-mode params)]
    (when (contains? options selected)
      selected)))

(defn sun-cup-needs-one-point-card? [db source-id params]
  (and (sun-move? db source-id params)
       (= :wasteland (sun-cup-target-kind params))
       (some? (selected-sun-disc-mode db source-id params))
       (not= :created-territory
             (selected-sun-disc-mode db source-id params))))

(defn- sun-disc-command-resolves? [db source-id params command]
  (let [state (game db)]
    (boolean
     (and state
          command
          (:ok? (game-state/resolve-disc-command
                 state
                 command
                 (or (world-source-opts db source-id params) {})))))))

(defn- sun-disc-piece-target? [db source-id params piece]
  (and piece
       (= :piece (selected-sun-disc-mode db source-id params))
       (let [candidate-params (assoc params
                                     :sun-disc-mode :piece
                                     :sun-disc-target-piece-id (:id piece))]
         (sun-disc-command-resolves?
          db
          source-id
          candidate-params
          (commands/sun-disc-resolver-command
           (command-context)
           db
           source-id
           candidate-params)))))

(defn sun-disc-territory-target? [db source-id params cell]
  (and cell
       (= :territory (selected-sun-disc-mode db source-id params))
       (let [candidate-params (assoc params
                                     :sun-disc-mode :territory
                                     :sun-disc-target-board-index (:index cell))]
         (sun-disc-command-resolves?
          db
          source-id
          candidate-params
          (commands/sun-disc-resolver-command
           (command-context)
           db
           source-id
           candidate-params)))))

(defn sun-disc-target-piece [db source-id params]
  (let [piece (piece-by-id db (:sun-disc-target-piece-id params))]
    (when (sun-disc-piece-target? db source-id params piece)
      piece)))

(defn sun-disc-orientation-available? [db source-id params]
  (and (= :piece (selected-sun-disc-mode db source-id params))
       (current-player-piece? db (sun-disc-target-piece db source-id params))))

(defn sun-disc-target-cell [db params]
  (board-cell-by-index db (:sun-disc-target-board-index params)))

(defn- sun-disc-replacement-source-cards [db source-id params]
  (let [source-card-id (source-hand-card-id source-id params)
        spent-card-ids (cond-> #{}
                         source-card-id (conj source-card-id)
                         (:one-point-card-id params) (conj (:one-point-card-id params)))]
    (remove #(contains? spent-card-ids (:id %))
            (current-player-hand db))))

(defn sun-disc-replacement-card-options-for [db source-id params]
  (let [replacement-cards (sun-disc-replacement-source-cards db source-id params)]
    (case (selected-sun-disc-mode db source-id params)
      :created-territory
      (->> replacement-cards
           (filter #(= 2 (cards/card-point-value %)))
           vec)

      :territory
      (let [original-card (:card (sun-disc-target-cell db params))]
        (if original-card
          (->> replacement-cards
               (filter #(card-worth-disc-actions-more? % original-card 1))
               vec)
          []))

      [])))

(defn sun-disc-replacement-card-by-id [db source-id params card-id]
  (some #(when (= card-id (:id %)) %)
        (sun-disc-replacement-card-options-for db source-id params)))

(defn replacement-card-source-option-ids [db source-id params]
  (cond
    (sun-move? db source-id params)
    [:hand]

    (disc-move? db source-id params)
    (disc-replacement-card-source-option-ids db source-id params)

    (sword-move? db source-id params)
    (sword-replacement-card-source-option-ids db source-id params)

    :else
    []))

(defn selected-replacement-card-source [db source-id params]
  (cond
    (sun-move? db source-id params)
    :hand

    (disc-move? db source-id params)
    (selected-disc-replacement-card-source db source-id params)

    (sword-move? db source-id params)
    (selected-sword-replacement-card-source db source-id params)

    :else
    nil))

(defn replacement-card-options-for-source [db source-id params card-source]
  (when (contains? (set (replacement-card-source-option-ids db source-id params))
                   card-source)
    (cond
      (and (sun-move? db source-id params)
           (= :hand card-source))
      (sun-disc-replacement-card-options-for db source-id params)

      (disc-move? db source-id params)
      (disc-replacement-card-options-for-source db source-id params card-source)

      (sword-move? db source-id params)
      (sword-replacement-card-options-for-source db source-id params card-source)

      :else
      [])))

(defn replacement-card-source-for-card [db source-id params card-id]
  (let [selected-source (selected-replacement-card-source db source-id params)
        candidate-sources (if selected-source
                            [selected-source]
                            (replacement-card-source-option-ids db source-id params))
        matching-sources (filterv
                          (fn [card-source]
                            (some #(= card-id (:id %))
                                  (replacement-card-options-for-source
                                   db
                                   source-id
                                   params
                                   card-source)))
                          candidate-sources)]
    (when (= 1 (count matching-sources))
      (first matching-sources))))

(defn sun-disc-territory-target-stage? [db source-id params]
  (and (sun-move? db source-id params)
       (= :territory (selected-sun-disc-mode db source-id params))))

(defn- rod-distance-options-for-piece [piece]
  (vec (range 1 (inc (or (pieces/pips piece) 0)))))

(defn move-disc-minion-orientation-required? [db]
  (let [{:keys [source params]} (move-selection db)]
    (star-disc-source? db source params)))

(defn move-distance-options [db]
  (let [piece (piece-by-id db (get-in (move-selection db)
                                      [:params :piece-id]))]
    (rod-distance-options-for-piece piece)))

(defn move-target-piece-options [db]
  (let [source (move-source db)
        params (move-params db)]
    (cond
      (rod-move? db source params)
      (if (= :push-piece (:rod-mode params))
        (filterv #(rod-piece-target? db source params %)
                 (board-pieces db))
        [])

      (= :piece (selected-sun-disc-mode db source params))
      (filterv #(sun-disc-piece-target? db source params %)
               (board-pieces db))

      (sun-move? db source params)
      (let [target-db (cup-target-db db source params)]
        (filterv #(cup-target-piece? db source params %)
                 (board-pieces target-db)))

      (cup-move? db source params)
      (let [target-db (cup-target-db db source params)]
        (filterv #(cup-target-piece? db source params %)
                 (board-pieces target-db)))

      (disc-move? db source params)
      (filterv #(disc-piece-target? db source params %)
               (board-pieces db))

      (sword-move? db source params)
      (filterv #(sword-piece-target? db source params %)
               (board-pieces db))

      (hierophant-move? db source params)
      (filterv #(hierophant-target-piece? db params %)
               (board-pieces db))

      (hermit-move? db source params)
      (if (hermit-target-selected? params)
        []
        (filterv #(hermit-target-piece? db params %)
                 (board-pieces db)))

      (devil-move? db source params)
      (let [target-db (devil-target-db db source params)]
        (filterv #(devil-target-piece? db source params %)
                 (board-pieces target-db)))

      (hanged-man-trade-stage? db source params)
      (board-pieces db)

      (justice-trade-stage? db source params)
      (board-pieces db)

      :else
      [])))

(defn- rod-target-piece [db params]
  (piece-by-id db (:target-piece-id params)))

(defn move-rod-orientation-required? [db]
  (let [{:keys [source params]} (move-selection db)]
    (and (rod-move? db source params)
         (case (:rod-mode params)
           :move-minion true
           :push-piece (current-player-piece? db (rod-target-piece db params))
           false))))

(defn move-disc-orientation-available? [db]
  (let [{:keys [source params]} (move-selection db)]
    (and (disc-move? db source params)
         (= :piece (:disc-target-kind params))
         (current-player-piece? db (disc-target-piece db params)))))

(defn move-sun-disc-orientation-available? [db]
  (let [{:keys [source params]} (move-selection db)]
    (sun-disc-orientation-available? db source params)))

(defn move-sword-orientation-available? [db]
  (let [{:keys [source params]} (move-selection db)]
    (sword-orientation-available? db source params)))

(defn move-hermit-orientation-required? [db]
  (let [{:keys [source params]} (move-selection db)]
    (and (hermit-move? db source params)
         (hermit-destination-complete? db params)
         (hermit-orientation-required? db params))))
