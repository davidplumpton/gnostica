(ns gnostica.app-state
  (:require [clojure.string :as str]
            [gnostica.board-layout :as layout]
            [gnostica.game-state :as game-state]
            [gnostica.move-selection :as move-selection]
            [gnostica.pieces :as pieces]))

(def default-player-specs
  (mapv #(select-keys % [:id :name]) pieces/players))

(def default-lobby-player-specs
  (subvec default-player-specs 0 game-state/min-players))

(def default-selected-board-index 0)

(def default-card-icon-mode :always)

(def default-hotkey-help-open? false)

(def default-icon-help-open? false)

(def target-score-options
  (vec (sort game-state/allowed-target-scores)))

(def panel-ids
  #{:cards :move :territory})

(def default-open-panels
  #{:cards})

(def default-three-runtime-status
  {:ok? false
   :code :three-unchecked
   :revision nil
   :expected-revision nil
   :message "Three.js runtime status has not been checked yet."})

(def card-icon-modes
  #{:always :popup})

(def empty-move-selection move-selection/empty-move-selection)

(declare initialize-game-db)

(defn- player-colour [player-id]
  (get pieces/players-by-id player-id))

(defn- known-player-id? [player-id]
  (contains? pieces/players-by-id player-id))

(defn- trim-name [name]
  (str/trim (str name)))

(defn- normalize-controller-id [controller-id]
  (let [controller-id (trim-name (if (keyword? controller-id)
                                   (name controller-id)
                                   controller-id))]
    (when-not (str/blank? controller-id)
      controller-id)))

(defn- normalize-local-controller [controller]
  (when-let [controller-id (normalize-controller-id (:id controller))]
    {:id controller-id
     :name (let [controller-name (trim-name (:name controller))]
             (if (str/blank? controller-name)
               "Local browser"
               controller-name))}))

(defn- normalize-target-score [target-score]
  (let [score (cond
                (int? target-score)
                target-score

                (string? target-score)
                (some #(when (= target-score (str %)) %)
                      target-score-options))]
    (when (contains? game-state/allowed-target-scores score)
      score)))

(defn- lobby-error [code message data]
  {:code code
   :message message
   :data data})

(defn- duplicate-lobby-player-ids [players]
  (->> players
       (map :id)
       frequencies
       (keep (fn [[player-id n]]
               (when (< 1 n)
                 player-id)))
       vec))

(defn- first-available-player-id [players]
  (let [used (set (map :id players))]
    (some #(when-not (contains? used (:id %))
             (:id %))
          pieces/players)))

(defn- normalize-player-controller [player-spec local-controller]
  (or local-controller
      (normalize-local-controller {:id (:controller-id player-spec)
                                   :name (:controller-name player-spec)})))

(defn- with-player-controller [player controller]
  (cond-> player
    controller
    (assoc :controller-id (:id controller)
           :controller-name (:name controller))))

(defn- normalize-lobby-player [local-controller slot-id player-spec]
  (let [requested-id (:id player-spec)
        player-id (if (known-player-id? requested-id)
                    requested-id
                    (:id (first pieces/players)))
        colour (player-colour player-id)
        name (trim-name (or (:name player-spec)
                            (:name colour)))
        controller (normalize-player-controller player-spec local-controller)]
    (with-player-controller {:slot-id slot-id
                             :id player-id
                             :name name}
                            controller)))

(defn- normalize-lobby-players [player-specs local-controller]
  (mapv (partial normalize-lobby-player local-controller)
        (range 1 (inc (count player-specs)))
        player-specs))

(defn- next-lobby-slot-id [players]
  (inc (reduce max 0 (map :slot-id players))))

(defn- lobby-player-specs [lobby]
  (mapv (fn [{:keys [id name]}]
          {:id id
           :name (trim-name name)})
        (:players lobby)))

(defn lobby-validation-error [lobby]
  (let [players (:players lobby)
        player-specs (lobby-player-specs lobby)
        duplicate-ids (duplicate-lobby-player-ids players)
        unknown-ids (->> players
                         (map :id)
                         (remove known-player-id?)
                         vec)
        blank-name-slot-ids (->> players
                                 (filter #(str/blank? (trim-name (:name %))))
                                 (mapv :slot-id))]
    (cond
      (< (count players) game-state/min-players)
      (lobby-error :too-few-players
                   "Add at least two players before starting."
                   {:count (count players)
                    :minimum game-state/min-players})

      (< game-state/max-players (count players))
      (lobby-error :too-many-players
                   "Gnostica supports no more than six players."
                   {:count (count players)
                    :maximum game-state/max-players})

      (seq unknown-ids)
      (lobby-error :unknown-player-colours
                   "Every player must use a known player colour."
                   {:player-ids unknown-ids
                    :known-ids (mapv :id pieces/players)})

      (seq duplicate-ids)
      (lobby-error :duplicate-player-colours
                   "Each player colour can only be used once."
                   {:player-ids duplicate-ids})

      (seq blank-name-slot-ids)
      (lobby-error :blank-player-names
                   "Every player needs a display name."
                   {:slot-ids blank-name-slot-ids})

      (not (game-state/valid-player-count? player-specs))
      (lobby-error :invalid-player-count
                   "Gnostica requires between two and six players."
                   {:count (count player-specs)
                    :minimum game-state/min-players
                    :maximum game-state/max-players}))))

(defn lobby-valid? [lobby]
  (nil? (lobby-validation-error lobby)))

(defn- create-lobby [{:keys [player-specs lobby-player-specs game-options demo-board-pieces
                             local-controller]
                      :as opts
                      :or {player-specs default-lobby-player-specs
                           game-options {}}}]
  (let [local-controller (normalize-local-controller local-controller)
        players (normalize-lobby-players (or lobby-player-specs player-specs)
                                         local-controller)]
    (cond-> {:players players
             :next-slot-id (next-lobby-slot-id players)
             :start-options (cond-> {:game-options (or game-options {})}
                              (contains? opts :demo-board-pieces)
                              (assoc :demo-board-pieces demo-board-pieces))}
      local-controller
      (assoc :local-controller local-controller)

      (lobby-validation-error {:players players})
      (assoc :error (lobby-validation-error {:players players})))))

(defn normalize-open-panels [open-panels]
  (set (filter panel-ids open-panels)))

(defn- state-with-demo-board-pieces [state opts]
  (if (contains? opts :demo-board-pieces)
    (game-state/with-board-pieces state (:demo-board-pieces opts))
    state))

(defn normalize-card-icon-mode [mode]
  (if (contains? card-icon-modes mode)
    mode
    default-card-icon-mode))

(defn normalize-three-runtime-status [status]
  (let [status (if (map? status) status {})]
    (merge default-three-runtime-status
           (select-keys status [:code :revision :expected-revision :message])
           {:ok? (true? (:ok? status))})))

(defn- base-db
  [{:keys [selected-board-index card-icon-mode open-panels three-runtime-status]
    :or {selected-board-index default-selected-board-index
         card-icon-mode default-card-icon-mode
         open-panels default-open-panels}}]
  {:selected-board-index selected-board-index
   :card-icon-mode (normalize-card-icon-mode card-icon-mode)
   :open-panels (normalize-open-panels open-panels)
   :hotkey-help-open? default-hotkey-help-open?
   :icon-help-open? default-icon-help-open?
   :move-selection (empty-move-selection)
   :three-runtime-status (normalize-three-runtime-status three-runtime-status)
   :three-texture-errors []})

(defn- initialize-game-db [db opts player-specs game-options]
  (let [result (game-state/create-game player-specs (or game-options {}))]
    (if (:ok? result)
      (-> db
          (assoc :game (state-with-demo-board-pieces (:state result) opts))
          (dissoc :turn-action)
          (dissoc :setup-error :lobby))
      (-> db
          (assoc :setup-error (:error result))
          (dissoc :game :turn-action)))))

(defn initialize
  ([] (initialize {}))
  ([{:keys [player-specs game-options start-in-lobby? bypass-lobby?]
     :as opts
     :or {player-specs default-player-specs
          game-options {}}}]
   (let [db (base-db opts)]
     (if (and start-in-lobby?
              (not bypass-lobby?))
       (assoc db :lobby (create-lobby opts))
       (initialize-game-db db opts player-specs game-options)))))

(defn game [db]
  (:game db))

(defn setup-error [db]
  (:setup-error db))

(defn lobby [db]
  (:lobby db))

(defn lobby-active? [db]
  (some? (lobby db)))

(defn lobby-players [db]
  (get-in db [:lobby :players] []))

(defn- refresh-lobby-error [db]
  (if-let [lobby (lobby db)]
    (let [validation-error (lobby-validation-error lobby)]
      (if validation-error
        (assoc-in db [:lobby :error] validation-error)
        (update db :lobby dissoc :error)))
    db))

(defn add-lobby-player [db]
  (let [players (lobby-players db)]
    (cond
      (not (lobby-active? db))
      db

      (<= game-state/max-players (count players))
      (assoc-in db [:lobby :error]
                (lobby-error :too-many-players
                             "Gnostica supports no more than six players."
                             {:count (count players)
                              :maximum game-state/max-players}))

      :else
      (if-let [player-id (first-available-player-id players)]
        (let [slot-id (get-in db [:lobby :next-slot-id] (next-lobby-slot-id players))
              colour (player-colour player-id)
              player (with-player-controller {:slot-id slot-id
                                              :id player-id
                                              :name (:name colour)}
                       (get-in db [:lobby :local-controller]))]
          (-> db
              (update-in [:lobby :players] conj player)
              (assoc-in [:lobby :next-slot-id] (inc slot-id))
              refresh-lobby-error))
        (assoc-in db [:lobby :error]
                  (lobby-error :no-player-colours-available
                               "No unused player colours are available."
                               {:known-ids (mapv :id pieces/players)}))))))

(defn remove-lobby-player [db slot-id]
  (if-not (lobby-active? db)
    db
    (-> db
        (update-in [:lobby :players]
                   (fn [players]
                     (vec (remove #(= slot-id (:slot-id %)) players))))
        refresh-lobby-error)))

(defn set-lobby-player-name [db slot-id name]
  (if-not (lobby-active? db)
    db
    (-> db
        (update-in [:lobby :players]
                   (fn [players]
                     (mapv (fn [player]
                             (if (= slot-id (:slot-id player))
                               (assoc player :name (str name))
                               player))
                           players)))
        refresh-lobby-error)))

(defn set-lobby-player-colour [db slot-id player-id]
  (let [player-id (if (string? player-id)
                    (keyword player-id)
                    player-id)
        players (lobby-players db)
        existing-player (some #(when (= slot-id (:slot-id %)) %) players)
        duplicate? (some #(and (not= slot-id (:slot-id %))
                               (= player-id (:id %)))
                         players)]
    (cond
      (not (lobby-active? db))
      db

      (nil? existing-player)
      (assoc-in db [:lobby :error]
                (lobby-error :unknown-lobby-player
                             "The selected lobby player no longer exists."
                             {:slot-id slot-id}))

      (not (known-player-id? player-id))
      (assoc-in db [:lobby :error]
                (lobby-error :unknown-player-colour
                             "Choose one of the known player colours."
                             {:player-id player-id
                              :known-ids (mapv :id pieces/players)}))

      duplicate?
      (assoc-in db [:lobby :error]
                (lobby-error :duplicate-player-colour
                             "Each player colour can only be used once."
                             {:player-id player-id}))

      :else
      (let [old-default-name (:name (player-colour (:id existing-player)))
            new-default-name (:name (player-colour player-id))]
        (-> db
            (update-in [:lobby :players]
                       (fn [players]
                         (mapv (fn [player]
                                 (if (= slot-id (:slot-id player))
                                   (cond-> (assoc player :id player-id)
                                     (= old-default-name (:name player))
                                     (assoc :name new-default-name))
                                   player))
                               players)))
            refresh-lobby-error)))))

(defn set-lobby-target-score [db target-score]
  (cond
    (not (lobby-active? db))
    db

    :else
    (if-let [target-score (normalize-target-score target-score)]
      (-> db
          (assoc-in [:lobby :start-options :game-options :target-score]
                    target-score)
          refresh-lobby-error)
      (assoc-in db [:lobby :error]
                (lobby-error :invalid-target-score
                             "Choose an 8, 9, or 10 point game."
                             {:target-score target-score
                              :allowed-target-scores target-score-options})))))

(defn- game-options-with-transition-shuffle [game-options transition-options]
  (let [game-options (or game-options {})
        shuffle-fn (:shuffle-fn transition-options)]
    (cond-> game-options
      (and shuffle-fn
           (not (contains? game-options :deck-order))
           (not (contains? game-options :shuffle-fn)))
      (assoc :shuffle-fn shuffle-fn))))

(defn start-lobby-game
  ([db] (start-lobby-game db {}))
  ([db transition-options]
   (if-not (lobby-active? db)
     db
     (let [db (refresh-lobby-error db)
           lobby (lobby db)
           validation-error (lobby-validation-error lobby)]
       (if validation-error
         (assoc-in db [:lobby :error] validation-error)
         (let [start-options (:start-options lobby)
               game-options (game-options-with-transition-shuffle
                             (:game-options start-options)
                             transition-options)]
           (initialize-game-db
            (assoc db :move-selection (empty-move-selection))
            start-options
            (lobby-player-specs lobby)
            game-options)))))))

(defn- lobby-colour-option [used-player-ids current-player-id colour]
  (let [player-id (:id colour)
        selected? (= current-player-id player-id)]
    (assoc (select-keys colour [:id :name :css-color])
           :selected? selected?
           :disabled? (and (not selected?)
                           (contains? used-player-ids player-id)))))

(defn lobby-view-model [{:keys [lobby]}]
  (let [players (:players lobby)
        local-controller (:local-controller lobby)
        target-score (or (get-in lobby [:start-options :game-options :target-score])
                         game-state/default-target-score)
        used-player-ids (set (map :id players))
        validation-error (when lobby
                           (lobby-validation-error lobby))
        error (or (:error lobby)
                  validation-error)]
    {:active? (some? lobby)
     :players (mapv (fn [player]
                      (let [colour (player-colour (:id player))]
                        (assoc player
                               :colour (select-keys colour [:id :name :css-color])
                               :colour-options (mapv #(lobby-colour-option
                                                       used-player-ids
                                                      (:id player)
                                                      %)
                                                     pieces/players))))
                    players)
     :local-controller local-controller
     :local-control? (some? local-controller)
     :target-score target-score
     :target-score-options target-score-options
     :available-colours (mapv #(assoc (select-keys % [:id :name :css-color])
                                      :available?
                                      (not (contains? used-player-ids (:id %))))
                              pieces/players)
     :player-count (count players)
     :min-players game-state/min-players
     :max-players game-state/max-players
     :can-add? (boolean
                (and (some? lobby)
                     (< (count players) game-state/max-players)
                     (some #(not (contains? used-player-ids (:id %)))
                           pieces/players)))
     :can-start? (boolean
                  (and (some? lobby)
                       (nil? validation-error)))
     :error error}))

(defn board [db]
  (get-in db [:game :board] []))

(defn board-pieces [db]
  (get-in db [:game :pieces :on-board] []))

(defn selected-board-index [db]
  (:selected-board-index db))

(defn board-cell-by-index [db index]
  (layout/cell-by-index (board db) index))

(defn selected-board-cell [db]
  (board-cell-by-index db (selected-board-index db)))

(defn selected-board-pieces [db]
  (pieces/pieces-for-space (board-pieces db) (selected-board-index db)))

(defn card-icon-mode [db]
  (normalize-card-icon-mode (:card-icon-mode db)))

(defn set-card-icon-mode [db mode]
  (assoc db :card-icon-mode (normalize-card-icon-mode mode)))

(defn toggle-card-icon-mode [db]
  (set-card-icon-mode db
                      (if (= :always (card-icon-mode db))
                        :popup
                        :always)))

(defn open-panels [db]
  (if (contains? db :open-panels)
    (normalize-open-panels (:open-panels db))
    default-open-panels))

(defn panel-open? [db panel-id]
  (contains? (open-panels db) panel-id))

(defn set-panel-open [db panel-id open?]
  (if (contains? panel-ids panel-id)
    (update db :open-panels
            (fn [open-panels]
              ((if open? conj disj)
               (normalize-open-panels (or open-panels default-open-panels))
               panel-id)))
    db))

(defn toggle-panel [db panel-id]
  (set-panel-open db panel-id (not (panel-open? db panel-id))))

(defn hotkey-help-open? [db]
  (true? (:hotkey-help-open? db)))

(defn set-hotkey-help-open [db open?]
  (assoc db :hotkey-help-open? (true? open?)))

(defn icon-help-open? [db]
  (true? (:icon-help-open? db)))

(defn set-icon-help-open [db open?]
  (assoc db :icon-help-open? (true? open?)))

(defn open-hotkey-help [db]
  (-> db
      (set-icon-help-open false)
      (set-hotkey-help-open true)))

(defn close-hotkey-help [db]
  (set-hotkey-help-open db false))

(defn open-icon-help [db]
  (-> db
      (set-hotkey-help-open false)
      (set-icon-help-open true)))

(defn close-icon-help [db]
  (set-icon-help-open db false))

(defn close-help-dialogs [db]
  (-> db
      close-hotkey-help
      close-icon-help))

(defn current-player [db]
  (some-> (game db) game-state/current-player))

(defn current-player-id [db]
  (get-in db [:game :turn :current-player-id]))

(defn game-status [db]
  (when-let [state (game db)]
    (let [scores (game-state/scores state)
          active-challenge-player-id (game-state/active-challenge-player-id state)]
      {:phase (:phase state)
       :finished? (game-state/finished? state)
       :winner (:winner state)
       :target-score (game-state/target-score state)
       :active-challenge-player-id active-challenge-player-id
       :players (mapv (fn [{:keys [id name eliminated? challenge]}]
                         {:id id
                          :name name
                          :score (get scores id 0)
                          :eliminated? (true? eliminated?)
                          :challenging? (= id active-challenge-player-id)
                          :challenge challenge})
                       (:players state))})))

(defn can-announce-challenge? [db]
  (if-let [player-id (current-player-id db)]
    (game-state/can-announce-challenge? (game db) player-id)
    false))

(defn current-player-hand [db]
  (vec (:hand (current-player db))))

(defn draw-pile [db]
  (vec (get-in db [:game :draw-pile] [])))

(defn discard-pile [db]
  (vec (get-in db [:game :discard-pile] [])))

(defn discard-top-card [db]
  (peek (discard-pile db)))

(defn three-runtime-status [db]
  (normalize-three-runtime-status (:three-runtime-status db)))

(defn set-three-runtime-status [db status]
  (assoc db :three-runtime-status (normalize-three-runtime-status status)))

(defn card-zones [db]
  {:hand (current-player-hand db)
   :draw-pile (draw-pile db)
   :discard-pile (discard-pile db)
   :draw-count (count (draw-pile db))
   :discard-count (count (discard-pile db))
   :discard-top-card (discard-top-card db)})

(defn board-view-model
  [{:keys [cells board-pieces selected-index card-icon-mode texture-errors
           renderer-error three-runtime-status]}]
  (let [wastelands (layout/wasteland-spaces cells)
        runtime-status (normalize-three-runtime-status three-runtime-status)]
    {:cells cells
     :empty? (empty? cells)
     :board-pieces board-pieces
     :pieces-by-space (pieces/pieces-by-space board-pieces)
     :wastelands wastelands
     :space-bounds (layout/space-bounds (concat cells wastelands))
     :selected-index selected-index
     :card-icon-mode card-icon-mode
     :texture-errors texture-errors
     :renderer-error renderer-error
     :three-runtime-status runtime-status
     :three-revision (:revision runtime-status)
     :three-renderer-available? (and (:ok? runtime-status)
                                     (not renderer-error))
     :three-renderer-message (if renderer-error
                               (str "Three.js WebGL rendering is unavailable; using the CSS board. "
                                    renderer-error)
                               (:message runtime-status))}))

(defn board-view [db]
  (board-view-model
   {:cells (board db)
    :board-pieces (board-pieces db)
    :selected-index (selected-board-index db)
    :card-icon-mode (card-icon-mode db)
    :texture-errors (:three-texture-errors db)
    :renderer-error (:three-renderer-error db)
    :three-runtime-status (three-runtime-status db)}))

(defn card-zones-view-model
  [{:keys [current-player card-icon-mode zones]}]
  {:current-player current-player
   :card-icon-mode card-icon-mode
   :zones zones})

(defn card-zones-view [db]
  (card-zones-view-model
   {:current-player (current-player db)
    :card-icon-mode (card-icon-mode db)
    :zones (card-zones db)}))

(defn territory-view-model
  [{:keys [cell selected-pieces]}]
  {:cell cell
   :selected-pieces selected-pieces
   :empty? (empty? selected-pieces)})

(defn territory-view [db]
  (territory-view-model
   {:cell (selected-board-cell db)
    :selected-pieces (selected-board-pieces db)}))

(def move-target-wasteland-options move-selection/move-target-wasteland-options)
(def turn-action-consumed? move-selection/turn-action-consumed?)
(def max-draw-count move-selection/max-draw-count)
(def draw-count-options move-selection/draw-count-options)
(def move-source-options move-selection/move-source-options)
(def move-selection move-selection/move-selection)
(def move-params move-selection/move-params)
(def move-power-options move-selection/move-power-options)
(def move-power move-selection/move-power)
(def move-world-copy-options move-selection/move-world-copy-options)
(def move-world-copied-power-options move-selection/move-world-copied-power-options)
(def move-world-copied-power move-selection/move-world-copied-power)
(def move-rod-mode-options move-selection/move-rod-mode-options)
(def move-disc-action-count-options move-selection/move-disc-action-count-options)
(def move-sword-action-count-options move-selection/move-sword-action-count-options)
(def move-devil-action-count-options move-selection/move-devil-action-count-options)
(def move-sun-disc-mode-options move-selection/move-sun-disc-mode-options)
(def move-fool-reveal-count-options move-selection/move-fool-reveal-count-options)
(def move-high-priestess-redraw-count-options move-selection/move-high-priestess-redraw-count-options)
(def move-high-priestess-redraw-options move-selection/move-high-priestess-redraw-options)
(def move-judgement-card-options move-selection/move-judgement-card-options)
(def move-judgement-card-maximum move-selection/move-judgement-card-maximum)
(def move-disc-minion-orientation-required? move-selection/move-disc-minion-orientation-required?)
(def move-disc-target-kind-options move-selection/move-disc-target-kind-options)
(def move-sword-target-kind-options move-selection/move-sword-target-kind-options)
(def move-distance-options move-selection/move-distance-options)
(def move-damage-options move-selection/move-damage-options)
(def move-target-piece-options move-selection/move-target-piece-options)
(def move-rod-orientation-required? move-selection/move-rod-orientation-required?)
(def move-disc-orientation-available? move-selection/move-disc-orientation-available?)
(def move-sun-disc-orientation-available? move-selection/move-sun-disc-orientation-available?)
(def move-sword-orientation-available? move-selection/move-sword-orientation-available?)
(def move-hermit-orientation-required? move-selection/move-hermit-orientation-required?)
(def move-ready? move-selection/move-ready?)
(def move-prompt move-selection/move-prompt)
(def select-move-source move-selection/select-move-source)
(def cancel-move move-selection/cancel-move)
(def select-board-for-active-move move-selection/select-board-for-active-move)

(defn select-board-card [db index]
  (if (board-cell-by-index db index)
    (select-board-for-active-move
     (-> db
         (assoc :selected-board-index index)
         (set-panel-open :territory true))
     index)
    db))

(defn- apply-end-turn-result [db result]
  (if (:ok? result)
    (-> db
        (assoc :game (:state result)
               :turn-result result
               :move-selection (assoc (empty-move-selection)
                                      :last-result result))
        (dissoc :turn-action))
    (assoc db :turn-result result)))

(defn end-turn
  ([db]
   (end-turn db {}))
  ([db command]
   (let [player-id (or (:player-id command)
                       (current-player-id db))
         result (if-let [state (game db)]
                  (game-state/end-turn
                   state
                   (assoc command :player-id player-id))
                  (game-state/failure :missing-game
                                      "Cannot end a turn before a game has started."
                                      {}))]
     (apply-end-turn-result db result))))

(def select-move-wasteland-target move-selection/select-move-wasteland-target)
(def select-move-piece move-selection/select-move-piece)
(def select-move-hand-card move-selection/select-move-hand-card)
(def select-move-power move-selection/select-move-power)
(def select-move-world-copy move-selection/select-move-world-copy)
(def select-move-rod-mode move-selection/select-move-rod-mode)
(def select-move-disc-target-kind move-selection/select-move-disc-target-kind)
(def select-move-sword-target-kind move-selection/select-move-sword-target-kind)
(def set-move-disc-action-count move-selection/set-move-disc-action-count)
(def set-move-sword-action-count move-selection/set-move-sword-action-count)
(def set-move-devil-action-count move-selection/set-move-devil-action-count)
(def set-move-fool-reveal-count move-selection/set-move-fool-reveal-count)
(def set-move-high-priestess-redraw-count move-selection/set-move-high-priestess-redraw-count)
(def toggle-move-high-priestess-discard-card move-selection/toggle-move-high-priestess-discard-card)
(def set-move-high-priestess-draw-count move-selection/set-move-high-priestess-draw-count)
(def toggle-move-judgement-card move-selection/toggle-move-judgement-card)
(def set-move-minion-orientation move-selection/set-move-minion-orientation)
(def select-move-sun-disc-mode move-selection/select-move-sun-disc-mode)
(def set-move-sun-disc-orientation move-selection/set-move-sun-disc-orientation)
(def select-move-target-piece move-selection/select-move-target-piece)
(def select-move-territory-card-source move-selection/select-move-territory-card-source)
(def select-move-one-point-card move-selection/select-move-one-point-card)
(def select-move-replacement-card move-selection/select-move-replacement-card)
(def set-move-orientation move-selection/set-move-orientation)
(def set-move-draw-count move-selection/set-move-draw-count)
(def toggle-move-discard-card move-selection/toggle-move-discard-card)
(def set-move-distance move-selection/set-move-distance)
(def set-move-damage move-selection/set-move-damage)
(def move-piece-options move-selection/move-piece-options)
(def move-hand-card-options move-selection/move-hand-card-options)
(def move-discard-card-options move-selection/move-discard-card-options)
(def move-source-board-options move-selection/move-source-board-options)
(def move-target-board-options move-selection/move-target-board-options)
(def move-one-point-card-options move-selection/move-one-point-card-options)
(def move-territory-card-source-options move-selection/move-territory-card-source-options)
(def move-replacement-card-options move-selection/move-replacement-card-options)
(def move-orientation-options move-selection/move-orientation-options)
(def move-command move-selection/move-command)

(defn move-panel-view-model
  [{:keys [current-player selection source-options prompt ready?
           board power power-options rod-mode-options disc-action-count-options
           world-copy-options world-copied-power-options world-copied-power
           sword-action-count-options devil-action-count-options
           sun-disc-mode-options fool-reveal-count-options
           high-priestess-redraw-count-options high-priestess-redraw-options
           judgement-card-options judgement-card-maximum
           disc-minion-orientation-required? disc-target-kind-options
           sword-target-kind-options piece-options
           target-piece-options hand-options discard-card-options source-board-options
           target-board-options target-wasteland-options
           territory-card-source-options one-point-card-options replacement-card-options
           orientation-options orientation-required? disc-orientation-available?
           sun-disc-orientation-available?
           sword-orientation-available? distance-options damage-options draw-options]}]
  {:current-player current-player
   :selection selection
   :source-options source-options
   :prompt prompt
   :ready? ready?
   :controls {:board board
              :power power
              :power-options power-options
              :world-copy-options world-copy-options
              :world-copied-power-options world-copied-power-options
              :world-copied-power world-copied-power
              :rod-mode-options rod-mode-options
              :disc-action-count-options disc-action-count-options
              :sword-action-count-options sword-action-count-options
              :devil-action-count-options devil-action-count-options
              :sun-disc-mode-options sun-disc-mode-options
              :fool-reveal-count-options fool-reveal-count-options
              :high-priestess-redraw-count-options high-priestess-redraw-count-options
              :high-priestess-redraw-options high-priestess-redraw-options
              :judgement-card-options judgement-card-options
              :judgement-card-maximum judgement-card-maximum
              :disc-minion-orientation-required? disc-minion-orientation-required?
              :disc-target-kind-options disc-target-kind-options
              :sword-target-kind-options sword-target-kind-options
              :piece-options piece-options
              :target-piece-options target-piece-options
              :hand-options hand-options
              :discard-card-options discard-card-options
              :source-board-options source-board-options
              :target-board-options target-board-options
              :target-wasteland-options target-wasteland-options
              :territory-card-source-options territory-card-source-options
              :one-point-card-options one-point-card-options
              :replacement-card-options replacement-card-options
              :orientation-options orientation-options
              :orientation-required? orientation-required?
              :disc-orientation-available? disc-orientation-available?
              :sun-disc-orientation-available? sun-disc-orientation-available?
              :sword-orientation-available? sword-orientation-available?
              :distance-options distance-options
              :damage-options damage-options
              :draw-options draw-options}})

(defn move-panel-view [db]
  (move-panel-view-model
   {:current-player (current-player db)
    :selection (move-selection db)
    :source-options (move-source-options db)
    :prompt (move-prompt db)
    :ready? (move-ready? db)
    :board (board db)
    :power (move-power db)
    :power-options (move-power-options db)
    :world-copy-options (move-world-copy-options db)
    :world-copied-power-options (move-world-copied-power-options db)
    :world-copied-power (move-world-copied-power db)
    :rod-mode-options (move-rod-mode-options db)
    :disc-action-count-options (move-disc-action-count-options db)
    :sword-action-count-options (move-sword-action-count-options db)
    :devil-action-count-options (move-devil-action-count-options db)
    :sun-disc-mode-options (move-sun-disc-mode-options db)
    :fool-reveal-count-options (move-fool-reveal-count-options db)
    :high-priestess-redraw-count-options (move-high-priestess-redraw-count-options db)
    :high-priestess-redraw-options (move-high-priestess-redraw-options db)
    :judgement-card-options (move-judgement-card-options db)
    :judgement-card-maximum (move-judgement-card-maximum db)
    :disc-minion-orientation-required? (move-disc-minion-orientation-required? db)
    :disc-target-kind-options (move-disc-target-kind-options db)
    :sword-target-kind-options (move-sword-target-kind-options db)
    :piece-options (move-piece-options db)
    :target-piece-options (move-target-piece-options db)
    :hand-options (move-hand-card-options db)
    :discard-card-options (move-discard-card-options db)
    :source-board-options (move-source-board-options db)
    :target-board-options (move-target-board-options db)
    :target-wasteland-options (move-target-wasteland-options db)
    :territory-card-source-options (move-territory-card-source-options db)
    :one-point-card-options (move-one-point-card-options db)
    :replacement-card-options (move-replacement-card-options db)
    :orientation-options (move-orientation-options db)
    :orientation-required? (or (move-rod-orientation-required? db)
                               (move-hermit-orientation-required? db))
    :disc-orientation-available? (move-disc-orientation-available? db)
    :sun-disc-orientation-available? (move-sun-disc-orientation-available? db)
    :sword-orientation-available? (move-sword-orientation-available? db)
    :distance-options (move-distance-options db)
    :damage-options (move-damage-options db)
    :draw-options (draw-count-options db)}))

(defn header-view-model
  [{:keys [current-player card-icon-mode open-panels lobby?
           game-status can-announce-challenge?]}]
  {:current-player current-player
   :card-icon-mode card-icon-mode
   :open-panels (normalize-open-panels open-panels)
   :lobby? (true? lobby?)
   :game-status game-status
   :can-end-turn? (boolean (and current-player
                                (not lobby?)
                                (not (:finished? game-status))))
   :can-announce-challenge? (boolean can-announce-challenge?)})

(defn header-view [db]
  (header-view-model
   {:current-player (current-player db)
    :card-icon-mode (card-icon-mode db)
    :open-panels (open-panels db)
    :lobby? (lobby-active? db)
    :game-status (game-status db)
    :can-announce-challenge? (can-announce-challenge? db)}))

(defn lobby-view [db]
  (lobby-view-model {:lobby (lobby db)}))

(defn help-dialogs-view-model
  [{:keys [hotkey-help-open? icon-help-open?]}]
  {:hotkey-help-open? (true? hotkey-help-open?)
   :icon-help-open? (true? icon-help-open?)})

(defn help-dialogs-view [db]
  (help-dialogs-view-model
   {:hotkey-help-open? (hotkey-help-open? db)
    :icon-help-open? (icon-help-open? db)}))

(defn app-view-model
  [{:keys [setup-error card-icon-mode open-panels lobby?]}]
  {:setup-error setup-error
   :card-icon-mode card-icon-mode
   :open-panels (normalize-open-panels open-panels)
   :lobby? (true? lobby?)})

(defn app-view [db]
  (app-view-model
   {:setup-error (setup-error db)
    :card-icon-mode (card-icon-mode db)
    :open-panels (open-panels db)
    :lobby? (lobby-active? db)}))

(defn confirm-move
  ([db] (move-selection/confirm-move db))
  ([db transition-options]
   (move-selection/confirm-move db transition-options)))
