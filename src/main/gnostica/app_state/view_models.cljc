(ns gnostica.app-state.view-models
  (:require [gnostica.app-state.db :as db]
            [gnostica.app-state.lobby :as lobby]
            [gnostica.board-layout :as layout]
            [gnostica.game-state :as game-state]
            [gnostica.move-selection :as move-selection]
            [gnostica.pieces :as pieces]))

(defn board [app-db]
  (get-in app-db [:game :board] []))

(defn board-pieces [app-db]
  (get-in app-db [:game :pieces :on-board] []))

(defn selected-board-index [app-db]
  (:selected-board-index app-db))

(defn board-cell-by-index [app-db index]
  (layout/cell-by-index (board app-db) index))

(defn selected-board-cell [app-db]
  (board-cell-by-index app-db (selected-board-index app-db)))

(defn selected-board-pieces [app-db]
  (pieces/pieces-for-space (board-pieces app-db) (selected-board-index app-db)))

(defn card-icon-mode [app-db]
  (db/normalize-card-icon-mode (:card-icon-mode app-db)))

(defn set-card-icon-mode [app-db mode]
  (assoc app-db :card-icon-mode (db/normalize-card-icon-mode mode)))

(defn toggle-card-icon-mode [app-db]
  (set-card-icon-mode app-db
                      (if (= :always (card-icon-mode app-db))
                        :popup
                        :always)))

(defn open-panels [app-db]
  (if (contains? app-db :open-panels)
    (db/normalize-open-panels (:open-panels app-db))
    db/default-open-panels))

(defn panel-open? [app-db panel-id]
  (contains? (open-panels app-db) panel-id))

(defn set-panel-open [app-db panel-id open?]
  (if (contains? db/panel-ids panel-id)
    (update app-db :open-panels
            (fn [open-panels]
              ((if open? conj disj)
               (db/normalize-open-panels (or open-panels db/default-open-panels))
               panel-id)))
    app-db))

(defn toggle-panel [app-db panel-id]
  (set-panel-open app-db panel-id (not (panel-open? app-db panel-id))))

(defn hotkey-help-open? [app-db]
  (true? (:hotkey-help-open? app-db)))

(defn set-hotkey-help-open [app-db open?]
  (assoc app-db :hotkey-help-open? (true? open?)))

(defn icon-help-open? [app-db]
  (true? (:icon-help-open? app-db)))

(defn set-icon-help-open [app-db open?]
  (assoc app-db :icon-help-open? (true? open?)))

(defn open-hotkey-help [app-db]
  (-> app-db
      (set-icon-help-open false)
      (set-hotkey-help-open true)))

(defn close-hotkey-help [app-db]
  (set-hotkey-help-open app-db false))

(defn open-icon-help [app-db]
  (-> app-db
      (set-hotkey-help-open false)
      (set-icon-help-open true)))

(defn close-icon-help [app-db]
  (set-icon-help-open app-db false))

(defn close-help-dialogs [app-db]
  (-> app-db
      close-hotkey-help
      close-icon-help))

(defn current-player [app-db]
  (some-> (db/game app-db) game-state/current-player))

(defn current-player-id [app-db]
  (get-in app-db [:game :turn :current-player-id]))

(defn game-status [app-db]
  (when-let [state (db/game app-db)]
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

(defn can-announce-challenge? [app-db]
  (if-let [player-id (current-player-id app-db)]
    (game-state/can-announce-challenge? (db/game app-db) player-id)
    false))

(defn can-end-turn? [app-db]
  (if-let [player-id (current-player-id app-db)]
    (game-state/can-end-turn? (db/game app-db) player-id)
    false))

(defn current-player-hand [app-db]
  (vec (:hand (current-player app-db))))

(defn draw-pile [app-db]
  (vec (get-in app-db [:game :draw-pile] [])))

(defn discard-pile [app-db]
  (vec (get-in app-db [:game :discard-pile] [])))

(defn discard-top-card [app-db]
  (peek (discard-pile app-db)))

(defn three-runtime-status [app-db]
  (db/normalize-three-runtime-status (:three-runtime-status app-db)))

(defn set-three-runtime-status [app-db status]
  (assoc app-db :three-runtime-status (db/normalize-three-runtime-status status)))

(defn direct-manipulation [app-db]
  (db/normalize-direct-manipulation
   {:direct-manipulation (:direct-manipulation app-db)}))

(defn set-detailed-entry-default [app-db enabled?]
  (let [current-settings (direct-manipulation app-db)
        settings (assoc current-settings
                        :detailed-entry-default?
                        (and (:detailed-entry-available? current-settings)
                             (true? enabled?)))]
    (cond-> (assoc app-db :direct-manipulation settings)
      (:detailed-entry-default? settings)
      (set-panel-open :move true))))

(defn card-zones [app-db]
  {:hand (current-player-hand app-db)
   :draw-pile (draw-pile app-db)
   :discard-pile (discard-pile app-db)
   :draw-count (count (draw-pile app-db))
   :discard-count (count (discard-pile app-db))
   :discard-top-card (discard-top-card app-db)})

(defn board-view-model
  [{:keys [cells board-pieces selected-index card-icon-mode texture-errors
           renderer-error three-runtime-status legal-targets move-preview
           direct-manipulation]}]
  (let [wastelands (layout/wasteland-spaces cells)
        runtime-status (db/normalize-three-runtime-status three-runtime-status)]
    {:cells cells
     :empty? (empty? cells)
     :board-pieces board-pieces
     :pieces-by-space (pieces/pieces-by-space board-pieces)
     :wastelands wastelands
     :space-bounds (layout/space-bounds (concat cells wastelands))
     :selected-index selected-index
     :legal-targets legal-targets
     :move-preview move-preview
     :card-icon-mode card-icon-mode
     :texture-errors texture-errors
     :renderer-error renderer-error
     :three-runtime-status runtime-status
     :direct-manipulation (db/normalize-direct-manipulation
                           {:direct-manipulation direct-manipulation})
     :three-revision (:revision runtime-status)
     :three-renderer-available? (and (:ok? runtime-status)
                                     (not renderer-error))
     :three-renderer-message (if renderer-error
                               (str "Three.js WebGL rendering is unavailable; using the CSS board. "
                                    renderer-error)
                               (:message runtime-status))}))

(defn board-view [app-db]
  (board-view-model
   {:cells (board app-db)
    :board-pieces (board-pieces app-db)
    :selected-index (selected-board-index app-db)
    :legal-targets (move-selection/move-legal-targets app-db)
    :move-preview (move-selection/move-preview app-db)
    :card-icon-mode (card-icon-mode app-db)
    :texture-errors (:three-texture-errors app-db)
    :renderer-error (:three-renderer-error app-db)
    :three-runtime-status (three-runtime-status app-db)
    :direct-manipulation (direct-manipulation app-db)}))

(defn card-zones-view-model
  [{:keys [current-player card-icon-mode zones legal-targets direct-manipulation]}]
  {:current-player current-player
   :card-icon-mode card-icon-mode
   :zones zones
   :legal-targets legal-targets
   :direct-manipulation (db/normalize-direct-manipulation
                         {:direct-manipulation direct-manipulation})})

(defn card-zones-view [app-db]
  (card-zones-view-model
   {:current-player (current-player app-db)
    :card-icon-mode (card-icon-mode app-db)
    :zones (card-zones app-db)
    :legal-targets (move-selection/move-legal-targets app-db)
    :direct-manipulation (direct-manipulation app-db)}))

(defn territory-view-model
  [{:keys [cell selected-pieces]}]
  {:cell cell
   :selected-pieces selected-pieces
   :empty? (empty? selected-pieces)})

(defn territory-view [app-db]
  (territory-view-model
   {:cell (selected-board-cell app-db)
    :selected-pieces (selected-board-pieces app-db)}))

(defn- selected-piece-target-summary [legal-targets piece-id]
  (when piece-id
    (some #(when (= piece-id (:piece-id %))
             (select-keys % [:piece-id :player-id :space-index :space
                             :size :orientation :piece :label]))
          (:pieces legal-targets))))

(defn move-panel-view-model
  [{:keys [current-player selection source-options prompt ready? control-groups
           action-ribbon direct-manipulation
           board power power-options rod-mode-options disc-action-count-options
           major-action-count-options major-action-count
           world-copy-options world-copied-power-options world-copied-power
           sword-action-count-options devil-action-count-options
           sun-disc-mode-options fool-reveal-count-options fool-reveal-state
           fool-play-power-options fool-play-power
           high-priestess-redraw-count-options high-priestess-redraw-options
           judgement-card-options judgement-card-maximum
           disc-minion-orientation-required? disc-target-kind-options
           sword-target-kind-options piece-options
           target-piece-options hand-options discard-card-options source-board-options
           target-board-options target-wasteland-options
           territory-card-source-options one-point-card-options replacement-card-options
           orientation-options orientation-required? disc-orientation-available?
           sun-disc-orientation-available?
           sword-orientation-available? distance-options damage-options draw-options
           legal-targets]}]
  (let [params (:params selection)
        sun-cup-target-piece (selected-piece-target-summary
                              legal-targets
                              (:target-piece-id params))
        sun-disc-target-piece (selected-piece-target-summary
                               legal-targets
                               (:sun-disc-target-piece-id params))]
    {:current-player current-player
     :selection selection
     :source-options source-options
     :prompt prompt
     :ready? ready?
     :control-groups control-groups
     :action-ribbon action-ribbon
     :direct-manipulation (db/normalize-direct-manipulation
                           {:direct-manipulation direct-manipulation})
     :controls {:board board
                :power power
                :power-options power-options
                :world-copy-options world-copy-options
                :world-copied-power-options world-copied-power-options
                :world-copied-power world-copied-power
                :rod-mode-options rod-mode-options
                :disc-action-count-options disc-action-count-options
                :major-action-count-options major-action-count-options
                :major-action-count major-action-count
                :sword-action-count-options sword-action-count-options
                :devil-action-count-options devil-action-count-options
                :sun-disc-mode-options sun-disc-mode-options
                :sun-cup-target-piece sun-cup-target-piece
                :sun-disc-target-piece sun-disc-target-piece
                :fool-reveal-count-options fool-reveal-count-options
                :fool-reveal-state fool-reveal-state
                :fool-play-power-options fool-play-power-options
                :fool-play-power fool-play-power
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
                :legal-targets legal-targets
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
                :draw-options draw-options}}))

(defn move-panel-view [app-db]
  (move-panel-view-model
   {:current-player (current-player app-db)
    :selection (move-selection/move-selection app-db)
    :source-options (move-selection/move-source-options app-db)
    :prompt (move-selection/move-prompt app-db)
    :ready? (move-selection/move-ready? app-db)
    :control-groups (move-selection/move-control-groups app-db)
    :action-ribbon (move-selection/move-action-ribbon app-db)
    :direct-manipulation (direct-manipulation app-db)
    :board (board app-db)
    :power (move-selection/move-power app-db)
    :power-options (move-selection/move-power-options app-db)
    :world-copy-options (move-selection/move-world-copy-options app-db)
    :world-copied-power-options
    (move-selection/move-world-copied-power-options app-db)
    :world-copied-power (move-selection/move-world-copied-power app-db)
    :rod-mode-options (move-selection/move-rod-mode-options app-db)
    :disc-action-count-options
    (move-selection/move-disc-action-count-options app-db)
    :major-action-count-options
    (move-selection/move-major-action-count-options app-db)
    :major-action-count (move-selection/move-major-action-count app-db)
    :sword-action-count-options
    (move-selection/move-sword-action-count-options app-db)
    :devil-action-count-options
    (move-selection/move-devil-action-count-options app-db)
    :sun-disc-mode-options (move-selection/move-sun-disc-mode-options app-db)
    :fool-reveal-count-options
    (move-selection/move-fool-reveal-count-options app-db)
    :fool-reveal-state (move-selection/move-fool-reveal-state app-db)
    :fool-play-power-options
    (move-selection/move-fool-play-power-options app-db)
    :fool-play-power (move-selection/move-fool-play-power app-db)
    :high-priestess-redraw-count-options
    (move-selection/move-high-priestess-redraw-count-options app-db)
    :high-priestess-redraw-options
    (move-selection/move-high-priestess-redraw-options app-db)
    :judgement-card-options (move-selection/move-judgement-card-options app-db)
    :judgement-card-maximum (move-selection/move-judgement-card-maximum app-db)
    :disc-minion-orientation-required?
    (move-selection/move-disc-minion-orientation-required? app-db)
    :disc-target-kind-options
    (move-selection/move-disc-target-kind-options app-db)
    :sword-target-kind-options
    (move-selection/move-sword-target-kind-options app-db)
    :piece-options (move-selection/move-piece-options app-db)
    :target-piece-options (move-selection/move-target-piece-options app-db)
    :hand-options (move-selection/move-hand-card-options app-db)
    :discard-card-options (move-selection/move-discard-card-options app-db)
    :source-board-options (move-selection/move-source-board-options app-db)
    :target-board-options (move-selection/move-target-board-options app-db)
    :target-wasteland-options
    (move-selection/move-target-wasteland-options app-db)
    :legal-targets (move-selection/move-legal-targets app-db)
    :territory-card-source-options
    (move-selection/move-territory-card-source-options app-db)
    :one-point-card-options (move-selection/move-one-point-card-options app-db)
    :replacement-card-options
    (move-selection/move-replacement-card-options app-db)
    :orientation-options (move-selection/move-orientation-options app-db)
    :orientation-required? (or (move-selection/move-rod-orientation-required?
                                app-db)
                               (move-selection/move-hermit-orientation-required?
                                app-db))
    :disc-orientation-available?
    (move-selection/move-disc-orientation-available? app-db)
    :sun-disc-orientation-available?
    (move-selection/move-sun-disc-orientation-available? app-db)
    :sword-orientation-available?
    (move-selection/move-sword-orientation-available? app-db)
    :distance-options (move-selection/move-distance-options app-db)
    :damage-options (move-selection/move-damage-options app-db)
    :draw-options (move-selection/draw-count-options app-db)}))

(defn header-view-model
  [{:keys [current-player card-icon-mode open-panels lobby?
           game-status can-end-turn? can-announce-challenge?]}]
  (let [lobby-active? (true? lobby?)]
    {:current-player current-player
     :card-icon-mode card-icon-mode
     :open-panels (db/normalize-open-panels open-panels)
     :lobby? lobby-active?
     :game-status game-status
     :show-turn-actions? (boolean (and current-player
                                       game-status
                                       (not lobby-active?)
                                       (not (:finished? game-status))))
     :can-end-turn? (boolean can-end-turn?)
     :can-announce-challenge? (boolean can-announce-challenge?)}))

(defn header-view [app-db]
  (header-view-model
   {:current-player (current-player app-db)
    :card-icon-mode (card-icon-mode app-db)
    :open-panels (open-panels app-db)
    :lobby? (lobby/lobby-active? app-db)
    :game-status (game-status app-db)
    :can-end-turn? (can-end-turn? app-db)
    :can-announce-challenge? (can-announce-challenge? app-db)}))

(defn lobby-view [app-db]
  (lobby/lobby-view app-db))

(defn help-dialogs-view-model
  [{:keys [hotkey-help-open? icon-help-open?]}]
  {:hotkey-help-open? (true? hotkey-help-open?)
   :icon-help-open? (true? icon-help-open?)})

(defn help-dialogs-view [app-db]
  (help-dialogs-view-model
   {:hotkey-help-open? (hotkey-help-open? app-db)
    :icon-help-open? (icon-help-open? app-db)}))

(defn app-view-model
  [{:keys [setup-error card-icon-mode open-panels lobby?]}]
  {:setup-error setup-error
   :card-icon-mode card-icon-mode
   :open-panels (db/normalize-open-panels open-panels)
   :lobby? (true? lobby?)})

(defn app-view [app-db]
  (app-view-model
   {:setup-error (db/setup-error app-db)
    :card-icon-mode (card-icon-mode app-db)
    :open-panels (open-panels app-db)
    :lobby? (lobby/lobby-active? app-db)}))
