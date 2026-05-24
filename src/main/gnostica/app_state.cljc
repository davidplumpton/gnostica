(ns gnostica.app-state
  (:require [gnostica.board-layout :as layout]
            [gnostica.game-state :as game-state]
            [gnostica.move-selection :as move-selection]
            [gnostica.pieces :as pieces]))

(def default-player-specs
  (mapv #(select-keys % [:id :name]) pieces/players))

(def default-selected-board-index 0)

(def default-card-icon-mode :always)

(def default-hotkey-help-open? false)

(def default-icon-help-open? false)

(def default-three-runtime-status
  {:ok? false
   :code :three-unchecked
   :revision nil
   :expected-revision nil
   :message "Three.js runtime status has not been checked yet."})

(def card-icon-modes
  #{:always :popup})

(def empty-move-selection move-selection/empty-move-selection)

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

(defn initialize
  ([] (initialize {}))
  ([{:keys [player-specs game-options selected-board-index card-icon-mode
            three-runtime-status]
     :as opts
     :or {player-specs default-player-specs
          game-options {}
          selected-board-index default-selected-board-index
          card-icon-mode default-card-icon-mode}}]
   (let [result (game-state/create-game player-specs game-options)
         base-db {:selected-board-index selected-board-index
                  :card-icon-mode (normalize-card-icon-mode card-icon-mode)
                  :hotkey-help-open? default-hotkey-help-open?
                  :icon-help-open? default-icon-help-open?
                  :move-selection (empty-move-selection)
                  :three-runtime-status (normalize-three-runtime-status three-runtime-status)
                  :three-texture-errors []}]
     (if (:ok? result)
       (assoc base-db :game (state-with-demo-board-pieces (:state result) opts))
       (assoc base-db :setup-error (:error result))))))

(defn game [db]
  (:game db))

(defn setup-error [db]
  (:setup-error db))

(defn board [db]
  (get-in db [:game :board] []))

(defn board-pieces [db]
  (get-in db [:game :pieces :on-board] []))

(defn selected-board-index [db]
  (:selected-board-index db))

(defn selected-board-cell [db]
  (get (board db) (selected-board-index db)))

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
(def max-draw-count move-selection/max-draw-count)
(def draw-count-options move-selection/draw-count-options)
(def move-source-options move-selection/move-source-options)
(def move-selection move-selection/move-selection)
(def move-params move-selection/move-params)
(def move-power-options move-selection/move-power-options)
(def move-power move-selection/move-power)
(def move-rod-mode-options move-selection/move-rod-mode-options)
(def move-distance-options move-selection/move-distance-options)
(def move-target-piece-options move-selection/move-target-piece-options)
(def move-rod-orientation-required? move-selection/move-rod-orientation-required?)
(def move-ready? move-selection/move-ready?)
(def move-prompt move-selection/move-prompt)
(def select-move-source move-selection/select-move-source)
(def cancel-move move-selection/cancel-move)
(def select-board-for-active-move move-selection/select-board-for-active-move)

(defn select-board-card [db index]
  (if (contains? (board db) index)
    (select-board-for-active-move (assoc db :selected-board-index index) index)
    db))

(def select-move-wasteland-target move-selection/select-move-wasteland-target)
(def select-move-piece move-selection/select-move-piece)
(def select-move-hand-card move-selection/select-move-hand-card)
(def select-move-power move-selection/select-move-power)
(def select-move-rod-mode move-selection/select-move-rod-mode)
(def select-move-target-piece move-selection/select-move-target-piece)
(def select-move-territory-card-source move-selection/select-move-territory-card-source)
(def select-move-one-point-card move-selection/select-move-one-point-card)
(def set-move-orientation move-selection/set-move-orientation)
(def set-move-draw-count move-selection/set-move-draw-count)
(def set-move-distance move-selection/set-move-distance)
(def move-piece-options move-selection/move-piece-options)
(def move-hand-card-options move-selection/move-hand-card-options)
(def move-source-board-options move-selection/move-source-board-options)
(def move-target-board-options move-selection/move-target-board-options)
(def move-one-point-card-options move-selection/move-one-point-card-options)
(def move-territory-card-source-options move-selection/move-territory-card-source-options)
(def move-orientation-options move-selection/move-orientation-options)
(def move-command move-selection/move-command)

(defn move-panel-view-model
  [{:keys [current-player selection source-options prompt ready?
           board power power-options rod-mode-options piece-options
           target-piece-options hand-options source-board-options
           target-board-options target-wasteland-options
           territory-card-source-options one-point-card-options
           orientation-options orientation-required? distance-options
           draw-options]}]
  {:current-player current-player
   :selection selection
   :source-options source-options
   :prompt prompt
   :ready? ready?
   :controls {:board board
              :power power
              :power-options power-options
              :rod-mode-options rod-mode-options
              :piece-options piece-options
              :target-piece-options target-piece-options
              :hand-options hand-options
              :source-board-options source-board-options
              :target-board-options target-board-options
              :target-wasteland-options target-wasteland-options
              :territory-card-source-options territory-card-source-options
              :one-point-card-options one-point-card-options
              :orientation-options orientation-options
              :orientation-required? orientation-required?
              :distance-options distance-options
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
    :rod-mode-options (move-rod-mode-options db)
    :piece-options (move-piece-options db)
    :target-piece-options (move-target-piece-options db)
    :hand-options (move-hand-card-options db)
    :source-board-options (move-source-board-options db)
    :target-board-options (move-target-board-options db)
    :target-wasteland-options (move-target-wasteland-options db)
    :territory-card-source-options (move-territory-card-source-options db)
    :one-point-card-options (move-one-point-card-options db)
    :orientation-options (move-orientation-options db)
    :orientation-required? (move-rod-orientation-required? db)
    :distance-options (move-distance-options db)
    :draw-options (draw-count-options db)}))

(defn header-view-model [{:keys [current-player card-icon-mode]}]
  {:current-player current-player
   :card-icon-mode card-icon-mode})

(defn header-view [db]
  (header-view-model
   {:current-player (current-player db)
    :card-icon-mode (card-icon-mode db)}))

(defn help-dialogs-view-model
  [{:keys [hotkey-help-open? icon-help-open?]}]
  {:hotkey-help-open? (true? hotkey-help-open?)
   :icon-help-open? (true? icon-help-open?)})

(defn help-dialogs-view [db]
  (help-dialogs-view-model
   {:hotkey-help-open? (hotkey-help-open? db)
    :icon-help-open? (icon-help-open? db)}))

(defn app-view-model
  [{:keys [setup-error card-icon-mode]}]
  {:setup-error setup-error
   :card-icon-mode card-icon-mode})

(defn app-view [db]
  (app-view-model
   {:setup-error (setup-error db)
    :card-icon-mode (card-icon-mode db)}))

(defn confirm-move
  ([db] (move-selection/confirm-move db))
  ([db transition-options]
   (move-selection/confirm-move db transition-options)))
