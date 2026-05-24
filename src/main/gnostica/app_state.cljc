(ns gnostica.app-state
  (:require [gnostica.game-state :as game-state]
            [gnostica.move-selection :as move-selection]
            [gnostica.pieces :as pieces]))

(def default-player-specs
  (mapv #(select-keys % [:id :name]) pieces/players))

(def default-selected-board-index 0)

(def default-card-icon-mode :always)

(def default-hotkey-help-open? false)

(def default-icon-help-open? false)

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

(defn initialize
  ([] (initialize {}))
  ([{:keys [player-specs game-options selected-board-index card-icon-mode]
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

(defn card-zones [db]
  {:hand (current-player-hand db)
   :draw-pile (draw-pile db)
   :discard-pile (discard-pile db)
   :draw-count (count (draw-pile db))
   :discard-count (count (discard-pile db))
   :discard-top-card (discard-top-card db)})

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
(def confirm-move move-selection/confirm-move)
