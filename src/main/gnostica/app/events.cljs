(ns gnostica.app.events
  (:require [gnostica.app-state :as app-state]
            [re-frame.core :as rf]))

(def initialize :gnostica.app/initialize)
(def select-board-card :gnostica.app/select-board-card)
(def select-move-source :gnostica.app/select-move-source)
(def select-move-piece :gnostica.app/select-move-piece)
(def select-move-hand-card :gnostica.app/select-move-hand-card)
(def select-move-wasteland-target :gnostica.app/select-move-wasteland-target)
(def select-move-one-point-card :gnostica.app/select-move-one-point-card)
(def select-move-territory-card-source :gnostica.app/select-move-territory-card-source)
(def select-move-power :gnostica.app/select-move-power)
(def select-move-rod-mode :gnostica.app/select-move-rod-mode)
(def select-move-target-piece :gnostica.app/select-move-target-piece)
(def set-move-orientation :gnostica.app/set-move-orientation)
(def set-move-distance :gnostica.app/set-move-distance)
(def set-move-draw-count :gnostica.app/set-move-draw-count)
(def confirm-move :gnostica.app/confirm-move)
(def cancel-move :gnostica.app/cancel-move)
(def toggle-card-icon-mode :gnostica.app/toggle-card-icon-mode)
(def open-hotkey-help :gnostica.app/open-hotkey-help)
(def close-hotkey-help :gnostica.app/close-hotkey-help)
(def open-icon-help :gnostica.app/open-icon-help)
(def close-icon-help :gnostica.app/close-icon-help)
(def close-help-dialogs :gnostica.app/close-help-dialogs)
(def clear-three-texture-errors :gnostica.app/clear-three-texture-errors)
(def three-texture-error :gnostica.app/three-texture-error)
(def three-renderer-error :gnostica.app/three-renderer-error)

(def game :gnostica.app/game)
(def setup-error :gnostica.app/setup-error)
(def board :gnostica.app/board)
(def pieces :gnostica.app/pieces)
(def selected-board-index :gnostica.app/selected-board-index)
(def current-player :gnostica.app/current-player)
(def card-zones :gnostica.app/card-zones)
(def three-texture-errors :gnostica.app/three-texture-errors)
(def selected-board-cell :gnostica.app/selected-board-cell)
(def selected-board-pieces :gnostica.app/selected-board-pieces)
(def move-selection :gnostica.app/move-selection)
(def move-source-options :gnostica.app/move-source-options)
(def move-prompt :gnostica.app/move-prompt)
(def move-ready? :gnostica.app/move-ready?)
(def move-piece-options :gnostica.app/move-piece-options)
(def move-hand-card-options :gnostica.app/move-hand-card-options)
(def move-source-board-options :gnostica.app/move-source-board-options)
(def move-target-board-options :gnostica.app/move-target-board-options)
(def move-target-wasteland-options :gnostica.app/move-target-wasteland-options)
(def move-one-point-card-options :gnostica.app/move-one-point-card-options)
(def move-territory-card-source-options :gnostica.app/move-territory-card-source-options)
(def move-power-options :gnostica.app/move-power-options)
(def move-power :gnostica.app/move-power)
(def move-rod-mode-options :gnostica.app/move-rod-mode-options)
(def move-target-piece-options :gnostica.app/move-target-piece-options)
(def move-distance-options :gnostica.app/move-distance-options)
(def move-rod-orientation-required? :gnostica.app/move-rod-orientation-required?)
(def move-orientation-options :gnostica.app/move-orientation-options)
(def draw-count-options :gnostica.app/draw-count-options)
(def card-icon-mode :gnostica.app/card-icon-mode)
(def hotkey-help-open? :gnostica.app/hotkey-help-open?)
(def icon-help-open? :gnostica.app/icon-help-open?)

(rf/reg-event-db
 initialize
 (fn [_ [_ opts]]
   (app-state/initialize opts)))

(rf/reg-event-db
 select-board-card
 (fn [db [_ index]]
   (app-state/select-board-card db index)))

(rf/reg-event-db
 select-move-source
 (fn [db [_ source-id]]
   (app-state/select-move-source db source-id)))

(rf/reg-event-db
 select-move-piece
 (fn [db [_ piece-id]]
   (app-state/select-move-piece db piece-id)))

(rf/reg-event-db
 select-move-hand-card
 (fn [db [_ card-id]]
   (app-state/select-move-hand-card db card-id)))

(rf/reg-event-db
 select-move-wasteland-target
 (fn [db [_ row col]]
   (app-state/select-move-wasteland-target db row col)))

(rf/reg-event-db
 select-move-one-point-card
 (fn [db [_ card-id]]
   (app-state/select-move-one-point-card db card-id)))

(rf/reg-event-db
 select-move-territory-card-source
 (fn [db [_ territory-card-source]]
   (app-state/select-move-territory-card-source db territory-card-source)))

(rf/reg-event-db
 select-move-power
 (fn [db [_ power]]
   (app-state/select-move-power db power)))

(rf/reg-event-db
 select-move-rod-mode
 (fn [db [_ mode]]
   (app-state/select-move-rod-mode db mode)))

(rf/reg-event-db
 select-move-target-piece
 (fn [db [_ piece-id]]
   (app-state/select-move-target-piece db piece-id)))

(rf/reg-event-db
 set-move-orientation
 (fn [db [_ orientation]]
   (app-state/set-move-orientation db orientation)))

(rf/reg-event-db
 set-move-distance
 (fn [db [_ distance]]
   (app-state/set-move-distance db distance)))

(rf/reg-event-db
 set-move-draw-count
 (fn [db [_ draw-count]]
   (app-state/set-move-draw-count db draw-count)))

(rf/reg-event-db
 confirm-move
 (fn [db _]
   (app-state/confirm-move db)))

(rf/reg-event-db
 cancel-move
 (fn [db _]
   (app-state/cancel-move db)))

(rf/reg-event-db
 toggle-card-icon-mode
 (fn [db _]
   (app-state/toggle-card-icon-mode db)))

(rf/reg-event-db
 open-hotkey-help
 (fn [db _]
   (app-state/open-hotkey-help db)))

(rf/reg-event-db
 close-hotkey-help
 (fn [db _]
   (app-state/close-hotkey-help db)))

(rf/reg-event-db
 open-icon-help
 (fn [db _]
   (app-state/open-icon-help db)))

(rf/reg-event-db
 close-icon-help
 (fn [db _]
   (app-state/close-icon-help db)))

(rf/reg-event-db
 close-help-dialogs
 (fn [db _]
   (app-state/close-help-dialogs db)))

(rf/reg-event-db
 clear-three-texture-errors
 (fn [db _]
   (assoc db :three-texture-errors [])))

(rf/reg-event-db
 three-texture-error
 (fn [db [_ image]]
   (update db :three-texture-errors (fnil conj []) image)))

(rf/reg-event-db
 three-renderer-error
 (fn [db [_ message]]
   (assoc db :three-renderer-error message)))

(rf/reg-sub
 game
 (fn [db _]
   (app-state/game db)))

(rf/reg-sub
 setup-error
 (fn [db _]
   (app-state/setup-error db)))

(rf/reg-sub
 board
 (fn [db _]
   (app-state/board db)))

(rf/reg-sub
 pieces
 (fn [db _]
   (app-state/board-pieces db)))

(rf/reg-sub
 selected-board-index
 (fn [db _]
   (app-state/selected-board-index db)))

(rf/reg-sub
 current-player
 (fn [db _]
   (app-state/current-player db)))

(rf/reg-sub
 card-zones
 (fn [db _]
   (app-state/card-zones db)))

(rf/reg-sub
 three-texture-errors
 (fn [db _]
   (:three-texture-errors db)))

(rf/reg-sub
 three-renderer-error
 (fn [db _]
   (:three-renderer-error db)))

(rf/reg-sub
 selected-board-cell
 (fn [db _]
   (app-state/selected-board-cell db)))

(rf/reg-sub
 selected-board-pieces
 (fn [db _]
   (app-state/selected-board-pieces db)))

(rf/reg-sub
 move-selection
 (fn [db _]
   (app-state/move-selection db)))

(rf/reg-sub
 move-source-options
 (fn [db _]
   (app-state/move-source-options db)))

(rf/reg-sub
 move-prompt
 (fn [db _]
   (app-state/move-prompt db)))

(rf/reg-sub
 move-ready?
 (fn [db _]
   (app-state/move-ready? db)))

(rf/reg-sub
 move-piece-options
 (fn [db _]
   (app-state/move-piece-options db)))

(rf/reg-sub
 move-hand-card-options
 (fn [db _]
   (app-state/move-hand-card-options db)))

(rf/reg-sub
 move-source-board-options
 (fn [db _]
   (app-state/move-source-board-options db)))

(rf/reg-sub
 move-target-board-options
 (fn [db _]
   (app-state/move-target-board-options db)))

(rf/reg-sub
 move-target-wasteland-options
 (fn [db _]
   (app-state/move-target-wasteland-options db)))

(rf/reg-sub
 move-one-point-card-options
 (fn [db _]
   (app-state/move-one-point-card-options db)))

(rf/reg-sub
 move-territory-card-source-options
 (fn [db _]
   (app-state/move-territory-card-source-options db)))

(rf/reg-sub
 move-power-options
 (fn [db _]
   (app-state/move-power-options db)))

(rf/reg-sub
 move-power
 (fn [db _]
   (app-state/move-power db)))

(rf/reg-sub
 move-rod-mode-options
 (fn [db _]
   (app-state/move-rod-mode-options db)))

(rf/reg-sub
 move-target-piece-options
 (fn [db _]
   (app-state/move-target-piece-options db)))

(rf/reg-sub
 move-distance-options
 (fn [db _]
   (app-state/move-distance-options db)))

(rf/reg-sub
 move-rod-orientation-required?
 (fn [db _]
   (app-state/move-rod-orientation-required? db)))

(rf/reg-sub
 move-orientation-options
 (fn [db _]
   (app-state/move-orientation-options db)))

(rf/reg-sub
 draw-count-options
 (fn [db _]
   (app-state/draw-count-options db)))

(rf/reg-sub
 card-icon-mode
 (fn [db _]
   (app-state/card-icon-mode db)))

(rf/reg-sub
 hotkey-help-open?
 (fn [db _]
   (app-state/hotkey-help-open? db)))

(rf/reg-sub
 icon-help-open?
 (fn [db _]
   (app-state/icon-help-open? db)))
