(ns gnostica.app-state
  (:require [gnostica.app-state.db :as db]
            [gnostica.app-state.gestures :as gestures]
            [gnostica.app-state.lobby :as lobby]
            [gnostica.app-state.moves :as moves]
            [gnostica.app-state.view-models :as views]
            #?(:clj [gnostica.app-state.facade-macros
                     :refer [def-facade-aliases]]))
  #?(:cljs
     (:require-macros [gnostica.app-state.facade-macros
                       :refer [def-facade-aliases]])))

(def default-player-specs db/default-player-specs)
(def default-lobby-player-specs db/default-lobby-player-specs)
(def default-selected-board-index db/default-selected-board-index)
(def default-card-icon-mode db/default-card-icon-mode)
(def default-hotkey-help-open? db/default-hotkey-help-open?)
(def default-icon-help-open? db/default-icon-help-open?)
(def default-dev-demo-hotkeys? db/default-dev-demo-hotkeys?)
(def target-score-options db/target-score-options)
(def panel-ids db/panel-ids)
(def default-open-panels db/default-open-panels)
(def default-three-runtime-status db/default-three-runtime-status)
(def default-direct-manipulation db/default-direct-manipulation)
(def card-icon-modes db/card-icon-modes)
(def empty-move-selection db/empty-move-selection)
(def normalize-open-panels db/normalize-open-panels)
(def normalize-card-icon-mode db/normalize-card-icon-mode)
(def normalize-three-runtime-status db/normalize-three-runtime-status)
(def normalize-direct-manipulation db/normalize-direct-manipulation)
(def normalize-dev-demo-hotkeys db/normalize-dev-demo-hotkeys)
(def dev-demo-hotkeys? db/dev-demo-hotkeys?)

(defn initialize
  ([] (initialize {}))
  ([{:keys [player-specs game-options start-in-lobby? bypass-lobby?]
     :as opts
     :or {player-specs default-player-specs
          game-options {}}}]
   (let [app-db (db/base-db opts)]
     (if (and start-in-lobby?
              (not bypass-lobby?))
       (assoc app-db :lobby (lobby/create-lobby opts))
       (db/initialize-game-db app-db opts player-specs game-options)))))

(def game db/game)
(def setup-error db/setup-error)
(def layout-shuffled-deck-as-territories
  db/layout-shuffled-deck-as-territories)

#?(:clj
   (def lobby lobby/lobby))
(def lobby-active? lobby/lobby-active?)
(def lobby-players lobby/lobby-players)
(def lobby-validation-error lobby/lobby-validation-error)
(def lobby-valid? lobby/lobby-valid?)
(def add-lobby-player lobby/add-lobby-player)
(def remove-lobby-player lobby/remove-lobby-player)
(def set-lobby-player-name lobby/set-lobby-player-name)
(def set-lobby-player-colour lobby/set-lobby-player-colour)
(def set-lobby-target-score lobby/set-lobby-target-score)
(def start-lobby-bidding lobby/start-lobby-bidding)
(def select-lobby-bid-card lobby/select-lobby-bid-card)
(def reveal-lobby-bids lobby/reveal-lobby-bids)
(def select-lobby-redraw-card lobby/select-lobby-redraw-card)
(def confirm-lobby-bidding lobby/confirm-lobby-bidding)
(def cancel-lobby-bidding lobby/cancel-lobby-bidding)
(def start-lobby-game lobby/start-lobby-game)
(def lobby-view-model lobby/lobby-view-model)
(def lobby-view lobby/lobby-view)

(def board views/board)
(def board-pieces views/board-pieces)
(def selected-board-index views/selected-board-index)
(def board-cell-by-index views/board-cell-by-index)
(def selected-board-cell views/selected-board-cell)
(def selected-board-pieces views/selected-board-pieces)
(def card-icon-mode views/card-icon-mode)
(def set-card-icon-mode views/set-card-icon-mode)
(def toggle-card-icon-mode views/toggle-card-icon-mode)
(def open-panels views/open-panels)
(def panel-open? views/panel-open?)
(def set-panel-open views/set-panel-open)
(def toggle-panel views/toggle-panel)
(def hotkey-help-open? views/hotkey-help-open?)
(def set-hotkey-help-open views/set-hotkey-help-open)
(def icon-help-open? views/icon-help-open?)
(def set-icon-help-open views/set-icon-help-open)
(def open-hotkey-help views/open-hotkey-help)
(def close-hotkey-help views/close-hotkey-help)
(def open-icon-help views/open-icon-help)
(def close-icon-help views/close-icon-help)
(def close-help-dialogs views/close-help-dialogs)
(def current-player views/current-player)
(def current-player-id views/current-player-id)
(def game-status views/game-status)
(def can-announce-challenge? views/can-announce-challenge?)
(def can-end-turn? views/can-end-turn?)
(def current-player-hand views/current-player-hand)
(def draw-pile views/draw-pile)
(def discard-pile views/discard-pile)
(def discard-top-card views/discard-top-card)
(def three-runtime-status views/three-runtime-status)
(def set-three-runtime-status views/set-three-runtime-status)
(def direct-manipulation views/direct-manipulation)
(def set-detailed-entry-default views/set-detailed-entry-default)
(def card-zones views/card-zones)
(def board-view-model views/board-view-model)
(def board-view views/board-view)
(def card-zones-view-model views/card-zones-view-model)
(def card-zones-view views/card-zones-view)
(def territory-view-model views/territory-view-model)
(def territory-view views/territory-view)

(def-facade-aliases
  moves
  gnostica.app-state.facade-exports/app-state-move-alias-vars)

(def gesture-intent gestures/gesture-intent)
(def cancel-gesture-intent gestures/cancel-gesture-intent)
(def open-gesture-detailed-entry gestures/open-gesture-detailed-entry)
(def start-gesture-intent gestures/start-gesture-intent)
(def pending-move-tray-view gestures/pending-move-tray-view)
(def cancel-move gestures/cancel-move)
(def select-board-card gestures/select-board-card)
(def gesture-drag-orientation-result gestures/gesture-drag-orientation-result)
(def set-gesture-drag-orientation gestures/set-gesture-drag-orientation)
(def pending-placement-orientation-result
  gestures/pending-placement-orientation-result)
(def set-pending-placement-orientation gestures/set-pending-placement-orientation)
(def keyboard-placement-targeting gestures/keyboard-placement-targeting)
(def keyboard-placement-targeting-active?
  gestures/keyboard-placement-targeting-active?)
(def keyboard-placement-targeting-mode gestures/keyboard-placement-targeting-mode)
(def start-keyboard-placement-targeting
  gestures/start-keyboard-placement-targeting)
(def move-keyboard-placement-target gestures/move-keyboard-placement-target)
(def accept-keyboard-placement-target
  gestures/accept-keyboard-placement-target)

(def move-panel-view-model views/move-panel-view-model)
(def move-panel-view views/move-panel-view)
(def header-view-model views/header-view-model)
(def header-view views/header-view)
(def help-dialogs-view-model views/help-dialogs-view-model)
(def help-dialogs-view views/help-dialogs-view)
(def app-view-model views/app-view-model)
(def app-view views/app-view)
