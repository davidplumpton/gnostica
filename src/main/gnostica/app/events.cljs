(ns gnostica.app.events
  (:require [gnostica.app.keyboard :as keyboard]
            [gnostica.app-state :as app-state]
            [gnostica.app.handlers :as handlers]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as piece-model]
            [gnostica.three-board.runtime :as three-runtime]
            [re-frame.core :as rf]))

(def initialize :gnostica.app/initialize)
(def install-keyboard-shortcuts :gnostica.app/install-keyboard-shortcuts)
(def uninstall-keyboard-shortcuts :gnostica.app/uninstall-keyboard-shortcuts)
(def add-lobby-player :gnostica.app/add-lobby-player)
(def remove-lobby-player :gnostica.app/remove-lobby-player)
(def set-lobby-player-name :gnostica.app/set-lobby-player-name)
(def set-lobby-player-colour :gnostica.app/set-lobby-player-colour)
(def set-lobby-target-score :gnostica.app/set-lobby-target-score)
(def start-lobby-game :gnostica.app/start-lobby-game)
(def select-board-card :gnostica.app/select-board-card)
(def select-move-source :gnostica.app/select-move-source)
(def select-move-piece :gnostica.app/select-move-piece)
(def select-move-hand-card :gnostica.app/select-move-hand-card)
(def select-move-wasteland-target :gnostica.app/select-move-wasteland-target)
(def select-move-one-point-card :gnostica.app/select-move-one-point-card)
(def select-move-territory-card-source :gnostica.app/select-move-territory-card-source)
(def select-move-replacement-card :gnostica.app/select-move-replacement-card)
(def select-move-power :gnostica.app/select-move-power)
(def select-move-world-copy :gnostica.app/select-move-world-copy)
(def select-move-rod-mode :gnostica.app/select-move-rod-mode)
(def select-move-disc-target-kind :gnostica.app/select-move-disc-target-kind)
(def select-move-sword-target-kind :gnostica.app/select-move-sword-target-kind)
(def set-move-disc-action-count :gnostica.app/set-move-disc-action-count)
(def set-move-major-action-count :gnostica.app/set-move-major-action-count)
(def set-move-sword-action-count :gnostica.app/set-move-sword-action-count)
(def set-move-devil-action-count :gnostica.app/set-move-devil-action-count)
(def set-move-fool-reveal-count :gnostica.app/set-move-fool-reveal-count)
(def set-move-high-priestess-redraw-count :gnostica.app/set-move-high-priestess-redraw-count)
(def toggle-move-high-priestess-discard-card :gnostica.app/toggle-move-high-priestess-discard-card)
(def set-move-high-priestess-draw-count :gnostica.app/set-move-high-priestess-draw-count)
(def toggle-move-judgement-card :gnostica.app/toggle-move-judgement-card)
(def set-move-minion-orientation :gnostica.app/set-move-minion-orientation)
(def select-move-sun-disc-mode :gnostica.app/select-move-sun-disc-mode)
(def set-move-sun-disc-orientation :gnostica.app/set-move-sun-disc-orientation)
(def select-move-target-piece :gnostica.app/select-move-target-piece)
(def set-move-orientation :gnostica.app/set-move-orientation)
(def set-move-distance :gnostica.app/set-move-distance)
(def set-move-damage :gnostica.app/set-move-damage)
(def set-move-draw-count :gnostica.app/set-move-draw-count)
(def toggle-move-discard-card :gnostica.app/toggle-move-discard-card)
(def confirm-move :gnostica.app/confirm-move)
(def cancel-move :gnostica.app/cancel-move)
(def start-gesture-intent :gnostica.app/start-gesture-intent)
(def cancel-gesture-intent :gnostica.app/cancel-gesture-intent)
(def open-gesture-detailed-entry :gnostica.app/open-gesture-detailed-entry)
(def set-detailed-entry-default :gnostica.app/set-detailed-entry-default)
(def end-turn :gnostica.app/end-turn)
(def announce-challenge :gnostica.app/announce-challenge)
(def toggle-card-icon-mode :gnostica.app/toggle-card-icon-mode)
(def toggle-panel :gnostica.app/toggle-panel)
(def set-panel-open :gnostica.app/set-panel-open)
(def open-hotkey-help :gnostica.app/open-hotkey-help)
(def close-hotkey-help :gnostica.app/close-hotkey-help)
(def open-icon-help :gnostica.app/open-icon-help)
(def close-icon-help :gnostica.app/close-icon-help)
(def close-help-dialogs :gnostica.app/close-help-dialogs)
(def clear-three-texture-errors :gnostica.app/clear-three-texture-errors)
(def three-texture-error :gnostica.app/three-texture-error)
(def three-renderer-error :gnostica.app/three-renderer-error)
(def refresh-three-runtime-status :gnostica.app/refresh-three-runtime-status)

(def game :gnostica.app/game)
(def setup-error :gnostica.app/setup-error)
(def lobby :gnostica.app/lobby)
(def board :gnostica.app/board)
(def pieces :gnostica.app/pieces)
(def selected-board-index :gnostica.app/selected-board-index)
(def current-player :gnostica.app/current-player)
(def game-status :gnostica.app/game-status)
(def card-zones :gnostica.app/card-zones)
(def three-texture-errors :gnostica.app/three-texture-errors)
(def three-runtime-status :gnostica.app/three-runtime-status)
(def direct-manipulation :gnostica.app/direct-manipulation)
(def selected-board-cell :gnostica.app/selected-board-cell)
(def selected-board-pieces :gnostica.app/selected-board-pieces)
(def move-selection :gnostica.app/move-selection)
(def gesture-intent :gnostica.app/gesture-intent)
(def pending-move-tray-view :gnostica.app/pending-move-tray-view)
(def move-source-options :gnostica.app/move-source-options)
(def move-prompt :gnostica.app/move-prompt)
(def move-ready? :gnostica.app/move-ready?)
(def move-control-groups :gnostica.app/move-control-groups)
(def move-action-ribbon :gnostica.app/move-action-ribbon)
(def move-piece-options :gnostica.app/move-piece-options)
(def move-hand-card-options :gnostica.app/move-hand-card-options)
(def move-source-board-options :gnostica.app/move-source-board-options)
(def move-target-board-options :gnostica.app/move-target-board-options)
(def move-target-wasteland-options :gnostica.app/move-target-wasteland-options)
(def move-one-point-card-options :gnostica.app/move-one-point-card-options)
(def move-territory-card-source-options :gnostica.app/move-territory-card-source-options)
(def move-power-options :gnostica.app/move-power-options)
(def move-power :gnostica.app/move-power)
(def move-world-copy-options :gnostica.app/move-world-copy-options)
(def move-world-copied-power-options :gnostica.app/move-world-copied-power-options)
(def move-world-copied-power :gnostica.app/move-world-copied-power)
(def move-rod-mode-options :gnostica.app/move-rod-mode-options)
(def move-disc-action-count-options :gnostica.app/move-disc-action-count-options)
(def move-major-action-count-options :gnostica.app/move-major-action-count-options)
(def move-major-action-count :gnostica.app/move-major-action-count)
(def move-sword-action-count-options :gnostica.app/move-sword-action-count-options)
(def move-devil-action-count-options :gnostica.app/move-devil-action-count-options)
(def move-sun-disc-mode-options :gnostica.app/move-sun-disc-mode-options)
(def move-fool-reveal-count-options :gnostica.app/move-fool-reveal-count-options)
(def move-high-priestess-redraw-count-options :gnostica.app/move-high-priestess-redraw-count-options)
(def move-high-priestess-redraw-options :gnostica.app/move-high-priestess-redraw-options)
(def move-judgement-card-options :gnostica.app/move-judgement-card-options)
(def move-judgement-card-maximum :gnostica.app/move-judgement-card-maximum)
(def move-disc-minion-orientation-required? :gnostica.app/move-disc-minion-orientation-required?)
(def move-disc-target-kind-options :gnostica.app/move-disc-target-kind-options)
(def move-sword-target-kind-options :gnostica.app/move-sword-target-kind-options)
(def move-legal-targets :gnostica.app/move-legal-targets)
(def move-target-piece-options :gnostica.app/move-target-piece-options)
(def move-distance-options :gnostica.app/move-distance-options)
(def move-damage-options :gnostica.app/move-damage-options)
(def move-rod-orientation-required? :gnostica.app/move-rod-orientation-required?)
(def move-disc-orientation-available? :gnostica.app/move-disc-orientation-available?)
(def move-sun-disc-orientation-available? :gnostica.app/move-sun-disc-orientation-available?)
(def move-sword-orientation-available? :gnostica.app/move-sword-orientation-available?)
(def move-replacement-card-options :gnostica.app/move-replacement-card-options)
(def move-orientation-options :gnostica.app/move-orientation-options)
(def move-discard-card-options :gnostica.app/move-discard-card-options)
(def draw-count-options :gnostica.app/draw-count-options)
(def card-icon-mode :gnostica.app/card-icon-mode)
(def open-panels :gnostica.app/open-panels)
(def hotkey-help-open? :gnostica.app/hotkey-help-open?)
(def icon-help-open? :gnostica.app/icon-help-open?)

(def app-view :gnostica.app/app-view)
(def lobby-view :gnostica.app/lobby-view)
(def header-view :gnostica.app/header-view)
(def board-view :gnostica.app/board-view)
(def card-zones-view :gnostica.app/card-zones-view)
(def territory-view :gnostica.app/territory-view)
(def move-panel-view :gnostica.app/move-panel-view)
(def help-dialogs-view :gnostica.app/help-dialogs-view)

(def shuffle-seed :gnostica.app/shuffle-seed)
(def three-runtime-detection :gnostica.app/three-runtime-detection)

(def ^:private max-browser-seed 4294967296)

(defn- browser-shuffle-seed []
  (js/Math.floor (* max-browser-seed (js/Math.random))))

(rf/reg-cofx
 shuffle-seed
 (fn [coeffects _]
   (assoc coeffects shuffle-seed (browser-shuffle-seed))))

(rf/reg-cofx
 three-runtime-detection
 (fn [coeffects _]
   (assoc coeffects three-runtime-status (three-runtime/runtime-status))))

(rf/reg-event-fx
 install-keyboard-shortcuts
 (fn [_ _]
   {keyboard/install-global-shortcuts-fx true}))

(rf/reg-event-fx
 uninstall-keyboard-shortcuts
 (fn [_ _]
   {keyboard/install-global-shortcuts-fx false}))

(rf/reg-event-fx
 initialize
 [(rf/inject-cofx shuffle-seed)
  (rf/inject-cofx three-runtime-detection)]
 (fn [coeffects [_ opts]]
   {:db (-> (handlers/initialize-db opts {:shuffle-seed (get coeffects shuffle-seed)})
            (app-state/set-three-runtime-status (get coeffects three-runtime-status)))}))

(rf/reg-event-db
 add-lobby-player
 (fn [db _]
   (app-state/add-lobby-player db)))

(rf/reg-event-db
 remove-lobby-player
 (fn [db [_ slot-id]]
   (app-state/remove-lobby-player db slot-id)))

(rf/reg-event-db
 set-lobby-player-name
 (fn [db [_ slot-id name]]
   (app-state/set-lobby-player-name db slot-id name)))

(rf/reg-event-db
 set-lobby-player-colour
 (fn [db [_ slot-id player-id]]
   (app-state/set-lobby-player-colour db slot-id player-id)))

(rf/reg-event-db
 set-lobby-target-score
 (fn [db [_ target-score]]
   (app-state/set-lobby-target-score db target-score)))

(rf/reg-event-fx
 start-lobby-game
 [(rf/inject-cofx shuffle-seed)]
 (fn [coeffects _]
   {:db (handlers/start-lobby-game-db (:db coeffects)
                                      {:shuffle-seed (get coeffects shuffle-seed)})}))

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
 select-move-replacement-card
 (fn [db [_ card-id]]
   (app-state/select-move-replacement-card db card-id)))

(rf/reg-event-db
 select-move-power
 (fn [db [_ power]]
   (app-state/select-move-power db power)))

(rf/reg-event-db
 select-move-world-copy
 (fn [db [_ board-index]]
   (app-state/select-move-world-copy db board-index)))

(rf/reg-event-db
select-move-rod-mode
(fn [db [_ mode]]
  (app-state/select-move-rod-mode db mode)))

(rf/reg-event-db
 select-move-disc-target-kind
 (fn [db [_ target-kind]]
   (app-state/select-move-disc-target-kind db target-kind)))

(rf/reg-event-db
 select-move-sword-target-kind
 (fn [db [_ target-kind]]
   (app-state/select-move-sword-target-kind db target-kind)))

(rf/reg-event-db
 set-move-disc-action-count
 (fn [db [_ action-count]]
   (app-state/set-move-disc-action-count db action-count)))

(rf/reg-event-db
 set-move-major-action-count
 (fn [db [_ action-count]]
   (app-state/set-move-major-action-count db action-count)))

(rf/reg-event-db
 set-move-sword-action-count
 (fn [db [_ action-count]]
   (app-state/set-move-sword-action-count db action-count)))

(rf/reg-event-db
 set-move-devil-action-count
 (fn [db [_ action-count]]
   (app-state/set-move-devil-action-count db action-count)))

(rf/reg-event-db
 set-move-fool-reveal-count
 (fn [db [_ reveal-count]]
   (app-state/set-move-fool-reveal-count db reveal-count)))

(rf/reg-event-db
 set-move-high-priestess-redraw-count
 (fn [db [_ redraw-count]]
   (app-state/set-move-high-priestess-redraw-count db redraw-count)))

(rf/reg-event-db
 toggle-move-high-priestess-discard-card
 (fn [db [_ pass-index card-id]]
   (app-state/toggle-move-high-priestess-discard-card db pass-index card-id)))

(rf/reg-event-db
 set-move-high-priestess-draw-count
 (fn [db [_ pass-index draw-count]]
   (app-state/set-move-high-priestess-draw-count db pass-index draw-count)))

(rf/reg-event-db
 toggle-move-judgement-card
 (fn [db [_ card-id]]
   (app-state/toggle-move-judgement-card db card-id)))

(rf/reg-event-db
 set-move-minion-orientation
 (fn [db [_ orientation]]
   (app-state/set-move-minion-orientation db orientation)))

(rf/reg-event-db
 select-move-sun-disc-mode
 (fn [db [_ mode]]
   (app-state/select-move-sun-disc-mode db mode)))

(rf/reg-event-db
 set-move-sun-disc-orientation
 (fn [db [_ orientation]]
   (app-state/set-move-sun-disc-orientation db orientation)))

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
 set-move-damage
 (fn [db [_ damage]]
   (app-state/set-move-damage db damage)))

(rf/reg-event-db
 set-move-draw-count
 (fn [db [_ draw-count]]
   (app-state/set-move-draw-count db draw-count)))

(rf/reg-event-db
 toggle-move-discard-card
 (fn [db [_ card-id]]
   (app-state/toggle-move-discard-card db card-id)))

(rf/reg-event-fx
 confirm-move
 [(rf/inject-cofx shuffle-seed)]
 (fn [coeffects _]
   {:db (handlers/confirm-move-db (:db coeffects)
                                   {:shuffle-seed (get coeffects shuffle-seed)})}))

(rf/reg-event-db
 cancel-move
 (fn [db _]
   (app-state/cancel-move db)))

(rf/reg-event-db
 start-gesture-intent
 (fn [db [_ input]]
   (app-state/start-gesture-intent db input)))

(rf/reg-event-db
 cancel-gesture-intent
 (fn [db _]
   (app-state/cancel-gesture-intent db)))

(rf/reg-event-db
 open-gesture-detailed-entry
 (fn [db _]
   (app-state/open-gesture-detailed-entry db)))

(rf/reg-event-db
 set-detailed-entry-default
 (fn [db [_ enabled?]]
   (app-state/set-detailed-entry-default db enabled?)))

(rf/reg-event-db
 end-turn
 (fn [db _]
   (app-state/end-turn db)))

(rf/reg-event-db
 announce-challenge
 (fn [db _]
   (app-state/end-turn db {:announce-challenge? true})))

(rf/reg-event-db
 toggle-card-icon-mode
 (fn [db _]
   (app-state/toggle-card-icon-mode db)))

(rf/reg-event-db
 toggle-panel
 (fn [db [_ panel-id]]
   (app-state/toggle-panel db panel-id)))

(rf/reg-event-db
 set-panel-open
 (fn [db [_ panel-id open?]]
   (app-state/set-panel-open db panel-id open?)))

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

(rf/reg-event-fx
 refresh-three-runtime-status
 [(rf/inject-cofx three-runtime-detection)]
 (fn [coeffects _]
   {:db (app-state/set-three-runtime-status (:db coeffects)
                                            (get coeffects three-runtime-status))}))

(rf/reg-sub
 game
 (fn [db _]
   (app-state/game db)))

(rf/reg-sub
 setup-error
 (fn [db _]
   (app-state/setup-error db)))

(rf/reg-sub
 lobby
 (fn [db _]
   (app-state/lobby db)))

(rf/reg-sub
 board
 :<- [game]
 (fn [state _]
   (or (:board state) [])))

(rf/reg-sub
 pieces
 :<- [game]
 (fn [state _]
   (or (get-in state [:pieces :on-board]) [])))

(rf/reg-sub
 selected-board-index
 (fn [db _]
   (app-state/selected-board-index db)))

(rf/reg-sub
 current-player
 :<- [game]
 (fn [state _]
   (some-> state game-state/current-player)))

(rf/reg-sub
 game-status
 (fn [db _]
   (app-state/game-status db)))

(rf/reg-sub
 card-zones
 :<- [game]
 (fn [state _]
   (app-state/card-zones {:game state})))

(rf/reg-sub
 three-texture-errors
 (fn [db _]
   (:three-texture-errors db)))

(rf/reg-sub
 three-runtime-status
 (fn [db _]
   (app-state/three-runtime-status db)))

(rf/reg-sub
 direct-manipulation
 (fn [db _]
   (app-state/direct-manipulation db)))

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
 :<- [pieces]
 :<- [selected-board-index]
 (fn [[board-pieces selected-index] _]
   (piece-model/pieces-for-space board-pieces selected-index)))

(rf/reg-sub
 move-selection
 (fn [db _]
   (app-state/move-selection db)))

(rf/reg-sub
 gesture-intent
 (fn [db _]
   (app-state/gesture-intent db)))

(rf/reg-sub
 pending-move-tray-view
 (fn [db _]
   (app-state/pending-move-tray-view db)))

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
 move-control-groups
 (fn [db _]
   (app-state/move-control-groups db)))

(rf/reg-sub
 move-action-ribbon
 (fn [db _]
   (app-state/move-action-ribbon db)))

(rf/reg-sub
 move-piece-options
 (fn [db _]
   (app-state/move-piece-options db)))

(rf/reg-sub
 move-hand-card-options
 (fn [db _]
   (app-state/move-hand-card-options db)))

(rf/reg-sub
 move-discard-card-options
 (fn [db _]
   (app-state/move-discard-card-options db)))

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
 move-world-copy-options
 (fn [db _]
   (app-state/move-world-copy-options db)))

(rf/reg-sub
 move-world-copied-power-options
 (fn [db _]
   (app-state/move-world-copied-power-options db)))

(rf/reg-sub
 move-world-copied-power
 (fn [db _]
   (app-state/move-world-copied-power db)))

(rf/reg-sub
move-rod-mode-options
(fn [db _]
  (app-state/move-rod-mode-options db)))

(rf/reg-sub
 move-disc-action-count-options
 (fn [db _]
   (app-state/move-disc-action-count-options db)))

(rf/reg-sub
 move-major-action-count-options
 (fn [db _]
   (app-state/move-major-action-count-options db)))

(rf/reg-sub
 move-major-action-count
 (fn [db _]
   (app-state/move-major-action-count db)))

(rf/reg-sub
 move-sword-action-count-options
 (fn [db _]
   (app-state/move-sword-action-count-options db)))

(rf/reg-sub
 move-devil-action-count-options
 (fn [db _]
   (app-state/move-devil-action-count-options db)))

(rf/reg-sub
 move-sun-disc-mode-options
 (fn [db _]
   (app-state/move-sun-disc-mode-options db)))

(rf/reg-sub
 move-fool-reveal-count-options
 (fn [db _]
   (app-state/move-fool-reveal-count-options db)))

(rf/reg-sub
 move-high-priestess-redraw-count-options
 (fn [db _]
   (app-state/move-high-priestess-redraw-count-options db)))

(rf/reg-sub
 move-high-priestess-redraw-options
 (fn [db _]
   (app-state/move-high-priestess-redraw-options db)))

(rf/reg-sub
 move-judgement-card-options
 (fn [db _]
   (app-state/move-judgement-card-options db)))

(rf/reg-sub
 move-judgement-card-maximum
 (fn [db _]
   (app-state/move-judgement-card-maximum db)))

(rf/reg-sub
 move-disc-minion-orientation-required?
 (fn [db _]
   (app-state/move-disc-minion-orientation-required? db)))

(rf/reg-sub
 move-disc-target-kind-options
 (fn [db _]
   (app-state/move-disc-target-kind-options db)))

(rf/reg-sub
 move-sword-target-kind-options
 (fn [db _]
   (app-state/move-sword-target-kind-options db)))

(rf/reg-sub
 move-legal-targets
 (fn [db _]
   (app-state/move-legal-targets db)))

(rf/reg-sub
move-target-piece-options
 (fn [db _]
   (app-state/move-target-piece-options db)))

(rf/reg-sub
 move-distance-options
 (fn [db _]
   (app-state/move-distance-options db)))

(rf/reg-sub
 move-damage-options
 (fn [db _]
   (app-state/move-damage-options db)))

(rf/reg-sub
move-rod-orientation-required?
(fn [db _]
  (or (app-state/move-rod-orientation-required? db)
      (app-state/move-hermit-orientation-required? db))))

(rf/reg-sub
 move-disc-orientation-available?
 (fn [db _]
   (app-state/move-disc-orientation-available? db)))

(rf/reg-sub
 move-sun-disc-orientation-available?
 (fn [db _]
   (app-state/move-sun-disc-orientation-available? db)))

(rf/reg-sub
 move-sword-orientation-available?
 (fn [db _]
   (app-state/move-sword-orientation-available? db)))

(rf/reg-sub
 move-replacement-card-options
 (fn [db _]
   (app-state/move-replacement-card-options db)))

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
 open-panels
 (fn [db _]
   (app-state/open-panels db)))

(rf/reg-sub
 hotkey-help-open?
 (fn [db _]
   (app-state/hotkey-help-open? db)))

(rf/reg-sub
 icon-help-open?
 (fn [db _]
   (app-state/icon-help-open? db)))

(rf/reg-sub
 app-view
 :<- [setup-error]
 :<- [lobby]
 :<- [card-icon-mode]
 :<- [open-panels]
 (fn [[setup-error lobby card-icon-mode open-panels] _]
   (app-state/app-view-model
    {:setup-error setup-error
     :lobby? (some? lobby)
     :card-icon-mode card-icon-mode
     :open-panels open-panels})))

(rf/reg-sub
 lobby-view
 :<- [lobby]
 (fn [lobby _]
   (app-state/lobby-view-model {:lobby lobby})))

(rf/reg-sub
 header-view
 (fn [db _]
   (app-state/header-view db)))

(rf/reg-sub
 board-view
 :<- [board]
 :<- [pieces]
 :<- [selected-board-index]
 :<- [card-icon-mode]
 :<- [three-texture-errors]
 :<- [three-renderer-error]
 :<- [three-runtime-status]
 :<- [move-legal-targets]
 :<- [direct-manipulation]
 (fn [[cells board-pieces selected-index card-icon-mode texture-errors
       renderer-error runtime-status legal-targets direct-manipulation] _]
   (app-state/board-view-model
    {:cells cells
     :board-pieces board-pieces
     :selected-index selected-index
     :legal-targets legal-targets
     :card-icon-mode card-icon-mode
     :texture-errors texture-errors
     :renderer-error renderer-error
     :three-runtime-status runtime-status
     :direct-manipulation direct-manipulation})))

(rf/reg-sub
 card-zones-view
 :<- [current-player]
 :<- [card-icon-mode]
 :<- [card-zones]
 :<- [move-legal-targets]
 :<- [direct-manipulation]
 (fn [[current-player card-icon-mode zones legal-targets direct-manipulation] _]
   (app-state/card-zones-view-model
    {:current-player current-player
     :card-icon-mode card-icon-mode
     :zones zones
     :legal-targets legal-targets
     :direct-manipulation direct-manipulation})))

(rf/reg-sub
 territory-view
 :<- [selected-board-cell]
 :<- [selected-board-pieces]
 (fn [[cell selected-pieces] _]
   (app-state/territory-view-model
    {:cell cell
     :selected-pieces selected-pieces})))

(rf/reg-sub
 move-panel-view
 :<- [current-player]
 :<- [move-selection]
 :<- [move-source-options]
 :<- [move-prompt]
 :<- [move-ready?]
 :<- [move-control-groups]
 :<- [move-action-ribbon]
 :<- [direct-manipulation]
 :<- [board]
 :<- [move-power]
 :<- [move-power-options]
 :<- [move-world-copy-options]
 :<- [move-world-copied-power-options]
 :<- [move-world-copied-power]
 :<- [move-rod-mode-options]
 :<- [move-disc-action-count-options]
 :<- [move-major-action-count-options]
 :<- [move-major-action-count]
 :<- [move-sword-action-count-options]
 :<- [move-devil-action-count-options]
 :<- [move-sun-disc-mode-options]
 :<- [move-fool-reveal-count-options]
 :<- [move-high-priestess-redraw-count-options]
 :<- [move-high-priestess-redraw-options]
 :<- [move-judgement-card-options]
 :<- [move-judgement-card-maximum]
 :<- [move-disc-minion-orientation-required?]
 :<- [move-disc-target-kind-options]
 :<- [move-sword-target-kind-options]
 :<- [move-piece-options]
 :<- [move-target-piece-options]
 :<- [move-hand-card-options]
 :<- [move-discard-card-options]
 :<- [move-source-board-options]
 :<- [move-target-board-options]
 :<- [move-target-wasteland-options]
 :<- [move-territory-card-source-options]
 :<- [move-one-point-card-options]
 :<- [move-replacement-card-options]
 :<- [move-orientation-options]
 :<- [move-rod-orientation-required?]
 :<- [move-disc-orientation-available?]
 :<- [move-sun-disc-orientation-available?]
 :<- [move-sword-orientation-available?]
 :<- [move-distance-options]
 :<- [move-damage-options]
 :<- [draw-count-options]
 :<- [move-legal-targets]
(fn [[current-player selection source-options prompt ready? control-groups
       action-ribbon direct-manipulation board power
       power-options world-copy-options world-copied-power-options world-copied-power
       rod-mode-options disc-action-count-options
       major-action-count-options major-action-count
       sword-action-count-options devil-action-count-options
       sun-disc-mode-options fool-reveal-count-options
       high-priestess-redraw-count-options high-priestess-redraw-options
       judgement-card-options judgement-card-maximum
       disc-minion-orientation-required? disc-target-kind-options
       sword-target-kind-options piece-options target-piece-options
       hand-options discard-card-options source-board-options target-board-options
       target-wasteland-options territory-card-source-options
       one-point-card-options replacement-card-options orientation-options orientation-required?
       disc-orientation-available? sun-disc-orientation-available?
       sword-orientation-available?
       distance-options damage-options draw-options legal-targets] _]
   (app-state/move-panel-view-model
    {:current-player current-player
     :selection selection
     :source-options source-options
     :prompt prompt
     :ready? ready?
     :control-groups control-groups
     :action-ribbon action-ribbon
     :direct-manipulation direct-manipulation
     :board board
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
     :draw-options draw-options})))

(rf/reg-sub
 help-dialogs-view
 :<- [hotkey-help-open?]
 :<- [icon-help-open?]
 (fn [[hotkey-help-open? icon-help-open?] _]
   (app-state/help-dialogs-view-model
    {:hotkey-help-open? hotkey-help-open?
     :icon-help-open? icon-help-open?})))
