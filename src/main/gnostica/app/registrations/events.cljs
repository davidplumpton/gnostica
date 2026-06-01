(ns gnostica.app.registrations.events
  (:require [gnostica.app.handlers :as handlers]
            [gnostica.app.ids :refer [initialize install-keyboard-shortcuts uninstall-keyboard-shortcuts add-lobby-player remove-lobby-player set-lobby-player-name set-lobby-player-colour set-lobby-target-score start-lobby-game start-lobby-bidding select-lobby-bid-card reveal-lobby-bids select-lobby-redraw-card confirm-lobby-bidding cancel-lobby-bidding select-board-card select-move-source select-move-piece select-move-hand-card select-move-wasteland-target select-move-one-point-card select-move-territory-card-source select-move-replacement-card select-move-power select-move-world-copy select-move-rod-mode select-move-disc-target-kind select-move-sword-target-kind set-move-disc-action-count set-move-major-action-count set-move-sword-action-count set-move-devil-action-count set-move-fool-reveal-count reveal-move-fool-card skip-move-fool-reveal play-move-fool-reveal select-move-fool-play-power set-move-high-priestess-redraw-count toggle-move-high-priestess-discard-card set-move-high-priestess-draw-count toggle-move-judgement-card set-move-minion-orientation select-move-sun-disc-mode set-move-sun-disc-orientation select-move-target-piece set-move-orientation set-move-distance set-move-damage set-move-draw-count toggle-move-discard-card confirm-move cancel-move start-gesture-intent cancel-gesture-intent open-gesture-detailed-entry set-gesture-drag-orientation set-detailed-entry-default end-turn announce-challenge toggle-card-icon-mode toggle-panel set-panel-open open-hotkey-help close-hotkey-help open-icon-help close-icon-help close-help-dialogs clear-three-texture-errors three-texture-error three-renderer-error refresh-three-runtime-status shuffle-seed three-runtime-detection three-runtime-status]]
            [gnostica.app.keyboard :as keyboard]
            [gnostica.app-state :as app-state]
            [gnostica.three-board.runtime :as three-runtime]
            [re-frame.core :as rf]))

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

(rf/reg-event-fx
 start-lobby-bidding
 [(rf/inject-cofx shuffle-seed)]
 (fn [coeffects _]
   {:db (handlers/start-lobby-bidding-db
         (:db coeffects)
         {:shuffle-seed (get coeffects shuffle-seed)})}))

(rf/reg-event-db
 select-lobby-bid-card
 (fn [db [_ player-id card-id]]
   (app-state/select-lobby-bid-card db player-id card-id)))

(rf/reg-event-db
 reveal-lobby-bids
 (fn [db _]
   (app-state/reveal-lobby-bids db)))

(rf/reg-event-db
 select-lobby-redraw-card
 (fn [db [_ player-id card-id]]
   (app-state/select-lobby-redraw-card db player-id card-id)))

(rf/reg-event-db
 confirm-lobby-bidding
 (fn [db _]
   (app-state/confirm-lobby-bidding db)))

(rf/reg-event-db
 cancel-lobby-bidding
 (fn [db _]
   (app-state/cancel-lobby-bidding db)))

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

(rf/reg-event-fx
 reveal-move-fool-card
 [(rf/inject-cofx shuffle-seed)]
 (fn [coeffects _]
   {:db (handlers/reveal-move-fool-card-db
         (:db coeffects)
         {:shuffle-seed (get coeffects shuffle-seed)})}))

(rf/reg-event-db
 skip-move-fool-reveal
 (fn [db _]
   (app-state/skip-move-fool-reveal db)))

(rf/reg-event-db
 play-move-fool-reveal
 (fn [db _]
   (app-state/play-move-fool-reveal db)))

(rf/reg-event-db
 select-move-fool-play-power
 (fn [db [_ power]]
   (app-state/select-move-fool-play-power db power)))

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
 set-gesture-drag-orientation
 (fn [db [_ result]]
   (app-state/set-gesture-drag-orientation db result)))

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
