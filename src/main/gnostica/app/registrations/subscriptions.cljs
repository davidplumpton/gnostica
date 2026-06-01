(ns gnostica.app.registrations.subscriptions
  (:require [gnostica.app.subscriptions :as app-subscriptions]
            [gnostica.app.ids :as ids
             :refer [game setup-error lobby board pieces selected-board-index current-player game-status card-zones three-texture-errors three-runtime-status three-renderer-error direct-manipulation selected-board-cell selected-board-pieces move-selection gesture-intent pending-move-tray-view move-source-options move-prompt move-ready? move-control-groups move-action-ribbon move-piece-options move-hand-card-options move-source-board-options move-target-board-options move-target-wasteland-options move-one-point-card-options move-territory-card-source-options move-power-options move-power move-world-copy-options move-world-copied-power-options move-world-copied-power move-rod-mode-options move-disc-action-count-options move-major-action-count-options move-major-action-count move-sword-action-count-options move-devil-action-count-options move-sun-disc-mode-options move-fool-reveal-count-options move-fool-reveal-state move-fool-play-power-options move-fool-play-power move-high-priestess-redraw-count-options move-high-priestess-redraw-options move-judgement-card-options move-judgement-card-maximum move-disc-minion-orientation-required? move-disc-target-kind-options move-sword-target-kind-options move-legal-targets move-preview move-target-piece-options move-distance-options move-damage-options move-rod-orientation-required? move-disc-orientation-available? move-sun-disc-orientation-available? move-sword-orientation-available? move-replacement-card-options move-orientation-options move-discard-card-options draw-count-options card-icon-mode open-panels hotkey-help-open? icon-help-open? app-view lobby-view header-view board-view card-zones-view territory-view move-panel-view help-dialogs-view]]
            [gnostica.app-state :as app-state]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as piece-model]
            [re-frame.core :as rf]))

(def registered-subscription-ids ids/subscription-ids)

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
   (:lobby db)))

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
 move-fool-reveal-state
 (fn [db _]
   (app-state/move-fool-reveal-state db)))

(rf/reg-sub
 move-fool-play-power-options
 (fn [db _]
   (app-state/move-fool-play-power-options db)))

(rf/reg-sub
 move-fool-play-power
 (fn [db _]
   (app-state/move-fool-play-power db)))

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
 move-preview
 (fn [db _]
   (app-state/move-preview db)))

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
 :<- [move-preview]
 :<- [direct-manipulation]
 (fn [[cells board-pieces selected-index card-icon-mode texture-errors
       renderer-error runtime-status legal-targets move-preview direct-manipulation] _]
   (app-state/board-view-model
    {:cells cells
     :board-pieces board-pieces
     :selected-index selected-index
     :legal-targets legal-targets
     :move-preview move-preview
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
 app-subscriptions/move-panel-view)

(rf/reg-sub
 help-dialogs-view
 :<- [hotkey-help-open?]
 :<- [icon-help-open?]
 (fn [[hotkey-help-open? icon-help-open?] _]
   (app-state/help-dialogs-view-model
    {:hotkey-help-open? hotkey-help-open?
     :icon-help-open? icon-help-open?})))
