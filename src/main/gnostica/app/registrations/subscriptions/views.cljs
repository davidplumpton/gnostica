(ns gnostica.app.registrations.subscriptions.views
  (:require [gnostica.app.subscriptions :as app-subscriptions]
            [gnostica.app.ids :as ids]
            [gnostica.app.registrations.subscriptions.base]
            [gnostica.app.registrations.subscriptions.chrome]
            [gnostica.app.registrations.subscriptions.move]
            [gnostica.app-state :as app-state]
            [re-frame.core :as rf]))

(rf/reg-sub
 ids/pending-move-tray-view
 (fn [db _]
   (app-state/pending-move-tray-view db)))

(rf/reg-sub
 ids/app-view
 :<- [ids/setup-error]
 :<- [ids/lobby]
 :<- [ids/card-icon-mode]
 :<- [ids/open-panels]
 (fn [[setup-error lobby card-icon-mode open-panels] _]
   (app-state/app-view-model
    {:setup-error setup-error
     :lobby? (some? lobby)
     :card-icon-mode card-icon-mode
     :open-panels open-panels})))

(rf/reg-sub
 ids/lobby-view
 :<- [ids/lobby]
 (fn [lobby _]
   (app-state/lobby-view-model {:lobby lobby})))

(rf/reg-sub
 ids/header-view
 (fn [db _]
   (app-state/header-view db)))

(rf/reg-sub
 ids/board-view
 :<- [ids/board]
 :<- [ids/pieces]
 :<- [ids/selected-board-index]
 :<- [ids/card-icon-mode]
 :<- [ids/three-texture-errors]
 :<- [ids/three-renderer-error]
 :<- [ids/three-runtime-status]
 :<- [ids/move-legal-targets]
 :<- [ids/move-preview]
 :<- [ids/direct-manipulation]
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
 ids/card-zones-view
 :<- [ids/current-player]
 :<- [ids/card-icon-mode]
 :<- [ids/card-zones]
 :<- [ids/move-legal-targets]
 :<- [ids/direct-manipulation]
 (fn [[current-player card-icon-mode zones legal-targets direct-manipulation] _]
   (app-state/card-zones-view-model
    {:current-player current-player
     :card-icon-mode card-icon-mode
     :zones zones
     :legal-targets legal-targets
     :direct-manipulation direct-manipulation})))

(rf/reg-sub
 ids/territory-view
 :<- [ids/selected-board-cell]
 :<- [ids/selected-board-pieces]
 (fn [[cell selected-pieces] _]
   (app-state/territory-view-model
    {:cell cell
     :selected-pieces selected-pieces})))

(rf/reg-sub
 ids/move-panel-view
 app-subscriptions/move-panel-view)

(rf/reg-sub
 ids/help-dialogs-view
 (fn [db _]
   (app-state/help-dialogs-view db)))
