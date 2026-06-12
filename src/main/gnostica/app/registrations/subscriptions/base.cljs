(ns gnostica.app.registrations.subscriptions.base
  (:require [gnostica.app.ids :as ids]
            [gnostica.app.registrations.subscriptions.helpers :as helpers]
            [gnostica.app-state :as app-state]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as piece-model]
            [re-frame.core :as rf]))

(helpers/register-db-subscriptions!
 [[ids/game app-state/game]
  [ids/setup-error app-state/setup-error]
  [ids/selected-board-index app-state/selected-board-index]
  [ids/game-status app-state/game-status]
  [ids/three-runtime-status app-state/three-runtime-status]
  [ids/direct-manipulation app-state/direct-manipulation]
  [ids/selected-board-cell app-state/selected-board-cell]
  [ids/move-selection app-state/move-selection]
  [ids/gesture-intent app-state/gesture-intent]])

(rf/reg-sub
 ids/lobby
 (fn [db _]
   (:lobby db)))

(rf/reg-sub
 ids/board
 :<- [ids/game]
 (fn [state _]
   (or (:board state) [])))

(rf/reg-sub
 ids/pieces
 :<- [ids/game]
 (fn [state _]
   (or (get-in state [:pieces :on-board]) [])))

(rf/reg-sub
 ids/current-player
 :<- [ids/game]
 (fn [state _]
   (some-> state game-state/current-player)))

(rf/reg-sub
 ids/card-zones
 :<- [ids/game]
 (fn [state _]
   (app-state/card-zones {:game state})))

(rf/reg-sub
 ids/three-texture-errors
 (fn [db _]
   (:three-texture-errors db)))

(rf/reg-sub
 ids/three-renderer-error
 (fn [db _]
   (:three-renderer-error db)))

(rf/reg-sub
 ids/selected-board-pieces
 :<- [ids/pieces]
 :<- [ids/selected-board-index]
 (fn [[board-pieces selected-index] _]
   (piece-model/pieces-for-space board-pieces selected-index)))
