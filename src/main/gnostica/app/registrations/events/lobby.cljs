(ns gnostica.app.registrations.events.lobby
  (:require [gnostica.app.handlers :as handlers]
            [gnostica.app.ids :as ids]
            [gnostica.app.registrations.events.cofx]
            [gnostica.app.registrations.events.helpers :as helpers]
            [gnostica.app-state :as app-state]
            [re-frame.core :as rf]))

(helpers/register-db-events!
 [[ids/add-lobby-player app-state/add-lobby-player]
  [ids/remove-lobby-player app-state/remove-lobby-player]
  [ids/set-lobby-player-name app-state/set-lobby-player-name]
  [ids/set-lobby-player-colour app-state/set-lobby-player-colour]
  [ids/set-lobby-target-score app-state/set-lobby-target-score]
  [ids/select-lobby-bid-card app-state/select-lobby-bid-card]
  [ids/reveal-lobby-bids app-state/reveal-lobby-bids]
  [ids/select-lobby-redraw-card app-state/select-lobby-redraw-card]
  [ids/confirm-lobby-bidding app-state/confirm-lobby-bidding]
  [ids/cancel-lobby-bidding app-state/cancel-lobby-bidding]])

(rf/reg-event-fx
 ids/start-lobby-game
 [(rf/inject-cofx ids/shuffle-seed)]
 (fn [coeffects _]
   {:db (handlers/start-lobby-game-db (:db coeffects)
                                      {:shuffle-seed (get coeffects ids/shuffle-seed)})}))

(rf/reg-event-fx
 ids/start-lobby-bidding
 [(rf/inject-cofx ids/shuffle-seed)]
 (fn [coeffects _]
   {:db (handlers/start-lobby-bidding-db
         (:db coeffects)
         {:shuffle-seed (get coeffects ids/shuffle-seed)})}))
