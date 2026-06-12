(ns gnostica.app.registrations.events.turn
  (:require [gnostica.app.ids :as ids]
            [gnostica.app-state :as app-state]
            [re-frame.core :as rf]))

(rf/reg-event-db
 ids/end-turn
 (fn [db _]
   (app-state/end-turn db)))

(rf/reg-event-db
 ids/announce-challenge
 (fn [db _]
   (app-state/end-turn db {:announce-challenge? true})))
