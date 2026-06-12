(ns gnostica.app.registrations.events.three
  (:require [gnostica.app.ids :as ids]
            [gnostica.app.registrations.events.cofx]
            [gnostica.app-state :as app-state]
            [re-frame.core :as rf]))

(rf/reg-event-db
 ids/clear-three-texture-errors
 (fn [db _]
   (assoc db :three-texture-errors [])))

(rf/reg-event-db
 ids/three-texture-error
 (fn [db [_ image]]
   (update db :three-texture-errors (fnil conj []) image)))

(rf/reg-event-db
 ids/three-renderer-error
 (fn [db [_ message]]
   (assoc db :three-renderer-error message)))

(rf/reg-event-fx
 ids/refresh-three-runtime-status
 [(rf/inject-cofx ids/three-runtime-detection)]
 (fn [coeffects _]
   {:db (app-state/set-three-runtime-status (:db coeffects)
                                            (get coeffects ids/three-runtime-status))}))
