(ns gnostica.app.registrations.events.lifecycle
  (:require [gnostica.app.handlers :as handlers]
            [gnostica.app.ids :as ids]
            [gnostica.app.keyboard :as keyboard]
            [gnostica.app.registrations.events.cofx]
            [gnostica.app-state :as app-state]
            [re-frame.core :as rf]))

(rf/reg-event-fx
 ids/install-keyboard-shortcuts
 (fn [_ _]
   {keyboard/install-global-shortcuts-fx true}))

(rf/reg-event-fx
 ids/uninstall-keyboard-shortcuts
 (fn [_ _]
   {keyboard/install-global-shortcuts-fx false}))

(rf/reg-event-fx
 ids/initialize
 [(rf/inject-cofx ids/shuffle-seed)
  (rf/inject-cofx ids/three-runtime-detection)]
 (fn [coeffects [_ opts]]
   {:db (-> (handlers/initialize-db opts {:shuffle-seed (get coeffects ids/shuffle-seed)})
            (app-state/set-three-runtime-status (get coeffects ids/three-runtime-status)))}))
