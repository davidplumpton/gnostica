(ns gnostica.app.registrations.events
  (:require [gnostica.app.ids :as ids]
            [gnostica.app.registrations.events.cofx]
            [gnostica.app.registrations.events.gestures]
            [gnostica.app.registrations.events.lifecycle]
            [gnostica.app.registrations.events.lobby]
            [gnostica.app.registrations.events.move]
            [gnostica.app.registrations.events.three]
            [gnostica.app.registrations.events.turn]
            [gnostica.app.registrations.events.ui]))

(def registered-event-ids ids/event-ids)
(def registered-cofx-ids ids/cofx-ids)
