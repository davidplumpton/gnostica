(ns gnostica.app.registrations.subscriptions
  (:require [gnostica.app.ids :as ids]
            [gnostica.app.registrations.subscriptions.base]
            [gnostica.app.registrations.subscriptions.chrome]
            [gnostica.app.registrations.subscriptions.move]
            [gnostica.app.registrations.subscriptions.views]))

(def registered-subscription-ids ids/subscription-ids)
