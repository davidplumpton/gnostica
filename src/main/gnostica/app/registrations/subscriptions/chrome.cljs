(ns gnostica.app.registrations.subscriptions.chrome
  (:require [gnostica.app.ids :as ids]
            [gnostica.app.registrations.subscriptions.helpers :as helpers]
            [gnostica.app-state :as app-state]))

(helpers/register-db-subscriptions!
 [[ids/card-icon-mode app-state/card-icon-mode]
  [ids/open-panels app-state/open-panels]
  [ids/hotkey-help-open? app-state/hotkey-help-open?]
  [ids/icon-help-open? app-state/icon-help-open?]])
