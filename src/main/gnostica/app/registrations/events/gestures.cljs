(ns gnostica.app.registrations.events.gestures
  (:require [gnostica.app.ids :as ids]
            [gnostica.app.registrations.events.helpers :as helpers]
            [gnostica.app-state :as app-state]))

(helpers/register-db-events!
 [[ids/start-gesture-intent app-state/start-gesture-intent]
  [ids/cancel-gesture-intent app-state/cancel-gesture-intent]
  [ids/open-gesture-detailed-entry app-state/open-gesture-detailed-entry]
  [ids/set-gesture-drag-orientation app-state/set-gesture-drag-orientation]
  [ids/set-pending-placement-orientation app-state/set-pending-placement-orientation]
  [ids/start-keyboard-placement-targeting app-state/start-keyboard-placement-targeting]
  [ids/move-keyboard-placement-target app-state/move-keyboard-placement-target]
  [ids/accept-keyboard-placement-target app-state/accept-keyboard-placement-target]
  [ids/set-detailed-entry-default app-state/set-detailed-entry-default]])
