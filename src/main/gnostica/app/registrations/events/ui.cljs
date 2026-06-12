(ns gnostica.app.registrations.events.ui
  (:require [gnostica.app.ids :as ids]
            [gnostica.app.registrations.events.helpers :as helpers]
            [gnostica.app-state :as app-state]))

(helpers/register-db-events!
 [[ids/select-board-card app-state/select-board-card]
  [ids/toggle-card-icon-mode app-state/toggle-card-icon-mode]
  [ids/layout-shuffled-deck-territories app-state/layout-shuffled-deck-as-territories]
  [ids/toggle-panel app-state/toggle-panel]
  [ids/set-panel-open app-state/set-panel-open]
  [ids/open-hotkey-help app-state/open-hotkey-help]
  [ids/close-hotkey-help app-state/close-hotkey-help]
  [ids/open-icon-help app-state/open-icon-help]
  [ids/close-icon-help app-state/close-icon-help]
  [ids/close-help-dialogs app-state/close-help-dialogs]])
