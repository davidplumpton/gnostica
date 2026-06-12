(ns gnostica.app.registrations.events.move
  (:require [gnostica.app.handlers :as handlers]
            [gnostica.app.ids :as ids]
            [gnostica.app.registrations.events.cofx]
            [gnostica.app.registrations.events.helpers :as helpers]
            [gnostica.app-state :as app-state]
            [re-frame.core :as rf]))

(helpers/register-db-events!
 [[ids/select-move-source app-state/select-move-source]
  [ids/select-move-piece app-state/select-move-piece]
  [ids/select-move-hand-card app-state/select-move-hand-card]
  [ids/select-move-wasteland-target app-state/select-move-wasteland-target]
  [ids/select-move-one-point-card app-state/select-move-one-point-card]
  [ids/select-move-territory-card-source app-state/select-move-territory-card-source]
  [ids/select-move-replacement-card app-state/select-move-replacement-card]
  [ids/select-move-power app-state/select-move-power]
  [ids/select-move-world-copy app-state/select-move-world-copy]
  [ids/select-move-rod-mode app-state/select-move-rod-mode]
  [ids/select-move-disc-target-kind app-state/select-move-disc-target-kind]
  [ids/select-move-sword-target-kind app-state/select-move-sword-target-kind]
  [ids/set-move-disc-action-count app-state/set-move-disc-action-count]
  [ids/set-move-major-action-count app-state/set-move-major-action-count]
  [ids/set-move-sword-action-count app-state/set-move-sword-action-count]
  [ids/set-move-devil-action-count app-state/set-move-devil-action-count]
  [ids/set-move-fool-reveal-count app-state/set-move-fool-reveal-count]
  [ids/skip-move-fool-reveal app-state/skip-move-fool-reveal]
  [ids/play-move-fool-reveal app-state/play-move-fool-reveal]
  [ids/select-move-fool-play-power app-state/select-move-fool-play-power]
  [ids/set-move-high-priestess-redraw-count app-state/set-move-high-priestess-redraw-count]
  [ids/toggle-move-high-priestess-discard-card app-state/toggle-move-high-priestess-discard-card]
  [ids/set-move-high-priestess-draw-count app-state/set-move-high-priestess-draw-count]
  [ids/toggle-move-judgement-card app-state/toggle-move-judgement-card]
  [ids/set-move-minion-orientation app-state/set-move-minion-orientation]
  [ids/select-move-sun-disc-mode app-state/select-move-sun-disc-mode]
  [ids/set-move-sun-disc-orientation app-state/set-move-sun-disc-orientation]
  [ids/select-move-target-piece app-state/select-move-target-piece]
  [ids/set-move-orientation app-state/set-move-orientation]
  [ids/set-move-distance app-state/set-move-distance]
  [ids/set-move-damage app-state/set-move-damage]
  [ids/set-move-draw-count app-state/set-move-draw-count]
  [ids/toggle-move-discard-card app-state/toggle-move-discard-card]
  [ids/cancel-move app-state/cancel-move]])

(rf/reg-event-fx
 ids/reveal-move-fool-card
 [(rf/inject-cofx ids/shuffle-seed)]
 (fn [coeffects _]
   {:db (handlers/reveal-move-fool-card-db
         (:db coeffects)
         {:shuffle-seed (get coeffects ids/shuffle-seed)})}))

(rf/reg-event-fx
 ids/confirm-move
 [(rf/inject-cofx ids/shuffle-seed)]
 (fn [coeffects _]
   {:db (handlers/confirm-move-db (:db coeffects)
                                  {:shuffle-seed (get coeffects ids/shuffle-seed)})}))
