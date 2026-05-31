(ns gnostica.app.subscriptions
  (:require [gnostica.app-state :as app-state]))

(defn move-panel-view
  [db _query-v]
  (app-state/move-panel-view db))
