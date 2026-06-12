(ns gnostica.app.registrations.subscriptions.helpers
  (:require [re-frame.core :as rf]))

(defn register-db-subscriptions!
  [subscription-handlers]
  (doseq [[subscription-id handler-fn] subscription-handlers]
    (rf/reg-sub
     subscription-id
     (fn [db _]
       (handler-fn db)))))
