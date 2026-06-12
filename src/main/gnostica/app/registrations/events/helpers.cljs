(ns gnostica.app.registrations.events.helpers
  (:require [re-frame.core :as rf]))

(defn- db-event
  [handler-fn]
  (fn [db [_ & args]]
    (apply handler-fn db args)))

(defn register-db-events!
  [event-handlers]
  (doseq [[event-id handler-fn] event-handlers]
    (rf/reg-event-db event-id (db-event handler-fn))))
