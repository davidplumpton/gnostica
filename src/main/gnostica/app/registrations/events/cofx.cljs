(ns gnostica.app.registrations.events.cofx
  (:require [gnostica.app.ids :as ids]
            [gnostica.three-board.runtime :as three-runtime]
            [re-frame.core :as rf]))

(def ^:private max-browser-seed 4294967296)

(defn- browser-shuffle-seed []
  (js/Math.floor (* max-browser-seed (js/Math.random))))

(rf/reg-cofx
 ids/shuffle-seed
 (fn [coeffects _]
   (assoc coeffects ids/shuffle-seed (browser-shuffle-seed))))

(rf/reg-cofx
 ids/three-runtime-detection
 (fn [coeffects _]
   (assoc coeffects ids/three-runtime-status (three-runtime/runtime-status))))
