(ns gnostica.game-state.result
  (:require [gnostica.game-state.score :as score]))

(defn success
  ([state]
   (success state []))
  ([state events]
   {:ok? true
    :state (score/with-current-scores state)
    :events (vec events)}))

(defn failure [code message data]
  {:ok? false
   :error {:code code
           :message message
           :data data}})
