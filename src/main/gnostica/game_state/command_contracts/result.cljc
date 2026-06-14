(ns gnostica.game-state.command-contracts.result
  (:require [gnostica.game-state.command-contracts.primitives :as primitives]
            [malli.core :as m]
            [malli.error :as me]))

(def ErrorShape
  [:map
   [:code :keyword]
   [:message primitives/NonBlankString]
   [:data :any]])

(def StateSuccessResult
  (primitives/closed-map
   [:ok? [:= true]]
   [:state :map]
   [:events [:vector :map]]))

(def CommandValidationSuccessResult
  (primitives/closed-map
   [:ok? [:= true]]
   [:command :map]))

(def SuccessResult
  [:or StateSuccessResult CommandValidationSuccessResult])

(def FailureResult
  (primitives/closed-map
   [:ok? [:= false]]
   [:error ErrorShape]))

(def Result
  [:or SuccessResult FailureResult])

(defn valid-result? [result]
  (m/validate Result result))

(defn explain-result [result]
  (when-let [explanation (m/explain Result result)]
    {:message "Result failed the game-state structured result contract."
     :errors (me/humanize explanation)
     :explanation explanation}))
