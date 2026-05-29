(ns gnostica.game-state.major-power
  (:require [gnostica.game-state.core :as core]))

(defmulti apply-card-power
  (fn [_state _command card _opts]
    (:id card)))

(defmethod apply-card-power :default
  [_state command card _opts]
  (core/failure :major-power-unavailable
                "The revealed major card does not have an implemented full-card power."
                {:card-id (:id card)
                 :power (:power command)}))
