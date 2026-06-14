(ns gnostica.game-state.sources
  (:require [gnostica.game-state.hands :as hands]))

(defn source-summary [source]
  (select-keys source [:kind :board-index :card-id :piece-id]))

(defn apply-source-cost [state player-id {:keys [source-card discard-source-card?]}]
  (if discard-source-card?
    (-> state
        (hands/remove-card-from-hand player-id (:id source-card))
        (hands/discard-card source-card))
    state))
