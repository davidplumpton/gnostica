(ns gnostica.game-state.constants)

(def min-players 2)

(def max-players 6)

(def starting-hand-size 6)

(def pieces-per-size-in-stash 5)

(def initial-phase :setup)

(def finished-phase :finished)

(def default-target-score 9)

(def allowed-target-scores #{8 9 10})

(def required-player-fields [:id :name :color :css-color])

(def required-card-fields [:id :title :image])
