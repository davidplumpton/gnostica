(ns gnostica.app-state.lobby
  (:require [gnostica.app-state.lobby-bidding :as bidding]
            [gnostica.app-state.lobby-setup :as setup]
            [gnostica.app-state.lobby-start :as start]
            [gnostica.app-state.lobby-view-models :as view]))

(def lobby setup/lobby)
(def lobby-active? setup/lobby-active?)
(def lobby-players setup/lobby-players)
(def lobby-validation-error setup/lobby-validation-error)
(def lobby-valid? setup/lobby-valid?)
(def create-lobby setup/create-lobby)
(def add-lobby-player setup/add-lobby-player)
(def remove-lobby-player setup/remove-lobby-player)
(def set-lobby-player-name setup/set-lobby-player-name)
(def set-lobby-player-colour setup/set-lobby-player-colour)
(def set-lobby-target-score setup/set-lobby-target-score)

(def start-lobby-bidding start/start-lobby-bidding)
(def select-lobby-bid-card bidding/select-lobby-bid-card)
(def reveal-lobby-bids bidding/reveal-lobby-bids)
(def select-lobby-redraw-card bidding/select-lobby-redraw-card)
(def confirm-lobby-bidding start/confirm-lobby-bidding)
(def cancel-lobby-bidding bidding/cancel-lobby-bidding)
(def start-lobby-game start/start-lobby-game)

(def lobby-view-model view/lobby-view-model)
(def lobby-view view/lobby-view)
