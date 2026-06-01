(ns gnostica.app-state.moves
  (:require [gnostica.app-state.db :as db]
            [gnostica.app-state.view-models :as views]
            [gnostica.game-state :as game-state]
            [gnostica.gesture-intent :as gesture-intent]
            [gnostica.move-selection :as move-selection]))

(def move-target-wasteland-options move-selection/move-target-wasteland-options)
(def turn-action-consumed? move-selection/turn-action-consumed?)
(def max-draw-count move-selection/max-draw-count)
(def draw-count-options move-selection/draw-count-options)
(def move-source-options move-selection/move-source-options)
(def move-selection move-selection/move-selection)
(def move-params move-selection/move-params)
(def move-control-groups move-selection/move-control-groups)
(def move-action-ribbon move-selection/move-action-ribbon)
(def move-power-options move-selection/move-power-options)
(def move-power move-selection/move-power)
(def move-world-copy-options move-selection/move-world-copy-options)
(def move-world-copied-power-options
  move-selection/move-world-copied-power-options)
(def move-world-copied-power move-selection/move-world-copied-power)
(def move-rod-mode-options move-selection/move-rod-mode-options)
(def move-disc-action-count-options move-selection/move-disc-action-count-options)
(def move-major-action-count-options move-selection/move-major-action-count-options)
(def move-major-action-count move-selection/move-major-action-count)
(def move-sword-action-count-options move-selection/move-sword-action-count-options)
(def move-devil-action-count-options move-selection/move-devil-action-count-options)
(def move-sun-disc-mode-options move-selection/move-sun-disc-mode-options)
(def move-fool-reveal-count-options move-selection/move-fool-reveal-count-options)
(def move-fool-play-power-options move-selection/move-fool-play-power-options)
(def move-fool-play-power move-selection/move-fool-play-power)
(def move-fool-reveal-state move-selection/move-fool-reveal-state)
(def move-high-priestess-redraw-count-options
  move-selection/move-high-priestess-redraw-count-options)
(def move-high-priestess-redraw-options
  move-selection/move-high-priestess-redraw-options)
(def move-judgement-card-options move-selection/move-judgement-card-options)
(def move-judgement-card-maximum move-selection/move-judgement-card-maximum)
(def move-disc-minion-orientation-required?
  move-selection/move-disc-minion-orientation-required?)
(def move-disc-target-kind-options move-selection/move-disc-target-kind-options)
(def move-sword-target-kind-options move-selection/move-sword-target-kind-options)
(def move-legal-targets move-selection/move-legal-targets)
(def move-preview move-selection/move-preview)
(def move-distance-options move-selection/move-distance-options)
(def move-damage-options move-selection/move-damage-options)
(def move-target-piece-options move-selection/move-target-piece-options)
(def move-rod-orientation-required?
  move-selection/move-rod-orientation-required?)
(def move-disc-orientation-available?
  move-selection/move-disc-orientation-available?)
(def move-sun-disc-orientation-available?
  move-selection/move-sun-disc-orientation-available?)
(def move-sword-orientation-available?
  move-selection/move-sword-orientation-available?)
(def move-hermit-orientation-required?
  move-selection/move-hermit-orientation-required?)
(def move-ready? move-selection/move-ready?)
(def move-prompt move-selection/move-prompt)
(def select-move-source move-selection/select-move-source)
(def select-board-for-active-move move-selection/select-board-for-active-move)
(def select-move-wasteland-target move-selection/select-move-wasteland-target)
(def select-move-piece move-selection/select-move-piece)
(def select-move-hand-card move-selection/select-move-hand-card)
(def select-move-power move-selection/select-move-power)
(def select-move-world-copy move-selection/select-move-world-copy)
(def select-move-rod-mode move-selection/select-move-rod-mode)
(def select-move-disc-target-kind move-selection/select-move-disc-target-kind)
(def select-move-sword-target-kind move-selection/select-move-sword-target-kind)
(def set-move-disc-action-count move-selection/set-move-disc-action-count)
(def set-move-major-action-count move-selection/set-move-major-action-count)
(def set-move-sword-action-count move-selection/set-move-sword-action-count)
(def set-move-devil-action-count move-selection/set-move-devil-action-count)
(def set-move-fool-reveal-count move-selection/set-move-fool-reveal-count)
(def reveal-move-fool-card move-selection/reveal-move-fool-card)
(def skip-move-fool-reveal move-selection/skip-move-fool-reveal)
(def play-move-fool-reveal move-selection/play-move-fool-reveal)
(def select-move-fool-play-power move-selection/select-move-fool-play-power)
(def set-move-high-priestess-redraw-count
  move-selection/set-move-high-priestess-redraw-count)
(def toggle-move-high-priestess-discard-card
  move-selection/toggle-move-high-priestess-discard-card)
(def set-move-high-priestess-draw-count
  move-selection/set-move-high-priestess-draw-count)
(def toggle-move-judgement-card move-selection/toggle-move-judgement-card)
(def set-move-minion-orientation move-selection/set-move-minion-orientation)
(def select-move-sun-disc-mode move-selection/select-move-sun-disc-mode)
(def set-move-sun-disc-orientation move-selection/set-move-sun-disc-orientation)
(def select-move-target-piece move-selection/select-move-target-piece)
(def select-move-territory-card-source
  move-selection/select-move-territory-card-source)
(def select-move-one-point-card move-selection/select-move-one-point-card)
(def select-move-replacement-card move-selection/select-move-replacement-card)
(def set-move-orientation move-selection/set-move-orientation)
(def set-move-draw-count move-selection/set-move-draw-count)
(def toggle-move-discard-card move-selection/toggle-move-discard-card)
(def set-move-distance move-selection/set-move-distance)
(def set-move-damage move-selection/set-move-damage)
(def move-piece-options move-selection/move-piece-options)
(def move-hand-card-options move-selection/move-hand-card-options)
(def move-discard-card-options move-selection/move-discard-card-options)
(def move-source-board-options move-selection/move-source-board-options)
(def move-target-board-options move-selection/move-target-board-options)
(def move-one-point-card-options move-selection/move-one-point-card-options)
(def move-territory-card-source-options
  move-selection/move-territory-card-source-options)
(def move-replacement-card-options move-selection/move-replacement-card-options)
(def move-orientation-options move-selection/move-orientation-options)
(def move-command move-selection/move-command)

(defn- apply-end-turn-result [app-db result]
  (if (:ok? result)
    (-> app-db
        (assoc :game (:state result)
               :turn-result result
               :move-selection (assoc (db/empty-move-selection)
                                      :last-result result))
        (dissoc :turn-action))
    (assoc app-db :turn-result result)))

(defn end-turn
  ([app-db]
   (end-turn app-db {}))
  ([app-db command]
   (let [player-id (or (:player-id command)
                       (views/current-player-id app-db))
         result (if-let [state (db/game app-db)]
                  (game-state/end-turn
                   state
                   (assoc command :player-id player-id))
                  (game-state/failure :missing-game
                                      "Cannot end a turn before a game has started."
                                      {}))]
     (apply-end-turn-result app-db result))))

(defn confirm-move
  ([app-db] (confirm-move app-db {}))
  ([app-db transition-options]
   (let [confirmed-db (move-selection/confirm-move app-db transition-options)]
     (if (true? (get-in confirmed-db [:move-selection :last-result :ok?]))
       (gesture-intent/cancel-gesture-intent confirmed-db)
       (gesture-intent/refresh-gesture-intent confirmed-db)))))
