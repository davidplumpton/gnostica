(ns gnostica.test-support.game-state-moves
  (:require [gnostica.game-state :as game-state]
            [gnostica.test-support.game-state :refer [state-with-board-cards]]))

(def rose-cup-minion
  {:id :rose-cup-minion
   :player-id :rose
   :space-index 3
   :size :small
   :orientation :east})

(def rose-cup-wasteland-minion
  (assoc rose-cup-minion :orientation :west))

(def rose-rod-minion
  {:id :rose-rod-minion
   :player-id :rose
   :space-index 3
   :size :medium
   :orientation :east})

(def rose-disc-minion
  {:id :rose-disc-minion
   :player-id :rose
   :space-index 3
   :size :medium
   :orientation :east})

(def rose-sword-minion
  {:id :rose-sword-minion
   :player-id :rose
   :space-index 3
   :size :medium
   :orientation :east})

(def rose-target-minion
  {:id :rose-target-minion
   :player-id :rose
   :space-index 4
   :size :medium
   :orientation :up})

(def indigo-cup-target
  {:id :indigo-cup-target
   :player-id :indigo
   :space-index 4
   :size :medium
   :orientation :west})

(defn rose-nine-point-challenge-state
  ([] (rose-nine-point-challenge-state {}))
  ([opts]
   (-> (state-with-board-cards {0 "cups2"
                                1 "cupsking"
                                2 "sun"
                                3 "magician"
                                4 "wheeloffortune"}
                               opts)
       (game-state/with-board-pieces
        [{:id :rose-spot
          :player-id :rose
          :space-index 0
          :size :small
          :orientation :north}
         {:id :rose-royalty
          :player-id :rose
          :space-index 1
          :size :small
          :orientation :north}
         {:id :rose-major-a
          :player-id :rose
          :space-index 2
          :size :small
          :orientation :north}
         {:id :rose-major-b
          :player-id :rose
          :space-index 3
          :size :small
          :orientation :north}
         {:id :indigo-piece
          :player-id :indigo
          :space-index 4
          :size :small
          :orientation :north}]))))

(defn resolve-rose-challenge [state]
  (let [announced (game-state/end-turn state {:player-id :rose
                                              :announce-challenge? true})
        indigo-ended (game-state/end-turn (:state announced) {:player-id :indigo})]
    (game-state/end-turn (:state indigo-ended) {:player-id :rose})))
