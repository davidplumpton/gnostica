(ns gnostica.feature-steps
  (:require [gnostica.board :as board]
            [gnostica.feature-runner :as feature-runner]
            [gnostica.feature-world :as world]))

(defn- parse-int [value]
  (Integer/parseInt value))

(defn- expect [world ok? code message data]
  (if ok?
    world
    (feature-runner/fail code
                         message
                         (assoc data :setup-summary (world/setup-summary world)))))

(def steps
  [{:pattern #"^a deterministic game with (\d+) players$"
    :run (fn [world player-count]
           (let [player-count (parse-int player-count)
                 world (world/create-deterministic-game world player-count)
                 result (:last-result world)]
             (expect world
                     (:ok? result)
                     :game-creation-failed
                     "The deterministic game could not be created."
                     {:player-count player-count
                      :error (:error result)})))}

   {:pattern #"^the game state is schema valid$"
    :run (fn [world]
           (if-let [explanation (world/schema-explanation world)]
             (feature-runner/fail :invalid-game-schema
                                  "Game state failed Malli validation."
                                  {:schema-explanation explanation
                                   :setup-summary (world/setup-summary world)})
             world))}

   {:pattern #"^there are (\d+) players$"
    :run (fn [world expected-count]
           (let [expected-count (parse-int expected-count)
                 actual-count (count (world/players world))]
             (expect world
                     (= expected-count actual-count)
                     :unexpected-player-count
                     "The game has a different player count than expected."
                     {:expected expected-count
                      :actual actual-count})))}

   {:pattern #"^each player has (\d+) cards in hand$"
    :run (fn [world expected-count]
           (let [expected-count (parse-int expected-count)
                 hand-sizes (world/hand-sizes world)]
             (expect world
                     (every? #(= expected-count %) hand-sizes)
                     :unexpected-hand-size
                     "At least one player hand has a different card count than expected."
                     {:expected expected-count
                      :actual hand-sizes})))}

   {:pattern #"^the board has (\d+) face-up territory cards$"
    :run (fn [world expected-count]
           (let [expected-count (parse-int expected-count)
                 board-count (count (world/board-cells world))
                 face-up-count (world/face-up-board-card-count world)]
             (expect world
                     (and (= board/board-card-count board-count)
                          (= expected-count board-count face-up-count))
                     :unexpected-board-card-count
                     "The board does not have the expected face-up territory cards."
                     {:expected expected-count
                      :actual-board-count board-count
                      :actual-face-up-count face-up-count})))}

   {:pattern #"^every tarot card is accounted for exactly once$"
    :run (fn [world]
           (expect world
                   (world/complete-deck-accounting? world)
                   :incomplete-deck-accounting
                   "The game state does not account for every tarot card exactly once."
                   (world/deck-accounting world)))}])
