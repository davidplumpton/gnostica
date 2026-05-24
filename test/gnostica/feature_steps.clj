(ns gnostica.feature-steps
  (:require [gnostica.board :as board]
            [gnostica.feature-runner :as feature-runner]
            [gnostica.feature-world :as world]))

(defn- parse-int [value]
  (Integer/parseInt value))

(defn- parse-keyword [value]
  (keyword value))

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
                   (world/deck-accounting world)))}

   {:pattern #"^a Rod territory-source game with Rose's medium minion at board index (\d+) facing ([a-z]+)$"
    :run (fn [world board-index orientation]
           (world/create-rod-territory-source-game world
                                                   (parse-int board-index)
                                                   (parse-keyword orientation)))}

   {:pattern #"^a Rod hand-card game with Rose's medium minion at board index (\d+) facing ([a-z]+) and an Indigo target at board index (\d+) facing ([a-z]+)$"
    :run (fn [world minion-board-index minion-orientation target-board-index target-orientation]
           (world/create-rod-hand-card-piece-game world
                                                  (parse-int minion-board-index)
                                                  (parse-keyword minion-orientation)
                                                  (parse-int target-board-index)
                                                  (parse-keyword target-orientation)))}

   {:pattern #"^a Rod territory-push game with Rose's minion at board index (\d+) facing ([a-z]+), a Rose piece on target board index (\d+), and a Rose piece in landing wasteland row (-?\d+) col (-?\d+)$"
    :run (fn [world minion-board-index minion-orientation target-board-index landing-row landing-col]
           (world/create-rod-territory-push-own-landing-game world
                                                             (parse-int minion-board-index)
                                                             (parse-keyword minion-orientation)
                                                             (parse-int target-board-index)
                                                             (parse-int landing-row)
                                                             (parse-int landing-col)))}

   {:pattern #"^a Rod full-destination game$"
    :run world/create-rod-full-destination-game}

   {:pattern #"^a Rod enemy-occupied territory-push game$"
    :run world/create-rod-enemy-territory-push-game}

   {:pattern #"^a Rod enemy-occupied landing-wasteland game$"
    :run world/create-rod-enemy-landing-territory-push-game}

   {:pattern #"^Rose moves the Rod minion (\d+) space(?:s)?$"
    :run (fn [world distance]
           (world/apply-rod-minion-move world (parse-int distance) nil))}

   {:pattern #"^Rose moves the Rod minion (\d+) space(?:s)? with orientation ([a-z]+)$"
    :run (fn [world distance orientation]
           (world/apply-rod-minion-move world
                                        (parse-int distance)
                                        (parse-keyword orientation)))}

   {:pattern #"^Rose pushes the Indigo piece (\d+) space(?:s)?$"
    :run (fn [world distance]
           (world/apply-rod-piece-push world (parse-int distance) nil))}

   {:pattern #"^Rose tries to push the Indigo piece (\d+) space(?:s)? with orientation ([a-z]+)$"
    :run (fn [world distance orientation]
           (world/apply-rod-piece-push world
                                       (parse-int distance)
                                       (parse-keyword orientation)))}

   {:pattern #"^Rose pushes the target territory (\d+) space(?:s)?$"
    :run (fn [world distance]
           (world/apply-rod-territory-push world (parse-int distance)))}

   {:pattern #"^the Rod action succeeds$"
    :run (fn [world]
           (let [result (:last-result world)]
             (expect world
                     (:ok? result)
                     :rod-action-failed
                     "The Rod action was expected to succeed."
                     {:error (:error result)
                      :last-action (:last-action world)})))}

   {:pattern #"^the Rod action is rejected with code :([a-z0-9-]+)$"
    :run (fn [world expected-code]
           (let [expected-code (parse-keyword expected-code)
                 result (:last-result world)
                 actual-code (get-in result [:error :code])]
             (expect world
                     (and (false? (:ok? result))
                          (= expected-code actual-code))
                     :unexpected-rod-rejection
                     "The Rod action rejection did not match the expected error code."
                     {:expected expected-code
                      :actual actual-code
                      :result result})))}

   {:pattern #"^the previous state was not mutated$"
    :run (fn [world]
           (expect world
                   (and (= (:previous-state world) (:state world))
                        (not (contains? (:last-result world) :state)))
                   :state-mutated-after-rejection
                   "A rejected action should not mutate the active game state."
                   {:previous-state (:previous-state world)
                    :current-state (:state world)
                    :result (:last-result world)}))}

   {:pattern #"^piece ([a-z0-9-]+) is on board index (\d+) facing ([a-z]+)$"
    :run (fn [world piece-id board-index orientation]
           (let [piece-id (parse-keyword piece-id)
                 board-index (parse-int board-index)
                 orientation (parse-keyword orientation)
                 piece (world/piece-by-id world piece-id)]
             (expect world
                     (and (= board-index (:space-index piece))
                          (= orientation (:orientation piece))
                          (not (contains? piece :space)))
                     :unexpected-piece-board-space
                     "The piece is not on the expected board space with the expected orientation."
                     {:piece-id piece-id
                      :expected {:space-index board-index
                                 :orientation orientation}
                      :actual piece})))}

   {:pattern #"^piece ([a-z0-9-]+) is in wasteland row (-?\d+) col (-?\d+) facing ([a-z]+)$"
    :run (fn [world piece-id row col orientation]
           (let [piece-id (parse-keyword piece-id)
                 row (parse-int row)
                 col (parse-int col)
                 orientation (parse-keyword orientation)
                 piece (world/piece-by-id world piece-id)]
             (expect world
                     (and (= {:kind :wasteland
                              :row row
                              :col col}
                             (:space piece))
                          (= orientation (:orientation piece)))
                     :unexpected-piece-wasteland-space
                     "The piece is not in the expected wasteland with the expected orientation."
                     {:piece-id piece-id
                      :expected {:space {:kind :wasteland
                                         :row row
                                         :col col}
                                 :orientation orientation}
                      :actual piece})))}

   {:pattern #"^board index (\d+) is at row (-?\d+) col (-?\d+) with orientation ([a-z]+)$"
    :run (fn [world board-index row col orientation]
           (let [board-index (parse-int board-index)
                 row (parse-int row)
                 col (parse-int col)
                 orientation (parse-keyword orientation)
                 cell (world/board-cell-by-index world board-index)]
             (expect world
                     (and (= row (:row cell))
                          (= col (:col cell))
                          (= orientation (:orientation cell)))
                     :unexpected-board-cell-position
                     "The territory cell is not at the expected coordinate or orientation."
                     {:board-index board-index
                      :expected {:row row
                                 :col col
                                 :orientation orientation}
                      :actual (select-keys cell [:index :row :col :orientation])})))}

   {:pattern #"^there is no territory at row (-?\d+) col (-?\d+)$"
    :run (fn [world row col]
           (let [row (parse-int row)
                 col (parse-int col)]
             (expect world
                     (nil? (world/board-cell-at world row col))
                     :unexpected-territory-at-coordinate
                     "A territory still occupies the coordinate that should be empty."
                     {:row row
                      :col col
                      :cell (world/board-cell-at world row col)})))}

   {:pattern #"^the discard pile contains exactly \"([^\"]+)\"$"
    :run (fn [world card-id]
           (expect world
                   (= [card-id] (world/discard-ids world))
                   :unexpected-discard-pile
                   "The discard pile does not contain the expected single card."
                   {:expected [card-id]
                    :actual (world/discard-ids world)}))}

   {:pattern #"^Rose no longer has \"([^\"]+)\" in hand$"
    :run (fn [world card-id]
           (expect world
                   (not (some #{card-id} (world/player-hand-ids world :rose)))
                   :unexpected-card-in-hand
                   "Rose still has the card that should have been spent."
                   {:card-id card-id
                    :hand (world/player-hand-ids world :rose)}))}

   {:pattern #"^the history records a :([a-z/-]+) event$"
    :run (fn [world event-type]
           (let [event-type (parse-keyword event-type)
                 event (peek (world/state-at world [:history]))]
             (expect world
                     (= event-type (:type event))
                     :unexpected-history-event
                     "The latest history event was not the expected Rod event."
                     {:expected event-type
                      :actual (:type event)
                      :event event})))}])
