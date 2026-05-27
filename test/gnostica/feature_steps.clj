(ns gnostica.feature-steps
  (:require [clojure.string :as str]
            [gnostica.board :as board]
            [gnostica.feature-runner :as feature-runner]
            [gnostica.feature-world :as world]))

(defn- parse-int [value]
  (Integer/parseInt value))

(defn- parse-keyword [value]
  (keyword value))

(defn- parse-player-id [value]
  (keyword (str/lower-case value)))

(defn- parse-quoted-values [value]
  (mapv second (re-seq #"\"([^\"]+)\"" value)))

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

   {:pattern #"^an official starting-bid game with a tied minor bid and Gold winning the rebid$"
    :run (fn [world]
           (let [world (world/create-official-starting-bid-game world)
                 result (:last-result world)]
             (expect world
                     (:ok? result)
                     :game-creation-failed
                     "The official starting-bid game could not be created."
                     {:error (:error result)})))}

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

   {:pattern #"^([A-Z][a-z]+) is the starting player$"
    :run (fn [world player-name]
           (let [player-id (parse-player-id player-name)
                 actual-starting-player-id (world/starting-player-id world)
                 actual-current-player-id (world/state-at world [:turn :current-player-id])]
             (expect world
                     (and (= player-id actual-starting-player-id)
                          (= player-id actual-current-player-id))
                     :unexpected-starting-player
                     "The starting player did not match."
                     {:expected player-id
                      :actual-starting-player-id actual-starting-player-id
                      :actual-current-player-id actual-current-player-id})))}

   {:pattern #"^the starting bid history has (\d+) rounds$"
    :run (fn [world expected-count]
           (let [expected-count (parse-int expected-count)
                 actual-count (count (world/bid-history world))]
             (expect world
                     (= expected-count actual-count)
                     :unexpected-bid-round-count
                     "The starting bid history has a different round count than expected."
                     {:expected expected-count
                      :actual actual-count
                      :bid-history (world/bid-history world)})))}

   {:pattern #"^the bid redraw order is (.+)$"
    :run (fn [world player-names]
           (let [expected-order (mapv parse-player-id (parse-quoted-values player-names))
                 actual-order (world/bid-redraw-order world)]
             (expect world
                     (= expected-order actual-order)
                     :unexpected-bid-redraw-order
                     "The bid redraw order did not match."
                     {:expected expected-order
                      :actual actual-order})))}

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

   {:pattern #"^a Rod unbounded full-destination game$"
    :run world/create-rod-unbounded-full-destination-game}

   {:pattern #"^a Rod enemy-occupied territory-push game$"
    :run world/create-rod-enemy-territory-push-game}

   {:pattern #"^a Rod enemy-occupied landing-wasteland game$"
    :run world/create-rod-enemy-landing-territory-push-game}

   {:pattern #"^a Disc territory-source game with Rose's ([a-z]+) minion at board index (\d+) facing ([a-z]+)$"
    :run (fn [world size board-index orientation]
           (world/create-disc-territory-source-piece-game
            world
            (parse-keyword size)
            (parse-int board-index)
            (parse-keyword orientation)))}

   {:pattern #"^a Disc hand-card game with Rose's medium minion at board index (\d+) facing ([a-z]+) and an Indigo target at board index (\d+) facing ([a-z]+)$"
    :run (fn [world minion-board-index minion-orientation target-board-index target-orientation]
           (world/create-disc-hand-card-piece-game
            world
            (parse-int minion-board-index)
            (parse-keyword minion-orientation)
            (parse-int target-board-index)
            (parse-keyword target-orientation)))}

   {:pattern #"^a Disc hand-card territory-growth game$"
    :run world/create-disc-hand-card-territory-growth-game}

   {:pattern #"^a Disc territory-source territory-growth game$"
    :run world/create-disc-territory-source-territory-growth-game}

   {:pattern #"^a Disc enemy-occupied territory-growth game$"
    :run world/create-disc-enemy-occupied-territory-growth-game}

   {:pattern #"^a Disc no-medium-stash game with Rose's small minion at board index (\d+) facing ([a-z]+)$"
    :run (fn [world board-index orientation]
           (world/create-disc-no-medium-stash-game
            world
            (parse-int board-index)
            (parse-keyword orientation)))}

   {:pattern #"^a Star hand-card territory-growth game$"
    :run world/create-star-disc-territory-growth-game}

   {:pattern #"^a Strength Disc shortcut game with Rose's small minion at board index (\d+) facing ([a-z]+)$"
    :run (fn [world board-index orientation]
           (world/create-strength-disc-shortcut-game
            world
            (parse-int board-index)
            (parse-keyword orientation)))}

   {:pattern #"^a Fool hand-card reveal game$"
    :run world/create-fool-hand-card-reveal-game}

   {:pattern #"^a High Priestess hand-card redraw game$"
    :run world/create-high-priestess-hand-card-redraw-game}

   {:pattern #"^a Judgement hand-card limit game$"
    :run world/create-judgement-hand-card-limit-game}

   {:pattern #"^a Hierophant hand-card replacement game$"
    :run world/create-hierophant-hand-card-game}

   {:pattern #"^a Hermit hand-card piece-relocation game$"
    :run world/create-hermit-hand-card-piece-game}

   {:pattern #"^a Hermit hand-card territory-relocation game$"
    :run world/create-hermit-hand-card-territory-game}

   {:pattern #"^a Devil territory-source retargeting game$"
    :run world/create-devil-territory-source-retargeting-game}

   {:pattern #"^an endgame challenge game where Rose controls 9 points$"
    :run world/create-endgame-winning-challenge-game}

   {:pattern #"^an endgame challenge game where Rose controls 3 points$"
    :run world/create-endgame-failing-challenge-game}

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

   {:pattern #"^Rose grows the Rose Disc piece$"
    :run (fn [world]
           (world/apply-disc-piece-growth world nil))}

   {:pattern #"^Rose grows the Rose Disc piece with orientation ([a-z]+)$"
    :run (fn [world orientation]
           (world/apply-disc-piece-growth world (parse-keyword orientation)))}

   {:pattern #"^Rose grows the Indigo Disc piece$"
    :run (fn [world]
           (world/apply-disc-piece-growth world nil))}

   {:pattern #"^Rose tries to grow the Indigo Disc piece with orientation ([a-z]+)$"
    :run (fn [world orientation]
           (world/apply-disc-piece-growth world (parse-keyword orientation)))}

   {:pattern #"^Rose grows the target territory without a replacement$"
    :run (fn [world]
           (world/apply-disc-territory-growth world nil nil))}

   {:pattern #"^Rose grows the target territory with hand replacement \"([^\"]+)\"$"
    :run (fn [world replacement-card-id]
           (world/apply-disc-territory-growth world :hand replacement-card-id))}

   {:pattern #"^Rose grows the target territory from discard replacement \"([^\"]+)\"$"
    :run (fn [world replacement-card-id]
           (world/apply-disc-territory-growth world :discard-pile replacement-card-id))}

   {:pattern #"^Rose uses Strength to grow the Rose Disc piece twice$"
    :run world/apply-strength-disc-piece-shortcut}

   {:pattern #"^Rose uses Fool to reveal (\d+) cards without playing them$"
    :run (fn [world reveal-count]
           (world/apply-fool-skip-reveals world (parse-int reveal-count)))}

   {:pattern #"^Rose uses High Priestess for two redraw passes$"
    :run world/apply-high-priestess-redraws}

   {:pattern #"^Rose tries to draw too many cards with Judgement$"
    :run world/apply-judgement-over-hand-limit}

   {:pattern #"^Rose replaces the targeted piece with Hierophant facing ([a-z]+)$"
    :run (fn [world orientation]
           (world/apply-hierophant-replacement world
                                                (parse-keyword orientation)))}

   {:pattern #"^Rose moves the Hermit target piece to board index (\d+)$"
    :run (fn [world board-index]
           (world/apply-hermit-piece-relocation world
                                                (parse-int board-index)))}

   {:pattern #"^Rose moves the Hermit target territory to wasteland row (-?\d+) col (-?\d+)$"
    :run (fn [world row col]
           (world/apply-hermit-territory-relocation world
                                                    (parse-int row)
                                                    (parse-int col)))}

   {:pattern #"^Rose uses Devil to orient the minion and then the enemy target$"
    :run world/apply-devil-retargeting}

   {:pattern #"^Rose announces a final-turn challenge$"
    :run (fn [world]
           (world/apply-end-turn world :rose true))}

   {:pattern #"^([A-Z][a-z]+) ends the turn$"
    :run (fn [world player-name]
           (world/apply-end-turn world (parse-player-id player-name)))}

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

   {:pattern #"^the Disc action succeeds$"
    :run (fn [world]
           (let [result (:last-result world)]
             (expect world
                     (:ok? result)
                     :disc-action-failed
                     "The Disc action was expected to succeed."
                     {:error (:error result)
                      :last-action (:last-action world)})))}

   {:pattern #"^the Disc action is rejected with code :([a-z0-9-]+)$"
    :run (fn [world expected-code]
           (let [expected-code (parse-keyword expected-code)
                 result (:last-result world)
                 actual-code (get-in result [:error :code])]
             (expect world
                     (and (false? (:ok? result))
                          (= expected-code actual-code))
                     :unexpected-disc-rejection
                     "The Disc action rejection did not match the expected error code."
                     {:expected expected-code
                     :actual actual-code
                     :result result})))}

   {:pattern #"^the draw-major action succeeds$"
    :run (fn [world]
           (let [result (:last-result world)]
             (expect world
                     (:ok? result)
                     :draw-major-action-failed
                     "The draw-major action was expected to succeed."
                     {:error (:error result)
                      :last-action (:last-action world)})))}

   {:pattern #"^the draw-major action is rejected with code :([a-z0-9-]+)$"
    :run (fn [world expected-code]
           (let [expected-code (parse-keyword expected-code)
                 result (:last-result world)
                 actual-code (get-in result [:error :code])]
             (expect world
                     (and (false? (:ok? result))
                          (= expected-code actual-code))
                     :unexpected-draw-major-rejection
                     "The draw-major action rejection did not match the expected error code."
                     {:expected expected-code
                      :actual actual-code
                      :result result})))}

   {:pattern #"^the manipulation-major action succeeds$"
    :run (fn [world]
           (let [result (:last-result world)]
             (expect world
                     (:ok? result)
                     :manipulation-major-action-failed
                     "The manipulation-major action was expected to succeed."
                     {:error (:error result)
                      :last-action (:last-action world)})))}

   {:pattern #"^the endgame action succeeds$"
    :run (fn [world]
           (let [result (:last-result world)]
             (expect world
                     (:ok? result)
                     :endgame-action-failed
                     "The endgame action was expected to succeed."
                     {:error (:error result)
                      :last-action (:last-action world)})))}

   {:pattern #"^([A-Z][a-z]+) has score (\d+)$"
    :run (fn [world player-name expected-score]
           (let [player-id (parse-player-id player-name)
                 expected-score (parse-int expected-score)
                 actual-score (world/player-score world player-id)]
             (expect world
                     (= expected-score actual-score)
                     :unexpected-player-score
                     "The player score did not match."
                     {:player-id player-id
                      :expected expected-score
                      :actual actual-score})))}

   {:pattern #"^([A-Z][a-z]+) has an unresolved challenge$"
    :run (fn [world player-name]
           (let [player-id (parse-player-id player-name)]
             (expect world
                     (= player-id (world/active-challenge-player-id world))
                     :unexpected-active-challenge
                     "The active challenge player did not match."
                     {:expected player-id
                      :actual (world/active-challenge-player-id world)})))}

   {:pattern #"^([A-Z][a-z]+) is eliminated$"
    :run (fn [world player-name]
           (let [player-id (parse-player-id player-name)]
             (expect world
                     (world/player-eliminated? world player-id)
                     :player-not-eliminated
                     "The player was expected to be eliminated."
                     {:player-id player-id
                      :player (get-in (:state world) [:players-by-id player-id])})))}

   {:pattern #"^([A-Z][a-z]+) has no pieces on the board$"
    :run (fn [world player-name]
           (let [player-id (parse-player-id player-name)
                 piece-count (world/player-piece-count world player-id)]
             (expect world
                     (zero? piece-count)
                     :unexpected-player-pieces
                     "The player still has board pieces."
                     {:player-id player-id
                      :piece-count piece-count})))}

   {:pattern #"^([A-Z][a-z]+) has (\d+) cards in hand$"
    :run (fn [world player-name expected-count]
           (let [player-id (parse-player-id player-name)
                 expected-count (parse-int expected-count)
                 actual-count (count (world/player-hand-ids world player-id))]
             (expect world
                     (= expected-count actual-count)
                     :unexpected-player-hand-count
                     "The player hand count did not match."
                     {:player-id player-id
                      :expected expected-count
                      :actual actual-count
                      :hand (world/player-hand-ids world player-id)})))}

   {:pattern #"^the game winner is ([A-Z][a-z]+) by ([a-z-]+)$"
    :run (fn [world player-name reason]
           (let [expected {:player-id (parse-player-id player-name)
                           :reason (parse-keyword reason)}
                 actual (select-keys (world/winner world) [:player-id :reason])]
             (expect world
                     (= expected actual)
                     :unexpected-game-winner
                     "The winner did not match."
                     {:expected expected
                      :actual actual
                      :winner (world/winner world)})))}

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

   {:pattern #"^board index (\d+) contains card \"([^\"]+)\"$"
    :run (fn [world board-index card-id]
           (let [board-index (parse-int board-index)
                 cell (world/board-cell-by-index world board-index)]
             (expect world
                     (= card-id (get-in cell [:card :id]))
                     :unexpected-territory-card
                     "The territory does not contain the expected card."
                     {:board-index board-index
                      :expected card-id
                      :actual (get-in cell [:card :id])
                      :cell cell})))}

   {:pattern #"^the discard pile contains exactly (\"[^\"]+\"(?:, \"[^\"]+\")*)$"
    :run (fn [world card-ids]
           (let [expected-card-ids (parse-quoted-values card-ids)]
             (expect world
                     (= expected-card-ids (world/discard-ids world))
                     :unexpected-discard-pile
                     "The discard pile does not contain the expected cards."
                     {:expected expected-card-ids
                      :actual (world/discard-ids world)})))}

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
                     "The latest history event was not the expected event."
                     {:expected event-type
                      :actual (:type event)
                      :event event})))}])
