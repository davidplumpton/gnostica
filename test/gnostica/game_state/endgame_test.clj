(ns gnostica.game-state.endgame-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.game-schema :as game-schema]
            [gnostica.game-state :as game-state]
            [gnostica.game-state.cup :as game-state-cup]
            [gnostica.game-state.disc :as game-state-disc]
            [gnostica.game-state.draw :as game-state-draw]
            [gnostica.game-state.manipulation :as game-state-manipulation]
            [gnostica.game-state.placement :as game-state-placement]
            [gnostica.game-state.rod :as game-state-rod]
            [gnostica.game-state.sword :as game-state-sword]
            [gnostica.game-state.world :as game-state-world]
            [gnostica.pieces :as pieces]
            [gnostica.test-support.board :refer [board-card-ids
                                                 board-cell-at
                                                 board-cell-by-index
                                                 state-with-board-card
                                                 with-board-cells-at]]
            [gnostica.test-support.deck :refer [card-ids
                                                deck-starting-with
                                                deck-with-board-card
                                                deck-with-cards-at
                                                hand-card-count
                                                board-card-position]]
            [gnostica.test-support.game-state :refer [all-card-ids
                                                      deterministic-game
                                                      move-card-to-discard
                                                      player-hand-ids
                                                      player-specs
                                                      replace-player-hand
                                                      remove-card-id
                                                      set-player-eliminated
                                                      state-with-board-cards
                                                      state-with-pieces
                                                      three-player-specs]]
            [gnostica.test-support.game-state-moves :refer :all]
            [gnostica.test-support.pieces :refer [piece-by-id]]))

(deftest scores-count-only-territories-controlled-by-one-player
  (let [state (-> (state-with-board-cards {0 "cups2"
                                           1 "cupsking"
                                           2 "sun"})
                  (game-state/with-board-pieces
                   [{:id :rose-mixed
                     :player-id :rose
                     :space-index 0
                     :size :small
                     :orientation :north}
                    {:id :indigo-mixed
                     :player-id :indigo
                     :space-index 0
                     :size :small
                     :orientation :north}
                    {:id :rose-royalty
                     :player-id :rose
                     :space-index 1
                     :size :small
                     :orientation :north}
                    {:id :rose-major
                     :player-id :rose
                     :space-index 2
                     :size :small
                     :orientation :north}])
                  game-state/with-current-scores)]
    (is (= {:rose 5
            :indigo 0}
           (game-state/scores state)))
    (is (= 5 (get-in state [:players-by-id :rose :score])))
    (is (game-schema/valid-game? state))))
(deftest end-turn-can-announce-and-resolve-a-winning-challenge
  (let [state (-> (state-with-board-cards {0 "cups2"
                                           1 "cupsking"
                                           2 "sun"
                                           3 "magician"})
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
                     :orientation :north}]))
        announced (game-state/end-turn state {:player-id :rose
                                              :announce-challenge? true})
        indigo-blocked (game-state/end-turn (:state announced)
                                            {:player-id :indigo
                                             :announce-challenge? true})
        indigo-ended (game-state/end-turn (:state announced) {:player-id :indigo})
        resolved (game-state/end-turn (:state indigo-ended) {:player-id :rose})]
    (is (:ok? announced))
    (is (= [:challenge/announced :turn/advanced]
           (mapv :type (:events announced))))
    (is (= :rose (game-state/active-challenge-player-id (:state announced))))
    (is (= :challenge-unavailable
           (get-in indigo-blocked [:error :code])))
    (is (:ok? resolved))
    (is (= game-state/finished-phase (:phase (:state resolved))))
    (is (= {:player-id :rose
            :reason :challenge
            :score 9
            :target-score 9}
           (:winner (:state resolved))))
    (is (game-schema/valid-game? (:state resolved)))))
(deftest announce-challenge-facade-claims-at-end-of-turn
  (let [state (rose-nine-point-challenge-state)
        announced (game-state/announce-challenge state {:player-id :rose})
        same-player-ended (game-state/end-turn (:state announced) {:player-id :rose})]
    (is (:ok? announced))
    (is (= [:challenge/announced :turn/advanced]
           (mapv :type (:events announced))))
    (is (= :indigo
           (get-in announced [:state :turn :current-player-id])))
    (is (= :rose
           (game-state/active-challenge-player-id (:state announced))))
    (is (= :not-current-player
           (get-in same-player-ended [:error :code])))
    (is (game-schema/valid-game? (:state announced)))))
(deftest challenge-resolution-uses-configured-target-score
  (let [short-game-result (resolve-rose-challenge
                           (rose-nine-point-challenge-state {:target-score 8}))
        long-game-result (resolve-rose-challenge
                          (rose-nine-point-challenge-state {:target-score 10}))
        long-game-state (:state long-game-result)]
    (is (:ok? short-game-result))
    (is (= {:player-id :rose
            :reason :challenge
            :score 9
            :target-score 8}
           (:winner (:state short-game-result))))
    (is (:ok? long-game-result))
    (is (= [:challenge/failed :game/won]
           (mapv :type (:events long-game-result))))
    (is (true? (get-in long-game-state [:players-by-id :rose :eliminated?])))
    (is (= {:player-id :indigo
            :reason :last-active-player
            :score 3
            :target-score 10}
           (:winner long-game-state)))
    (is (game-schema/valid-game? (:state short-game-result)))
    (is (game-schema/valid-game? long-game-state))))
(deftest end-turn-and-challenge-reject-after-game-is-finished
  (let [resolved (resolve-rose-challenge (rose-nine-point-challenge-state))
        finished-state (:state resolved)
        ended (game-state/end-turn finished-state {:player-id :rose})
        challenged (game-state/end-turn finished-state {:player-id :rose
                                                        :announce-challenge? true})
        announced (game-state/announce-challenge finished-state {:player-id :rose})]
    (is (= game-state/finished-phase (:phase finished-state)))
    (is (= :game-finished
           (get-in ended [:error :code])))
    (is (= :game-finished
           (get-in challenged [:error :code])))
    (is (= :game-finished
           (get-in announced [:error :code])))
    (is (false? (game-state/can-end-turn? finished-state :rose)))
    (is (not (game-state/can-announce-challenge? finished-state :rose)))))
(deftest failed-challenge-eliminates-player-and-discards-their-hand
  (let [state (-> (state-with-board-cards {0 "sun"
                                           4 "magician"})
                  (game-state/with-board-pieces
                   [{:id :rose-major
                     :player-id :rose
                     :space-index 0
                     :size :small
                     :orientation :north}
                    {:id :indigo-piece
                     :player-id :indigo
                     :space-index 4
                     :size :small
                     :orientation :north}]))
        rose-hand-ids (player-hand-ids state :rose)
        announced (game-state/end-turn state {:player-id :rose
                                              :announce-challenge? true})
        indigo-ended (game-state/end-turn (:state announced) {:player-id :indigo})
        resolved (game-state/end-turn (:state indigo-ended) {:player-id :rose})
        resolved-state (:state resolved)]
    (is (:ok? resolved))
    (is (= [:challenge/failed :game/won]
           (mapv :type (:events resolved))))
    (is (true? (get-in resolved-state [:players-by-id :rose :eliminated?])))
    (is (empty? (get-in resolved-state [:players-by-id :rose :hand])))
    (is (empty? (filter #(= :rose (:player-id %))
                        (get-in resolved-state [:pieces :on-board]))))
    (is (= rose-hand-ids
           (mapv :id (take-last (count rose-hand-ids)
                                (:discard-pile resolved-state)))))
    (is (= {:player-id :indigo
            :reason :last-active-player
            :score 3
            :target-score 9}
           (:winner resolved-state)))
    (is (game-schema/valid-game? resolved-state))))
(deftest no-piece-player-must-place-initial-small-before-ending-turn
  (let [state (deterministic-game)
        ended (game-state/end-turn state {:player-id :rose})
        challenged (game-state/end-turn state {:player-id :rose
                                               :announce-challenge? true})
        placed (game-state/apply-initial-placement
                state
                {:player-id :rose
                 :target {:kind :territory
                          :board-index 0}
                 :orientation :north})
        ended-after-placement (game-state/end-turn (:state placed)
                                                   {:player-id :rose})]
    (is (= :initial-placement-required
           (get-in ended [:error :code])))
    (is (= :end-turn
           (get-in ended [:error :data :action])))
    (is (= "A player with no pieces must place their initial small piece before ending the turn."
           (get-in ended [:error :message])))
    (is (= :initial-placement-required
           (get-in challenged [:error :code])))
    (is (false? (game-state/can-end-turn? state :rose)))
    (is (not (game-state/can-announce-challenge? state :rose)))
    (is (:ok? placed))
    (is (:ok? ended-after-placement))
    (is (= :indigo
           (get-in ended-after-placement [:state :turn :current-player-id])))
    (is (game-schema/valid-game? (:state ended-after-placement)))))
