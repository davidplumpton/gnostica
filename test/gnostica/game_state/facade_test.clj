(ns gnostica.game-state.facade-test
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

(deftest focused-transition-namespaces-match-public-facade
  (let [draw-state (state-with-pieces [rose-target-minion])
        draw-card (first (get-in draw-state [:players-by-id :rose :hand]))
        draw-command {:player-id :rose
                      :discard-card-ids [(:id draw-card)]
                      :draw-count 1
                      :shuffle-fn identity}
        orient-state (state-with-pieces [rose-target-minion])
        orient-command {:player-id :rose
                        :piece-id :rose-target-minion
                        :orientation :west}
        cup-state (-> (state-with-pieces [rose-cup-minion])
                      (state-with-board-card 3 "cups2"))
        cup-command {:player-id :rose
                     :source {:kind :territory
                              :board-index 3
                              :piece-id :rose-cup-minion}
                     :target {:kind :territory
                              :board-index 4}
                     :orientation :east}
        rod-state (-> (state-with-pieces [rose-rod-minion])
                      (state-with-board-card 3 "wands2"))
        rod-command {:player-id :rose
                     :source {:kind :territory
                              :board-index 3
                              :piece-id :rose-rod-minion}
                     :mode :move-minion
                     :distance 1
                     :orientation :south}
        disc-state (-> (state-with-pieces [rose-disc-minion])
                       (state-with-board-card 3 "coins2"))
        disc-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-disc-minion}
                      :target {:kind :piece
                               :piece-id :rose-disc-minion}
                      :orientation :south}
        sword-state (-> (state-with-pieces [rose-sword-minion])
                        (state-with-board-card 3 "swords2"))
        sword-command {:player-id :rose
                       :source {:kind :territory
                                :board-index 3
                                :piece-id :rose-sword-minion}
                       :target {:kind :piece
                                :piece-id :rose-sword-minion}
                       :damage 1
                       :orientation :south}
        draw-major-state (-> (:state (game-state/create-game
                                       player-specs
                                       {:deck-order
                                        (deck-starting-with
                                         ["fool" "high-priestess" "judgement"])}))
                             (game-state/with-board-pieces [rose-target-minion]))
        fool-command {:player-id :rose
                      :source {:kind :hand-card
                               :card-id "fool"}
                      :reveals []}
        high-priestess-command {:player-id :rose
                                :source {:kind :hand-card
                                         :card-id "high-priestess"}
                                :redraws []}
        judgement-command {:player-id :rose
                           :source {:kind :hand-card
                                    :card-id "judgement"}
                           :piece-id :rose-target-minion
                           :card-ids []}
        manipulation-state (-> (:state (game-state/create-game
                                         player-specs
                                         {:deck-order
                                          (deck-starting-with
                                           ["hierophant" "hermit" "devil"])}))
                               (game-state/with-board-pieces
                                [rose-target-minion
                                 indigo-cup-target]))
        hierophant-command {:player-id :rose
                            :source {:kind :hand-card
                                     :card-id "hierophant"
                                     :piece-id :rose-target-minion}
                            :target {:kind :piece
                                     :piece-id :indigo-cup-target}
                            :orientation :south}
        hermit-command {:player-id :rose
                        :source {:kind :hand-card
                                 :card-id "hermit"
                                 :piece-id :rose-target-minion}
                        :target {:kind :piece
                                 :piece-id :indigo-cup-target}
                        :destination {:kind :territory
                                      :board-index 0}}
        devil-command {:player-id :rose
                       :source {:kind :hand-card
                                :card-id "devil"
                                :piece-id :rose-target-minion}
                       :target {:kind :piece
                                :piece-id :indigo-cup-target}
                       :orientation :south}
        world-state (-> (:state (game-state/create-game
                                  player-specs
                                  {:deck-order
                                   (deck-with-cards-at
                                    {0 "world"
                                     (board-card-position 3) "magician"})}))
                        (game-state/with-board-pieces [rose-cup-minion]))
        world-command {:player-id :rose
                       :source {:kind :hand-card
                                :card-id "world"
                                :piece-id :rose-cup-minion}
                       :copied-board-index 3
                       :power :cup
                       :target {:kind :territory
                                :board-index 4}
                       :orientation :east}]
    (is (= (game-state/apply-draw-move draw-state draw-command)
           (game-state-draw/apply-draw-move draw-state draw-command)))
    (is (= (game-state/apply-fool-move draw-major-state fool-command)
           (game-state-draw/apply-fool-move draw-major-state fool-command)))
    (is (= (game-state/apply-high-priestess-move draw-major-state
                                                  high-priestess-command)
           (game-state-draw/apply-high-priestess-move draw-major-state
                                                       high-priestess-command)))
    (is (= (game-state/apply-judgement-move draw-major-state judgement-command)
           (game-state-draw/apply-judgement-move draw-major-state judgement-command)))
    (is (= (game-state/apply-orient-move orient-state orient-command)
           (game-state-placement/apply-orient-move orient-state orient-command)))
    (is (= (game-state/apply-cup-move cup-state cup-command)
           (game-state-cup/apply-cup-move cup-state cup-command)))
    (is (= (game-state/resolve-rod-command rod-state rod-command)
           (game-state-rod/resolve-rod-command rod-state rod-command)))
    (is (= (game-state/apply-rod-move rod-state rod-command)
           (game-state-rod/apply-rod-move rod-state rod-command)))
	    (is (= (game-state/resolve-disc-command disc-state disc-command)
	           (game-state-disc/resolve-disc-command disc-state disc-command)))
    (is (= (game-state/apply-disc-move disc-state disc-command)
           (game-state-disc/apply-disc-move disc-state disc-command)))
    (is (= (game-state/resolve-sword-command sword-state sword-command)
           (game-state-sword/resolve-sword-command sword-state sword-command)))
    (is (= (game-state/apply-sword-move sword-state sword-command)
           (game-state-sword/apply-sword-move sword-state sword-command)))
    (is (= (game-state/apply-hierophant-move manipulation-state hierophant-command)
           (game-state-manipulation/apply-hierophant-move manipulation-state
                                                            hierophant-command)))
    (is (= (game-state/apply-hermit-move manipulation-state hermit-command)
           (game-state-manipulation/apply-hermit-move manipulation-state
                                                       hermit-command)))
    (is (= (game-state/apply-devil-move manipulation-state devil-command)
           (game-state-manipulation/apply-devil-move manipulation-state
                                                      devil-command)))
    (is (= (game-state/world-major-territories world-state)
           (game-state-world/world-major-territories world-state)))
    (is (= (game-state/apply-world-move world-state world-command)
           (game-state-world/apply-world-move world-state world-command)))))
(deftest turn-action-facades-reject-illegal-turn-state
  (let [active-state (state-with-pieces [rose-target-minion])
        finished-result (game-state/apply-orient-move
                         (assoc active-state :phase game-state/finished-phase)
                         {:player-id :rose
                          :piece-id :rose-target-minion
                          :orientation :west})
        eliminated-result (game-state/apply-orient-move
                           (set-player-eliminated active-state :rose true)
                           {:player-id :rose
                            :piece-id :rose-target-minion
                            :orientation :west})
        non-current-result (game-state/apply-orient-move
                            active-state
                            {:player-id :indigo
                             :piece-id :rose-target-minion
                             :orientation :west})
        no-piece-draw-result (game-state/apply-draw-move
                              (deterministic-game)
                              {:player-id :rose
                               :draw-count 0
                               :shuffle-fn identity})]
    (is (= :game-finished
           (get-in finished-result [:error :code])))
    (is (= :player-eliminated
           (get-in eliminated-result [:error :code])))
    (is (= :not-current-player
           (get-in non-current-result [:error :code])))
    (is (= :initial-placement-required
           (get-in no-piece-draw-result [:error :code])))
    (is (false? (:ok? finished-result)))
    (is (false? (:ok? eliminated-result)))
    (is (false? (:ok? non-current-result)))
    (is (false? (:ok? no-piece-draw-result)))))
