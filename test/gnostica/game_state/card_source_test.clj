(ns gnostica.game-state.card-source-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]
            [gnostica.game-state.card-source :as card-source]
            [gnostica.game-state.disc :as game-state-disc]
            [gnostica.game-state.sword :as game-state-sword]
            [gnostica.test-support.board :refer [board-cell-by-index]]
            [gnostica.test-support.game-state :refer [move-card-to-discard
                                                      replace-player-hand
                                                      state-with-board-cards]]
            [gnostica.test-support.game-state-moves :refer [rose-disc-minion]]))

(deftest suit-source-helper-resolves-territory-sources-with-suit-config
  (let [state (-> (state-with-board-cards {3 "coins2"})
                  (game-state/with-board-pieces [rose-disc-minion]))
        source {:kind :territory
                :board-index 3
                :piece-id :rose-disc-minion}
        result (card-source/resolve-suit-source
                state
                :rose
                source
                nil
                game-state-disc/disc-source-config)]
    (is (:ok? result))
    (is (= source (:source result)))
    (is (= "coins2" (get-in result [:source-card :id])))
    (is (= :disc (:disc-variant result)))
    (is (= rose-disc-minion (:piece result)))))

(deftest territory-target-cell-helper-keeps-suit-specific-errors
  (let [state (state-with-board-cards {4 "cupsking"})
        ok-result (card-source/territory-target-cell
                   state
                   {:kind :territory
                    :board-index 4}
                   game-state-disc/disc-source-config)
        bad-result (card-source/territory-target-cell
                    state
                    {:kind :piece
                     :piece-id :rose-disc-minion}
                    game-state-disc/disc-source-config)]
    (is (:ok? ok-result))
    (is (= 4 (get-in ok-result [:cell :index])))
    (is (= :invalid-disc-target (get-in bad-result [:error :code])))))

(deftest replacement-option-helper-preserves-disc-and-sword-gates
  (let [target {:kind :territory
                :board-index 4}
        disc-result (card-source/resolve-replacement-card-options
                     {:disc-variant :disc}
                     target
                     {:replacement-card-source :discard-pile
                      :replacement-card-id "cupsking"}
                     game-state-disc/disc-replacement-card-config)
        star-result (card-source/resolve-replacement-card-options
                     {:disc-variant :disc-from-discard}
                     target
                     {:replacement-card-source :discard-pile
                      :replacement-card-id "cupsking"}
                     game-state-disc/disc-replacement-card-config)
        destroyed-sword-result (card-source/resolve-replacement-card-options
                                {:sword-variant :sword}
                                target
                                {:destroyed? true
                                 :replacement-card-id "cups2"}
                                game-state-sword/sword-replacement-card-config)]
    (is (= :disc-variant-option-unavailable
           (get-in disc-result [:error :code])))
    (is (:ok? star-result))
    (is (= :discard-pile (:replacement-card-source star-result)))
    (is (= :invalid-sword-replacement
           (get-in destroyed-sword-result [:error :code])))))

(deftest replacement-card-helpers-read-remove-and-replace-cards
  (let [hand-card (cards/card-by-id "cupsking")
        discard-card (cards/card-by-id "cups2")
        state (-> (state-with-board-cards {3 "coins2"})
                  (replace-player-hand :rose [hand-card])
                  (move-card-to-discard "cups2"))
        replaced-state (card-source/replace-board-cell-card state 3 hand-card)
        hand-removed-state (card-source/remove-replacement-card
                            state
                            :rose
                            :hand
                            "cupsking")
        discard-removed-state (card-source/remove-replacement-card
                               state
                               :rose
                               :discard-pile
                               "cups2")]
    (is (= hand-card
           (card-source/replacement-card-from-source
            state
            :rose
            :hand
            "cupsking")))
    (is (= discard-card
           (card-source/replacement-card-from-source
            state
            :rose
            :discard-pile
            "cups2")))
    (is (= "cupsking" (get-in (board-cell-by-index replaced-state 3)
                              [:card :id])))
    (is (not (some #{"cupsking"}
                   (map :id (get-in hand-removed-state
                                    [:players-by-id :rose :hand])))))
    (is (not (some #{"cups2"}
                   (map :id (:discard-pile discard-removed-state)))))))
