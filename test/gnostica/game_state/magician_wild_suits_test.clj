(ns gnostica.game-state.magician-wild-suits-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.cards :as cards]
            [gnostica.game-schema :as game-schema]
            [gnostica.game-state :as game-state]
            [gnostica.test-support.deck :refer [deck-starting-with]]
            [gnostica.test-support.game-state :refer [all-card-ids
                                                      player-hand-ids
                                                      player-specs]]
            [gnostica.test-support.game-state-moves :refer [rose-cup-minion
                                                            rose-disc-minion
                                                            rose-rod-minion
                                                            rose-sword-minion]]
            [gnostica.test-support.pieces :refer [piece-by-id]]))

(defn- magician-hand-state [pieces]
  (-> (:state (game-state/create-game
               player-specs
               {:deck-order (deck-starting-with ["magician"])}))
      (game-state/with-board-pieces pieces)))

(defn- assert-paid-magician-source [state events event variant-key source]
  (is (= :wild-suits (get event variant-key)))
  (is (= source (:source event)))
  (is (= ["magician"] (mapv :id (:discard-pile state))))
  (is (not (some #{"magician"} (player-hand-ids state :rose))))
  (is (= events [(peek (:history state))]))
  (is (= (count cards/deck) (count (all-card-ids state))))
  (is (= (count cards/deck) (count (set (all-card-ids state)))))
  (is (game-schema/valid-game? state)))

(def indigo-sword-target
  {:id :indigo-sword-target
   :player-id :indigo
   :space-index 4
   :size :medium
   :orientation :north})

(def magician-wild-suit-cases
  [{:label "Cup"
    :command-kind :cup
    :transition game-state/apply-cup-move
    :variant-key :cup-variant
    :event-type :cup/small-piece-created
    :pieces [rose-cup-minion]
    :command {:player-id :rose
              :source {:kind :hand-card
                       :card-id "magician"
                       :piece-id :rose-cup-minion}
              :cup-variant :wild-suits
              :target {:kind :territory
                       :board-index 4}
              :orientation :north}
    :assert-state (fn [state]
                    (is (= {:id :rose-small-1
                            :player-id :rose
                            :space-index 4
                            :size :small
                            :orientation :north}
                           (piece-by-id state :rose-small-1))))}
   {:label "Rod"
    :command-kind :rod
    :transition game-state/apply-rod-move
    :variant-key :rod-variant
    :event-type :rod/minion-moved
    :pieces [rose-rod-minion]
    :command {:player-id :rose
              :source {:kind :hand-card
                       :card-id "magician"
                       :piece-id :rose-rod-minion}
              :rod-variant :wild-suits
              :mode :move-minion
              :distance 1
              :orientation :south}
    :assert-state (fn [state]
                    (is (= {:id :rose-rod-minion
                            :player-id :rose
                            :space-index 4
                            :size :medium
                            :orientation :south}
                           (piece-by-id state :rose-rod-minion))))}
   {:label "Disc"
    :command-kind :disc
    :transition game-state/apply-disc-move
    :variant-key :disc-variant
    :event-type :disc/piece-grown
    :pieces [rose-disc-minion]
    :command {:player-id :rose
              :source {:kind :hand-card
                       :card-id "magician"
                       :piece-id :rose-disc-minion}
              :disc-variant :wild-suits
              :target {:kind :piece
                       :piece-id :rose-disc-minion}
              :orientation :north}
    :assert-state (fn [state]
                    (is (= {:id :rose-large-1
                            :player-id :rose
                            :space-index 3
                            :size :large
                            :orientation :north}
                           (piece-by-id state :rose-large-1))))}
   {:label "Sword"
    :command-kind :sword
    :transition game-state/apply-sword-move
    :variant-key :sword-variant
    :event-type :sword/piece-shrunk
    :pieces [rose-sword-minion indigo-sword-target]
    :command {:player-id :rose
              :source {:kind :hand-card
                       :card-id "magician"
                       :piece-id :rose-sword-minion}
              :sword-variant :wild-suits
              :target {:kind :piece
                       :piece-id :indigo-sword-target}
              :damage 1}
    :assert-state (fn [state]
                    (is (= {:id :indigo-small-1
                            :player-id :indigo
                            :space-index 4
                            :size :small
                            :orientation :north}
                           (piece-by-id state :indigo-small-1))))}])

(deftest magician-hand-card-exercises-all-four-suit-facades
  (doseq [{:keys [label command-kind command transition pieces variant-key
                  event-type assert-state]}
          magician-wild-suit-cases]
    (testing label
      (let [state (magician-hand-state pieces)
            source (:source command)
            {:keys [ok? state events] :as result} (transition state command)
            event (first events)]
        (is (game-state/valid-command? command-kind command))
        (is ok?)
        (is (game-state/valid-result? result))
        (is (= event-type (:type event)))
        (assert-state state)
        (assert-paid-magician-source state events event variant-key source)))))
