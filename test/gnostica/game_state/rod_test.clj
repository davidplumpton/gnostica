(ns gnostica.game-state.rod-test
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

(deftest rod-command-normalizes-territory-source
  (let [state (-> (state-with-pieces [rose-rod-minion])
                  (state-with-board-card 3 "wands2"))
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-rod-minion}
                 :mode :move-minion
                 :distance 2
                 :orientation :south}
        result (game-state/resolve-rod-command state command)]
    (is (:ok? result))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 3
                     :piece-id :rose-rod-minion}
            :rod-variant :rod
            :mode :move-minion
            :target {:kind :piece
                     :piece-id :rose-rod-minion
                     :player-id :rose
                     :row 1
                     :col 0
                     :destination {:row 1
                                   :col 2}
                     :orientation :south}
            :distance 2
            :direction :east
            :orientation :south}
           (:command result)))
    (is (= "wands2" (get-in result [:source-card :id])))
    (is (= rose-rod-minion (:piece result)))))
(deftest rod-command-normalizes-hand-card-source-and-piece-target
  (let [deck-order (deck-starting-with ["wands2"])
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces
               state
               [rose-rod-minion
                {:id :indigo-rod-target
                 :player-id :indigo
                 :space-index 4
                 :size :small
                 :orientation :north}])
        result (game-state/resolve-rod-command
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "wands2"
                          :piece-id :rose-rod-minion}
                 :mode :push-piece
                 :target {:kind :piece
                          :piece-id :indigo-rod-target}
                 :distance 1})]
    (is (:ok? result))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "wands2"
                     :piece-id :rose-rod-minion}
            :rod-variant :rod
            :mode :push-piece
            :target {:kind :piece
                     :piece-id :indigo-rod-target
                     :player-id :indigo
                     :row 1
                     :col 1
                     :destination {:row 1
                                   :col 2}}
            :distance 1
            :direction :east}
           (:command result)))
    (is (= "wands2" (get-in result [:source-card :id])))
    (is (= :indigo-rod-target (get-in result [:target-piece :id])))))
(deftest rod-command-rejects-enemy-piece-reorientation
  (let [deck-order (deck-starting-with ["wands2"])
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces
               state
               [rose-rod-minion
                {:id :indigo-rod-target
                 :player-id :indigo
                 :space-index 4
                 :size :small
                 :orientation :north}])
        result (game-state/resolve-rod-command
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "wands2"
                          :piece-id :rose-rod-minion}
                 :mode :push-piece
                 :target {:kind :piece
                          :piece-id :indigo-rod-target}
                 :distance 1
                 :orientation :west})]
    (is (= :invalid-orientation
           (get-in result [:error :code])))
    (is (= {:piece-id :indigo-rod-target
            :piece-player-id :indigo
            :orientation :west}
           (get-in result [:error :data])))
    (is (false? (:ok? result)))
    (is (not (contains? result :state)))))
(deftest rod-command-normalizes-territory-target-coordinates
  (let [state (-> (state-with-pieces [rose-rod-minion])
                  (state-with-board-card 3 "wands2"))
        result (game-state/resolve-rod-command
                state
                {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-rod-minion}
                 :mode :push-territory
                 :target {:kind :territory
                          :row 1
                          :col 1}
                 :distance 1})]
    (is (:ok? result))
    (is (= {:kind :territory
            :board-index 4
            :row 1
            :col 1
            :destination {:row 1
                          :col 2}}
           (get-in result [:command :target])))
    (is (= :rod (get-in result [:command :rod-variant])))
    (is (= 4 (get-in result [:target-cell :index])))))
(deftest rod-command-carries-source-variant
  (let [emperor-state (-> (state-with-pieces [rose-rod-minion])
                          (state-with-board-card 3 "emperor"))
        emperor-result (game-state/resolve-rod-command
                        emperor-state
                        {:player-id :rose
                         :source {:kind :territory
                                  :board-index 3
                                  :piece-id :rose-rod-minion}
                         :mode :move-minion
                         :distance 1})
        magician-state (-> (state-with-pieces [rose-rod-minion])
                           (state-with-board-card 3 "magician"))
        magician-result (game-state/resolve-rod-command
                         magician-state
                         {:player-id :rose
                          :source {:kind :territory
                                   :board-index 3
                                   :piece-id :rose-rod-minion}
                          :mode :move-minion
                          :distance 1})]
    (is (:ok? emperor-result))
    (is (:ok? magician-result))
    (is (= :rod-unbounded
           (get-in emperor-result [:command :rod-variant])))
    (is (= :wild-suits
           (get-in magician-result [:command :rod-variant])))))
(deftest rod-command-rejects-unavailable-variants
  (let [state (-> (state-with-pieces [rose-rod-minion])
                  (state-with-board-card 3 "wands2"))
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-rod-minion}
                      :mode :move-minion
                      :distance 1}
        unavailable-result (game-state/resolve-rod-command
                            state
                            (assoc base-command :rod-variant :rod-unbounded))
        invalid-result (game-state/resolve-rod-command
                        state
                        (assoc base-command :rod-variant :wheel-cup))]
    (is (= :rod-variant-unavailable
           (get-in unavailable-result [:error :code])))
    (is (= :invalid-rod-variant
           (get-in invalid-result [:error :code])))))
(deftest rod-command-rejects-invalid-sources-and-upright-minions
  (let [state (-> (state-with-pieces [rose-rod-minion])
                  (state-with-board-card 3 "cups2"))
        non-rod-result (game-state/resolve-rod-command
                        state
                        {:player-id :rose
                         :source {:kind :territory
                                  :board-index 3
                                  :piece-id :rose-rod-minion}
                         :mode :move-minion
                         :distance 1})
        upright-state (-> (state-with-pieces [(assoc rose-rod-minion
                                                     :orientation :up)])
                          (state-with-board-card 3 "wands2"))
        upright-result (game-state/resolve-rod-command
                        upright-state
                        {:player-id :rose
                         :source {:kind :territory
                                  :board-index 3
                                  :piece-id :rose-rod-minion}
                         :mode :move-minion
                         :distance 1})]
    (is (= :source-card-not-rod
           (get-in non-rod-result [:error :code])))
    (is (= :rod-minion-upright
           (get-in upright-result [:error :code])))))
(deftest rod-command-rejects-invalid-direction-distance-and_targets_without_mutation
  (let [state (-> (state-with-pieces [rose-rod-minion
                                      {:id :indigo-off-axis-target
                                       :player-id :indigo
                                       :space-index 0
                                       :size :small
                                       :orientation :north}])
                  (state-with-board-card 3 "wands2"))
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-rod-minion}
                      :mode :move-minion
                      :distance 1}
        zero-result (game-state/resolve-rod-command state
                                                    (assoc base-command
                                                           :distance 0))
        too-far-result (game-state/resolve-rod-command state
                                                       (assoc base-command
                                                              :distance 3))
        direction-result (game-state/resolve-rod-command state
                                                         (assoc base-command
                                                                :direction :north))
        target-result (game-state/resolve-rod-command
                       state
                       (assoc base-command
                              :mode :push-piece
                              :target {:kind :piece
                                       :piece-id :indigo-off-axis-target}))]
    (is (= :invalid-rod-distance
           (get-in zero-result [:error :code])))
    (is (= :invalid-rod-distance
           (get-in too-far-result [:error :code])))
    (is (= 2 (get-in too-far-result [:error :data :maximum])))
    (is (= :invalid-rod-direction
           (get-in direction-result [:error :code])))
    (is (= :invalid-rod-target
           (get-in target-result [:error :code])))
    (is (false? (:ok? zero-result)))
    (is (not (contains? zero-result :state)))
    (is (= [rose-rod-minion
            {:id :indigo-off-axis-target
             :player-id :indigo
             :space-index 0
             :size :small
             :orientation :north}]
           (get-in state [:pieces :on-board])))))
(deftest rod-move-moves-minion-to-territory-and-may-reorient-owned-piece
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 3 "wands2")}))
        state (game-state/with-board-pieces state [rose-rod-minion])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-rod-minion}
                 :mode :move-minion
                 :distance 2
                 :orientation :south}
        {:keys [ok? state events]} (game-state/apply-rod-move state command)
        moved-piece (piece-by-id state :rose-rod-minion)]
    (is ok?)
    (is (= {:id :rose-rod-minion
            :player-id :rose
            :space-index 5
            :size :medium
            :orientation :south}
           moved-piece))
    (is (= [{:type :rod/minion-moved
             :player-id :rose
             :source {:kind :territory
                      :board-index 3
                      :piece-id :rose-rod-minion}
             :rod-variant :rod
             :target {:kind :piece
                      :piece-id :rose-rod-minion
                      :player-id :rose
                      :row 1
                      :col 0}
             :destination {:kind :territory
                           :board-index 5
                           :row 1
                           :col 2}
             :distance 2
             :direction :east
             :piece moved-piece}]
           events))
    (is (= events [(peek (:history state))]))
    (is (game-schema/valid-game? state))))
(deftest rod-move-pushes-enemy-piece-and-discards-hand-source
  (let [deck-order (deck-starting-with ["wands2"])
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces
               state
               [rose-rod-minion
                {:id :indigo-rod-target
                 :player-id :indigo
                 :space-index 4
                 :size :small
                 :orientation :north}])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "wands2"
                          :piece-id :rose-rod-minion}
                 :mode :push-piece
                 :target {:kind :piece
                          :piece-id :indigo-rod-target}
                 :distance 1}
        {:keys [ok? state events]} (game-state/apply-rod-move state command)
        pushed-piece (piece-by-id state :indigo-rod-target)]
    (is ok?)
    (is (= {:id :indigo-rod-target
            :player-id :indigo
            :space-index 5
            :size :small
            :orientation :north}
           pushed-piece))
    (is (= ["wands2"] (mapv :id (:discard-pile state))))
    (is (not (some #{"wands2"} (player-hand-ids state :rose))))
    (is (= [{:type :rod/piece-pushed
             :player-id :rose
             :source {:kind :hand-card
                      :card-id "wands2"
                      :piece-id :rose-rod-minion}
             :rod-variant :rod
             :target {:kind :piece
                      :piece-id :indigo-rod-target
                      :player-id :indigo
                      :row 1
                      :col 1}
             :destination {:kind :territory
                           :board-index 5
                           :row 1
                           :col 2}
             :distance 1
             :direction :east
             :piece pushed-piece}]
           events))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
(deftest rod-move-represents-wasteland-destinations-as-wasteland-spaces
  (let [rod-minion (assoc rose-rod-minion
                          :space-index 2
                          :orientation :east)
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 2 "wands2")}))
        state (game-state/with-board-pieces state [rod-minion])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 2
                          :piece-id :rose-rod-minion}
                 :mode :move-minion
                 :distance 1}
        {:keys [ok? state events]} (game-state/apply-rod-move state command)
        moved-piece (piece-by-id state :rose-rod-minion)]
    (is ok?)
    (is (= {:id :rose-rod-minion
            :player-id :rose
            :space {:kind :wasteland
                    :row 0
                    :col 3}
            :size :medium
            :orientation :east}
           moved-piece))
    (is (= {:kind :wasteland
            :row 0
            :col 3}
           (get-in events [0 :destination])))
    (is (not (contains? moved-piece :space-index)))
    (is (game-schema/valid-game? state))))
(deftest rod-move-pushes-territory-into-wasteland-without-moving-pieces
  (let [deck-order (deck-starting-with ["wands2"])
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        target-card (get-in state [:board 5 :card])
        rod-minion (assoc rose-rod-minion :space-index 4)
        state (game-state/with-board-pieces
               state
               [rod-minion
                (assoc rose-target-minion :space-index 5)
                {:id :rose-landing-minion
                 :player-id :rose
                 :space {:kind :wasteland
                         :row 1
                         :col 3}
                 :size :small
                 :orientation :west}])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "wands2"
                          :piece-id :rose-rod-minion}
                 :mode :push-territory
                 :target {:kind :territory
                          :board-index 5}
                 :distance 1}
        {:keys [ok? state events]} (game-state/apply-rod-move state command)
        moved-cell (board-cell-by-index state 5)
        old-target-piece (piece-by-id state :rose-target-minion)
        landing-piece (piece-by-id state :rose-landing-minion)]
    (is ok?)
    (is (= {:index 5
            :row 1
            :col 3
            :orientation :portrait
            :face :up
            :card target-card}
           moved-cell))
    (is (nil? (board-cell-at state 1 2)))
    (is (= {:id :rose-target-minion
            :player-id :rose
            :space {:kind :wasteland
                    :row 1
                    :col 2}
            :size :medium
            :orientation :up}
           old-target-piece))
    (is (= {:id :rose-landing-minion
            :player-id :rose
            :space-index 5
            :size :small
            :orientation :west}
           landing-piece))
    (is (= ["wands2"] (mapv :id (:discard-pile state))))
    (is (not (some #{"wands2"} (player-hand-ids state :rose))))
    (is (= [{:type :rod/territory-pushed
             :player-id :rose
             :source {:kind :hand-card
                      :card-id "wands2"
                      :piece-id :rose-rod-minion}
             :rod-variant :rod
             :target {:kind :territory
                      :board-index 5
                      :row 1
                      :col 2}
             :destination {:kind :wasteland
                           :row 1
                           :col 3}
             :distance 1
             :direction :east
             :territory moved-cell}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
(deftest rod-move-returns-pieces-left-in-void-by-territory-relocation
  (let [initial-state (:state (game-state/create-game
                               player-specs
                               {:deck-order (deck-starting-with ["wands2"])}))
        state (with-board-cells-at initial-state
                [[0 {:row 0 :col 0}]
                 [8 {:row 0 :col 3}]])
        target-card (get-in state [:board 0 :card])
        rod-minion {:id :rose-rod-wasteland-minion
                    :player-id :rose
                    :space {:kind :wasteland
                            :row 0
                            :col -1}
                    :size :medium
                    :orientation :east}
        passenger {:id :rose-rod-passenger
                   :player-id :rose
                   :space-index 0
                   :size :small
                   :orientation :north}
        state (game-state/with-board-pieces state [rod-minion
                                                   passenger])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "wands2"
                          :piece-id :rose-rod-wasteland-minion}
                 :mode :push-territory
                 :target {:kind :territory
                          :board-index 0}
                 :distance 2}
        {:keys [ok? state events]} (game-state/apply-rod-move state command)
        moved-cell (board-cell-by-index state 0)]
    (is ok?)
    (is (= {:index 0
            :row 0
            :col 2
            :orientation :portrait
            :face :up
            :card target-card}
           moved-cell))
    (is (nil? (board-cell-at state 0 0)))
    (is (nil? (piece-by-id state :rose-rod-wasteland-minion)))
    (is (nil? (piece-by-id state :rose-rod-passenger)))
    (is (= 5 (get-in state [:pieces :stashes :rose :small])))
    (is (= 5 (get-in state [:pieces :stashes :rose :medium])))
    (is (= ["wands2"] (mapv :id (:discard-pile state))))
    (is (= :rod/territory-pushed (get-in events [0 :type])))
    (is (game-schema/valid-game? state))))
(deftest rod-move-rejects-enemy-occupied-territory-push-targets_without_mutation
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 4 "wands2")}))
        pieces [(assoc rose-rod-minion :space-index 4)
                {:id :indigo-target-minion
                 :player-id :indigo
                 :space-index 5
                 :size :small
                 :orientation :north}]
        state (game-state/with-board-pieces state pieces)
        result (game-state/apply-rod-move
                state
                {:player-id :rose
                 :source {:kind :territory
                          :board-index 4
                          :piece-id :rose-rod-minion}
                 :mode :push-territory
                 :target {:kind :territory
                          :board-index 5}
                 :distance 1})]
    (is (= :target-territory-occupied-by-enemy
           (get-in result [:error :code])))
    (is (= [:indigo-target-minion]
           (get-in result [:error :data :enemy-piece-ids])))
    (is (not (contains? result :state)))
    (is (= pieces (get-in state [:pieces :on-board])))))
(deftest rod-move-rejects-territory-pushes-to-enemy-wastelands-and-void_without_mutation
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 4 "wands2")}))
        enemy-landing-pieces [(assoc rose-rod-minion :space-index 4)
                              {:id :indigo-landing-minion
                               :player-id :indigo
                               :space {:kind :wasteland
                                       :row 1
                                       :col 3}
                               :size :small
                               :orientation :south}]
        enemy-landing-state (game-state/with-board-pieces state enemy-landing-pieces)
        enemy-landing-result (game-state/apply-rod-move
                              enemy-landing-state
                              {:player-id :rose
                               :source {:kind :territory
                                        :board-index 4
                                        :piece-id :rose-rod-minion}
                               :mode :push-territory
                               :target {:kind :territory
                                        :board-index 5}
                               :distance 1})
        void-state (game-state/with-board-pieces
                    state
                    [(assoc rose-rod-minion :space-index 4)])
        void-result (game-state/apply-rod-move
                     void-state
                     {:player-id :rose
                      :source {:kind :territory
                               :board-index 4
                               :piece-id :rose-rod-minion}
                      :mode :push-territory
                      :target {:kind :territory
                               :board-index 5}
                      :distance 2})]
    (is (= :wasteland-occupied-by-enemy
           (get-in enemy-landing-result [:error :code])))
    (is (= [:indigo-landing-minion]
           (get-in enemy-landing-result [:error :data :enemy-piece-ids])))
    (is (= :rod-destination-void
           (get-in void-result [:error :code])))
    (is (not (contains? enemy-landing-result :state)))
    (is (not (contains? void-result :state)))
    (is (= enemy-landing-pieces
           (get-in enemy-landing-state [:pieces :on-board])))
    (is (= [(assoc rose-rod-minion :space-index 4)]
           (get-in void-state [:pieces :on-board])))))
(deftest rod-move-rejects-void-and-full-territory-destinations_without_mutation
  (let [full-pieces [rose-rod-minion
                     rose-target-minion
                     {:id :indigo-target-minion
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
                     {:id :indigo-target-guard
                      :player-id :indigo
                      :space-index 4
                      :size :large
                      :orientation :south}]
        full-state (:state (game-state/create-game
                            player-specs
                            {:deck-order (deck-with-board-card 3 "wands2")}))
        full-state (game-state/with-board-pieces full-state full-pieces)
        full-result (game-state/apply-rod-move
                     full-state
                     {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-rod-minion}
                      :mode :move-minion
                      :distance 1})
        void-minion (assoc rose-rod-minion
                           :space-index 2
                           :orientation :east)
        void-state (:state (game-state/create-game
                            player-specs
                            {:deck-order (deck-with-board-card 2 "wands2")}))
        void-state (game-state/with-board-pieces void-state [void-minion])
        void-result (game-state/apply-rod-move
                     void-state
                     {:player-id :rose
                      :source {:kind :territory
                               :board-index 2
                               :piece-id :rose-rod-minion}
                      :mode :move-minion
                      :distance 2})]
    (is (= :target-territory-full
           (get-in full-result [:error :code])))
    (is (= :rod-destination-void
           (get-in void-result [:error :code])))
    (is (not (contains? full-result :state)))
    (is (not (contains? void-result :state)))
    (is (= (get-in full-state [:pieces :on-board])
           full-pieces))
    (is (= [void-minion]
           (get-in void-state [:pieces :on-board])))))
(deftest rod-unbounded-variant-ignores-full-territory-destination-limit
  (let [full-pieces [rose-rod-minion
                     rose-target-minion
                     {:id :indigo-target-minion
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
                     {:id :indigo-target-guard
                      :player-id :indigo
                      :space-index 4
                      :size :large
                      :orientation :south}]
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 3 "emperor")}))
        state (game-state/with-board-pieces state full-pieces)
        {:keys [ok? state events]} (game-state/apply-rod-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :territory
                                              :board-index 3
                                              :piece-id :rose-rod-minion}
                                     :mode :move-minion
                                     :distance 1})
        moved-piece (piece-by-id state :rose-rod-minion)
        destination-pieces (filter #(= 4 (:space-index %))
                                   (get-in state [:pieces :on-board]))]
    (is ok?)
    (is (= {:id :rose-rod-minion
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :east}
           moved-piece))
    (is (= 4 (count destination-pieces)))
    (is (= :rod-unbounded
           (get-in events [0 :rod-variant])))
    (is (game-schema/valid-game? state))))
