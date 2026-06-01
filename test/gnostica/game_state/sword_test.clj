(ns gnostica.game-state.sword-test
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

(deftest sword-command-normalizes-territory-source-and-piece-target
  (let [target-piece {:id :indigo-sword-target
                      :player-id :indigo
                      :space-index 4
                      :size :medium
                      :orientation :north}
        state (-> (state-with-pieces [rose-sword-minion target-piece])
                  (state-with-board-card 3 "swords2"))
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-sword-minion}
                 :target {:kind :piece
                          :piece-id :indigo-sword-target}
                 :damage 1}
        result (game-state/resolve-sword-command state command)]
    (is (:ok? result))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 3
                     :piece-id :rose-sword-minion}
            :sword-variant :sword
            :target {:kind :piece
                     :piece-id :indigo-sword-target
                     :player-id :indigo
                     :board-index 4
                     :row 1
                     :col 1}
            :damage 1}
           (:command result)))
    (is (= "swords2" (get-in result [:source-card :id])))
    (is (= rose-sword-minion (:piece result)))
    (is (= target-piece (:target-piece result)))
    (is (false? (:destroyed? result)))))
(deftest sword-command-normalizes-hand-card-source-and-territory-target_options
  (let [deck-order (deck-starting-with ["swords2" "cups2"])
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (-> state
                  (game-state/with-board-pieces [rose-sword-minion])
                  (state-with-board-card 4 "cupsking"))
        result (game-state/resolve-sword-command
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "swords2"
                          :piece-id :rose-sword-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :damage 1
                 :replacement-card-id "cups2"})]
    (is (:ok? result))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "swords2"
                     :piece-id :rose-sword-minion}
            :sword-variant :sword
            :target {:kind :territory
                     :board-index 4
                     :row 1
                     :col 1}
            :damage 1
            :replacement-card-source :hand
            :replacement-card-id "cups2"}
           (:command result)))
    (is (= "swords2" (get-in result [:source-card :id])))
    (is (= 4 (get-in result [:target-cell :index])))))
(deftest sword-command-allows-upright_current_space_and_minion_self_targeting
  (let [upright-state (-> (state-with-pieces [(assoc rose-sword-minion
                                                      :orientation :up)])
                          (state-with-board-card 3 "swords2"))
        territory-result (game-state/resolve-sword-command
                          upright-state
                          {:player-id :rose
                           :source {:kind :territory
                                    :board-index 3
                                    :piece-id :rose-sword-minion}
                           :target {:kind :territory
                                    :board-index 3}
                           :damage 1})
        self-state (-> (state-with-pieces [rose-sword-minion])
                       (state-with-board-card 3 "swords2"))
        self-result (game-state/resolve-sword-command
                     self-state
                     {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-sword-minion}
                      :target {:kind :piece
                               :piece-id :rose-sword-minion}
                      :damage 1
                      :orientation :south})]
    (is (:ok? territory-result))
    (is (= {:kind :territory
            :board-index 3
            :row 1
            :col 0}
           (get-in territory-result [:command :target])))
    (is (:ok? self-result))
    (is (= {:kind :piece
            :piece-id :rose-sword-minion
            :player-id :rose
            :board-index 3
            :row 1
            :col 0
            :orientation :south}
           (get-in self-result [:command :target])))
    (is (= :south (get-in self-result [:command :orientation])))))
(deftest sword-command-carries-source-variants
  (let [base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-sword-minion}
                      :target {:kind :piece
                               :piece-id :rose-sword-minion}
                      :damage 1}
        justice-result (game-state/resolve-sword-command
                        (-> (state-with-pieces [rose-sword-minion])
                            (state-with-board-card 3 "justice"))
                        base-command)
        death-result (game-state/resolve-sword-command
                      (-> (state-with-pieces [rose-sword-minion])
                          (state-with-board-card 3 "death"))
                      base-command)
        tower-result (game-state/resolve-sword-command
                      (-> (state-with-pieces [rose-sword-minion])
                          (state-with-board-card 3 "tower"))
                      base-command)
        moon-result (game-state/resolve-sword-command
                     (-> (state-with-pieces [rose-sword-minion])
                         (state-with-board-card 3 "moon"))
                     base-command)
        magician-result (game-state/resolve-sword-command
                         (-> (state-with-pieces [rose-sword-minion])
                             (state-with-board-card 3 "magician"))
                         base-command)]
    (is (:ok? justice-result))
    (is (:ok? death-result))
    (is (:ok? tower-result))
    (is (:ok? moon-result))
    (is (:ok? magician-result))
    (is (= :sword (get-in justice-result [:command :sword-variant])))
    (is (= :sword (get-in death-result [:command :sword-variant])))
    (is (= :sword-from-discard (get-in tower-result [:command :sword-variant])))
    (is (= :sword (get-in moon-result [:command :sword-variant])))
    (is (= :wild-suits (get-in magician-result [:command :sword-variant])))))
(deftest sword-command-rejects-unavailable_and_invalid_variants
  (let [state (-> (state-with-pieces [rose-sword-minion])
                  (state-with-board-card 3 "swords2")
                  (state-with-board-card 4 "cupsking"))
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-sword-minion}
                      :target {:kind :piece
                               :piece-id :rose-sword-minion}
                      :damage 1}
        unavailable-result (game-state/resolve-sword-command
                            state
                            (assoc base-command :sword-variant :sword-from-discard))
        invalid-result (game-state/resolve-sword-command
                        state
                        (assoc base-command :sword-variant :disc))
        discard-source-result (game-state/resolve-sword-command
                               state
                               (assoc base-command
                                      :target {:kind :territory
                                               :board-index 4}
                                      :replacement-card-source :discard-pile
                                      :replacement-card-id "cups2"))
        non-sword-result (game-state/resolve-sword-command
                          (-> (state-with-pieces [rose-sword-minion])
                              (state-with-board-card 3 "coins2"))
                          base-command)]
    (is (= :sword-variant-unavailable
           (get-in unavailable-result [:error :code])))
    (is (= :invalid-sword-variant
           (get-in invalid-result [:error :code])))
    (is (= :sword-variant-option-unavailable
           (get-in discard-source-result [:error :code])))
    (is (= :source-card-not-sword
           (get-in non-sword-result [:error :code])))))
(deftest sword-command-rejects_invalid_targets_damage_and_options_without_mutation
  (let [off-axis-target {:id :indigo-off-axis-sword-target
                         :player-id :indigo
                         :space-index 0
                         :size :small
                         :orientation :north}
        enemy-target {:id :indigo-sword-target
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
        state (-> (state-with-pieces [rose-sword-minion
                                      off-axis-target
                                      enemy-target])
                  (state-with-board-card 3 "swords2")
                  (state-with-board-card 4 "cupsking"))
        over-target-state (-> (state-with-pieces [(assoc rose-sword-minion
                                                          :size :large)])
                              (state-with-board-card 3 "swords2")
                              (state-with-board-card 4 "cupsking"))
        invalid-source-state (-> (state-with-pieces [(assoc rose-sword-minion
                                                            :size :tiny)])
                                 (state-with-board-card 3 "swords2"))
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-sword-minion}}
        invalid-source-result (game-state/resolve-sword-command
                               invalid-source-state
                               (assoc base-command
                                      :target {:kind :piece
                                               :piece-id :rose-sword-minion}
                                      :damage 1))
        off-axis-result (game-state/resolve-sword-command
                         state
                         (assoc base-command
                                :target {:kind :piece
                                         :piece-id :indigo-off-axis-sword-target}
                                :damage 1))
        zero-damage-result (game-state/resolve-sword-command
                            state
                            (assoc base-command
                                   :target {:kind :piece
                                            :piece-id :indigo-sword-target}
                                   :damage 0))
        over-minion-result (game-state/resolve-sword-command
                            state
                            (assoc base-command
                                   :target {:kind :piece
                                            :piece-id :indigo-sword-target}
                                   :damage 3))
        over-target-result (game-state/resolve-sword-command
                            over-target-state
                            (assoc base-command
                                   :target {:kind :territory
                                            :board-index 4}
                                   :damage 3))
        enemy-orientation-result (game-state/resolve-sword-command
                                  state
                                  (assoc base-command
                                         :target {:kind :piece
                                                  :piece-id :indigo-sword-target}
                                         :damage 1
                                         :orientation :west))
        enemy-territory-result (game-state/resolve-sword-command
                                state
                                (assoc base-command
                                       :target {:kind :territory
                                                :board-index 4}
                                       :damage 1
                                       :replacement-card-id "cups2"))
        piece-replacement-result (game-state/resolve-sword-command
                                  state
                                  (assoc base-command
                                         :target {:kind :piece
                                                  :piece-id :rose-sword-minion}
                                         :damage 1
                                         :replacement-card-id "cups2"))
        territory-orientation-result (game-state/resolve-sword-command
                                      state
                                      (assoc base-command
                                             :target {:kind :territory
                                                      :board-index 4}
                                             :damage 1
                                             :orientation :west))]
    (is (= :invalid-piece-size
           (get-in invalid-source-result [:error :code])))
    (is (= :invalid-sword-target
           (get-in off-axis-result [:error :code])))
    (is (= :invalid-sword-damage
           (get-in zero-damage-result [:error :code])))
    (is (= :invalid-sword-damage
           (get-in over-minion-result [:error :code])))
    (is (= :invalid-sword-damage
           (get-in over-target-result [:error :code])))
    (is (= :invalid-orientation
           (get-in enemy-orientation-result [:error :code])))
    (is (= :target-territory-occupied-by-enemy
           (get-in enemy-territory-result [:error :code])))
    (is (= :invalid-sword-replacement
           (get-in piece-replacement-result [:error :code])))
    (is (= :invalid-orientation
           (get-in territory-orientation-result [:error :code])))
    (is (false? (:ok? off-axis-result)))
    (is (not (contains? off-axis-result :state)))
    (is (= [rose-sword-minion off-axis-target enemy-target]
           (get-in state [:pieces :on-board])))))
(deftest tower-sword-command-allows_discard_pile_replacement_source
  (let [state (-> (state-with-pieces [rose-sword-minion])
                  (state-with-board-card 3 "tower")
                  (state-with-board-card 4 "cupsking"))
        result (game-state/resolve-sword-command
                state
                {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-sword-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :damage 1
                 :replacement-card-source :discard-pile
                 :replacement-card-id "cups2"})]
    (is (:ok? result))
    (is (= :sword-from-discard
           (get-in result [:command :sword-variant])))
    (is (= :discard-pile
           (get-in result [:command :replacement-card-source])))))
(deftest plain-sword-major-hand-sources-use-single_attack_contract
  (doseq [card-id ["justice" "death" "moon"]]
    (let [enemy-piece {:id :indigo-sword-target
                       :player-id :indigo
                       :space-index 4
                       :size :large
                       :orientation :north}
          state (:state (game-state/create-game
                         player-specs
                         {:deck-order (deck-starting-with [card-id])}))
          state (game-state/with-board-pieces state [rose-sword-minion enemy-piece])
          command {:player-id :rose
                   :source {:kind :hand-card
                            :card-id card-id
                            :piece-id :rose-sword-minion}
                   :target {:kind :piece
                            :piece-id :indigo-sword-target}
                   :damage 1}
          {:keys [ok? state events]} (game-state/apply-sword-move state command)
          shrunk-piece (piece-by-id state :indigo-medium-1)]
      (is ok? card-id)
      (is (= :sword/piece-shrunk (get-in events [0 :type])) card-id)
      (is (= :sword (get-in events [0 :sword-variant])) card-id)
      (is (= {:kind :hand-card
              :card-id card-id
              :piece-id :rose-sword-minion}
             (get-in events [0 :source]))
          card-id)
      (is (= {:id :indigo-medium-1
              :player-id :indigo
              :space-index 4
              :size :medium
              :orientation :north}
             shrunk-piece)
          card-id)
      (is (= [card-id] (mapv :id (:discard-pile state))) card-id)
      (is (not (some #{card-id} (player-hand-ids state :rose))) card-id)
      (is (= events [(peek (:history state))]) card-id)
      (is (= (count cards/deck) (count (all-card-ids state))) card-id)
      (is (= (count cards/deck) (count (set (all-card-ids state)))) card-id)
      (is (game-schema/valid-game? state) card-id))))
(deftest magician-wild_suit_sword_variant_is_carried_through_attack_application
  (let [enemy-piece {:id :indigo-sword-target
                     :player-id :indigo
                     :space-index 4
                     :size :medium
                     :orientation :north}
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["magician"])}))
        state (game-state/with-board-pieces state [rose-sword-minion enemy-piece])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "magician"
                          :piece-id :rose-sword-minion}
                 :target {:kind :piece
                          :piece-id :indigo-sword-target}
                 :damage 1}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)]
    (is (= [:wild-suits] (cards/sword-variants (cards/card-by-id "magician"))))
    (is ok?)
    (is (= :sword/piece-shrunk (get-in events [0 :type])))
    (is (= :wild-suits (get-in events [0 :sword-variant])))
    (is (= ["magician"] (mapv :id (:discard-pile state))))
    (is (not (some #{"magician"} (player-hand-ids state :rose))))
    (is (= events [(peek (:history state))]))
    (is (game-schema/valid-game? state))))
(deftest tower-sword-discard_pile_replacement_is_only_for_surviving_territories
  (let [state (-> (state-with-pieces [rose-sword-minion])
                  (state-with-board-card 3 "tower")
                  (state-with-board-card 4 "cups2")
                  (move-card-to-discard "cupsking"))
        piece-result (game-state/resolve-sword-command
                      state
                      {:player-id :rose
                       :source {:kind :territory
                                :board-index 3
                                :piece-id :rose-sword-minion}
                       :target {:kind :piece
                                :piece-id :rose-sword-minion}
                       :damage 1
                       :replacement-card-source :discard-pile
                       :replacement-card-id "cupsking"})
        destroyed-territory-result (game-state/resolve-sword-command
                                    state
                                    {:player-id :rose
                                     :source {:kind :territory
                                              :board-index 3
                                              :piece-id :rose-sword-minion}
                                     :target {:kind :territory
                                              :board-index 4}
                                     :damage 1
                                     :replacement-card-source :discard-pile
                                     :replacement-card-id "cupsking"})]
    (is (= :invalid-sword-replacement
           (get-in piece-result [:error :code])))
    (is (= :invalid-sword-replacement
           (get-in destroyed-territory-result [:error :code])))
    (is (not (contains? piece-result :state)))
    (is (not (contains? destroyed-territory-result :state)))))
(deftest sword-move-shrinks-current-player-piece-and-may-reorient-it
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 3 "swords2")}))
        state (game-state/with-board-pieces state [rose-sword-minion])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-sword-minion}
                 :target {:kind :piece
                          :piece-id :rose-sword-minion}
                 :damage 1
                 :orientation :south}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)
        shrunk-piece (piece-by-id state :rose-small-1)]
    (is ok?)
    (is (nil? (piece-by-id state :rose-sword-minion)))
    (is (= {:id :rose-small-1
            :player-id :rose
            :space-index 3
            :size :small
            :orientation :south}
           shrunk-piece))
    (is (= 4 (get-in state [:players-by-id :rose :stash :small])))
    (is (= 5 (get-in state [:players-by-id :rose :stash :medium])))
    (is (= 4 (get-in state [:pieces :stashes :rose :small])))
    (is (= 5 (get-in state [:pieces :stashes :rose :medium])))
    (is (= [{:type :sword/piece-shrunk
             :player-id :rose
             :source {:kind :territory
                      :board-index 3
                      :piece-id :rose-sword-minion}
             :sword-variant :sword
             :target {:kind :piece
                      :piece-id :rose-sword-minion
                      :player-id :rose
                      :board-index 3
                      :row 1
                      :col 0}
             :damage 1
             :from-size :medium
             :to-size :small
             :replaced-piece rose-sword-minion
             :piece shrunk-piece}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
(deftest sword-move-shrinks-enemy-piece-and-discards-hand-source
  (let [enemy-piece {:id :indigo-sword-target
                     :player-id :indigo
                     :space-index 4
                     :size :large
                     :orientation :north}
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["swords2"])}))
        state (game-state/with-board-pieces state [rose-sword-minion enemy-piece])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "swords2"
                          :piece-id :rose-sword-minion}
                 :target {:kind :piece
                          :piece-id :indigo-sword-target}
                 :damage 1}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)
        shrunk-piece (piece-by-id state :indigo-medium-1)]
    (is ok?)
    (is (nil? (piece-by-id state :indigo-sword-target)))
    (is (= {:id :indigo-medium-1
            :player-id :indigo
            :space-index 4
            :size :medium
            :orientation :north}
           shrunk-piece))
    (is (= ["swords2"] (mapv :id (:discard-pile state))))
    (is (not (some #{"swords2"} (player-hand-ids state :rose))))
    (is (= 4 (get-in state [:players-by-id :indigo :stash :medium])))
    (is (= 5 (get-in state [:players-by-id :indigo :stash :large])))
    (is (= :sword/piece-shrunk (get-in events [0 :type])))
    (is (= :indigo (get-in events [0 :target :player-id])))
    (is (= enemy-piece (get-in events [0 :replaced-piece])))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
(deftest sword-move-destroys-small-piece
  (let [enemy-piece {:id :indigo-sword-target
                     :player-id :indigo
                     :space-index 4
                     :size :small
                     :orientation :north}
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["swords2"])}))
        state (game-state/with-board-pieces state [rose-sword-minion enemy-piece])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "swords2"
                          :piece-id :rose-sword-minion}
                 :target {:kind :piece
                          :piece-id :indigo-sword-target}
                 :damage 1}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)]
    (is ok?)
    (is (= [rose-sword-minion]
           (get-in state [:pieces :on-board])))
    (is (= 5 (get-in state [:players-by-id :indigo :stash :small])))
    (is (= 5 (get-in state [:pieces :stashes :indigo :small])))
    (is (= ["swords2"] (mapv :id (:discard-pile state))))
    (is (= [{:type :sword/piece-destroyed
             :player-id :rose
             :source {:kind :hand-card
                      :card-id "swords2"
                      :piece-id :rose-sword-minion}
             :sword-variant :sword
             :target {:kind :piece
                      :piece-id :indigo-sword-target
                      :player-id :indigo
                      :board-index 4
                      :row 1
                      :col 1}
             :damage 1
             :from-size :small
             :destroyed-piece enemy-piece}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
(deftest sword-move-rejects-missing-smaller-piece-and-overdamage_without_mutation
  (let [target-piece {:id :indigo-sword-target
                      :player-id :indigo
                      :space-index 4
                      :size :medium
                      :orientation :north}
        small-pieces [{:id :indigo-small-a
                       :player-id :indigo
                       :space-index 0
                       :size :small
                       :orientation :north}
                      {:id :indigo-small-b
                       :player-id :indigo
                       :space-index 1
                       :size :small
                       :orientation :east}
                      {:id :indigo-small-c
                       :player-id :indigo
                       :space-index 2
                       :size :small
                       :orientation :south}
                      {:id :indigo-small-d
                       :player-id :indigo
                       :space-index 5
                       :size :small
                       :orientation :west}
                      {:id :indigo-small-e
                       :player-id :indigo
                       :space-index 6
                       :size :small
                       :orientation :up}]
        no-small-pieces (vec (concat [rose-sword-minion target-piece]
                                     small-pieces))
        no-small-state (:state (game-state/create-game
                                player-specs
                                {:deck-order (deck-with-board-card 3 "swords2")}))
        no-small-state (game-state/with-board-pieces no-small-state
                                                     no-small-pieces)
        no-small-result (game-state/apply-sword-move
                         no-small-state
                         {:player-id :rose
                          :source {:kind :territory
                                   :board-index 3
                                   :piece-id :rose-sword-minion}
                          :target {:kind :piece
                                   :piece-id :indigo-sword-target}
                          :damage 1})
        small-target {:id :indigo-small-target
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
        overdamage-state (:state (game-state/create-game
                                  player-specs
                                  {:deck-order (deck-with-board-card 3 "swords2")}))
        overdamage-state (game-state/with-board-pieces
                          overdamage-state
                          [rose-sword-minion small-target])
        overdamage-result (game-state/apply-sword-move
                           overdamage-state
                           {:player-id :rose
                            :source {:kind :territory
                                     :board-index 3
                                     :piece-id :rose-sword-minion}
                            :target {:kind :piece
                                     :piece-id :indigo-small-target}
                            :damage 2})]
    (is (= :no-smaller-piece-available
           (get-in no-small-result [:error :code])))
    (is (= :invalid-sword-damage
           (get-in overdamage-result [:error :code])))
    (is (not (contains? no-small-result :state)))
    (is (not (contains? overdamage-result :state)))
    (is (= no-small-pieces
           (get-in no-small-state [:pieces :on-board])))
    (is (= [rose-sword-minion small-target]
           (get-in overdamage-state [:pieces :on-board])))
    (is (empty? (:discard-pile no-small-state)))
    (is (empty? (:discard-pile overdamage-state)))))
(deftest sword-move-shrinks-royalty-territory-with-hand-replacement
  (let [deck-order (deck-with-cards-at {0 "cups2"
                                        (board-card-position 3) "swords2"
                                        (board-card-position 4) "cupsking"})
        target-piece (assoc rose-target-minion
                            :space-index 4
                            :orientation :south)
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-sword-minion target-piece])
        original-cell (board-cell-by-index state 4)
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-sword-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :damage 1
                 :replacement-card-source :hand
                 :replacement-card-id "cups2"}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)
        shrunk-cell (board-cell-by-index state 4)]
    (is ok?)
    (is (= "cups2" (get-in shrunk-cell [:card :id])))
    (is (= (select-keys original-cell [:index :row :col :orientation :face])
           (select-keys shrunk-cell [:index :row :col :orientation :face])))
    (is (= target-piece (piece-by-id state :rose-target-minion)))
    (is (= ["cupsking"] (mapv :id (:discard-pile state))))
    (is (not (some #{"cups2"} (player-hand-ids state :rose))))
    (is (= [{:type :sword/territory-shrunk
             :player-id :rose
             :source {:kind :territory
                      :board-index 3
                      :piece-id :rose-sword-minion}
             :sword-variant :sword
             :target {:kind :territory
                      :board-index 4
                      :row 1
                      :col 1}
             :damage 1
             :replacement-card-source :hand
             :original-card-id "cupsking"
             :replacement-card-id "cups2"
             :from-value 2
             :to-value 1
             :territory shrunk-cell}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
(deftest sword-move-rejects-missing_reused_and_invalid_territory_replacements_without_mutation
  (let [deck-order (deck-with-cards-at {0 "swords2"
                                        1 "swordsking"
                                        (board-card-position 4) "cupsking"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-sword-minion])
        base-command {:player-id :rose
                      :source {:kind :hand-card
                               :card-id "swords2"
                               :piece-id :rose-sword-minion}
                      :target {:kind :territory
                               :board-index 4}
                      :damage 1}
        missing-result (game-state/apply-sword-move state base-command)
        reused-result (game-state/apply-sword-move
                       state
                       (assoc base-command :replacement-card-id "swords2"))
        invalid-result (game-state/apply-sword-move
                        state
                        (assoc base-command :replacement-card-id "swordsking"))]
    (is (= :invalid-sword-replacement
           (get-in missing-result [:error :code])))
    (is (= :card-already-used
           (get-in reused-result [:error :code])))
    (is (= :invalid-sword-replacement-card
           (get-in invalid-result [:error :code])))
    (is (not (contains? missing-result :state)))
    (is (not (contains? reused-result :state)))
    (is (not (contains? invalid-result :state)))
    (is (= "cupsking" (get-in (board-cell-by-index state 4) [:card :id])))
    (is (= ["swords2" "swordsking"]
           (filterv #{"swords2" "swordsking"} (player-hand-ids state :rose))))
    (is (empty? (:discard-pile state)))))
(deftest tower-sword-move-can-shrink-major-territory-from-discard_source
  (let [deck-order (deck-with-cards-at {0 "cupsking"
                                        (board-card-position 3) "tower"
                                        (board-card-position 4) "star"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (-> state
                  (move-card-to-discard "cupsking")
                  (game-state/with-board-pieces [rose-sword-minion]))
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-sword-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :damage 1
                 :replacement-card-source :discard-pile
                 :replacement-card-id "cupsking"}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)]
    (is ok?)
    (is (= "cupsking" (get-in (board-cell-by-index state 4) [:card :id])))
    (is (= ["star"] (mapv :id (:discard-pile state))))
    (is (= :sword/territory-shrunk (get-in events [0 :type])))
    (is (= :sword-from-discard (get-in events [0 :sword-variant])))
    (is (= :discard-pile (get-in events [0 :replacement-card-source])))
    (is (= 3 (get-in events [0 :from-value])))
    (is (= 2 (get-in events [0 :to-value])))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
(deftest sword-move-destroys-spot-territory-and-voided-pieces
  (let [deck-order (deck-with-cards-at {(board-card-position 0) "cups2"
                                        (board-card-position 1) "swords2"})
        sword-minion (assoc rose-sword-minion
                            :space-index 1
                            :size :small
                            :orientation :west)
        target-piece {:id :rose-target-territory-minion
                      :player-id :rose
                      :space-index 0
                      :size :medium
                      :orientation :south}
        voided-piece {:id :rose-outboard-minion
                      :player-id :rose
                      :space {:kind :wasteland
                              :row 0
                              :col -1}
                      :size :small
                      :orientation :north}
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [sword-minion
                                                   target-piece
                                                   voided-piece])
        original-cell (board-cell-by-index state 0)
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 1
                          :piece-id :rose-sword-minion}
                 :target {:kind :territory
                          :board-index 0}
                 :damage 1}
        {:keys [ok? state events]} (game-state/apply-sword-move state command)
        surviving-target-piece (piece-by-id state :rose-target-territory-minion)]
    (is ok?)
    (is (nil? (board-cell-by-index state 0)))
    (is (= "cups2" (get-in original-cell [:card :id])))
    (is (= ["cups2"] (mapv :id (:discard-pile state))))
    (is (= sword-minion (piece-by-id state :rose-sword-minion)))
    (is (= {:id :rose-target-territory-minion
            :player-id :rose
            :space {:kind :wasteland
                    :row 0
                    :col 0}
            :size :medium
            :orientation :south}
           surviving-target-piece))
    (is (nil? (piece-by-id state :rose-outboard-minion)))
    (is (= 4 (get-in state [:players-by-id :rose :stash :small])))
    (is (= 4 (get-in state [:players-by-id :rose :stash :medium])))
    (is (= [{:type :sword/territory-destroyed
             :player-id :rose
             :source {:kind :territory
                      :board-index 1
                      :piece-id :rose-sword-minion}
             :sword-variant :sword
             :target {:kind :territory
                      :board-index 0
                      :row 0
                      :col 0}
             :damage 1
             :original-card-id "cups2"
             :from-value 1
             :destroyed-territory original-cell
             :destroyed-pieces [voided-piece]}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
