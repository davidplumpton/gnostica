(ns gnostica.game-state.disc-test
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

(deftest disc-command-normalizes-territory-source-and-piece-target
  (let [target-piece {:id :indigo-disc-target
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
        state (-> (state-with-pieces [rose-disc-minion target-piece])
                  (state-with-board-card 3 "coins2"))
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-disc-minion}
                 :target {:kind :piece
                          :piece-id :indigo-disc-target}}
        result (game-state/resolve-disc-command state command)]
    (is (:ok? result))
    (is (= {:player-id :rose
            :source {:kind :territory
                     :board-index 3
                     :piece-id :rose-disc-minion}
            :disc-variant :disc
            :target {:kind :piece
                     :piece-id :indigo-disc-target
                     :player-id :indigo
                     :board-index 4
                     :row 1
                     :col 1}}
           (:command result)))
    (is (= "coins2" (get-in result [:source-card :id])))
    (is (= rose-disc-minion (:piece result)))
    (is (= target-piece (:target-piece result)))))
(deftest disc-command-normalizes-hand-card-source-and-territory-target_options
  (let [deck-order (deck-starting-with ["coins2" "cupsking"])
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [(assoc rose-disc-minion
                                                          :orientation :up)])
        result (game-state/resolve-disc-command
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "coins2"
                          :piece-id :rose-disc-minion}
                 :target {:kind :territory
                          :board-index 3}
                 :replacement-card-id "cupsking"})]
    (is (:ok? result))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "coins2"
                     :piece-id :rose-disc-minion}
            :disc-variant :disc
            :target {:kind :territory
                     :board-index 3
                     :row 1
                     :col 0}
            :replacement-card-source :hand
            :replacement-card-id "cupsking"}
           (:command result)))
    (is (= "coins2" (get-in result [:source-card :id])))
    (is (= 3 (get-in result [:target-cell :index])))))
(deftest disc-command-allows-upright_current_space_and_minion_self_targeting
  (let [upright-state (-> (state-with-pieces [(assoc rose-disc-minion
                                                     :orientation :up)])
                          (state-with-board-card 3 "coins2"))
        territory-result (game-state/resolve-disc-command
                          upright-state
                          {:player-id :rose
                           :source {:kind :territory
                                    :board-index 3
                                    :piece-id :rose-disc-minion}
                           :target {:kind :territory
                                    :board-index 3}})
        self-state (-> (state-with-pieces [rose-disc-minion])
                       (state-with-board-card 3 "coins2"))
        self-result (game-state/resolve-disc-command
                     self-state
                     {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-disc-minion}
                      :target {:kind :piece
                               :piece-id :rose-disc-minion}
                      :orientation :south})]
    (is (:ok? territory-result))
    (is (= {:kind :territory
            :board-index 3
            :row 1
            :col 0}
           (get-in territory-result [:command :target])))
    (is (:ok? self-result))
    (is (= {:kind :piece
            :piece-id :rose-disc-minion
            :player-id :rose
            :board-index 3
            :row 1
            :col 0
            :orientation :south}
           (get-in self-result [:command :target])))
    (is (= :south (get-in self-result [:command :orientation])))))
(deftest disc-command-carries-source-variants
  (let [base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-disc-minion}
                      :target {:kind :piece
                               :piece-id :rose-disc-minion}}
        strength-result (game-state/resolve-disc-command
                         (-> (state-with-pieces [rose-disc-minion])
                             (state-with-board-card 3 "strength"))
                         base-command)
        star-result (game-state/resolve-disc-command
                     (-> (state-with-pieces [rose-disc-minion])
                         (state-with-board-card 3 "star"))
                     base-command)
        sun-result (game-state/resolve-disc-command
                    (-> (state-with-pieces [rose-disc-minion])
                        (state-with-board-card 3 "sun"))
                    base-command)
        magician-result (game-state/resolve-disc-command
                         (-> (state-with-pieces [rose-disc-minion])
                             (state-with-board-card 3 "magician"))
                         base-command)]
    (is (:ok? strength-result))
    (is (:ok? star-result))
    (is (:ok? sun-result))
    (is (:ok? magician-result))
    (is (= :disc (get-in strength-result [:command :disc-variant])))
    (is (= :disc-from-discard (get-in star-result [:command :disc-variant])))
    (is (= :disc (get-in sun-result [:command :disc-variant])))
    (is (= :wild-suits (get-in magician-result [:command :disc-variant])))))
(deftest disc-command-rejects-unavailable_and_invalid_variants
  (let [state (-> (state-with-pieces [rose-disc-minion])
                  (state-with-board-card 3 "coins2"))
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-disc-minion}
                      :target {:kind :piece
                               :piece-id :rose-disc-minion}}
        unavailable-result (game-state/resolve-disc-command
                            state
                            (assoc base-command :disc-variant :disc-from-discard))
        invalid-result (game-state/resolve-disc-command
                        state
                        (assoc base-command :disc-variant :rod))
        discard-source-result (game-state/resolve-disc-command
                               state
                               (assoc base-command
                                      :target {:kind :territory
                                               :board-index 4}
                                      :replacement-card-source :discard-pile
                                      :replacement-card-id "cupsking"))
        non-disc-result (game-state/resolve-disc-command
                         (-> (state-with-pieces [rose-disc-minion])
                             (state-with-board-card 3 "wands2"))
                         base-command)]
    (is (= :disc-variant-unavailable
           (get-in unavailable-result [:error :code])))
    (is (= :invalid-disc-variant
           (get-in invalid-result [:error :code])))
    (is (= :disc-variant-option-unavailable
           (get-in discard-source-result [:error :code])))
    (is (= :source-card-not-disc
           (get-in non-disc-result [:error :code])))))
(deftest disc-command-rejects_invalid_targets_and_options_without_mutation
  (let [off-axis-target {:id :indigo-off-axis-disc-target
                         :player-id :indigo
                         :space-index 0
                         :size :small
                         :orientation :north}
        enemy-target {:id :indigo-disc-target
                      :player-id :indigo
                      :space-index 4
                      :size :small
                      :orientation :north}
        state (-> (state-with-pieces [rose-disc-minion off-axis-target enemy-target])
                  (state-with-board-card 3 "coins2"))
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-disc-minion}}
        off-axis-result (game-state/resolve-disc-command
                         state
                         (assoc base-command
                                :target {:kind :piece
                                         :piece-id :indigo-off-axis-disc-target}))
        enemy-orientation-result (game-state/resolve-disc-command
                                  state
                                  (assoc base-command
                                         :target {:kind :piece
                                                  :piece-id :indigo-disc-target}
                                         :orientation :west))
        enemy-territory-result (game-state/resolve-disc-command
                                state
                                (assoc base-command
                                       :target {:kind :territory
                                                :board-index 4}))
        piece-replacement-result (game-state/resolve-disc-command
                                  state
                                  (assoc base-command
                                         :target {:kind :piece
                                                  :piece-id :rose-disc-minion}
                                         :replacement-card-id "cupsking"))]
    (is (= :invalid-disc-target
           (get-in off-axis-result [:error :code])))
    (is (= :invalid-orientation
           (get-in enemy-orientation-result [:error :code])))
    (is (= :target-territory-occupied-by-enemy
           (get-in enemy-territory-result [:error :code])))
    (is (= :invalid-disc-replacement
           (get-in piece-replacement-result [:error :code])))
    (is (false? (:ok? off-axis-result)))
    (is (not (contains? off-axis-result :state)))
    (is (= [rose-disc-minion off-axis-target enemy-target]
           (get-in state [:pieces :on-board])))))
(deftest star-disc-command-allows_discard_pile_replacement_source
  (let [state (-> (state-with-pieces [rose-disc-minion])
                  (state-with-board-card 3 "star"))
        result (game-state/resolve-disc-command
                state
                {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-disc-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :replacement-card-source :discard-pile
                 :replacement-card-id "cupsking"})]
    (is (:ok? result))
    (is (= :disc-from-discard
           (get-in result [:command :disc-variant])))
    (is (= :discard-pile
           (get-in result [:command :replacement-card-source])))))
(deftest disc-move-grows-spot-territory-with-hand-replacement
  (let [deck-order (deck-with-cards-at {0 "cupsking"
                                        (board-card-position 3) "coins2"
                                        (board-card-position 4) "cups2"})
        target-piece (assoc rose-target-minion
                            :space-index 4
                            :orientation :south)
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-disc-minion target-piece])
        original-cell (board-cell-by-index state 4)
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-disc-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :replacement-card-source :hand
                 :replacement-card-id "cupsking"}
        {:keys [ok? state events]} (game-state/apply-disc-move state command)
        grown-cell (board-cell-by-index state 4)]
    (is ok?)
    (is (= "cupsking" (get-in grown-cell [:card :id])))
    (is (= (select-keys original-cell [:index :row :col :orientation :face])
           (select-keys grown-cell [:index :row :col :orientation :face])))
    (is (= target-piece (piece-by-id state :rose-target-minion)))
    (is (= ["cups2"] (mapv :id (:discard-pile state))))
    (is (not (some #{"cupsking"} (player-hand-ids state :rose))))
    (is (= [{:type :disc/territory-grown
             :player-id :rose
             :source {:kind :territory
                      :board-index 3
                      :piece-id :rose-disc-minion}
             :disc-variant :disc
             :target {:kind :territory
                      :board-index 4
                      :row 1
                      :col 1}
             :replacement-card-source :hand
             :original-card-id "cups2"
             :replacement-card-id "cupsking"
             :from-value 1
             :to-value 2
             :territory grown-cell}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
(deftest disc-move-hand-source-grows-territory-without-duplicating-cards
  (let [deck-order (deck-with-cards-at {0 "coins2"
                                        1 "cupsking"
                                        (board-card-position 4) "cups2"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-disc-minion])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "coins2"
                          :piece-id :rose-disc-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :replacement-card-source :hand
                 :replacement-card-id "cupsking"}
        {:keys [ok? state events]} (game-state/apply-disc-move state command)]
    (is ok?)
    (is (= "cupsking" (get-in (board-cell-by-index state 4) [:card :id])))
    (is (= ["coins2" "cups2"] (mapv :id (:discard-pile state))))
    (is (not (some #{"coins2"} (player-hand-ids state :rose))))
    (is (not (some #{"cupsking"} (player-hand-ids state :rose))))
    (is (= :disc/territory-grown (get-in events [0 :type])))
    (is (= :hand-card (get-in events [0 :source :kind])))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
(deftest star-disc-move-can-grow-royalty-territory-from-discard_source
  (let [deck-order (deck-with-cards-at {0 "star"
                                        (board-card-position 4) "cupsking"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-disc-minion])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "star"
                          :piece-id :rose-disc-minion}
                 :target {:kind :territory
                          :board-index 4}
                 :replacement-card-source :discard-pile
                 :replacement-card-id "star"}
        {:keys [ok? state events]} (game-state/apply-disc-move state command)]
    (is ok?)
    (is (= "star" (get-in (board-cell-by-index state 4) [:card :id])))
    (is (= ["cupsking"] (mapv :id (:discard-pile state))))
    (is (not (some #{"star"} (player-hand-ids state :rose))))
    (is (= :disc-from-discard (get-in events [0 :disc-variant])))
    (is (= :discard-pile (get-in events [0 :replacement-card-source])))
    (is (= 2 (get-in events [0 :from-value])))
    (is (= 3 (get-in events [0 :to-value])))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
(deftest star-disc-orients-minion-before-targeting
  (let [deck-order (deck-with-cards-at {0 "star"
                                        (board-card-position 4) "cupsking"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces
                state
                [(assoc rose-disc-minion :orientation :north)])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "star"
                          :piece-id :rose-disc-minion}
                 :minion-orientation :east
                 :target {:kind :territory
                          :board-index 4}
                 :replacement-card-source :discard-pile
                 :replacement-card-id "star"}
        {:keys [ok? state events]} (game-state/apply-disc-move state command)]
    (is ok?)
    (is (= [:piece/oriented :disc/territory-grown]
           (mapv :type events)))
    (is (= :east (:orientation (piece-by-id state :rose-disc-minion))))
    (is (= "star" (get-in (board-cell-by-index state 4) [:card :id])))
    (is (= ["cupsking"] (mapv :id (:discard-pile state))))
    (is (game-schema/valid-game? state))))
(deftest territory-strength-disc-promotes-grown-piece-into-next-minion
  (let [first-target {:id :rose-strength-first-target
                      :player-id :rose
                      :space-index 4
                      :size :small
                      :orientation :east}
        second-target {:id :rose-strength-second-target
                       :player-id :rose
                       :space-index 5
                       :size :small
                       :orientation :south}
        state (-> (:state (game-state/create-game
                           player-specs
                           {:deck-order (deck-with-board-card 3 "strength")}))
                  (game-state/with-board-pieces [rose-disc-minion
                                                 first-target
                                                 second-target]))
        {:keys [ok? state events]} (game-state/apply-disc-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :territory
                                              :board-index 3
                                              :piece-id :rose-disc-minion}
                                     :disc-actions [{:target {:kind :piece
                                                              :piece-id (:id first-target)}}
                                                    {:piece-id :rose-medium-1
                                                     :target {:kind :piece
                                                              :piece-id (:id second-target)}}]})
        first-grown (piece-by-id state :rose-medium-1)
        second-grown (piece-by-id state :rose-medium-2)]
    (is ok?)
    (is (= [:disc/piece-grown :disc/piece-grown]
           (mapv :type events)))
    (is (= {:id :rose-medium-1
            :player-id :rose
            :space-index 4
            :size :medium
            :orientation :east}
           first-grown))
    (is (= {:id :rose-medium-2
            :player-id :rose
            :space-index 5
            :size :medium
            :orientation :south}
           second-grown))
    (is (nil? (piece-by-id state (:id first-target))))
    (is (nil? (piece-by-id state (:id second-target))))
    (is (empty? (:discard-pile state)))
    (is (game-schema/valid-game? state))))
(deftest territory-strength-disc-rejects-unpromoted-later-minion
  (let [first-target {:id :rose-strength-first-target
                      :player-id :rose
                      :space-index 4
                      :size :small
                      :orientation :east}
        second-target {:id :rose-strength-second-target
                       :player-id :rose
                       :space-index 5
                       :size :small
                       :orientation :south}
        unrelated-piece {:id :rose-unpromoted-disc-minion
                         :player-id :rose
                         :space-index 0
                         :size :small
                         :orientation :east}
        state (-> (:state (game-state/create-game
                           player-specs
                           {:deck-order (deck-with-board-card 3 "strength")}))
                  (game-state/with-board-pieces [rose-disc-minion
                                                 first-target
                                                 second-target
                                                 unrelated-piece]))
        result (game-state/apply-disc-move
                state
                {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-disc-minion}
                 :disc-actions [{:target {:kind :piece
                                          :piece-id (:id first-target)}}
                                {:piece-id (:id unrelated-piece)
                                 :target {:kind :piece
                                          :piece-id (:id second-target)}}]})]
    (is (= :invalid-major-minion
           (get-in result [:error :code])))
    (is (not (contains? result :state)))
    (is (nil? (piece-by-id state :rose-medium-1)))
    (is (= first-target (piece-by-id state (:id first-target))))
    (is (= second-target (piece-by-id state (:id second-target))))
    (is (game-schema/valid-game? state))))
(deftest strength-disc-can_skip_intermediate_piece_size
  (let [small-minion (assoc rose-disc-minion :size :small)
        medium-pieces [{:id :rose-medium-a
                        :player-id :rose
                        :space-index 0
                        :size :medium
                        :orientation :north}
                       {:id :rose-medium-b
                        :player-id :rose
                        :space-index 1
                        :size :medium
                        :orientation :east}
                       {:id :rose-medium-c
                        :player-id :rose
                        :space-index 2
                        :size :medium
                        :orientation :south}
                       {:id :rose-medium-d
                        :player-id :rose
                        :space-index 4
                        :size :medium
                        :orientation :west}
                       {:id :rose-medium-e
                        :player-id :rose
                        :space-index 5
                        :size :medium
                        :orientation :up}]
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["strength"])}))
        state (game-state/with-board-pieces
                state
                (vec (cons small-minion medium-pieces)))
        {:keys [ok? state events]} (game-state/apply-disc-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "strength"
                                              :piece-id :rose-disc-minion}
                                     :disc-actions [{:target {:kind :piece
                                                              :piece-id :rose-disc-minion}}
                                                    {:target {:kind :piece
                                                              :piece-id :rose-disc-minion}}]})
        grown-piece (piece-by-id state :rose-large-1)]
    (is ok?)
    (is (= {:id :rose-large-1
            :player-id :rose
            :space-index 3
            :size :large
            :orientation :east}
           grown-piece))
    (is (= ["strength"] (mapv :id (:discard-pile state))))
    (is (= :small (get-in events [0 :from-size])))
    (is (= :large (get-in events [0 :to-size])))
    (is (= 2 (get-in events [0 :action-count])))
    (is (true? (get-in events [0 :shortcut?])))
    (is (= 0 (get-in state [:pieces :stashes :rose :medium])))
    (is (game-schema/valid-game? state))))
(deftest strength-disc-can_skip_intermediate_territory_value
  (let [deck-order (deck-with-cards-at {0 "strength"
                                        1 "star"
                                        (board-card-position 4) "cups2"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-disc-minion])
        {:keys [ok? state events]} (game-state/apply-disc-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :hand-card
                                              :card-id "strength"
                                              :piece-id :rose-disc-minion}
                                     :disc-actions [{:target {:kind :territory
                                                              :board-index 4}}
                                                    {:target {:kind :territory
                                                              :board-index 4}
                                                     :replacement-card-id "star"}]})
        grown-cell (board-cell-by-index state 4)]
    (is ok?)
    (is (= "star" (get-in grown-cell [:card :id])))
    (is (= ["strength" "cups2"] (mapv :id (:discard-pile state))))
    (is (= 1 (get-in events [0 :from-value])))
    (is (= 3 (get-in events [0 :to-value])))
    (is (= 2 (get-in events [0 :action-count])))
    (is (true? (get-in events [0 :shortcut?])))
    (is (not (some #{"strength" "star"} (player-hand-ids state :rose))))
    (is (game-schema/valid-game? state))))
(deftest disc-move-rejects-missing-or-invalid-territory_replacements_without_mutation
  (let [deck-order (deck-with-cards-at {0 "sun"
                                        (board-card-position 3) "coins2"
                                        (board-card-position 4) "cups2"})
        state (:state (game-state/create-game player-specs {:deck-order deck-order}))
        state (game-state/with-board-pieces state [rose-disc-minion])
        base-command {:player-id :rose
                      :source {:kind :territory
                               :board-index 3
                               :piece-id :rose-disc-minion}
                      :target {:kind :territory
                               :board-index 4}}
        missing-result (game-state/apply-disc-move state base-command)
        invalid-result (game-state/apply-disc-move
                        state
                        (assoc base-command
                               :replacement-card-id "sun"))]
    (is (= :invalid-disc-replacement
           (get-in missing-result [:error :code])))
    (is (= :invalid-disc-replacement-card
           (get-in invalid-result [:error :code])))
    (is (not (contains? missing-result :state)))
    (is (not (contains? invalid-result :state)))
    (is (= "cups2" (get-in (board-cell-by-index state 4) [:card :id])))
    (is (= ["sun"] (take 1 (player-hand-ids state :rose))))
    (is (empty? (:discard-pile state)))))
(deftest disc-move-grows-current-player-piece-and-may-reorient-it
  (let [small-minion (assoc rose-disc-minion :size :small)
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 3 "coins2")}))
        state (game-state/with-board-pieces state [small-minion])
        command {:player-id :rose
                 :source {:kind :territory
                          :board-index 3
                          :piece-id :rose-disc-minion}
                 :target {:kind :piece
                          :piece-id :rose-disc-minion}
                 :orientation :south}
        {:keys [ok? state events]} (game-state/apply-disc-move state command)
        grown-piece (piece-by-id state :rose-medium-1)]
    (is ok?)
    (is (nil? (piece-by-id state :rose-disc-minion)))
    (is (= {:id :rose-medium-1
            :player-id :rose
            :space-index 3
            :size :medium
            :orientation :south}
           grown-piece))
    (is (= 5 (get-in state [:players-by-id :rose :stash :small])))
    (is (= 4 (get-in state [:players-by-id :rose :stash :medium])))
    (is (= 5 (get-in state [:pieces :stashes :rose :small])))
    (is (= 4 (get-in state [:pieces :stashes :rose :medium])))
    (is (= [{:type :disc/piece-grown
             :player-id :rose
             :source {:kind :territory
                      :board-index 3
                      :piece-id :rose-disc-minion}
             :disc-variant :disc
             :target {:kind :piece
                      :piece-id :rose-disc-minion
                      :player-id :rose
                      :board-index 3
                      :row 1
                      :col 0}
             :from-size :small
             :to-size :medium
             :replaced-piece small-minion
             :piece grown-piece}]
           events))
    (is (= events [(peek (:history state))]))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
(deftest disc-move-grows-medium-pieces-to-large
  (let [state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-with-board-card 3 "coins2")}))
        state (game-state/with-board-pieces state [rose-disc-minion])
        {:keys [ok? state events]} (game-state/apply-disc-move
                                    state
                                    {:player-id :rose
                                     :source {:kind :territory
                                              :board-index 3
                                              :piece-id :rose-disc-minion}
                                     :target {:kind :piece
                                              :piece-id :rose-disc-minion}})
        grown-piece (piece-by-id state :rose-large-1)]
    (is ok?)
    (is (= {:id :rose-large-1
            :player-id :rose
            :space-index 3
            :size :large
            :orientation :east}
           grown-piece))
    (is (= :medium (get-in events [0 :from-size])))
    (is (= :large (get-in events [0 :to-size])))
    (is (= 5 (get-in state [:players-by-id :rose :stash :medium])))
    (is (= 4 (get-in state [:players-by-id :rose :stash :large])))
    (is (game-schema/valid-game? state))))
(deftest disc-move-grows-enemy-piece-and-discards-hand-source
  (let [enemy-piece {:id :indigo-disc-target
                     :player-id :indigo
                     :space-index 4
                     :size :small
                     :orientation :north}
        state (:state (game-state/create-game
                       player-specs
                       {:deck-order (deck-starting-with ["coins2"])}))
        state (game-state/with-board-pieces state [rose-disc-minion enemy-piece])
        command {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "coins2"
                          :piece-id :rose-disc-minion}
                 :target {:kind :piece
                          :piece-id :indigo-disc-target}}
        {:keys [ok? state events]} (game-state/apply-disc-move state command)
        grown-piece (piece-by-id state :indigo-medium-1)]
    (is ok?)
    (is (= {:id :indigo-medium-1
            :player-id :indigo
            :space-index 4
            :size :medium
            :orientation :north}
           grown-piece))
    (is (= ["coins2"] (mapv :id (:discard-pile state))))
    (is (not (some #{"coins2"} (player-hand-ids state :rose))))
    (is (= 5 (get-in state [:players-by-id :indigo :stash :small])))
    (is (= 4 (get-in state [:players-by-id :indigo :stash :medium])))
    (is (= :disc/piece-grown (get-in events [0 :type])))
    (is (= :indigo (get-in events [0 :target :player-id])))
    (is (= enemy-piece (get-in events [0 :replaced-piece])))
    (is (= (count cards/deck) (count (all-card-ids state))))
    (is (= (count cards/deck) (count (set (all-card-ids state)))))
    (is (game-schema/valid-game? state))))
(deftest disc-move-rejects-large-pieces-and-missing-replacements_without_mutation
  (let [large-piece (assoc rose-disc-minion :size :large)
        large-state (-> (:state (game-state/create-game
                                 player-specs
                                 {:deck-order (deck-with-board-card 3 "coins2")}))
                        (game-state/with-board-pieces [large-piece]))
        large-result (game-state/apply-disc-move
                      large-state
                      {:player-id :rose
                       :source {:kind :territory
                                :board-index 3
                                :piece-id :rose-disc-minion}
                       :target {:kind :piece
                                :piece-id :rose-disc-minion}})
        small-piece (assoc rose-disc-minion :size :small)
        medium-pieces [{:id :rose-medium-a
                        :player-id :rose
                        :space-index 0
                        :size :medium
                        :orientation :north}
                       {:id :rose-medium-b
                        :player-id :rose
                        :space-index 1
                        :size :medium
                        :orientation :east}
                       {:id :rose-medium-c
                        :player-id :rose
                        :space-index 2
                        :size :medium
                        :orientation :south}
                       {:id :rose-medium-d
                        :player-id :rose
                        :space-index 4
                        :size :medium
                        :orientation :west}
                       {:id :rose-medium-e
                        :player-id :rose
                        :space-index 5
                        :size :medium
                        :orientation :up}]
        no-medium-pieces (vec (cons small-piece medium-pieces))
        no-medium-state (-> (:state (game-state/create-game
                                     player-specs
                                     {:deck-order (deck-with-board-card 3 "coins2")}))
                            (game-state/with-board-pieces no-medium-pieces))
        no-medium-result (game-state/apply-disc-move
                          no-medium-state
                          {:player-id :rose
                           :source {:kind :territory
                                    :board-index 3
                                    :piece-id :rose-disc-minion}
                           :target {:kind :piece
                                    :piece-id :rose-disc-minion}})]
    (is (= :target-piece-max-size
           (get-in large-result [:error :code])))
    (is (= :no-larger-piece-available
           (get-in no-medium-result [:error :code])))
    (is (not (contains? large-result :state)))
    (is (not (contains? no-medium-result :state)))
    (is (= [large-piece]
           (get-in large-state [:pieces :on-board])))
    (is (= no-medium-pieces
           (get-in no-medium-state [:pieces :on-board])))))
