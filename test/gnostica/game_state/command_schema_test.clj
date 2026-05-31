(ns gnostica.game-state.command-schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.game-state :as game-state]
            [gnostica.test-support.game-state :refer [player-specs]]))

(def hand-source
  {:kind :hand-card
   :card-id "sun"
   :piece-id :rose-minion})

(def major-hand-source
  {:kind :hand-card
   :card-id "fool"})

(def territory-source
  {:kind :territory
   :board-index 3
   :piece-id :rose-minion})

(deftest validates-representative-high-variance-command-contracts
  (let [commands {:world {:player-id :rose
                          :source (assoc hand-source :card-id "world")
                          :copied-board-index 4
                          :copied-power :cup
                          :target {:kind :territory
                                   :board-index 5}
                          :orientation :east}
                  :sun {:player-id :rose
                        :source hand-source
                        :cup {:target {:kind :territory
                                       :board-index 4}
                              :orientation :east}
                        :disc {:target {:kind :created-piece}
                               :orientation :up}}
                  :fool {:player-id :rose
                         :source major-hand-source
                         :reveals [{} {:power :cup
                                        :piece-id :rose-minion
                                        :play-command {:target {:kind :territory
                                                                :board-index 4}
                                                       :orientation :south}}]}
                  :high-priestess {:player-id :rose
                                   :source (assoc major-hand-source
                                                  :card-id "high-priestess")
                                   :redraws [{:discard-card-ids ["cups2"]
                                              :draw-count 1}
                                             {:draw-count 0}]}
                  :judgement {:player-id :rose
                              :source (assoc major-hand-source
                                             :card-id "judgement")
                              :piece-id :rose-minion
                              :card-ids ["cups2" "wands3"]}
                  :moon {:player-id :rose
                         :source (assoc hand-source :card-id "moon")
                         :rod {:mode :move-minion
                               :distance 1
                               :orientation :east}
                         :sword {:target {:kind :piece
                                          :piece-id :indigo-target}
                                 :damage 1}}
                  :empress {:player-id :rose
                            :source (assoc hand-source :card-id "empress")
                            :actions [{:power :orient-minion
                                       :piece-id :rose-minion
                                       :orientation :east}
                                      {:power :cup
                                       :piece-id :rose-minion
                                       :target {:kind :piece
                                                :piece-id :indigo-target}}]}
                  :disc {:player-id :rose
                         :source (assoc territory-source :board-index 4)
                         :disc-variant :disc-from-discard
                         :disc-actions [{:target {:kind :territory
                                                  :board-index 5}
                                         :replacement-card-source :discard-pile
                                         :replacement-card-id "cups3"}]}
                  :sword {:player-id :rose
                          :source (assoc hand-source :card-id "tower")
                          :sword-variant :sword-from-discard
                          :minion-orientation :east
                          :target {:kind :territory
                                   :board-index 5}
                          :damage 1
                          :replacement-card-source :discard-pile
                          :replacement-card-id "cups2"}}]
    (doseq [[command-kind command] commands]
      (testing command-kind
        (is (game-state/valid-command? command-kind command))
        (is (nil? (game-state/explain-command command-kind command)))
        (is (= {:ok? true
                :command command}
               (game-state/validate-command command-kind command)))))))

(deftest command-contract-validation-reports-structured-errors
  (let [invalid-world {:player-id :rose
                       :source (assoc hand-source :card-id "world")
                       :copied-power :cup}
        result (game-state/validate-command :world invalid-world)]
    (is (false? (game-state/valid-command? :world invalid-world)))
    (is (= :invalid-command-contract
           (get-in result [:error :code])))
    (is (= :world
           (get-in result [:error :data :command-kind])))
    (is (some? (get-in result [:error :data :errors])))
    (is (game-state/valid-result? result)))
  (is (false? (game-state/valid-command? :sword
                                          {:player-id :rose
                                           :source (assoc hand-source
                                                          :card-id "swords2")})))
  (is (false? (game-state/valid-command? :moon
                                          {:player-id :rose
                                           :source (assoc hand-source
                                                          :card-id "moon")}))))

(deftest world-and-fool-command-contracts-validate-delegated-shapes
  (let [world-cup {:player-id :rose
                   :source (assoc hand-source :card-id "world")
                   :copied-board-index 4
                   :copied-power :cup
                   :target {:kind :territory
                            :board-index 5}
                   :orientation :east}
        invalid-world-target (assoc world-cup
                                    :target {:kind :nonsense})
        invalid-world-extra-target {:player-id :rose
                                    :source (assoc hand-source :card-id "world")
                                    :copied-board-index 4
                                    :target {:kind :nonsense}
                                    :orientation :east}
        invalid-world-missing-minion (assoc world-cup
                                            :source {:kind :hand-card
                                                     :card-id "world"})
        fool-cup {:player-id :rose
                  :source major-hand-source
                  :reveals [{:power :cup
                             :piece-id :rose-minion
                             :play-command {:target {:kind :territory
                                                     :board-index 4}
                                            :orientation :south}}]}
        invalid-fool-target (assoc-in fool-cup
                                      [:reveals 0 :play-command :target]
                                      {:kind :nonsense})
        invalid-fool-missing-minion (update-in fool-cup
                                               [:reveals 0]
                                               dissoc
                                               :piece-id)]
    (is (game-state/valid-command? :world world-cup))
    (is (false? (game-state/valid-command? :world invalid-world-target)))
    (is (false? (game-state/valid-command? :world invalid-world-extra-target)))
    (is (false? (game-state/valid-command? :world invalid-world-missing-minion)))
    (is (game-state/valid-command? :fool fool-cup))
    (is (false? (game-state/valid-command? :fool invalid-fool-target)))
    (is (false? (game-state/valid-command? :fool invalid-fool-missing-minion)))))

(deftest suit-command-contracts-require-acting-minion-source
  (let [hand-source-without-minion (dissoc hand-source :piece-id)
        territory-source-without-minion (dissoc territory-source :piece-id)
        cases [["Cup hand-card source"
                :cup
                {:player-id :rose
                 :source (assoc hand-source-without-minion :card-id "cups2")
                 :target {:kind :territory
                          :board-index 4}
                 :orientation :east}]
               ["Rod territory source"
                :rod
                {:player-id :rose
                 :source (assoc territory-source-without-minion :board-index 4)
                 :mode :move-minion
                 :distance 1
                 :orientation :south}]
               ["Disc single action"
                :disc
                {:player-id :rose
                 :source (assoc hand-source-without-minion :card-id "coins2")
                 :target {:kind :piece
                          :piece-id :rose-minion}}]
               ["Disc multi-action"
                :disc
                {:player-id :rose
                 :source (assoc hand-source-without-minion :card-id "strength")
                 :disc-actions [{:target {:kind :piece
                                          :piece-id :rose-minion}}]}]
               ["Sun ordered suit action"
                :sun
                {:player-id :rose
                 :source (assoc hand-source-without-minion :card-id "sun")
                 :cup {:target {:kind :territory
                                :board-index 4}
                       :orientation :east}}]
               ["Sword single action"
                :sword
                {:player-id :rose
                 :source (assoc hand-source-without-minion :card-id "swords2")
                 :target {:kind :piece
                          :piece-id :indigo-target}
                 :damage 1}]
               ["Moon Rod/Sword action"
                :moon
                {:player-id :rose
                 :source (assoc hand-source-without-minion :card-id "moon")
                 :rod {:mode :move-minion
                       :distance 1
                       :orientation :east}}]]]
    (doseq [[label command-kind command] cases]
      (testing label
        (let [result (game-state/validate-command command-kind command)]
          (is (false? (game-state/valid-command? command-kind command)))
          (is (= :invalid-command-contract
                 (get-in result [:error :code])))
          (is (game-state/valid-result? result)))))))

(deftest command-contract-validation-distinguishes-shape-from-rule-semantics
  (let [structurally-valid {:player-id :rose
                            :source territory-source
                            :target {:kind :piece
                                     :piece-id :rose-minion}
                            :disc-variant :not-a-real-disc-variant}]
    (is (game-state/valid-command? :disc structurally-valid))
    (is (nil? (game-state/explain-command :disc structurally-valid)))))

(deftest structured-result-contract-covers-public-result-maps
  (let [create-result (game-state/create-game player-specs {:shuffle-fn identity})
        failure-result (game-state/failure :example
                                           "Example failure."
                                           {:field :value})]
    (is (game-state/valid-result? create-result))
    (is (game-state/valid-result? failure-result))
    (is (nil? (game-state/explain-result failure-result)))
    (is (false? (game-state/valid-result? {:ok? false
                                           :error {:code :missing-message}})))))
