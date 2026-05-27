(ns gnostica.major-sequence-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]))

(def player-specs
  [{:id :rose
    :name "Rose"}
   {:id :indigo
    :name "Indigo"}])

(defn- deck-starting-with [card-ids]
  (let [front-ids (set card-ids)]
    (vec
     (concat
      (map cards/card-by-id card-ids)
      (remove #(contains? front-ids (:id %)) cards/deck)))))

(defn- deterministic-game
  ([]
   (:state (game-state/create-game player-specs {:shuffle-fn identity})))
  ([card-ids]
   (:state (game-state/create-game player-specs
                                   {:deck-order
                                    (deck-starting-with card-ids)}))))

(defn- state-with-board-card [state board-index card-id]
  (update state :board
          (fn [cells]
            (mapv (fn [cell]
                    (if (= board-index (:index cell))
                      (assoc cell :card (cards/card-by-id card-id))
                      cell))
                  cells))))

(defn- state-with-pieces [state pieces]
  (game-state/with-board-pieces state pieces))

(defn- result-code [result]
  (get-in result [:error :code]))

(defn- action-recorder [calls]
  (fn [state context action]
    (swap! calls conj {:power (:power action)
                       :piece-id (:piece-id action)
                       :source-card-id (get-in context [:source-card :id])
                       :source-opts (:source-opts context)})
    {:ok? true
     :state state
     :events [{:type :test/major-action
               :power (:power action)
               :piece-id (:piece-id action)}]
     :affected-piece-ids (:affected-piece-ids action)}))

(def rose-minion-a
  {:id :rose-minion-a
   :player-id :rose
   :space-index 4
   :size :small
   :orientation :east})

(def rose-minion-b
  {:id :rose-minion-b
   :player-id :rose
   :space-index 5
   :size :small
   :orientation :west})

(def indigo-minion
  {:id :indigo-minion
   :player-id :indigo
   :space-index 4
   :size :small
   :orientation :up})

(deftest hand-major-source-costs-card-once-and-all-owned-pieces-start-as-minions
  (let [state (-> (deterministic-game ["sun"])
                  (state-with-pieces [rose-minion-a rose-minion-b indigo-minion]))
        calls (atom [])
        result (game-state/apply-major-sequence
                state
                {:player-id :rose
                 :source {:kind :hand-card
                          :card-id "sun"}
                 :actions [{:power :cup
                            :piece-id :rose-minion-a}
                           {:power :disc
                            :piece-id :rose-minion-b}]}
                {:card-id "sun"
                 :power-order [:cup :disc]
                 :apply-action-fn (action-recorder calls)})]
    (is (:ok? result))
    (is (= [:cup :disc] (mapv :power @calls)))
    (is (= ["sun"] (mapv :id (:discard-pile (:state result)))))
    (is (not (some #{"sun"}
                   (map :id (get-in result [:state :players-by-id :rose :hand])))))
    (is (every? #(= "sun" (:source-card-id %)) @calls))
    (is (= [{:source-card (cards/card-by-id "sun")
             :source-card-already-discarded? true}
            {:source-card (cards/card-by-id "sun")
             :source-card-already-discarded? true}]
           (mapv :source-opts @calls)))))

(deftest territory-major-source-uses-touching-pieces-and-promotes-affected-owned-pieces
  (let [state (-> (deterministic-game)
                  (state-with-board-card 4 "lovers")
                  (state-with-pieces [rose-minion-a rose-minion-b indigo-minion]))
        rejected (game-state/apply-major-sequence
                  state
                  {:player-id :rose
                   :source {:kind :territory
                            :board-index 4}
                   :actions [{:power :cup
                              :piece-id :rose-minion-b}]}
                  {:card-id "lovers"
                   :power-order [:rod :cup]
                   :apply-action-fn (action-recorder (atom []))})
        calls (atom [])
        accepted (game-state/apply-major-sequence
                  state
                  {:player-id :rose
                   :source {:kind :territory
                            :board-index 4}
                   :actions [{:power :rod
                              :piece-id :rose-minion-a
                              :affected-piece-ids [:rose-minion-b]}
                             {:power :cup
                              :piece-id :rose-minion-b}]}
                  {:card-id "lovers"
                   :power-order [:rod :cup]
                   :apply-action-fn (action-recorder calls)})]
    (is (= :invalid-major-minion (result-code rejected)))
    (is (:ok? accepted))
    (is (= [{:power :rod
             :piece-id :rose-minion-a}
            {:power :cup
             :piece-id :rose-minion-b}]
           (mapv #(select-keys % [:power :piece-id]) @calls)))))

(deftest major-sequence-enforces-order-and-offers-same-target-shortcuts
  (let [state (-> (deterministic-game ["sun"])
                  (state-with-pieces [rose-minion-a]))
        reversed (game-state/apply-major-sequence
                  state
                  {:player-id :rose
                   :source {:kind :hand-card
                            :card-id "sun"}
                   :actions [{:power :disc
                              :piece-id :rose-minion-a}
                             {:power :cup
                              :piece-id :rose-minion-a}]}
                  {:card-id "sun"
                   :power-order [:cup :disc]
                   :apply-action-fn (action-recorder (atom []))})
        optional-calls (atom [])
        disc-only (game-state/apply-major-sequence
                   state
                   {:player-id :rose
                    :source {:kind :hand-card
                             :card-id "sun"}
                    :actions [{:power :disc
                               :piece-id :rose-minion-a}]}
                   {:card-id "sun"
                    :power-order [:cup :disc]
                    :apply-action-fn (action-recorder optional-calls)})
        calls (atom [])
        shortcuts (atom [])
        shortcut-result (game-state/apply-major-sequence
                         state
                         {:player-id :rose
                          :source {:kind :hand-card
                                   :card-id "sun"}
                          :actions [{:power :cup
                                     :piece-id :rose-minion-a
                                     :target-key [:piece :new]}
                                    {:power :disc
                                     :piece-id :rose-minion-a
                                     :target-key [:piece :new]}]}
                         {:card-id "sun"
                          :power-order [:cup :disc]
                          :shortcut-key-fn :target-key
                          :shortcut-fn
                          (fn [current-state _context left-action right-action]
                            (swap! shortcuts conj [(:power left-action)
                                                   (:power right-action)])
                            {:ok? true
                             :state current-state
                             :events [{:type :test/major-shortcut
                                       :powers [(:power left-action)
                                                (:power right-action)]}]})
                          :apply-action-fn (action-recorder calls)})]
    (is (= :invalid-major-action-order (result-code reversed)))
    (is (:ok? disc-only))
    (is (= [:disc] (mapv :power @optional-calls)))
    (is (= ["sun"] (mapv :id (:discard-pile (:state disc-only)))))
    (is (:ok? shortcut-result))
    (is (= [[:cup :disc]] @shortcuts))
    (is (empty? @calls))
    (is (= [{:type :test/major-shortcut
             :powers [:cup :disc]}]
           (:events shortcut-result)))))
