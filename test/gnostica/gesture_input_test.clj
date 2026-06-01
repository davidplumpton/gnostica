(ns gnostica.gesture-input-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.gesture-input :as gesture-input]))

(deftest shared-gesture-inputs-use-stable-object-identifiers
  (is (= {:source {:kind :territory
                   :board-index 7}}
         (gesture-input/territory-source-input {:index 7})))
  (is (= {:kind :territory
          :board-index 7}
         (gesture-input/territory-target 7)))
  (is (= {:source {:kind :hand-card
                   :card-id "cups2"}}
         (gesture-input/hand-card-source-input {:id "cups2"})))
  (is (= {:source {:kind :piece
                   :piece-id :rose-scout}}
         (gesture-input/piece-source-input {:id :rose-scout})))
  (is (= {:kind :piece
          :piece-id :rose-scout}
         (gesture-input/piece-target :rose-scout)))
  (is (= {:source {:kind :stash-piece
                   :player-id :rose
                   :size :small}}
         (gesture-input/stash-piece-source-input {:id :rose})))
  (is (= {:kind :wasteland
          :row 2
          :col -1}
         (gesture-input/wasteland-target {:row 2 :col -1}))))

(deftest drag-inputs-match-detailed-selection-roles
  (let [piece {:id :rose-scout}
        card {:id "coins2"}]
    (is (= {:preserve-selection? true
            :fields {:target-board-index 3}}
           (gesture-input/territory-drag-input
            {:index 3}
            {:active? true
             :role :target})))
    (is (= {:source {:kind :territory
                     :board-index 3}}
           (gesture-input/territory-drag-input {:index 3} nil)))
    (is (= {:preserve-selection? true
            :fields {:piece-id :rose-scout}}
           (gesture-input/piece-drag-input piece {:role :minion
                                                  :enabled? true})))
    (is (= {:preserve-selection? true
            :fields {:target-piece-id :rose-scout}}
           (gesture-input/piece-drag-input piece {:role :target
                                                  :enabled? true})))
    (is (= {:source {:kind :piece
                     :piece-id :rose-scout}}
           (gesture-input/piece-drag-input piece {:source-enabled? true})))
    (is (nil? (gesture-input/piece-drag-input piece {:role :target
                                                     :enabled? false})))
    (is (= {:source {:kind :hand-card
                     :card-id "coins2"}}
           (gesture-input/hand-card-drag-input card {:role :source})))
    (is (= {:preserve-selection? true
            :fields {:replacement-card-id "coins2"
                     :replacement-card-source :discard-pile}}
           (gesture-input/hand-card-drag-input
            card
            {:role :replacement-card
             :enabled? true
             :replacement-card-source :discard-pile})))
    (is (nil? (gesture-input/discard-card-drag-input
               card
               {:role :judgement-card
                :enabled? true})))))

(deftest drag-payload-parsing-accepts-only-gnostica-gesture-shapes
  (let [source-input (gesture-input/hand-card-source-input {:id "coins2"})
        field-input {:preserve-selection? true
                     :fields {:target-piece-id :rose-scout
                              :orientation :south}}
        target-input (assoc source-input
                            :target (gesture-input/wasteland-target 2 -1))]
    (is (true? (gesture-input/gesture-input? source-input)))
    (is (true? (gesture-input/gesture-input? field-input)))
    (is (true? (gesture-input/gesture-input? target-input)))
    (is (= source-input
           (gesture-input/parse-gesture-input-string
            (gesture-input/gesture-input-string source-input))))
    (is (= source-input
           (gesture-input/parse-gesture-input-fallback-string
            (gesture-input/gesture-input-fallback-string source-input))))
    (testing "normal external text/plain drags are ignored"
      (is (nil? (gesture-input/parse-gesture-input-fallback-string
                 (gesture-input/gesture-input-string source-input))))
      (is (nil? (gesture-input/parse-gesture-input-fallback-string
                 "{:source {:kind :hand-card :card-id \"coins2\"}}"))))
    (testing "malformed or unrelated prefixed EDN is not returned"
      (is (nil? (gesture-input/parse-gesture-input-string "not edn")))
      (is (nil? (gesture-input/parse-gesture-input-string "[]")))
      (is (nil? (gesture-input/parse-gesture-input-string
                 "{:source {:kind :hand-card}}")))
      (is (nil? (gesture-input/parse-gesture-input-fallback-string
                 "gnostica-gesture:{:unrelated true}")))
      (is (nil? (gesture-input/parse-gesture-input-fallback-string
                 "gnostica-gesture:{:source {:kind :hand-card :card-id \"coins2\"} :unknown true}"))))))

(deftest drag-data-transfer-policy-requires-custom-mime-or-prefixed-fallback
  (let [input (gesture-input/stash-piece-source-input {:id :rose})
        custom-payload (gesture-input/gesture-input-string input)
        fallback-payload (gesture-input/gesture-input-fallback-string input)
        active-input (gesture-input/with-drag-orientation input :east)]
    (is (true? (gesture-input/gesture-data-transfer-types?
                [gesture-input/mime-type]
                nil)))
    (is (true? (gesture-input/gesture-data-transfer-types?
                [gesture-input/fallback-mime-type]
                fallback-payload)))
    (is (false? (gesture-input/gesture-data-transfer-types?
                 [gesture-input/fallback-mime-type]
                 custom-payload)))
    (is (false? (gesture-input/gesture-data-transfer-types?
                 ["text/html"]
                 fallback-payload)))
    (is (= input
           (gesture-input/gesture-input-from-drag-data
            {:types [gesture-input/mime-type]
             :custom-payload custom-payload})))
    (is (= input
           (gesture-input/gesture-input-from-drag-data
            {:types [gesture-input/fallback-mime-type]
             :fallback-payload fallback-payload})))
    (is (nil? (gesture-input/gesture-input-from-drag-data
               {:types [gesture-input/fallback-mime-type]
                :fallback-payload custom-payload})))
    (is (nil? (gesture-input/gesture-input-from-drag-data
               {:types [gesture-input/fallback-mime-type]
                :fallback-payload "gnostica-gesture:[]"})))
    (is (= input
           (gesture-input/gesture-input-from-drag-data
            {:types [gesture-input/fallback-mime-type]
             :fallback-payload fallback-payload
             :active-input (gesture-input/hand-card-source-input {:id "cups2"})})))
    (testing "active orientation is reused only for the same serialized drag"
      (is (= active-input
             (gesture-input/gesture-input-from-drag-data
              {:types [gesture-input/mime-type]
               :custom-payload custom-payload
               :active-input active-input})))
      (is (= active-input
             (gesture-input/gesture-input-from-drag-data
              {:types [gesture-input/mime-type]
               :custom-payload ""
               :active-input active-input})))
      (is (= input
             (gesture-input/gesture-input-from-drag-data
              {:types [gesture-input/mime-type]
               :custom-payload custom-payload
               :active-input (gesture-input/hand-card-source-input {:id "cups2"})}))))))

(deftest board-space-drags-highlight-only-hovered-board-target
  (let [stash-source {:kind :stash-piece
                      :player-id :rose
                      :size :small}
        piece-source {:kind :piece
                      :piece-id :rose-small-1}
        hand-source {:kind :hand-card
                     :card-id "cups2"}
        territory-target {:kind :territory
                          :board-index 3}
        other-territory {:kind :territory
                         :board-index 4}
        wasteland-target {:kind :wasteland
                          :row 0
                          :col 3}]
    (is (true? (gesture-input/board-space-drag-source? stash-source)))
    (is (true? (gesture-input/board-space-drag-source?
                {:source piece-source})))
    (is (false? (gesture-input/board-space-drag-source? hand-source)))
    (is (= [:territory 3]
           (gesture-input/target-key territory-target)))
    (is (= [:wasteland 0 3]
           (gesture-input/target-key wasteland-target)))
    (is (true? (gesture-input/show-target-highlight?
                {:source stash-source
                 :target territory-target}
                territory-target)))
    (is (false? (gesture-input/show-target-highlight?
                 {:source stash-source
                  :target territory-target}
                 other-territory)))
    (is (false? (gesture-input/show-target-highlight?
                 {:source stash-source}
                 territory-target)))
    (is (true? (gesture-input/show-target-highlight?
                {:source hand-source}
                territory-target)))))

(deftest drag-orientation-key-requests-are-explicit
  (is (= :up
         (gesture-input/orientation-key-request {:key "ArrowUp"})))
  (is (= :east
         (gesture-input/orientation-key-request {:key "ArrowRight"})))
  (is (= :south
         (gesture-input/orientation-key-request {:key "ArrowDown"})))
  (is (= :west
         (gesture-input/orientation-key-request {:key "ArrowLeft"})))
  (is (= :cycle
         (gesture-input/orientation-key-request {:key "O"})))
  (is (= :cycle
         (gesture-input/orientation-key-request {:code "KeyO"})))
  (is (nil? (gesture-input/orientation-key-request {:key "O"
                                                    :ctrl? true})))
  (is (= :up
         (gesture-input/orientation-request->orientation nil :cycle)))
  (is (= :north
         (gesture-input/orientation-request->orientation :up :cycle)))
  (is (= :up
         (gesture-input/orientation-request->orientation :west :cycle)))
  (is (= :south
         (gesture-input/orientation-request->orientation :up :south))))

(deftest drag-orientation-is-carried-in-drag-payloads
  (let [stash-input {:source {:kind :stash-piece
                              :player-id :rose
                              :size :small}}
        minion-input {:preserve-selection? true
                      :fields {:piece-id :rose-scout}}]
    (is (true? (gesture-input/orientation-drag-input? stash-input)))
    (is (true? (gesture-input/orientation-drag-input? minion-input)))
    (is (= {:source {:kind :stash-piece
                     :player-id :rose
                     :size :small
                     :orientation :east}
            :fields {:orientation :east}}
           (gesture-input/with-drag-orientation stash-input :east)))
    (is (= {:preserve-selection? true
            :fields {:piece-id :rose-scout
                     :orientation :south}}
           (gesture-input/with-drag-orientation minion-input :south)))))
