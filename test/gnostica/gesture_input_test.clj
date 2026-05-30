(ns gnostica.gesture-input-test
  (:require [clojure.test :refer [deftest is]]
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
