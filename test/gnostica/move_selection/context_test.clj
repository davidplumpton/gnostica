(ns gnostica.move-selection.context-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.move-selection.commands :as commands]
            [gnostica.move-selection.confirmation :as confirmation]
            [gnostica.move-selection.controls :as controls]
            [gnostica.move-selection.flow :as flow]
            [gnostica.move-selection.flow.advancement :as flow-advancement]
            [gnostica.move-selection.flow.requirements :as flow-requirements]
            [gnostica.move-selection.flow.stages :as flow-stages]
            [gnostica.move-selection.prompt :as prompt]
            [gnostica.move-selection.target-options :as target-options]
            [gnostica.move-selection.targets :as targets]))

(defn- dummy-deps [required-keys]
  (zipmap required-keys (repeat (fn [& _] nil))))

(deftest helper-context-constructors-report-missing-dependencies
  (doseq [[label make-context required-keys]
          [["gnostica.move-selection.controls"
            controls/make-context
            controls/required-context-keys]
           ["gnostica.move-selection.targets"
            targets/make-context
            targets/required-context-keys]
           ["gnostica.move-selection.commands"
            commands/make-context
            commands/required-context-keys]
           ["gnostica.move-selection.flow"
            flow/make-context
            flow/required-context-keys]
           ["gnostica.move-selection.flow.advancement"
            flow-advancement/make-context
            flow-advancement/required-context-keys]
           ["gnostica.move-selection.flow.requirements"
            flow-requirements/make-context
            flow-requirements/required-context-keys]
           ["gnostica.move-selection.flow.stages"
            flow-stages/make-context
            flow-stages/required-context-keys]
           ["gnostica.move-selection.prompt"
            prompt/make-context
            prompt/required-context-keys]
           ["gnostica.move-selection.target-options"
            target-options/make-context
            target-options/required-context-keys]
           ["gnostica.move-selection.confirmation"
            confirmation/make-context
            confirmation/required-context-keys]]]
    (testing label
      (let [missing-key (first (sort-by pr-str required-keys))]
        (try
          (make-context (dissoc (dummy-deps required-keys) missing-key))
          (is false "missing context dependency should throw")
          (catch clojure.lang.ExceptionInfo ex
            (let [data (ex-data ex)]
              (is (= :move-selection-context/missing-dependencies (:code data)))
              (is (= label (:context data)))
              (is (= [missing-key] (:missing-keys data)))
              (is (contains? (set (:required-keys data)) missing-key))
              (is (re-find #"context missing required dependencies"
                           (.getMessage ex))))))))))

(deftest helper-context-calls-report-non-callable-dependencies
  (let [ctx (controls/make-context
             (assoc (dummy-deps controls/required-context-keys)
                    :move-source-order [:draw-cards]
                    :move-source-definitions {:draw-cards {:id :draw-cards}}
                    :source-unavailable-reason 42))]
    (try
      (controls/move-source-options ctx {})
      (is false "non-callable context dependency should throw")
      (catch clojure.lang.ExceptionInfo ex
        (let [data (ex-data ex)]
          (is (= :move-selection-context/non-callable-dependency (:code data)))
          (is (= "gnostica.move-selection.controls" (:context data)))
          (is (= :source-unavailable-reason (:dependency data)))
          (is (re-find #"context dependency is not callable"
                       (.getMessage ex))))))))

(deftest extracted-state-selectors-stay-out-of-dynamic-contexts
  (testing "flow uses direct state selectors before dynamic callbacks"
    (let [ctx (flow/make-context (dummy-deps flow/required-context-keys))]
      (is (= [:source] (flow/move-missing-fields ctx {}))))
    (doseq [key [:active-power
                 :current-player-piece-by-id
                 :move-selection
                 :rod-modes
                 :selected-power
                 :source-card
                 :valid-board-index?
                 :world-copy-board-cell]]
      (is (not (contains? flow/required-context-keys key)))
      (is (not (contains? flow-requirements/required-context-keys key)))))
  (testing "command builders do not depend on state selector callbacks"
    (doseq [key [:active-card
                 :current-player-id
                 :selected-power
                 :selected-rod-variant
                 :source-command
                 :stored-fool-reveal-actions
                 :world-move?]]
      (is (not (contains? commands/required-context-keys key))))))
