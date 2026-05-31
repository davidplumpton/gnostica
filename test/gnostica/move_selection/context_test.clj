(ns gnostica.move-selection.context-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.move-selection.commands :as commands]
            [gnostica.move-selection.controls :as controls]
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
            commands/required-context-keys]]]
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
