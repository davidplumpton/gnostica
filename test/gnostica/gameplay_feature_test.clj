(ns gnostica.gameplay-feature-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gnostica.feature-runner :as feature-runner]
            [gnostica.feature-steps :as feature-steps]))

(def setup-feature
  (str (io/file "features" "game_setup.feature")))

(deftest initial-game-setup-feature
  (feature-runner/assert-results-pass
   (feature-runner/run-feature-file setup-feature feature-steps/steps)))

(deftest feature-failure-output-includes-scenario-and-step
  (let [result (feature-runner/run-scenario
                {:path "features/example.feature"
                 :name "Example"}
                []
                {:line 3
                 :name "Broken example"
                 :steps [{:line 4
                          :keyword "Then"
                          :text "an unmatched step"}]})
        formatted (feature-runner/format-result result)]
    (is (false? (:ok? result)))
    (is (str/includes? formatted "Scenario: Broken example"))
    (is (str/includes? formatted "Step: Then an unmatched step"))
    (is (str/includes? formatted "Code: :undefined-step"))))
