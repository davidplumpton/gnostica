(ns gnostica.gameplay-feature-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gnostica.feature-runner :as feature-runner]
            [gnostica.feature-steps :as feature-steps]))

(def feature-files
  (->> (file-seq (io/file "features"))
       (filter #(.isFile %))
       (filter #(str/ends-with? (.getName %) ".feature"))
       (map str)
       sort
       vec))

(defn- parsed-scenarios [feature-file]
  (let [feature (feature-runner/parse-feature-file feature-file)
        scenarios (vec (feature-runner/feature-scenarios feature))]
    (is (seq scenarios)
        (str "Expected at least one gameplay scenario in " feature-file "."))
    {:feature feature
     :scenarios scenarios}))

(defn- temp-feature-file [contents]
  (doto (java.io.File/createTempFile "gnostica-feature-runner-" ".feature")
    (.deleteOnExit)
    (spit contents)))

(deftest gameplay-features
  (is (seq feature-files) "Expected at least one gameplay feature file.")
  (doseq [feature-file feature-files]
    (let [{:keys [feature scenarios]} (parsed-scenarios feature-file)]
      (feature-runner/assert-results-pass
       (mapv #(feature-runner/run-scenario feature feature-steps/steps %) scenarios)))))

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

(deftest empty-feature-file-produces-a-failing-result
  (let [feature-file (temp-feature-file "")
        results (feature-runner/run-feature-file (str feature-file) [])]
    (is (= 1 (count results)))
    (is (false? (:ok? (first results))))
    (is (= :empty-feature (get-in (first results) [:failure :code])))))

(deftest empty-scenario-produces-a-failing-result
  (let [feature-file (temp-feature-file "Feature: Empty examples\n\nScenario: No steps\n")
        results (feature-runner/run-feature-file (str feature-file) [])]
    (is (= 1 (count results)))
    (is (false? (:ok? (first results))))
    (is (= :empty-scenario (get-in (first results) [:failure :code])))))
