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

(defn- temp-feature-file [contents]
  (doto (java.io.File/createTempFile "gnostica-feature-runner-" ".feature")
    (.deleteOnExit)
    (spit contents)))

(def passing-steps
  [{:pattern #"a passing step"
    :run (fn [world] world)}
   {:pattern #"the outline value is (.+)"
    :run (fn [world _value] world)}])

(defn- result-with-code [results code]
  (some (fn [result]
          (when (= code (get-in result [:failure :code]))
            result))
        results))

(deftest gameplay-features
  (is (seq feature-files) "Expected at least one gameplay feature file.")
  (doseq [feature-file feature-files]
    (feature-runner/assert-results-pass
     (feature-runner/run-feature-file feature-file feature-steps/steps))))

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

(deftest scenario-outline-without-examples-produces-a-failing-result
  (let [feature-file (temp-feature-file
                      (str "Feature: Outline validation\n"
                           "\n"
                           "Scenario: Valid scenario\n"
                           "  Given a passing step\n"
                           "\n"
                           "Scenario Outline: Missing examples\n"
                           "  Given a passing step\n"))
        results (feature-runner/run-feature-file (str feature-file) passing-steps)
        failure (result-with-code results :missing-outline-examples)
        formatted (feature-runner/format-result failure)]
    (is (= 2 (count results)))
    (is (some :ok? results))
    (is failure)
    (is (= "Missing examples" (get-in failure [:scenario :name])))
    (is (= 6 (get-in failure [:failure :data :scenario-line])))
    (is (str/includes? formatted "Scenario: Missing examples"))
    (is (str/includes? formatted "Code: :missing-outline-examples"))))

(deftest scenario-outline-with-empty-examples-produces-a-failing-result
  (let [feature-file (temp-feature-file
                      (str "Feature: Outline validation\n"
                           "\n"
                           "Scenario Outline: Empty examples\n"
                           "  Given a passing step\n"
                           "\n"
                           "  Examples:\n"))
        results (feature-runner/run-feature-file (str feature-file) passing-steps)
        failure (result-with-code results :empty-outline-examples)]
    (is (= 1 (count results)))
    (is failure)
    (is (= "Empty examples" (get-in failure [:scenario :name])))
    (is (= 6 (get-in failure [:failure :data :line])))))

(deftest scenario-outline-with-malformed-example-row-produces-a-failing-result
  (let [feature-file (temp-feature-file
                      (str "Feature: Outline validation\n"
                           "\n"
                           "Scenario Outline: Malformed examples\n"
                           "  Given the outline value is <value>\n"
                           "\n"
                           "  Examples:\n"
                           "    | value | extra |\n"
                           "    | one   |\n"))
        results (feature-runner/run-feature-file (str feature-file) passing-steps)
        failure (result-with-code results :malformed-outline-examples)]
    (is (= 1 (count results)))
    (is failure)
    (is (= "Malformed examples" (get-in failure [:scenario :name])))
    (is (= 2 (get-in failure [:failure :data :expected-columns])))
    (is (= 1 (get-in failure [:failure :data :actual-columns])))
    (is (= 8 (get-in failure [:failure :data :line])))))

(deftest valid-scenario-outline-examples-still-pass
  (let [feature-file (temp-feature-file
                      (str "Feature: Outline validation\n"
                           "\n"
                           "Scenario Outline: Valid examples\n"
                           "  Given the outline value is <value>\n"
                           "\n"
                           "  Examples:\n"
                           "    | value |\n"
                           "    | one   |\n"
                           "    | two   |\n"))
        results (feature-runner/run-feature-file (str feature-file) passing-steps)]
    (is (= 2 (count results)))
    (is (every? :ok? results))
    (is (= ["Valid examples [value=one]"
            "Valid examples [value=two]"]
           (mapv #(get-in % [:scenario :name]) results)))))
