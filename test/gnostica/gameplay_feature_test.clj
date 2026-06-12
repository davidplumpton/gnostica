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

(deftest malformed-feature-heading-produces-a-failing-result
  (let [feature-file (temp-feature-file
                      (str "Feature: Parser validation\n"
                           "\n"
                           "Scenario Missing colon\n"))
        results (feature-runner/run-feature-file (str feature-file) passing-steps)
        failure (result-with-code results :unexpected-feature-content)
        formatted (feature-runner/format-result failure)]
    (is (= 1 (count results)))
    (is failure)
    (is (= 3 (get-in failure [:failure :data :line])))
    (is (= "Scenario Missing colon" (get-in failure [:failure :data :text])))
    (is (str/includes? formatted (str feature-file ":3")))))

(deftest malformed-feature-step-text-produces-a-failing-result
  (let [feature-file (temp-feature-file
                      (str "Feature: Parser validation\n"
                           "\n"
                           "Scenario: Typoed step\n"
                           "  Givne a passing step\n"
                           "  Then a passing step\n"))
        results (feature-runner/run-feature-file (str feature-file) passing-steps)
        failure (result-with-code results :unexpected-feature-content)
        formatted (feature-runner/format-result failure)]
    (is (= 2 (count results)))
    (is failure)
    (is (some :ok? results))
    (is (= "Typoed step" (get-in failure [:scenario :name])))
    (is (= 4 (get-in failure [:failure :data :line])))
    (is (= "Givne a passing step" (get-in failure [:failure :data :text])))
    (is (str/includes? formatted "Scenario: Typoed step"))
    (is (str/includes? formatted (str feature-file ":4")))))

(deftest step-before-background-or-scenario-produces-a-failing-result
  (let [feature-file (temp-feature-file
                      (str "Feature: Parser validation\n"
                           "\n"
                           "Given a passing step\n"
                           "\n"
                           "Scenario: Valid scenario\n"
                           "  Given a passing step\n"))
        results (feature-runner/run-feature-file (str feature-file) passing-steps)
        failure (result-with-code results :step-outside-section)]
    (is (= 2 (count results)))
    (is failure)
    (is (some :ok? results))
    (is (= 3 (get-in failure [:failure :data :line])))
    (is (= "Given a passing step" (get-in failure [:failure :data :text])))
    (is (= "Given" (get-in failure [:failure :data :keyword])))))

(deftest table-outside-examples-produces-a-failing-result
  (let [feature-file (temp-feature-file
                      (str "Feature: Parser validation\n"
                           "\n"
                           "Scenario: Misplaced table\n"
                           "  Given a passing step\n"
                           "  | value |\n"))
        results (feature-runner/run-feature-file (str feature-file) passing-steps)
        failure (result-with-code results :unexpected-feature-content)]
    (is (= 2 (count results)))
    (is failure)
    (is (some :ok? results))
    (is (= "Misplaced table" (get-in failure [:scenario :name])))
    (is (= 5 (get-in failure [:failure :data :line])))
    (is (= :table-row (get-in failure [:failure :data :kind])))))

(deftest comments-and-blank-lines-remain-ignored
  (let [feature-file (temp-feature-file
                      (str "Feature: Parser validation\n"
                           "\n"
                           "# This comment is ignored.\n"
                           "\n"
                           "Scenario: Valid comments\n"
                           "  # This scenario comment is ignored.\n"
                           "\n"
                           "  Given a passing step\n"
                           "\n"))
        results (feature-runner/run-feature-file (str feature-file) passing-steps)]
    (is (= 1 (count results)))
    (is (every? :ok? results))
    (is (= "Valid comments" (get-in (first results) [:scenario :name])))))

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

(deftest scenario-outline-with-unresolved-placeholder-produces-a-failing-result
  (let [feature-file (temp-feature-file
                      (str "Feature: Outline validation\n"
                           "\n"
                           "Scenario Outline: Unresolved placeholder\n"
                           "  Given the outline value is <valu>\n"
                           "\n"
                           "  Examples:\n"
                           "    | value |\n"
                           "    | one   |\n"))
        results (feature-runner/run-feature-file (str feature-file) passing-steps)
        failure (result-with-code results :unresolved-outline-placeholder)
        formatted (feature-runner/format-result failure)]
    (is (= 1 (count results)))
    (is failure)
    (is (= "Unresolved placeholder" (get-in failure [:scenario :name])))
    (is (= 4 (get-in failure [:failure :data :line])))
    (is (= "valu" (get-in failure [:failure :data :placeholder])))
    (is (= ["value"] (get-in failure [:failure :data :available-columns])))
    (is (str/includes? formatted "Scenario: Unresolved placeholder"))
    (is (str/includes? formatted (str feature-file ":4")))))

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
