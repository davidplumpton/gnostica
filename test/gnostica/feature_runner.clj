(ns gnostica.feature-runner
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [is]]))

(def step-keywords #{"Given" "When" "Then" "And" "But"})

(defn fail [code message data]
  {::failure {:code code
              :message message
              :data data}})

(defn- failed-step? [value]
  (contains? value ::failure))

(defn- indented-data [value]
  (->> (with-out-str (pprint/pprint value))
       str/split-lines
       (map #(str "  " %))
       (str/join "\n")))

(defn- parse-heading [prefix line]
  (let [marker (str prefix ":")]
    (when (str/starts-with? (str/trim line) marker)
      (str/trim (subs (str/trim line) (count marker))))))

(defn- step-line [line-number line]
  (let [trimmed (str/trim line)
        [keyword & text-parts] (str/split trimmed #"\s+" 2)]
    (when (and (contains? step-keywords keyword)
               (seq text-parts))
      {:line line-number
       :keyword keyword
       :text (first text-parts)})))

(defn- table-row [line]
  (let [trimmed (str/trim line)]
    (when (and (str/starts-with? trimmed "|")
               (str/ends-with? trimmed "|"))
      (mapv str/trim (str/split (subs trimmed 1 (dec (count trimmed))) #"\|")))))

(defn- add-scenario [feature scenario]
  (cond-> feature
    scenario (update :scenarios conj scenario)))

(defn- add-step [feature scenario section step]
  (case section
    :background [(update feature :background conj step) scenario]
    :scenario [feature (update scenario :steps conj step)]
    [feature scenario]))

(defn- add-example-row [scenario header row line-number]
  (update scenario :examples conj {:line line-number
                                   :values (zipmap header row)}))

(defn parse-feature-file [path]
  (let [lines (map-indexed (fn [index line]
                             {:line (inc index)
                              :text line})
                           (str/split-lines (slurp path)))]
    (loop [{:keys [line text]} (first lines)
           remaining (rest lines)
           feature {:path path
                    :name nil
                    :background []
                    :scenarios []}
           scenario nil
           section nil
           example-header nil]
      (if-not text
        (add-scenario feature scenario)
        (let [trimmed (str/trim text)]
          (cond
            (or (str/blank? trimmed)
                (str/starts-with? trimmed "#"))
            (recur (first remaining) (rest remaining) feature scenario section example-header)

            (parse-heading "Feature" trimmed)
            (recur (first remaining)
                   (rest remaining)
                   (assoc feature :name (parse-heading "Feature" trimmed))
                   scenario
                   section
                   example-header)

            (parse-heading "Background" trimmed)
            (recur (first remaining)
                   (rest remaining)
                   (add-scenario feature scenario)
                   nil
                   :background
                   nil)

            (parse-heading "Scenario Outline" trimmed)
            (recur (first remaining)
                   (rest remaining)
                   (add-scenario feature scenario)
                   {:line line
                    :name (parse-heading "Scenario Outline" trimmed)
                    :outline? true
                    :steps []
                    :examples []}
                   :scenario
                   nil)

            (parse-heading "Scenario" trimmed)
            (recur (first remaining)
                   (rest remaining)
                   (add-scenario feature scenario)
                   {:line line
                    :name (parse-heading "Scenario" trimmed)
                    :outline? false
                    :steps []
                    :examples []}
                   :scenario
                   nil)

            (parse-heading "Examples" trimmed)
            (recur (first remaining)
                   (rest remaining)
                   feature
                   scenario
                   :examples
                   nil)

            (and (= section :examples) (table-row trimmed))
            (let [row (table-row trimmed)]
              (if example-header
                (recur (first remaining)
                       (rest remaining)
                       feature
                       (add-example-row scenario example-header row line)
                       section
                       example-header)
                (recur (first remaining)
                       (rest remaining)
                       feature
                       scenario
                       section
                       row)))

            (step-line line trimmed)
            (let [[feature scenario] (add-step feature scenario section (step-line line trimmed))]
              (recur (first remaining)
                     (rest remaining)
                     feature
                     scenario
                     section
                     example-header))

            :else
            (recur (first remaining) (rest remaining) feature scenario section example-header)))))))

(defn- replace-placeholders [text values]
  (reduce-kv (fn [result key value]
               (str/replace result (str "<" key ">") value))
             text
             values))

(defn- expand-outline-step [values step]
  (update step :text replace-placeholders values))

(defn- example-label [values]
  (->> values
       (map (fn [[key value]]
              (str key "=" value)))
       (str/join ", ")))

(defn- expand-scenario [feature scenario]
  (let [steps (into (:background feature) (:steps scenario))]
    (if (:outline? scenario)
      (mapv (fn [{:keys [line values]}]
              (assoc scenario
                     :line line
                     :name (str (:name scenario) " [" (example-label values) "]")
                     :steps (mapv #(expand-outline-step values %) steps)
                     :example values))
            (:examples scenario))
      [(assoc scenario :steps steps)])))

(defn feature-scenarios [feature]
  (mapcat #(expand-scenario feature %) (:scenarios feature)))

(defn- regex-groups [match]
  (if (vector? match)
    (subvec match 1)
    []))

(defn- matching-step-definition [step-definitions step]
  (some (fn [{:keys [pattern] :as step-definition}]
          (when-let [match (re-matches pattern (:text step))]
            (assoc step-definition :args (regex-groups match))))
        step-definitions))

(defn- step-failure [scenario step failure]
  {:ok? false
   :scenario scenario
   :step step
   :failure failure})

(defn- feature-failure [feature scenario code message data]
  {:ok? false
   :feature feature
   :scenario scenario
   :failure {:code code
             :message message
             :data data}})

(defn- no-scenarios-failure [feature]
  (feature-failure feature
                   nil
                   :empty-feature
                   "Feature files must contain at least one executable scenario."
                   {:path (:path feature)
                    :feature (:name feature)}))

(defn- empty-scenario-failure [feature scenario]
  (feature-failure feature
                   scenario
                   :empty-scenario
                   "Feature scenarios must contain at least one executable step."
                   {:path (:path feature)
                    :feature (:name feature)
                    :scenario (:name scenario)
                    :line (:line scenario)}))

(defn- run-step [world step step-definition]
  (if-not step-definition
    {:ok? false
     :failure {:code :undefined-step
               :message "No step definition matched the feature step."
               :data {:step (:text step)}}}
    (try
      (let [result (apply (:run step-definition) world (:args step-definition))]
        (cond
          (failed-step? result)
          {:ok? false
           :failure (::failure result)}

          (map? result)
          {:ok? true
           :world result}

          :else
          {:ok? false
           :failure {:code :invalid-step-return
                     :message "Step definitions must return the updated world map or feature-runner/fail."
                     :data {:returned result}}}))
      (catch Throwable error
        {:ok? false
         :failure {:code :step-threw
                   :message (.getMessage error)
                   :data {:exception (class error)
                          :ex-data (ex-data error)}}}))))

(defn run-scenario [feature step-definitions scenario]
  (if-not (seq (:steps scenario))
    (empty-scenario-failure feature scenario)
    (loop [world {}
           [step & remaining] (:steps scenario)]
      (if-not step
        {:ok? true
         :feature feature
         :scenario scenario
         :world world}
        (let [step-result (run-step world step (matching-step-definition step-definitions step))]
          (if (:ok? step-result)
            (recur (:world step-result) remaining)
            (merge (step-failure scenario step (:failure step-result))
                   {:feature feature})))))))

(defn run-feature-file [path step-definitions]
  (let [feature (parse-feature-file path)
        scenarios (vec (feature-scenarios feature))]
    (if (seq scenarios)
      (mapv #(run-scenario feature step-definitions %) scenarios)
      [(no-scenarios-failure feature)])))

(defn- result-line [scenario step]
  (or (:line step)
      (:line scenario)
      1))

(defn- result-scenario-name [scenario]
  (or (:name scenario) "<none>"))

(defn- result-step-label [step]
  (if step
    (str (:keyword step) " " (:text step))
    "<no executable step>"))

(defn format-result [result]
  (if (:ok? result)
    (str "Feature scenario passed: " (get-in result [:scenario :name]))
    (let [{:keys [feature scenario step failure]} result]
      (str "Feature scenario failed\n"
           "File: " (:path feature) ":" (result-line scenario step) "\n"
           "Feature: " (:name feature) "\n"
           "Scenario: " (result-scenario-name scenario) "\n"
           "Step: " (result-step-label step) "\n"
           "Reason: " (:message failure) "\n"
           "Code: " (:code failure) "\n"
           "Data:\n" (indented-data (:data failure))))))

(defn assert-results-pass [results]
  (is (seq results) "Expected at least one feature scenario result.")
  (doseq [result results]
    (is (:ok? result) (format-result result)))
  (boolean (and (seq results)
                (every? :ok? results))))
