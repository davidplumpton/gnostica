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

(def ^:private heading-prefixes ["Scenario Outline" "Feature" "Background" "Scenario" "Examples"])

(defn- malformed-heading-line? [line]
  (boolean
   (some (fn [prefix]
           (and (or (= line prefix)
                    (str/starts-with? line (str prefix " ")))
                (not (parse-heading prefix line))))
         heading-prefixes)))

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
                                   :header header
                                   :cells row
                                   :values (zipmap header row)}))

(defn- add-example-header [scenario header line-number]
  (assoc scenario
         :example-header {:line line-number
                          :cells header}))

(defn- parse-error-data [line text section scenario data]
  (cond-> (merge {:line line
                  :text text}
                 data)
    section (assoc :section section)
    scenario (assoc :scenario (:name scenario)
                    :scenario-line (:line scenario))))

(defn- add-parse-error [feature code message line text section scenario data]
  (update feature :parse-errors conj
          {:code code
           :message message
           :data (parse-error-data line text section scenario data)}))

(defn parse-feature-file [path]
  (let [lines (map-indexed (fn [index line]
                             {:line (inc index)
                              :text line})
                           (str/split-lines (slurp path)))]
    (loop [{:keys [line text]} (first lines)
           remaining (rest lines)
           feature {:path path
                    :name nil
                    :description []
                    :background []
                    :scenarios []
                    :parse-errors []}
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
            (if (and (nil? (:name feature))
                     (nil? scenario)
                     (nil? section)
                     (empty? (:scenarios feature)))
              (recur (first remaining)
                     (rest remaining)
                     (assoc feature :name (parse-heading "Feature" trimmed))
                     scenario
                     section
                     example-header)
              (recur (first remaining)
                     (rest remaining)
                     (add-parse-error
                      feature
                      :unexpected-feature-content
                      "Feature headings are only valid before Background or Scenario sections."
                      line
                      trimmed
                      section
                      scenario
                      {:kind :feature-heading})
                     scenario
                     section
                     example-header))

            (parse-heading "Background" trimmed)
            (if (and (nil? scenario)
                     (empty? (:scenarios feature))
                     (not= section :background))
              (recur (first remaining)
                     (rest remaining)
                     (add-scenario feature scenario)
                     nil
                     :background
                     nil)
              (recur (first remaining)
                     (rest remaining)
                     (add-parse-error
                      feature
                      :unexpected-feature-content
                      "Background headings are only valid before Scenario sections."
                      line
                      trimmed
                      section
                      scenario
                      {:kind :background-heading})
                     scenario
                     section
                     example-header))

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
            (if (and (= section :scenario)
                     (:outline? scenario))
              (recur (first remaining)
                     (rest remaining)
                     feature
                     (assoc scenario :examples-line line)
                     :examples
                     nil)
              (recur (first remaining)
                     (rest remaining)
                     (add-parse-error
                      feature
                      :unexpected-feature-content
                      "Examples sections are only valid inside Scenario Outline sections."
                      line
                      trimmed
                      section
                      scenario
                      {:kind :examples-heading})
                     scenario
                     section
                     example-header))

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
                       (add-example-header scenario row line)
                       section
                       row)))

            (table-row trimmed)
            (recur (first remaining)
                   (rest remaining)
                   (add-parse-error
                    feature
                    :unexpected-feature-content
                    "Table rows are only valid inside Scenario Outline Examples sections."
                    line
                    trimmed
                    section
                    scenario
                    {:kind :table-row})
                   scenario
                   section
                   example-header)

            (step-line line trimmed)
            (let [step (step-line line trimmed)]
              (if (contains? #{:background :scenario} section)
                (let [[feature scenario] (add-step feature scenario section step)]
                  (recur (first remaining)
                         (rest remaining)
                         feature
                         scenario
                         section
                         example-header))
                (recur (first remaining)
                       (rest remaining)
                       (add-parse-error
                        feature
                        :step-outside-section
                        "Feature steps are only valid inside Background or Scenario sections."
                        line
                        trimmed
                        section
                        scenario
                        {:kind :step
                         :keyword (:keyword step)
                         :step-text (:text step)})
                       scenario
                       section
                       example-header)))

            (malformed-heading-line? trimmed)
            (recur (first remaining)
                   (rest remaining)
                   (add-parse-error
                    feature
                    :unexpected-feature-content
                    "Feature headings must use a recognized heading followed by a colon."
                    line
                    trimmed
                    section
                    scenario
                    {:kind :heading})
                   scenario
                   section
                   example-header)

            (and (:name feature)
                 (nil? scenario)
                 (nil? section)
                 (empty? (:scenarios feature)))
            (recur (first remaining)
                   (rest remaining)
                   (update feature :description conj {:line line
                                                      :text trimmed})
                   scenario
                   section
                   example-header)

            :else
            (recur (first remaining)
                   (rest remaining)
                   (add-parse-error
                    feature
                    :unexpected-feature-content
                    "Unexpected feature file content."
                    line
                    trimmed
                    section
                    scenario
                    {:kind :line})
                   scenario
                   section
                   example-header)))))))

(defn- replace-placeholders [text values]
  (reduce-kv (fn [result key value]
               (str/replace result (str "<" key ">") value))
             text
             values))

(def ^:private outline-placeholder-pattern #"<([^<>]+)>")

(defn- outline-placeholders [text]
  (->> (re-seq outline-placeholder-pattern text)
       (map second)
       vec))

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

(defn- duplicate-values [values]
  (->> values
       frequencies
       (filter (fn [[_ count]] (> count 1)))
       (map first)
       vec))

(declare feature-failure)

(defn- outline-validation-failure [feature scenario code message data]
  (feature-failure feature
                   scenario
                   code
                   message
                   (merge {:path (:path feature)
                           :feature (:name feature)
                           :scenario (:name scenario)
                           :scenario-line (:line scenario)}
                          data)))

(defn- outline-header-failure [feature scenario header]
  (let [columns (:cells header)
        blanks (->> columns
                    (keep-indexed (fn [index column]
                                    (when (str/blank? column)
                                      (inc index))))
                    vec)
        duplicates (duplicate-values columns)]
    (cond
      (seq blanks)
      (outline-validation-failure
       feature
       scenario
       :malformed-outline-examples
       "Scenario Outline Examples headers must not contain blank column names."
       {:line (:line header)
        :blank-columns blanks
        :columns columns})

      (seq duplicates)
      (outline-validation-failure
       feature
       scenario
       :malformed-outline-examples
       "Scenario Outline Examples headers must not contain duplicate column names."
       {:line (:line header)
        :duplicate-columns duplicates
        :columns columns}))))

(defn- outline-row-failure [feature scenario row]
  (when (not= (count (:header row)) (count (:cells row)))
    (outline-validation-failure
     feature
     scenario
     :malformed-outline-examples
     "Scenario Outline Examples rows must have the same number of cells as the header."
     {:line (:line row)
      :expected-columns (count (:header row))
      :actual-columns (count (:cells row))
      :header (:header row)
      :row (:cells row)})))

(defn- unresolved-outline-placeholder-failures [feature scenario]
  (let [columns (get-in scenario [:example-header :cells])
        available-columns (set columns)
        steps (into (:background feature) (:steps scenario))]
    (vec
     (mapcat
      (fn [step]
        (map (fn [placeholder]
               (outline-validation-failure
                feature
                scenario
                :unresolved-outline-placeholder
                "Scenario Outline steps must not contain unresolved placeholders."
                {:line (:line step)
                 :step-text (:text step)
                 :placeholder placeholder
                 :available-columns columns}))
             (->> (outline-placeholders (:text step))
                  distinct
                  (remove available-columns))))
      steps))))

(defn- outline-validation-failures [feature scenario]
  (if-not (:outline? scenario)
    []
    (cond
      (nil? (:examples-line scenario))
      [(outline-validation-failure
        feature
        scenario
        :missing-outline-examples
        "Scenario Outline entries must include an Examples section."
        {:line (:line scenario)})]

      (nil? (:example-header scenario))
      [(outline-validation-failure
        feature
        scenario
        :empty-outline-examples
        "Scenario Outline Examples sections must include a table header and at least one row."
        {:line (:examples-line scenario)})]

      (empty? (:examples scenario))
      [(outline-validation-failure
        feature
        scenario
        :empty-outline-examples
        "Scenario Outline Examples sections must include at least one data row."
        {:line (:examples-line scenario)
         :header-line (get-in scenario [:example-header :line])
         :header (get-in scenario [:example-header :cells])})]

      :else
      (vec (keep identity
                 (concat [(outline-header-failure feature scenario (:example-header scenario))]
                         (map #(outline-row-failure feature scenario %) (:examples scenario))
                         (unresolved-outline-placeholder-failures feature scenario)))))))

(defn feature-scenarios [feature]
  (mapcat (fn [scenario]
            (when-not (seq (outline-validation-failures feature scenario))
              (expand-scenario feature scenario)))
          (:scenarios feature)))

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

(defn- parse-validation-failure [feature {:keys [code message data]}]
  (let [failure-data (merge {:path (:path feature)
                             :feature (:name feature)}
                            data)
        scenario (when-let [line (:scenario-line failure-data)]
                   {:line line
                    :name (:scenario failure-data)})]
    (feature-failure feature scenario code message failure-data)))

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
        parse-failures (mapv #(parse-validation-failure feature %)
                             (:parse-errors feature))
        validation-failures (vec (mapcat #(outline-validation-failures feature %)
                                         (:scenarios feature)))
        structural-failures (vec (concat parse-failures validation-failures))
        scenarios (vec (feature-scenarios feature))]
    (cond
      (seq structural-failures)
      (vec (concat structural-failures
                   (mapv #(run-scenario feature step-definitions %) scenarios)))

      (seq scenarios)
      (mapv #(run-scenario feature step-definitions %) scenarios)

      :else
      [(no-scenarios-failure feature)])))

(defn- result-line [result]
  (or (get-in result [:step :line])
      (get-in result [:failure :data :line])
      (get-in result [:scenario :line])
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
           "File: " (:path feature) ":" (result-line result) "\n"
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
