(ns gnostica.verify
  (:require [clj-kondo.core :as clj-kondo]
            [cljfmt.config :as cljfmt-config]
            [cljfmt.report :as cljfmt-report]
            [cljfmt.tool :as cljfmt-tool])
  (:gen-class))

(def lint-paths
  ["src/main" "test" "deps.edn" "shadow-cljs.edn"])

(def format-paths
  ["src/main" "test" "deps.edn" "shadow-cljs.edn"])

(def format-file-pattern
  #"\.(clj[csx]?|edn)$")

(defn- status-code [{:keys [failed?]}]
  (if failed? 1 0))

(defn- failing-lint-finding? [finding]
  (contains? #{:warning :error} (:level finding)))

(defn- run-clj-kondo []
  (println "Running clj-kondo...")
  (let [result (clj-kondo/run! {:lint lint-paths
                                :cache false
                                :parallel true})
        failing-findings (filterv failing-lint-finding? (:findings result))]
    (clj-kondo/print! (assoc result :findings failing-findings))
    {:failed? (boolean (seq failing-findings))
     :finding-count (count failing-findings)}))

(defn- print-format-failure-summary [{:keys [incorrect-files errored-files]}]
  (doseq [[path {:keys [diff]}] incorrect-files]
    (binding [*out* *err*]
      (println path "has incorrect formatting")
      (when diff
        (println diff))))
  (doseq [[path {:keys [exception]}] errored-files]
    (binding [*out* *err*]
      (println "Failed to format file:" path)
      (when exception
        (.printStackTrace ^Throwable exception *err*)))))

(defn- run-cljfmt-check []
  (println "Running cljfmt check...")
  (let [options (merge cljfmt-config/default-config
                       {:paths format-paths
                        :file-pattern format-file-pattern
                        :ansi? false
                        :parallel? false
                        :report cljfmt-report/clojure})
        result (cljfmt-tool/check-no-config options)
        {:keys [counts] :as report} (:results result)
        failure-count (+ (:incorrect counts 0)
                         (:error counts 0))]
    (if (zero? failure-count)
      (println "All source files formatted correctly")
      (print-format-failure-summary report))
    {:failed? (pos? failure-count)
     :counts counts}))

(defn -main [& _args]
  (let [lint-result (run-clj-kondo)
        format-result (run-cljfmt-check)
        exit-code (max (status-code lint-result)
                       (status-code format-result))]
    (when (zero? exit-code)
      (println "Lint and formatting checks passed"))
    (shutdown-agents)
    (System/exit exit-code)))
