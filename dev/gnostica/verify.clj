(ns gnostica.verify
  (:require [clj-kondo.core :as clj-kondo]
            [clojure.java.io :as io]
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

(def legacy-bd-sentinel-path
  ".beads/embeddeddolt")

(def br-jsonl-path
  ".beads/issues.jsonl")

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

(defn- tracker-guard-failures []
  (let [sentinel (io/file legacy-bd-sentinel-path)
        jsonl (io/file br-jsonl-path)]
    (cond-> []
      (not (.exists sentinel))
      (conj (str legacy-bd-sentinel-path
                 " is missing; expected a regular file that blocks legacy bd"))

      (.isDirectory sentinel)
      (conj (str legacy-bd-sentinel-path
                 " is a directory; move or delete the stale legacy bd cache"))

      (and (.exists sentinel)
           (not (.isDirectory sentinel))
           (not (.isFile sentinel)))
      (conj (str legacy-bd-sentinel-path
                 " is not a regular file"))

      (not (.exists jsonl))
      (conj (str br-jsonl-path
                 " is missing; recover it with br sync --flush-only --force")))))

(defn- run-tracker-guard-check []
  (println "Checking br tracker guard...")
  (let [failures (tracker-guard-failures)]
    (if (seq failures)
      (do
        (binding [*out* *err*]
          (doseq [failure failures]
            (println failure)))
        {:failed? true
         :failure-count (count failures)})
      (do
        (println "br tracker guard is intact")
        {:failed? false
         :failure-count 0}))))

(defn -main [& _args]
  (let [guard-result (run-tracker-guard-check)
        lint-result (run-clj-kondo)
        format-result (run-cljfmt-check)
        exit-code (max (status-code guard-result)
                       (status-code lint-result)
                       (status-code format-result))]
    (when (zero? exit-code)
      (println "Lint and formatting checks passed"))
    (shutdown-agents)
    (System/exit exit-code)))
