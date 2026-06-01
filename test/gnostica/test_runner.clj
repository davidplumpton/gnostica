(ns gnostica.test-runner
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [run-tests]]))

(def namespace-prefix "gnostica")

(defn- normalized-relative-path [root file]
  (-> (.relativize (.toPath (io/file root)) (.toPath (io/file file)))
      str
      (str/replace "\\" "/")))

(defn- smoke-helper-file? [relative-path]
  (or (= "smoke_runner.clj" relative-path)
      (str/starts-with? relative-path "smoke/")))

(defn test-file? [root file]
  (let [relative-path (normalized-relative-path root file)]
    (and (.isFile (io/file file))
         (str/ends-with? relative-path "_test.clj")
         (not (smoke-helper-file? relative-path)))))

(defn test-file->namespace [root file]
  (->> (normalized-relative-path root file)
       (#(str/replace % #"\.clj$" ""))
       (#(str/replace % "_" "-"))
       (#(str/replace % "/" "."))
       (str namespace-prefix ".")
       symbol))

(defn test-namespaces-in [root]
  (->> (file-seq (io/file root))
       (filter #(test-file? root %))
       (map #(test-file->namespace root %))
       (sort-by str)
       vec))

(defn test-root []
  (if-let [resource (io/resource "gnostica/test_runner.clj")]
    (try
      (-> resource .toURI io/file .getParentFile)
      (catch Exception _
        (io/file "test" "gnostica")))
    (io/file "test" "gnostica")))

(defn test-namespaces []
  (test-namespaces-in (test-root)))

(defn require-test-namespaces! [test-namespaces]
  (doseq [test-ns test-namespaces]
    (require test-ns)))

(defn -main [& _]
  (let [test-namespaces (test-namespaces)]
    (when (empty? test-namespaces)
      (throw (ex-info "No test namespaces found."
                      {:test-root (str (test-root))})))
    (require-test-namespaces! test-namespaces)
    (let [{:keys [fail error]} (apply run-tests test-namespaces)]
      (System/exit (if (zero? (+ fail error)) 0 1)))))
