(ns gnostica.test-runner-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [gnostica.test-runner :as test-runner])
  (:import [java.io File]))

(defn- temp-root! []
  (let [file (File/createTempFile "gnostica-test-runner" "dir")]
    (io/delete-file file)
    (when-not (.mkdir file)
      (throw (ex-info "Could not create temporary test root."
                      {:path (str file)})))
    file))

(defn- delete-recursive! [file]
  (when (.exists (io/file file))
    (doseq [child (reverse (file-seq (io/file file)))]
      (io/delete-file child true))))

(defn- write-file! [root & segments]
  (let [file (apply io/file root segments)]
    (.mkdirs (.getParentFile file))
    (spit file "(ns placeholder)\n")
    file))

(deftest test-namespace-discovery-normalizes-matching-files
  (testing "normal tests are discovered and support/helper files are ignored"
    (let [root (temp-root!)]
      (try
        (write-file! root "cards_test.clj")
        (write-file! root "feature_world.clj")
        (write-file! root "game_state" "command_schema_test.clj")
        (write-file! root "move_selection" "context_test.clj")
        (write-file! root "smoke" "browser_test.clj")
        (is (= '[gnostica.cards-test
                 gnostica.game-state.command-schema-test
                 gnostica.move-selection.context-test]
               (test-runner/test-namespaces-in root)))
        (finally
          (delete-recursive! root))))))

(deftest default-discovery-includes-runner-regression-test
  (is (contains? (set (test-runner/test-namespaces))
                 'gnostica.test-runner-test)))
