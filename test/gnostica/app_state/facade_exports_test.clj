(ns gnostica.app-state.facade-exports-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [gnostica.app-state]
            [gnostica.app-state.facade-exports :as facade-exports]
            [gnostica.app-state.moves]
            [gnostica.move-selection]))

(defn- public-var-names [ns-sym]
  (set (keys (ns-publics ns-sym))))

(defn- root-value [ns-sym var-name]
  (var-get (ns-resolve ns-sym var-name)))

(defn- hook-def-value [var-name]
  (let [forms (read-string
               (str "[" (slurp ".clj-kondo/gnostica/kondo/facade.clj") "]"))
        form (some #(when (and (seq? %)
                               (= 'def (first %))
                               (= var-name (second %)))
                      %)
                   forms)
        value-form (nth form 2)]
    (if (and (seq? value-form)
             (= 'quote (first value-form)))
      (second value-form)
      value-form)))

(deftest move-selection-export-table-drives-app-state-move-facades
  (let [move-selection-exports (set facade-exports/move-selection-alias-vars)
        app-state-move-exports (set facade-exports/app-state-move-alias-vars)
        move-selection-publics (public-var-names 'gnostica.move-selection)
        moves-publics (public-var-names 'gnostica.app-state.moves)
        app-state-publics (public-var-names 'gnostica.app-state)]
    (testing "app-state.moves exports only the shared move-selection aliases plus its local app-db transitions"
      (is (= (set/union move-selection-exports
                        '#{end-turn confirm-move})
             moves-publics)))
    (testing "every listed move-selection alias resolves through both facades"
      (doseq [var-name move-selection-exports]
        (is (contains? move-selection-publics var-name))
        (is (contains? moves-publics var-name))
        (is (contains? app-state-publics var-name))
        (is (identical? (root-value 'gnostica.move-selection var-name)
                        (root-value 'gnostica.app-state.moves var-name)))
        (is (identical? (root-value 'gnostica.app-state.moves var-name)
                        (root-value 'gnostica.app-state var-name)))))
    (testing "app-state also re-exports app-state.moves local transition helpers"
      (doseq [var-name app-state-move-exports]
        (is (contains? app-state-publics var-name))
        (is (identical? (root-value 'gnostica.app-state.moves var-name)
                        (root-value 'gnostica.app-state var-name)))))))

(deftest clj-kondo-hook-alias-table-stays-in-sync
  (is (= facade-exports/move-selection-alias-vars
         (hook-def-value 'move-selection-alias-vars))))
