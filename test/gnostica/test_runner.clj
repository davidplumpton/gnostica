(ns gnostica.test-runner
  (:require [clojure.test :refer [run-tests]]
            [gnostica.board-test]
            [gnostica.cards-test]))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'gnostica.cards-test
                                        'gnostica.board-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
