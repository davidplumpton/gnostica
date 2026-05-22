(ns gnostica.test-runner
  (:require [clojure.test :refer [run-tests]]
            [gnostica.app-state-test]
            [gnostica.board-test]
            [gnostica.cards-test]
            [gnostica.game-state-test]
            [gnostica.pieces-test]))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'gnostica.cards-test
                                        'gnostica.board-test
                                        'gnostica.app-state-test
                                        'gnostica.pieces-test
                                        'gnostica.game-state-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
