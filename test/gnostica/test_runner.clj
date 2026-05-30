(ns gnostica.test-runner
  (:require [clojure.test :refer [run-tests]]
            [gnostica.app-state-test]
            [gnostica.board-layout-test]
            [gnostica.board-test]
            [gnostica.cards-test]
            [gnostica.fixtures-test]
            [gnostica.gameplay-feature-test]
            [gnostica.game-schema-test]
            [gnostica.game-state-test]
            [gnostica.gesture-input-test]
            [gnostica.icon-layout-test]
            [gnostica.keyboard-shortcuts-test]
            [gnostica.major-sequence-test]
            [gnostica.pieces-test]))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'gnostica.cards-test
                                        'gnostica.board-test
                                        'gnostica.board-layout-test
                                        'gnostica.app-state-test
                                        'gnostica.fixtures-test
                                        'gnostica.pieces-test
                                        'gnostica.game-schema-test
                                        'gnostica.game-state-test
                                        'gnostica.gesture-input-test
                                        'gnostica.icon-layout-test
                                        'gnostica.keyboard-shortcuts-test
                                        'gnostica.major-sequence-test
                                        'gnostica.gameplay-feature-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
