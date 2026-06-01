(ns gnostica.three-board.accessibility-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [gnostica.three-board.accessibility :as accessibility]))

(deftest board-aria-label-describes-current-territory-count
  (testing "zero territories"
    (is (str/includes? (accessibility/board-aria-label [] nil :always)
                       "with no face-up tarot territory cards and Icehouse pieces")))
  (testing "one territory"
    (is (str/includes? (accessibility/board-aria-label [{:index 7}] nil :always)
                       "with one face-up tarot territory card and Icehouse pieces")))
  (testing "many territories can be gapped or appended"
    (let [cells [{:index 0} {:index 2} {:index 11}]
          label (accessibility/board-aria-label cells nil :always)]
      (is (str/includes? label
                         "with 3 face-up tarot territory cards and Icehouse pieces"))
      (is (not (str/includes? label "nine face-up tarot territory cards"))))))

(deftest board-aria-label-includes-selected-popup-card-icons
  (let [selected-card {:gnostica-icons [:rod :cup]}]
    (is (str/includes? (accessibility/board-aria-label [{}]
                                                       selected-card
                                                       :popup)
                       "Selected card special moves: Rod, Cup"))
    (is (not (str/includes? (accessibility/board-aria-label [{}]
                                                            selected-card
                                                            :always)
                            "Selected card special moves")))))
