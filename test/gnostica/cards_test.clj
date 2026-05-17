(ns gnostica.cards-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]
            [gnostica.cards :as cards]))

(deftest deck-has-card-models-for-assets
  (is (= (count cards/image-files) (count cards/deck)))
  (is (pos? (count cards/deck)))
  (is (every? :id cards/deck))
  (is (every? :title cards/deck))
  (is (every? #(str/starts-with? (:image %) "/images/") cards/deck)))

(deftest known-cards-are-labelled
  (is (= "The High Priestess" (:title (cards/card-by-id "high-priestess"))))
  (is (= "Ace of Wands" (:title (cards/card-by-id "wandsace"))))
  (is (= "Queen of Cups" (:title (cards/card-by-id "cupsqueen")))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'gnostica.cards-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
