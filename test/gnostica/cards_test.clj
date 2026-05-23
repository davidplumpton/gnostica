(ns gnostica.cards-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests]]
            [gnostica.cards :as cards]
            [gnostica.icons :as icons]))

(deftest deck-has-card-models-for-assets
  (is (= (count cards/image-files) (count cards/deck)))
  (is (pos? (count cards/deck)))
  (is (every? :id cards/deck))
  (is (every? :title cards/deck))
  (is (every? #(str/starts-with? (:image %) "/images/") cards/deck)))

(deftest deck-image-files-match-resources
  (is (= 78 (count cards/image-files)))
  (is (= (count cards/image-files)
         (count (set cards/image-files))))
  (is (empty? (remove #(io/resource (str "images/" %)) cards/image-files))))

(deftest known-cards-are-labelled
  (is (= "The High Priestess" (:title (cards/card-by-id "high-priestess"))))
  (is (= "Ace of Wands" (:title (cards/card-by-id "wandsace"))))
  (is (= "Queen of Cups" (:title (cards/card-by-id "cupsqueen")))))

(deftest major-arcana-icon-triplets-are-complete
  (let [major-cards (filter #(= :major (:arcana %)) cards/deck)
        minor-cards (filter #(= :minor (:arcana %)) cards/deck)]
    (is (= 22 (count major-cards)))
    (is (= (set (map :id major-cards))
           (set icons/major-arcana-card-ids)))
    (is (= (set (map :id major-cards))
           (set (keys icons/major-arcana-icon-triplets))))
    (doseq [{:keys [id gnostica-icons]} major-cards]
      (is (= 3 (count gnostica-icons))
          (str id " must have exactly three Gnostica icons"))
      (is (empty? (icons/unknown-icon-ids gnostica-icons))
          (str id " references only known Gnostica icons")))
    (is (empty? (keep :gnostica-icons minor-cards)))
    (is (= [:rod :rod :empty]
           (:gnostica-icons (cards/card-by-id "chariot"))))
    (is (= [:cup :disc :empty]
           (:gnostica-icons (cards/card-by-id "sun"))))
    (is (str/includes? icons/stickas-reference "GnosticaStickas.pdf"))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'gnostica.cards-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
