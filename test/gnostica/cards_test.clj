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

(deftest major-arcana-card-icons-are-complete
  (let [major-cards (filter #(= :major (:arcana %)) cards/deck)
        minor-cards (filter #(= :minor (:arcana %)) cards/deck)]
    (is (= 22 (count major-cards)))
    (is (= (set (map :id major-cards))
           (set icons/major-arcana-card-ids)))
    (is (= (set (map :id major-cards))
           (set (keys icons/major-arcana-card-icons))))
    (is (= #{1 2 3}
           (set (map (comp count :gnostica-icons) major-cards))))
    (is (not (contains? icons/icon-ids :empty)))
    (doseq [{:keys [id gnostica-icons]} major-cards]
      (is (<= 1 (count gnostica-icons) 3)
          (str id " must have between one and three Gnostica icons"))
      (is (not-any? #{:empty nil} gnostica-icons)
          (str id " must not use blank Gnostica icon placeholders"))
      (is (empty? (icons/unknown-icon-ids gnostica-icons))
          (str id " references only known Gnostica icons")))
    (is (empty? (keep :gnostica-icons minor-cards)))
    (is (= [:wild-suits]
           (:gnostica-icons (cards/card-by-id "magician"))))
    (is (= [:rod :rod]
           (:gnostica-icons (cards/card-by-id "chariot"))))
    (is (= [:orient-target :orient-target :orient-target]
           (:gnostica-icons (cards/card-by-id "devil"))))
    (is (= [:cup :disc]
           (icons/present-icon-ids [:cup :disc :empty nil])))
    (is (= "Cup, Disc"
           (icons/icon-stack-label [:cup :disc])))
    (is (str/includes? icons/stickas-reference "GnosticaStickas.pdf"))))

(deftest cup-and-one-point-card-helpers-identify-move-cards
  (is (cards/cup-card? (cards/card-by-id "cups2")))
  (is (cards/cup-card? (cards/card-by-id "sun")))
  (is (cards/cup-card? (cards/card-by-id "magician")))
  (is (not (cards/cup-card? (cards/card-by-id "coins2"))))
  (is (cards/cup-card? {:id "cups3"
                        :title "Three of Cups"
                        :image "/images/cups3.png"}))
  (is (cards/one-point-card? (cards/card-by-id "coins2")))
  (is (cards/one-point-card? (cards/card-by-id "cupsace")))
  (is (cards/one-point-card? {:id "wands10"
                              :title "Ten of Wands"
                              :image "/images/wands10.png"}))
  (is (not (cards/one-point-card? (cards/card-by-id "cupsqueen"))))
  (is (not (cards/one-point-card? (cards/card-by-id "sun")))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'gnostica.cards-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
