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
    (is (= icons/icon-ids
           (set icons/icon-definition-order)))
    (is (= (count icons/icon-ids)
           (count (icons/icon-glossary-items))))
    (doseq [{:keys [id label description]} (icons/icon-glossary-items)]
      (is (contains? icons/icon-ids id))
      (is (not (str/blank? label)))
      (is (not (str/blank? description))))
    (is (str/includes? icons/stickas-reference "GnosticaStickas.pdf"))))

(deftest cup-and-one-point-card-helpers-identify-move-cards
  (is (cards/cup-card? (cards/card-by-id "cups2")))
  (is (cards/cup-card? (cards/card-by-id "sun")))
  (is (cards/cup-card? (cards/card-by-id "empress")))
  (is (cards/cup-card? (cards/card-by-id "wheeloffortune")))
  (is (cards/cup-card? (cards/card-by-id "magician")))
  (is (not (cards/cup-card? (cards/card-by-id "coins2"))))
  (is (= [:cup] (cards/cup-variants (cards/card-by-id "cups2"))))
  (is (= [:cup] (cards/cup-variants (cards/card-by-id "sun"))))
  (is (= [:cup-unbounded] (cards/cup-variants (cards/card-by-id "empress"))))
  (is (= [:wheel-cup] (cards/cup-variants (cards/card-by-id "wheeloffortune"))))
  (is (= [:wild-suits] (cards/cup-variants (cards/card-by-id "magician"))))
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

(deftest rod-card-helper-identifies-rod-sources
  (is (cards/rod-card? (cards/card-by-id "wands2")))
  (is (cards/rod-card? (cards/card-by-id "chariot")))
  (is (cards/rod-card? (cards/card-by-id "emperor")))
  (is (cards/rod-card? (cards/card-by-id "magician")))
  (is (= [:rod] (cards/rod-variants (cards/card-by-id "wands2"))))
  (is (= [:rod] (cards/rod-variants (cards/card-by-id "chariot"))))
  (is (= [:rod-unbounded] (cards/rod-variants (cards/card-by-id "emperor"))))
  (is (= [:wild-suits] (cards/rod-variants (cards/card-by-id "magician"))))
  (is (cards/rod-card? {:id "wands3"
                        :title "Three of Wands"
                        :image "/images/wands3.png"}))
  (is (not (cards/rod-card? (cards/card-by-id "cups2"))))
  (is (not (cards/rod-card? (cards/card-by-id "sun")))))

(deftest disc-card-helper-identifies-disc-sources
  (is (cards/disc-card? (cards/card-by-id "coins2")))
  (is (cards/disc-card? (cards/card-by-id "coinsqueen")))
  (is (cards/disc-card? (cards/card-by-id "strength")))
  (is (cards/disc-card? (cards/card-by-id "star")))
  (is (cards/disc-card? (cards/card-by-id "sun")))
  (is (cards/disc-card? (cards/card-by-id "magician")))
  (is (= [:disc] (cards/disc-variants (cards/card-by-id "coins2"))))
  (is (= [:disc] (cards/disc-variants (cards/card-by-id "coinsqueen"))))
  (is (= [:disc] (cards/disc-variants (cards/card-by-id "strength"))))
  (is (= [:disc-from-discard] (cards/disc-variants (cards/card-by-id "star"))))
  (is (= [:disc] (cards/disc-variants (cards/card-by-id "sun"))))
  (is (= [:wild-suits] (cards/disc-variants (cards/card-by-id "magician"))))
  (is (cards/disc-card? {:id "coins3"
                         :title "Three of Coins"
                         :image "/images/coins3.png"}))
  (is (not (cards/disc-card? (cards/card-by-id "cups2"))))
  (is (not (cards/disc-card? (cards/card-by-id "chariot")))))

(deftest sword-card-helper-identifies-sword-sources
  (is (cards/sword-card? (cards/card-by-id "swords2")))
  (is (cards/sword-card? (cards/card-by-id "swordsqueen")))
  (is (cards/sword-card? (cards/card-by-id "justice")))
  (is (cards/sword-card? (cards/card-by-id "death")))
  (is (cards/sword-card? (cards/card-by-id "tower")))
  (is (cards/sword-card? (cards/card-by-id "moon")))
  (is (cards/sword-card? (cards/card-by-id "magician")))
  (is (= [:sword] (cards/sword-variants (cards/card-by-id "swords2"))))
  (is (= [:sword] (cards/sword-variants (cards/card-by-id "swordsqueen"))))
  (is (= [:sword] (cards/sword-variants (cards/card-by-id "justice"))))
  (is (= [:sword] (cards/sword-variants (cards/card-by-id "death"))))
  (is (= [:sword-from-discard] (cards/sword-variants (cards/card-by-id "tower"))))
  (is (= [:sword] (cards/sword-variants (cards/card-by-id "moon"))))
  (is (= [:wild-suits] (cards/sword-variants (cards/card-by-id "magician"))))
  (is (cards/sword-card? {:id "swords3"
                          :title "Three of Swords"
                          :image "/images/swords3.png"}))
  (is (not (cards/sword-card? (cards/card-by-id "cups2"))))
  (is (not (cards/sword-card? (cards/card-by-id "strength")))))

(deftest bid-ranks-follow-official-starting-order
  (is (= 0 (cards/major-bid-rank (cards/card-by-id "fool"))))
  (is (= 21 (cards/major-bid-rank (cards/card-by-id "world"))))
  (is (= 8 (cards/major-bid-rank (cards/card-by-id "justice"))))
  (is (= 11 (cards/major-bid-rank (cards/card-by-id "strength"))))
  (is (= 1 (cards/minor-bid-rank (cards/card-by-id "cupsace"))))
  (is (= 10 (cards/minor-bid-rank (cards/card-by-id "swords10"))))
  (is (= 14 (cards/minor-bid-rank (cards/card-by-id "wandsking"))))
  (is (= {:arcana :major
          :rank 21
          :card-id "world"}
         (cards/bid-rank (cards/card-by-id "world"))))
  (is (= {:arcana :minor
          :rank 14
          :suit-key "cups"
          :rank-key "king"
          :card-id "cupsking"}
         (cards/bid-rank (cards/card-by-id "cupsking"))))
  (is (nil? (cards/bid-rank {:id "unknown"
                             :title "Unknown"
                             :image "/images/unknown.png"}))))

(deftest card-point-values-support-territory-growth_steps
  (is (= 1 (cards/card-point-value (cards/card-by-id "coins2"))))
  (is (= 2 (cards/card-point-value (cards/card-by-id "cupsking"))))
  (is (= 3 (cards/card-point-value (cards/card-by-id "sun"))))
  (is (nil? (cards/card-point-value {:id "unknown"})))
  (is (cards/card-worth-one-more? (cards/card-by-id "cupsking")
                                  (cards/card-by-id "coins2")))
  (is (cards/card-worth-one-more? (cards/card-by-id "sun")
                                  (cards/card-by-id "cupsking")))
  (is (not (cards/card-worth-one-more? (cards/card-by-id "sun")
                                       (cards/card-by-id "coins2")))))

(defn -main [& _]
  (let [{:keys [fail error]} (run-tests 'gnostica.cards-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
