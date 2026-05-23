(ns gnostica.cards
  (:require [clojure.string :as str]
            [gnostica.icons :as icons]))

(def image-files
  ["chariot.png"
   "coins2.png"
   "coins3.png"
   "coins4.png"
   "coins5.png"
   "coins6.png"
   "coins7.png"
   "coins8.png"
   "coins9.png"
   "coins10.png"
   "coinsace.png"
   "coinsking.png"
   "coinsknight.png"
   "coinspage.png"
   "coinsqueen.png"
   "cups10.png"
   "cups2.png"
   "cups3.png"
   "cups4.png"
   "cups5.png"
   "cups6.png"
   "cups7.png"
   "cups8.png"
   "cups9.png"
   "cupsace.png"
   "cupsking.png"
   "cupsknight.png"
   "cupspage.png"
   "cupsqueen.png"
   "death.png"
   "devil.png"
   "emperor.png"
   "empress.png"
   "fool.png"
   "hangedman.png"
   "hermit.png"
   "hierophant.png"
   "high-priestess.png"
   "judgement.png"
   "justice.png"
   "lovers.png"
   "magician.png"
   "moon.png"
   "star.png"
   "strength.png"
   "sun.png"
   "swords2.png"
   "swords3.png"
   "swords4.png"
   "swords5.png"
   "swords6.png"
   "swords7.png"
   "swords8.png"
   "swords9.png"
   "swords10.png"
   "swordsace.png"
   "swordsking.png"
   "swordsknight.png"
   "swordspage.png"
   "swordsqueen.png"
   "temperance.png"
   "tower.png"
   "wands2.png"
   "wands3.png"
   "wands4.png"
   "wands5.png"
   "wands6.png"
   "wands7.png"
   "wands8.png"
   "wands9.png"
   "wands10.png"
   "wandsace.png"
   "wandsking.png"
   "wandsknight.png"
   "wandspage.png"
   "wandsqueen.png"
   "wheeloffortune.png"
   "world.png"])

(def major-titles
  {"chariot" "The Chariot"
   "death" "Death"
   "devil" "The Devil"
   "emperor" "The Emperor"
   "empress" "The Empress"
   "fool" "The Fool"
   "hangedman" "The Hanged Man"
   "hermit" "The Hermit"
   "hierophant" "The Hierophant"
   "high-priestess" "The High Priestess"
   "judgement" "Judgement"
   "justice" "Justice"
   "lovers" "The Lovers"
   "magician" "The Magician"
   "moon" "The Moon"
   "star" "The Star"
   "strength" "Strength"
   "sun" "The Sun"
   "temperance" "Temperance"
   "tower" "The Tower"
   "wheeloffortune" "Wheel of Fortune"
   "world" "The World"})

(def suit-titles
  {"coins" "Coins"
   "cups" "Cups"
   "swords" "Swords"
   "wands" "Wands"})

(def suit-prefixes ["coins" "cups" "swords" "wands"])

(def one-point-rank-keys #{"ace" "2" "3" "4" "5" "6" "7" "8" "9" "10"})

(def cup-icon-ids #{:cup :cup-unbounded :wheel-cup :wild-suits})

(def rank-titles
  {"1" "One"
   "2" "Two"
   "3" "Three"
   "4" "Four"
   "5" "Five"
   "6" "Six"
   "7" "Seven"
   "8" "Eight"
   "9" "Nine"
   "10" "Ten"
   "11" "Eleven"
   "ace" "Ace"
   "page" "Page"
   "knight" "Knight"
   "queen" "Queen"
   "king" "King"})

(defn- stem [file-name]
  (str/replace file-name #"\.png$" ""))

(defn- starts-with? [value prefix]
  #?(:clj (.startsWith ^String value prefix)
     :cljs (.startsWith value prefix)))

(defn- minor-card [card-stem]
  (some
   (fn [suit-key]
     (when (starts-with? card-stem suit-key)
       (let [rank-key (subs card-stem (count suit-key))
             rank (get rank-titles rank-key (str/capitalize rank-key))
             suit (get suit-titles suit-key)]
         {:arcana :minor
          :group suit
          :suit-key suit-key
          :rank-key rank-key
          :rank rank
          :suit suit
          :title (str rank " of " suit)})))
   suit-prefixes))

(defn- card [file-name]
  (let [card-stem (stem file-name)]
    (merge
     {:id card-stem
      :image (str "/images/" file-name)}
     (if-let [title (get major-titles card-stem)]
       (cond-> {:arcana :major
                :group "Major Arcana"
                :title title}
         (icons/major-card-id? card-stem)
         (assoc :gnostica-icons (icons/present-icon-ids
                                  (get icons/major-arcana-card-icons card-stem))))
       (minor-card card-stem)))))

(def deck
  (mapv card image-files))

(def cards-by-id
  (into {} (map (juxt :id identity) deck)))

(defn card-by-id [id]
  (get cards-by-id id))

(defn- known-card [card]
  (merge (get cards-by-id (:id card)) card))

(defn- minor-parts [card]
  (let [card (known-card card)]
    (if (and (:suit-key card) (:rank-key card))
      [(:suit-key card) (:rank-key card)]
      (some
       (fn [suit-key]
         (when (and (string? (:id card))
                    (starts-with? (:id card) suit-key))
           [suit-key (subs (:id card) (count suit-key))]))
       suit-prefixes))))

(defn cup-card? [card]
  (let [card (known-card card)
        [suit-key] (minor-parts card)]
    (or (= "cups" suit-key)
        (boolean (some cup-icon-ids (:gnostica-icons card))))))

(defn one-point-card? [card]
  (let [card (known-card card)
        [suit-key rank-key] (minor-parts card)]
    (and (= :minor (:arcana card))
         (contains? (set suit-prefixes) suit-key)
         (contains? one-point-rank-keys rank-key))))
