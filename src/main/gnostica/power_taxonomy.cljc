(ns gnostica.power-taxonomy
  (:require [gnostica.cards :as cards]))

(def suit-power-values
  [:cup :rod :disc :sword])

(def suit-power-set
  (set suit-power-values))

(def composite-major-card-powers
  {"empress" :empress
   "emperor" :emperor
   "lovers" :lovers
   "chariot" :chariot
   "hangedman" :hanged-man
   "temperance" :temperance})

(def composite-major-power-set
  (set (vals composite-major-card-powers)))

(def sword-major-card-powers
  {"justice" :justice
   "death" :death
   "tower" :tower
   "moon" :moon})

(def sword-major-power-set
  (set (vals sword-major-card-powers)))

(def implemented-full-card-power-entries
  [["fool" :fool]
   ["high-priestess" :high-priestess]
   ["empress" :empress]
   ["emperor" :emperor]
   ["hierophant" :hierophant]
   ["lovers" :lovers]
   ["chariot" :chariot]
   ["hermit" :hermit]
   ["hangedman" :hanged-man]
   ["temperance" :temperance]
   ["devil" :devil]
   ["moon" :moon]
   ["sun" :sun]
   ["judgement" :judgement]
   ["justice" :justice]
   ["death" :death]
   ["tower" :tower]
   ["world" :world]])

(def implemented-full-card-powers
  (into {} implemented-full-card-power-entries))

(def implemented-full-card-power-values
  (mapv second implemented-full-card-power-entries))

(def implemented-full-card-power-set
  (set implemented-full-card-power-values))

(def world-copied-full-card-power-values
  (vec (remove #{:world} implemented-full-card-power-values)))

(def world-copied-full-card-power-set
  (set world-copied-full-card-power-values))

(def world-copied-power-values
  (vec (concat suit-power-values world-copied-full-card-power-values)))

(def default-world-suit-copy-card-ids
  #{"wheeloffortune"
    "strength"
    "justice"
    "death"
    "tower"
    "star"})

(defn suit-power? [power]
  (contains? suit-power-set power))

(defn implemented-full-card-power? [power]
  (contains? implemented-full-card-power-set power))

(defn world-copied-full-card-power? [power]
  (contains? world-copied-full-card-power-set power))

(defn major-card? [card]
  (= :major (:arcana card)))

(defn world-card? [card]
  (= "world" (:id card)))

(defn world-copy-card? [card]
  (and (major-card? card)
       (not (world-card? card))))

(defn suit-powers-for-card [card]
  (cond-> []
    (cards/cup-card? card) (conj :cup)
    (cards/rod-card? card) (conj :rod)
    (cards/disc-card? card) (conj :disc)
    (cards/sword-card? card) (conj :sword)))

(defn full-card-power [card]
  (get implemented-full-card-powers (:id card)))

(defn powers-for-card [card]
  (when card
    (let [suit-powers (suit-powers-for-card card)
          full-power (full-card-power card)]
      (cond
        (nil? full-power)
        suit-powers

        (or (contains? composite-major-power-set full-power)
            (contains? sword-major-power-set full-power))
        (vec (cons full-power suit-powers))

        :else
        (conj suit-powers full-power)))))

(defn world-copied-power-ids-for-card [card]
  (vec (remove #{:world} (powers-for-card card))))

(defn fool-play-power-ids-for-card [card]
  (vec (remove #{:fool :world} (powers-for-card card))))
