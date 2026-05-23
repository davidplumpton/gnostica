(ns gnostica.icons
  (:require [clojure.string :as str]))

(def stickas-reference
  "Gnostica Stickers PDF, Looney Labs/Wunderland: https://www.wunderland.com/icehouse/GnosticaStickas.pdf")

(def icon-definitions
  {:question-card {:label "Turn and play the next draw card"}
   :wild-suits {:label "Sword, rod, cup, or disc"}
   :draw-hand {:label "Discard and redraw hand"}
   :orient-minion {:label "Orient a minion"}
   :cup-unbounded {:label "Cup without the normal space limit"}
   :rod-unbounded {:label "Rod without the normal space limit"}
   :convert-piece {:label "Replace and orient a piece"}
   :rod {:label "Rod"}
   :cup {:label "Cup"}
   :trade-hand {:label "Trade hands"}
   :sword {:label "Sword"}
   :relocate {:label "Move to any empty space"}
   :wheel-cup {:label "Cup with optional draw-pile territory"}
   :disc {:label "Disc"}
   :orient-target {:label "Orient a target piece"}
   :sword-from-discard {:label "Sword with optional discard-pile territory"}
   :disc-from-discard {:label "Disc with optional discard-pile territory"}
   :judgement {:label "Draw from the discard pile"}
   :world {:label "Any major arcana power"}})

(def icon-ids
  (set (keys icon-definitions)))

(def major-arcana-card-ids
  ["fool"
   "magician"
   "high-priestess"
   "empress"
   "emperor"
   "hierophant"
   "lovers"
   "chariot"
   "justice"
   "hermit"
   "wheeloffortune"
   "strength"
   "hangedman"
   "death"
   "temperance"
   "devil"
   "tower"
   "star"
   "moon"
   "sun"
   "judgement"
   "world"])

(def major-arcana-card-icons
  {"fool" [:question-card :question-card]
   "magician" [:wild-suits]
   "high-priestess" [:draw-hand :draw-hand]
   "empress" [:orient-minion :cup-unbounded]
   "emperor" [:orient-minion :rod-unbounded]
   "hierophant" [:convert-piece]
   "lovers" [:rod :cup]
   "chariot" [:rod :rod]
   "justice" [:trade-hand :sword]
   "hermit" [:relocate]
   "wheeloffortune" [:wheel-cup]
   "strength" [:disc :disc]
   "hangedman" [:rod :trade-hand]
   "death" [:sword :sword]
   "temperance" [:cup :cup]
   "devil" [:orient-target :orient-target :orient-target]
   "tower" [:orient-minion :sword-from-discard]
   "star" [:orient-minion :disc-from-discard]
   "moon" [:rod :sword]
   "sun" [:cup :disc]
   "judgement" [:judgement]
   "world" [:world]})

(defn major-card-id? [card-id]
  (contains? major-arcana-card-icons card-id))

(defn present-icon-ids [ids]
  (vec (remove #(or (nil? %) (= :empty %)) ids)))

(defn icon-label [icon-id]
  (get-in icon-definitions [icon-id :label] (name icon-id)))

(defn icon-stack-label [icon-ids]
  (->> icon-ids
       present-icon-ids
       (map icon-label)
       (str/join ", ")))

(defn unknown-icon-ids [ids]
  (vec (remove icon-ids (present-icon-ids ids))))
