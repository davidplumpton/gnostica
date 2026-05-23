(ns gnostica.icons
  (:require [clojure.string :as str]))

(def stickas-reference
  "Gnostica Stickers PDF, Looney Labs/Wunderland: https://www.wunderland.com/icehouse/GnosticaStickas.pdf")

(def icon-definitions
  {:empty {:label "Empty power"}
   :question-card {:label "Turn and play the next draw card"}
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

(def major-arcana-icon-triplets
  {"fool" [:question-card :question-card :empty]
   "magician" [:wild-suits :empty :empty]
   "high-priestess" [:draw-hand :draw-hand :empty]
   "empress" [:orient-minion :cup-unbounded :empty]
   "emperor" [:orient-minion :rod-unbounded :empty]
   "hierophant" [:convert-piece :empty :empty]
   "lovers" [:rod :cup :empty]
   "chariot" [:rod :rod :empty]
   "justice" [:trade-hand :sword :empty]
   "hermit" [:relocate :empty :empty]
   "wheeloffortune" [:wheel-cup :empty :empty]
   "strength" [:disc :disc :empty]
   "hangedman" [:rod :trade-hand :empty]
   "death" [:sword :sword :empty]
   "temperance" [:cup :cup :empty]
   "devil" [:orient-target :orient-target :orient-target]
   "tower" [:orient-minion :sword-from-discard :empty]
   "star" [:orient-minion :disc-from-discard :empty]
   "moon" [:rod :sword :empty]
   "sun" [:cup :disc :empty]
   "judgement" [:judgement :empty :empty]
   "world" [:world :empty :empty]})

(defn major-card-id? [card-id]
  (contains? major-arcana-icon-triplets card-id))

(defn icon-label [icon-id]
  (get-in icon-definitions [icon-id :label] (name icon-id)))

(defn icon-triplet-label [icon-ids]
  (->> icon-ids
       (map icon-label)
       (str/join ", ")))

(defn unknown-icon-ids [ids]
  (vec (remove icon-ids ids)))
