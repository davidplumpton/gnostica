(ns gnostica.icons
  (:require [clojure.string :as str]))

(def stickas-reference
  "Gnostica Stickers PDF, Looney Labs/Wunderland: https://www.wunderland.com/icehouse/GnosticaStickas.pdf")

(def icon-definitions
  {:question-card
   {:label "Turn and play the next draw card"
    :description "Reveal the next draw-pile card and use its power. The Fool repeats this once, and each reveal is optional."}
   :wild-suits
   {:label "Sword, rod, cup, or disc"
    :description "Choose one suit power when the card is used: attack with a sword, move with a rod, create with a cup, or grow with a disc."}
   :draw-hand
   {:label "Discard and redraw hand"
    :description "Discard any number of cards, then draw back toward the six-card hand limit. High Priestess gives two separate redraw chances."}
   :orient-minion
   {:label "Orient a minion"
    :description "Turn one active piece to any legal direction before using the next listed power."}
   :cup-unbounded
   {:label "Cup without the normal space limit"
    :description "Use a Cup create action while ignoring the usual three-piece limit in the target space."}
   :rod-unbounded
   {:label "Rod without the normal space limit"
    :description "Use a Rod move action while allowing movement through or into spaces that would normally be blocked by the three-piece limit."}
   :convert-piece
   {:label "Replace and orient a piece"
    :description "Replace the target piece with one of your pieces of the same size, then choose its orientation."}
   :rod
   {:label "Rod"
    :description "Move your minion, push a target piece, or push a territory in the minion's direction up to that minion's pip count."}
   :cup
   {:label "Cup"
    :description "Create a small piece on a target territory, or create a new territory by placing a one-point card into a targeted wasteland."}
   :trade-hand
   {:label "Trade hands"
    :description "Exchange hands with the player who owns the piece your minion targets."}
   :sword
   {:label "Sword"
    :description "Shrink or destroy a target piece, or reduce a territory's value, up to the active minion's pip count."}
   :relocate
   {:label "Move to any empty space"
    :description "Move a targeted piece to any empty territory or wasteland, or move a territory to an eligible wasteland."}
   :wheel-cup
   {:label "Cup with optional draw-pile territory"
    :description "Use a Cup action. When creating territory, the new card may come from the top of the draw pile instead of your hand."}
   :disc
   {:label "Disc"
    :description "Grow a target piece by one size, or replace a target territory with a card worth exactly one more point."}
   :orient-target
   {:label "Orient a target piece"
    :description "Turn a targeted piece to any legal direction. This can affect enemy pieces when the Devil is used."}
   :sword-from-discard
   {:label "Sword with optional discard-pile territory"
    :description "Use a Sword action. When reducing a territory, the replacement card may come from the discard pile instead of your hand."}
   :disc-from-discard
   {:label "Disc with optional discard-pile territory"
    :description "Use a Disc action. When growing a territory, the replacement card may come from the discard pile instead of your hand."}
   :judgement
   {:label "Draw from the discard pile"
    :description "Draw cards from anywhere in the discard pile into your hand, up to the active minion's pip count and the hand limit."}
   :world
   {:label "Any major arcana power"
    :description "Use the power of any major arcana territory currently on the board."}})

(def icon-definition-order
  [:question-card
   :wild-suits
   :draw-hand
   :orient-minion
   :cup-unbounded
   :rod-unbounded
   :convert-piece
   :rod
   :cup
   :trade-hand
   :sword
   :relocate
   :wheel-cup
   :disc
   :orient-target
   :sword-from-discard
   :disc-from-discard
   :judgement
   :world])

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

(defn icon-description [icon-id]
  (get-in icon-definitions [icon-id :description] (icon-label icon-id)))

(defn icon-stack-label [icon-ids]
  (->> icon-ids
       present-icon-ids
       (map icon-label)
       (str/join ", ")))

(defn icon-glossary-item [icon-id]
  {:id icon-id
   :label (icon-label icon-id)
   :description (icon-description icon-id)})

(defn icon-glossary-items []
  (mapv icon-glossary-item icon-definition-order))

(defn unknown-icon-ids [ids]
  (vec (remove icon-ids (present-icon-ids ids))))
