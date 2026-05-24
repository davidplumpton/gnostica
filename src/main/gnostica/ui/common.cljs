(ns gnostica.ui.common
  (:require [gnostica.pieces :as pieces]))

(defn orientation-label [orientation]
  (case orientation
    :portrait "Portrait"
    :landscape "Landscape"
    "Unknown"))

(defn card-count-label [n]
  (str n " card" (when (not= 1 n) "s")))

(defn piece-summary [piece]
  (let [player (pieces/player-for piece)]
    (str (:name player)
         " "
         (pieces/size-label (:size piece))
         ", "
         (pieces/orientation-label (:orientation piece)))))

(defn wasteland-label [{:keys [row col]}]
  (str "Wasteland row "
       (inc row)
       ", column "
       (inc col)))
