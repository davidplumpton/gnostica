(ns gnostica.three-board.accessibility
  (:require [gnostica.icons :as icons]))

(defn territory-card-count-label [card-count]
  (case card-count
    0 "no face-up tarot territory cards"
    1 "one face-up tarot territory card"
    (str card-count " face-up tarot territory cards")))

(defn board-aria-label [cells selected-card card-icon-mode]
  (str "Three-dimensional Gnostica board with "
       (territory-card-count-label (count cells))
       " and Icehouse pieces. "
       "Use W, A, S, D, or arrow keys to move the board view when focused"
       (when-let [summary (and (= :popup card-icon-mode)
                               (icons/icon-stack-label (:gnostica-icons selected-card)))]
         (when (seq summary)
           (str ". Selected card special moves: " summary)))))
