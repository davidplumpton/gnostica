(ns gnostica.app.smoke
  (:require [gnostica.cards :as cards]))

(defn- smoke-major-icon-deck-order []
  (let [major-hand-card (cards/card-by-id "magician")
        major-board-cards [(cards/card-by-id "chariot")
                           (cards/card-by-id "devil")]
        minor-cards (filter #(= :minor (:arcana %)) cards/deck)
        current-hand-minors (take 5 minor-cards)
        other-hand-cards (take 30 (drop 5 minor-cards))
        board-minors (take 7 (drop 35 minor-cards))
        used-card-ids (set (map :id (concat [major-hand-card]
                                            major-board-cards
                                            current-hand-minors
                                            other-hand-cards
                                            board-minors)))
        remaining-cards (remove #(contains? used-card-ids (:id %)) cards/deck)]
    (vec (concat [major-hand-card]
                 current-hand-minors
                 other-hand-cards
                 major-board-cards
                 board-minors
                 remaining-cards))))

(defn init-options []
  (let [params (js/URLSearchParams. (.. js/window -location -search))]
    (when (= "major-icons" (.get params "gnostica-smoke"))
      {:game-options {:deck-order (smoke-major-icon-deck-order)}})))
