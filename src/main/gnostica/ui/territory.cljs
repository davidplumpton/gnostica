(ns gnostica.ui.territory
  (:require [gnostica.app.events :as events]
            [gnostica.pieces :as pieces]
            [gnostica.ui.common :as ui]
            [re-frame.core :as rf]))

(defn territory-panel []
  (let [{:keys [cell selected-pieces]} @(rf/subscribe [events/territory-view])
        {:keys [row col orientation card]} cell
        {:keys [title group rank]} card]
    [:aside.territory-panel
     {:class (when-not (seq selected-pieces) "is-empty")}
     [:p.eyebrow "Territory"]
     [:h1 title]
     [:dl.territory-facts
      [:div
       [:dt "Arcana"]
       [:dd group]]
      (when rank
        [:div
         [:dt "Rank"]
         [:dd rank]])
      [:div
       [:dt "Orientation"]
       [:dd (ui/orientation-label orientation)]]
      [:div
       [:dt "Position"]
       [:dd (str "Row " (inc row) ", Column " (inc col))]]
      [:div
       [:dt "Pieces"]
       [:dd
        (if (seq selected-pieces)
          [:ul.territory-pieces
           (for [piece selected-pieces]
             (let [player (pieces/player-for piece)]
               ^{:key (:id piece)}
               [:li
                [:span.territory-piece-swatch
                 {:style {"--piece-color" (:css-color player)}}]
                [:span (ui/piece-summary piece)]]))]
          "None")]]]]))
