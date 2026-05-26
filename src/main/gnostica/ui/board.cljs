(ns gnostica.ui.board
  (:require [gnostica.app.events :as events]
            [gnostica.pieces :as pieces]
            [gnostica.three-board :as three-board]
            [gnostica.ui.card :as card-ui]
            [gnostica.ui.common :as ui]
            [re-frame.core :as rf]))

(defn- css-space-offset [step-count edge-offset]
  (str "calc(var(--board-void-margin) + "
       step-count
       " * var(--card-step) + "
       edge-offset
       ")"))

(defn- board-stage-length [min-value max-value]
  (str "calc("
       (- max-value min-value)
       " * var(--card-step) + var(--card-long) + 2 * var(--board-void-margin))"))

(defn- board-stage-style [{:keys [min-row max-row min-col max-col]}]
  {"width" (board-stage-length min-col max-col)
   "height" (board-stage-length min-row max-row)})

(defn- board-space-style [{:keys [min-row min-col]} {:keys [row col orientation]}]
  (let [relative-row (- row min-row)
        relative-col (- col min-col)]
    {"left" (css-space-offset relative-col
                              (if (= :portrait orientation)
                                "var(--card-offset)"
                                "0px"))
     "top" (css-space-offset relative-row
                             (if (= :landscape orientation)
                               "var(--card-offset)"
                               "0px"))}))

(defn- board-piece-marker [slot piece]
  (let [pips (pieces/pips piece)
        player (pieces/player-for piece)]
    ^{:key (:id piece)}
    [:span.board-piece
     {:class (str "is-slot-" slot
                  " is-" (name (:size piece))
                  " is-" (name (:orientation piece)))
      :style {"--piece-color" (:css-color player)}}
     [:span.board-piece__body
      [:span.board-piece__pips
       (for [pip (range pips)]
         ^{:key pip}
         [:span.board-piece__pip])]]]))

(defn- board-wasteland [bounds {:keys [id orientation] :as space}]
  ^{:key id}
  [:div.board-wasteland
   {:class (str "is-" (name orientation))
    :style (board-space-style bounds space)
    :aria-hidden "true"}])

(defn board-card [bounds {:keys [index row col orientation card] :as cell} selected? board-pieces card-icon-mode]
  (let [{:keys [title]} card]
    [:button.board-card
     {:type "button"
      :class (str "is-" (name orientation)
                  " is-row-" row
                  " is-col-" col
                  (when selected? " is-selected"))
      :style (board-space-style bounds cell)
      :aria-label (str title
                       ", "
                       (ui/orientation-label orientation)
                       ", row "
                       (inc row)
                       ", column "
                       (inc col)
                       (when-let [summary (card-ui/card-icon-summary card)]
                         (str ", special moves: " summary))
                       (when (seq board-pieces)
                         (str ", pieces: "
                              (apply str (interpose "; " (map ui/piece-summary board-pieces))))))
      :on-click #(rf/dispatch [events/select-board-card index])}
     [card-ui/card-face card "board-card__face" card-icon-mode]
     (when (seq board-pieces)
       [:div.board-card__pieces
        {:aria-hidden "true"}
        (for [[slot piece] (map-indexed vector (take pieces/max-pieces-per-space board-pieces))]
          (board-piece-marker slot piece))])]))

(defn board-stage []
  (let [{:keys [cells board-pieces pieces-by-space wastelands space-bounds
                selected-index card-icon-mode texture-errors three-revision
                three-renderer-available? three-renderer-message]}
        @(rf/subscribe [events/board-view])]
    [:section.board-area
     {:data-three-revision (or three-revision "unavailable")}
     (cond
       (empty? cells)
       [:div.board-empty
        [:p "Board setup pending"]]

       three-renderer-available?
       [three-board/scene
        cells
        board-pieces
        selected-index
        card-icon-mode
        texture-errors
        {:on-card-select #(rf/dispatch [events/select-board-card %])
         :on-clear-texture-errors #(rf/dispatch [events/clear-three-texture-errors])
         :on-renderer-error #(rf/dispatch [events/three-renderer-error %])
         :on-texture-error #(rf/dispatch [events/three-texture-error %])}]

       :else
       [:div.board-fallback
        [:p.board-3d-status.is-error
         three-renderer-message]
        [:div.board-stage
         {:role "group"
          :aria-label "Gnostica board"
          :data-wasteland-count (count wastelands)
          :data-table-surface-color three-board/table-surface-css-color
          :data-table-clear-color three-board/table-clear-css-color
          :style (board-stage-style space-bounds)}
         (for [space wastelands]
           (board-wasteland space-bounds space))
         (for [cell cells]
           ^{:key (:index cell)}
           [board-card
            space-bounds
            cell
            (= selected-index (:index cell))
            (get pieces-by-space (:index cell))
            card-icon-mode])]])]))
