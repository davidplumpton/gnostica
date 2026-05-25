(ns gnostica.ui.move-panel
  (:require [gnostica.app.events :as events]
            [gnostica.pieces :as pieces]
            [gnostica.ui.common :as ui]
            [re-frame.core :as rf]))

(defn- move-source-picker [options selected-source]
  [:div.move-source-list
   (for [{:keys [id label summary enabled? reason]} options]
     ^{:key id}
     [:button.move-source-option
      {:type "button"
       :class (when (= selected-source id) "is-selected")
       :disabled (not enabled?)
       :aria-pressed (= selected-source id)
       :on-click #(rf/dispatch [events/select-move-source id])}
      [:span.move-source-option__label label]
      [:span.move-source-option__summary (if enabled? summary reason)]])])

(defn- board-cell-label [{:keys [row col card]}]
  (str (:title card)
       ", row "
       (inc row)
       ", column "
       (inc col)))

(defn- board-choice-grid [label cells selected-index]
  [:div.move-step
   [:div.move-step__header
    [:span label]
    [:strong
     (if-let [selected-cell (some #(when (= selected-index (:index %)) %) cells)]
       (:title (:card selected-cell))
       "None")]]
   [:div.move-board-choice-grid
    (for [cell cells]
      ^{:key (:index cell)}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-index (:index cell)) "is-selected")
        :aria-pressed (= selected-index (:index cell))
        :on-click #(rf/dispatch [events/select-board-card (:index cell)])}
       (board-cell-label cell)])]])

(defn- same-wasteland? [selected-space space]
  (and selected-space
       (= (:row selected-space) (:row space))
       (= (:col selected-space) (:col space))))

(defn- target-choice-grid [cells wastelands selected-index selected-wasteland]
  [:div.move-step
   [:div.move-step__header
    [:span "Target"]
    [:strong
     (if-let [selected-cell (some #(when (= selected-index (:index %)) %) cells)]
       (:title (:card selected-cell))
       (if selected-wasteland
         (ui/wasteland-label selected-wasteland)
         "None"))]]
   [:div.move-board-choice-grid
    (for [cell cells]
      ^{:key (str "territory-" (:index cell))}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-index (:index cell)) "is-selected")
        :aria-pressed (= selected-index (:index cell))
        :on-click #(rf/dispatch [events/select-board-card (:index cell)])}
       (str "Territory: " (board-cell-label cell))])
    (for [space wastelands]
      ^{:key (:id space)}
      [:button.move-chip
       {:type "button"
        :class (when (same-wasteland? selected-wasteland space) "is-selected")
        :aria-pressed (same-wasteland? selected-wasteland space)
        :on-click #(rf/dispatch [events/select-move-wasteland-target (:row space) (:col space)])}
       (ui/wasteland-label space)])]])

(defn- hand-card-choices [cards selected-card-id]
  [:div.move-step
   [:div.move-step__header
    [:span "Hand card"]
    [:strong
     (or (:title (some #(when (= selected-card-id (:id %)) %) cards))
         "None")]]
   [:div.move-choice-list
    (for [card cards]
      ^{:key (:id card)}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-card-id (:id card)) "is-selected")
        :aria-pressed (= selected-card-id (:id card))
        :on-click #(rf/dispatch [events/select-move-hand-card (:id card)])}
       (:title card)])]])

(defn- one-point-card-choices [cards selected-card-id]
  [:div.move-step
   [:div.move-step__header
    [:span "One-point card"]
    [:strong
     (or (:title (some #(when (= selected-card-id (:id %)) %) cards))
         "None")]]
   (if (seq cards)
     [:div.move-choice-list
      (for [card cards]
        ^{:key (:id card)}
        [:button.move-chip
         {:type "button"
          :class (when (= selected-card-id (:id card)) "is-selected")
          :aria-pressed (= selected-card-id (:id card))
          :on-click #(rf/dispatch [events/select-move-one-point-card (:id card)])}
         (:title card)])]
     [:p.move-step__empty "No one-point cards available."])])

(defn- replacement-card-choices [cards selected-card-id]
  [:div.move-step
   [:div.move-step__header
    [:span "Replacement card"]
    [:strong
     (or (:title (some #(when (= selected-card-id (:id %)) %) cards))
         "None")]]
   (if (seq cards)
     [:div.move-choice-list
      (for [card cards]
        ^{:key (:id card)}
        [:button.move-chip
         {:type "button"
          :class (when (= selected-card-id (:id card)) "is-selected")
          :aria-pressed (= selected-card-id (:id card))
          :on-click #(rf/dispatch [events/select-move-replacement-card (:id card)])}
         (:title card)])]
     [:p.move-step__empty "No replacement cards available."])])

(defn- territory-card-source-choices
  ([options selected-source]
   (territory-card-source-choices "Territory card" options selected-source))
  ([label options selected-source]
  (when (< 1 (count options))
    [:div.move-step
     [:div.move-step__header
      [:span label]
      [:strong
       (or (:label (some #(when (= selected-source (:id %)) %) options))
           "None")]]
     [:div.move-choice-list.is-compact
      (for [{:keys [id label]} options]
        ^{:key id}
        [:button.move-chip
         {:type "button"
          :class (when (= selected-source id) "is-selected")
          :aria-pressed (= selected-source id)
          :on-click #(rf/dispatch [events/select-move-territory-card-source id])}
         label])]])))

(defn- power-choices [options selected-power]
  (when (< 1 (count options))
    [:div.move-step
     [:div.move-step__header
      [:span "Power"]
      [:strong
       (or (:label (some #(when (= selected-power (:id %)) %) options))
           "None")]]
     [:div.move-choice-list.is-compact
      (for [{:keys [id label]} options]
        ^{:key id}
        [:button.move-chip
         {:type "button"
          :class (when (= selected-power id) "is-selected")
          :aria-pressed (= selected-power id)
          :on-click #(rf/dispatch [events/select-move-power id])}
         label])]]))

(defn- rod-mode-choices [options selected-mode]
  [:div.move-step
   [:div.move-step__header
    [:span "Rod move"]
    [:strong
     (or (:label (some #(when (= selected-mode (:id %)) %) options))
         "None")]]
   [:div.move-choice-list
    (for [{:keys [id label]} options]
      ^{:key id}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-mode id) "is-selected")
        :aria-pressed (= selected-mode id)
        :on-click #(rf/dispatch [events/select-move-rod-mode id])}
         label])]])

(defn- disc-target-kind-choices [options selected-kind]
  [:div.move-step
   [:div.move-step__header
    [:span "Disc move"]
    [:strong
     (or (:label (some #(when (= selected-kind (:id %)) %) options))
         "None")]]
   [:div.move-choice-list
    (for [{:keys [id label]} options]
      ^{:key id}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-kind id) "is-selected")
        :aria-pressed (= selected-kind id)
        :on-click #(rf/dispatch [events/select-move-disc-target-kind id])}
       label])]])

(defn- piece-choice-label [board piece]
  (let [cell (get board (:space-index piece))]
    (str (ui/piece-summary piece)
         " on "
         (cond
           cell (:title (:card cell))
           (:space piece) (ui/wasteland-label (:space piece))
           :else "unknown space"))))

(defn- piece-choices [board pieces selected-piece-id]
  [:div.move-step
   [:div.move-step__header
    [:span "Minion"]
    [:strong
     (if-let [piece (some #(when (= selected-piece-id (:id %)) %) pieces)]
       (ui/piece-summary piece)
       "None")]]
   (if (seq pieces)
     [:div.move-choice-list
      (for [piece pieces]
        ^{:key (:id piece)}
        [:button.move-chip
         {:type "button"
          :class (when (= selected-piece-id (:id piece)) "is-selected")
          :aria-pressed (= selected-piece-id (:id piece))
          :on-click #(rf/dispatch [events/select-move-piece (:id piece)])}
         (piece-choice-label board piece)])]
     [:p.move-step__empty "No pieces available."])])

(defn- target-piece-choices [board pieces selected-piece-id]
  [:div.move-step
   [:div.move-step__header
    [:span "Target piece"]
    [:strong
     (if-let [piece (some #(when (= selected-piece-id (:id %)) %) pieces)]
       (ui/piece-summary piece)
       "None")]]
   (if (seq pieces)
     [:div.move-choice-list
      (for [piece pieces]
        ^{:key (:id piece)}
        [:button.move-chip
         {:type "button"
          :class (when (= selected-piece-id (:id piece)) "is-selected")
          :aria-pressed (= selected-piece-id (:id piece))
          :on-click #(rf/dispatch [events/select-move-target-piece (:id piece)])}
         (piece-choice-label board piece)])]
     [:p.move-step__empty "No target pieces available."])])

(defn- orientation-choices [options selected-orientation]
  [:div.move-step
   [:div.move-step__header
    [:span "Orientation"]
    [:strong (pieces/orientation-label selected-orientation)]]
   [:div.move-choice-list.is-compact
    (for [{:keys [id label]} options]
      ^{:key id}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-orientation id) "is-selected")
        :aria-pressed (= selected-orientation id)
        :on-click #(rf/dispatch [events/set-move-orientation id])}
       label])]])

(defn- draw-count-choices [options selected-count]
  [:div.move-step
   [:div.move-step__header
    [:span "Draw count"]
    [:strong (or selected-count "None")]]
   [:div.move-choice-list.is-compact
    (for [draw-count options]
      ^{:key draw-count}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-count draw-count) "is-selected")
        :aria-pressed (= selected-count draw-count)
        :on-click #(rf/dispatch [events/set-move-draw-count draw-count])}
       draw-count])]])

(defn- distance-choices [options selected-distance]
  [:div.move-step
   [:div.move-step__header
    [:span "Distance"]
    [:strong (or selected-distance "None")]]
   [:div.move-choice-list.is-compact
    (for [distance options]
      ^{:key distance}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-distance distance) "is-selected")
        :aria-pressed (= selected-distance distance)
        :on-click #(rf/dispatch [events/set-move-distance distance])}
       distance])]])

(defn- cup-move-controls
  [params board target-piece-options target-board-options target-wasteland-options
   territory-card-source-options one-point-card-options orientation-options]
  [:<>
   [target-choice-grid target-board-options
    target-wasteland-options
    (:target-board-index params)
    (:target-wasteland params)]
   [target-piece-choices board target-piece-options (:target-piece-id params)]
   (when (:target-board-index params)
     [orientation-choices orientation-options (:orientation params)])
   (when (:target-wasteland params)
     [territory-card-source-choices territory-card-source-options
      (:territory-card-source params)])
   (when (and (:target-wasteland params)
              (or (= 1 (count territory-card-source-options))
                  (= :hand (:territory-card-source params))))
     [one-point-card-choices one-point-card-options (:one-point-card-id params)])])

(defn- rod-move-controls
  [params board rod-mode-options target-piece-options target-board-options distance-options
   orientation-options orientation-required?]
  [:<>
   [rod-mode-choices rod-mode-options (:rod-mode params)]
   (case (:rod-mode params)
     :move-minion
     [:<>
      [distance-choices distance-options (:distance params)]
      (when (and orientation-required? (:distance params))
        [orientation-choices orientation-options (:orientation params)])]

     :push-piece
     [:<>
      [target-piece-choices board target-piece-options (:target-piece-id params)]
      (when (:target-piece-id params)
        [distance-choices distance-options (:distance params)])
      (when (and orientation-required? (:distance params))
        [orientation-choices orientation-options (:orientation params)])]

     :push-territory
     [:<>
      [board-choice-grid "Target territory" target-board-options (:target-board-index params)]
      (when (:target-board-index params)
        [distance-choices distance-options (:distance params)])]

     nil)])

(defn- disc-move-controls
  [params board disc-target-kind-options target-piece-options target-board-options
   replacement-source-options replacement-card-options orientation-options
   orientation-available?]
  [:<>
   [disc-target-kind-choices disc-target-kind-options (:disc-target-kind params)]
   (case (:disc-target-kind params)
     :piece
     [:<>
      [target-piece-choices board target-piece-options (:target-piece-id params)]
      (when orientation-available?
        [orientation-choices orientation-options (:orientation params)])]

     :territory
     [:<>
      [board-choice-grid "Target territory" target-board-options (:target-board-index params)]
      (when (:target-board-index params)
        [territory-card-source-choices "Replacement source"
         replacement-source-options
         (:replacement-card-source params)])
      (when (and (:target-board-index params)
                 (or (= 1 (count replacement-source-options))
                     (:replacement-card-source params)))
        [replacement-card-choices replacement-card-options (:replacement-card-id params)])]

     nil)])

(defn- move-active-controls [selection controls]
  (let [{:keys [source params]} selection
        {:keys [board power power-options rod-mode-options piece-options
                disc-target-kind-options
                target-piece-options hand-options source-board-options
                target-board-options target-wasteland-options
                territory-card-source-options one-point-card-options
                replacement-card-options orientation-options orientation-required?
                disc-orientation-available? distance-options
                draw-options]} controls]
    (case source
      :activate-territory
      [:<>
       [board-choice-grid "Source territory" source-board-options (:source-board-index params)]
       (when (:source-board-index params)
         [piece-choices board piece-options (:piece-id params)])
       (when (:piece-id params)
         [:<>
          [power-choices power-options power]
          (case power
            :rod [rod-move-controls params
                  board
                  rod-mode-options
                  target-piece-options
                  target-board-options
                  distance-options
                  orientation-options
                  orientation-required?]
            :cup [cup-move-controls params
                  board
                  target-piece-options
                  target-board-options
                  target-wasteland-options
                  territory-card-source-options
                  one-point-card-options
                  orientation-options]
            :disc [disc-move-controls params
                   board
                   disc-target-kind-options
                   target-piece-options
                   target-board-options
                   territory-card-source-options
                   replacement-card-options
                   orientation-options
                   disc-orientation-available?]
            nil)])]

      :play-hand-card
      [:<>
       [hand-card-choices hand-options (:hand-card-id params)]
       (when (:hand-card-id params)
         [piece-choices board piece-options (:piece-id params)])
       (when (:piece-id params)
         [:<>
          [power-choices power-options power]
          (case power
            :rod [rod-move-controls params
                  board
                  rod-mode-options
                  target-piece-options
                  target-board-options
                  distance-options
                  orientation-options
                  orientation-required?]
            :cup [cup-move-controls params
                  board
                  target-piece-options
                  target-board-options
                  target-wasteland-options
                  territory-card-source-options
                  one-point-card-options
                  orientation-options]
            :disc [disc-move-controls params
                   board
                   disc-target-kind-options
                   target-piece-options
                   target-board-options
                   territory-card-source-options
                   replacement-card-options
                   orientation-options
                   disc-orientation-available?]
            nil)])]

      :draw-cards
      [draw-count-choices draw-options (:draw-count params)]

      :orient-piece
      [:<>
       [piece-choices board piece-options (:piece-id params)]
       (when (:piece-id params)
         [orientation-choices orientation-options (:orientation params)])]

      :place-initial-small
      [:<>
       [target-choice-grid target-board-options
        target-wasteland-options
        (:target-board-index params)
        (:target-wasteland params)]
       (when (or (:target-board-index params)
                 (:target-wasteland params))
         [orientation-choices orientation-options (:orientation params)])]

      nil)))

(defn move-panel []
  (let [{:keys [current-player selection source-options prompt ready? controls]}
        @(rf/subscribe [events/move-panel-view])
        {:keys [source error]} selection]
    [:section.move-panel
     {:id "move-panel"
      :class (if source "is-active" "is-idle")}
     [:div.move-panel__heading
      [:p.eyebrow "Move"]
      [:div.panel-heading-actions
       [:h2 (if current-player
              (:name current-player)
              "No player")]
       [:button.panel-close
        {:type "button"
         :aria-label "Close move panel"
         :on-click #(rf/dispatch [events/set-panel-open :move false])}
        "Close"]]]
     [move-source-picker source-options source]
     [:p.move-panel__prompt prompt]
     (when source
       [move-active-controls selection controls])
     (when error
       [:p.move-error
        {:role "alert"}
        (:message error)])
     (when source
       [:div.move-actions
        [:button.move-action
         {:type "button"
          :on-click #(rf/dispatch [events/cancel-move])}
         "Cancel"]
        [:button.move-action.is-primary
         {:type "button"
          :disabled (not ready?)
          :on-click #(rf/dispatch [events/confirm-move])}
         "Confirm"]])]))
