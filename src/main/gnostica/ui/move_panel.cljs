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

(defn- world-copy-choices [cells selected-index]
  [:div.move-step
   [:div.move-step__header
    [:span "World copy"]
    [:strong
     (if-let [selected-cell (some #(when (= selected-index (:index %)) %) cells)]
       (:title (:card selected-cell))
       "None")]]
   (if (seq cells)
     [:div.move-board-choice-grid
      (for [cell cells]
        ^{:key (:index cell)}
        [:button.move-chip
         {:type "button"
          :class (when (= selected-index (:index cell)) "is-selected")
          :aria-pressed (= selected-index (:index cell))
          :on-click #(rf/dispatch [events/select-move-world-copy (:index cell)])}
         (board-cell-label cell)])]
     [:p.move-step__empty "No major territories available."])])

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

(defn- discard-card-choices [cards selected-card-ids]
  (let [selected-card-ids (set selected-card-ids)
        selected-count (count selected-card-ids)]
    [:div.move-step
     [:div.move-step__header
      [:span "Discard"]
      [:strong (if (pos? selected-count)
                 (ui/card-count-label selected-count)
                 "None")]]
     (if (seq cards)
       [:div.move-discard-list
        (for [card cards
              :let [selected? (contains? selected-card-ids (:id card))]]
          ^{:key (:id card)}
          [:label.move-discard-choice
           {:class (when selected? "is-selected")}
           [:input
            {:type "checkbox"
             :checked selected?
             :on-change #(rf/dispatch [events/toggle-move-discard-card (:id card)])}]
           [:span (:title card)]])]
       [:p.move-step__empty "No hand cards available."])]))

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

(defn- target-kind-choices [label event-id options selected-kind]
  [:div.move-step
   [:div.move-step__header
    [:span label]
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
        :on-click #(rf/dispatch [event-id id])}
       label])]])

(defn- disc-target-kind-choices [options selected-kind]
  [target-kind-choices "Disc move" events/select-move-disc-target-kind options selected-kind])

(defn- sword-target-kind-choices [options selected-kind]
  [target-kind-choices "Sword move" events/select-move-sword-target-kind options selected-kind])

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

(defn- orientation-choices
  ([options selected-orientation]
   (orientation-choices "Orientation" events/set-move-orientation options selected-orientation))
  ([label event-id options selected-orientation]
   [:div.move-step
    [:div.move-step__header
     [:span label]
     [:strong (pieces/orientation-label selected-orientation)]]
    [:div.move-choice-list.is-compact
     (for [{:keys [id label]} options]
       ^{:key id}
       [:button.move-chip
        {:type "button"
         :class (when (= selected-orientation id) "is-selected")
         :aria-pressed (= selected-orientation id)
         :on-click #(rf/dispatch [event-id id])}
        label])]]))

(defn- disc-action-count-choices [options selected-count]
  [:div.move-step
   [:div.move-step__header
    [:span "Disc actions"]
    [:strong (or selected-count "None")]]
   [:div.move-choice-list.is-compact
    (for [action-count options]
      ^{:key action-count}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-count action-count) "is-selected")
        :aria-pressed (= selected-count action-count)
        :on-click #(rf/dispatch [events/set-move-disc-action-count action-count])}
       action-count])]])

(defn- sword-action-count-choices [options selected-count]
  [:div.move-step
   [:div.move-step__header
    [:span "Sword actions"]
    [:strong (or selected-count "None")]]
   [:div.move-choice-list.is-compact
    (for [action-count options]
      ^{:key action-count}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-count action-count) "is-selected")
        :aria-pressed (= selected-count action-count)
        :on-click #(rf/dispatch [events/set-move-sword-action-count action-count])}
       action-count])]])

(defn- sun-disc-mode-choices [options selected-mode]
  [:div.move-step
   [:div.move-step__header
    [:span "Sun Disc"]
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
        :on-click #(rf/dispatch [events/select-move-sun-disc-mode id])}
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

(defn- count-choices [label event-id options selected-count]
  [:div.move-step
   [:div.move-step__header
    [:span label]
    [:strong (or selected-count "None")]]
   [:div.move-choice-list.is-compact
    (for [option options]
      ^{:key option}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-count option) "is-selected")
        :aria-pressed (= selected-count option)
        :on-click #(rf/dispatch [event-id option])}
       option])]])

(defn- fool-reveal-count-choices [options selected-count]
  [count-choices "Reveals" events/set-move-fool-reveal-count options selected-count])

(defn- high-priestess-redraw-count-choices [options selected-count]
  [count-choices "Redraw passes" events/set-move-high-priestess-redraw-count options selected-count])

(defn- high-priestess-pass-controls
  [{:keys [pass-index discard-card-options selected-discard-card-ids
           draw-count-options selected-draw-count]}]
  (let [selected-discard-card-ids (set selected-discard-card-ids)
        selected-count (count selected-discard-card-ids)]
    [:div.move-step
     [:div.move-step__header
      [:span (str "Redraw " pass-index)]
      [:strong (if (pos? selected-count)
                 (ui/card-count-label selected-count)
                 "No discards")]]
     (if (seq discard-card-options)
       [:div.move-discard-list
        (for [card discard-card-options
              :let [selected? (contains? selected-discard-card-ids (:id card))]]
          ^{:key (str "hp-" pass-index "-" (:id card))}
          [:label.move-discard-choice
           {:class (when selected? "is-selected")}
           [:input
            {:type "checkbox"
             :checked selected?
             :on-change #(rf/dispatch [events/toggle-move-high-priestess-discard-card
                                        pass-index
                                        (:id card)])}]
           [:span (:title card)]])]
       [:p.move-step__empty "No hand cards available."])
     [:div.move-choice-list.is-compact
      (for [draw-count draw-count-options]
        ^{:key (str "hp-draw-" pass-index "-" draw-count)}
        [:button.move-chip
         {:type "button"
          :class (when (= selected-draw-count draw-count) "is-selected")
          :aria-pressed (= selected-draw-count draw-count)
          :on-click #(rf/dispatch [events/set-move-high-priestess-draw-count
                                   pass-index
                                   draw-count])}
         draw-count])]]))

(defn- high-priestess-redraw-controls [redraw-options]
  [:<>
   (for [redraw-option redraw-options]
     ^{:key (:pass-index redraw-option)}
     [high-priestess-pass-controls redraw-option])])

(defn- judgement-card-choices [cards selected-card-ids maximum]
  (let [selected-card-ids (set selected-card-ids)
        selected-count (count selected-card-ids)]
    [:div.move-step
     [:div.move-step__header
      [:span "Judgement cards"]
      [:strong (str selected-count "/" maximum)]]
     (if (seq cards)
       [:div.move-discard-list
        (for [card cards
              :let [selected? (contains? selected-card-ids (:id card))
                    limit-reached? (and (not selected?)
                                        (<= maximum selected-count))]]
          ^{:key (str "judgement-" (:id card))}
          [:label.move-discard-choice
           {:class (cond
                     selected? "is-selected"
                     limit-reached? "is-disabled")}
           [:input
            {:type "checkbox"
             :checked selected?
             :disabled limit-reached?
             :on-change #(rf/dispatch [events/toggle-move-judgement-card (:id card)])}]
           [:span (:title card)]])]
       [:p.move-step__empty "No discard-pile cards available."])]))

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

(defn- damage-choices [options selected-damage]
  [:div.move-step
   [:div.move-step__header
    [:span "Damage"]
    [:strong (or selected-damage "None")]]
   [:div.move-choice-list.is-compact
    (for [damage options]
      ^{:key damage}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-damage damage) "is-selected")
        :aria-pressed (= selected-damage damage)
        :on-click #(rf/dispatch [events/set-move-damage damage])}
       damage])]])

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

(defn- sun-move-controls
  [params board sun-disc-mode-options target-piece-options target-board-options
   target-wasteland-options one-point-card-options replacement-card-options
   orientation-options sun-disc-orientation-available?]
  (let [cup-target-ready? (or (:target-piece-id params)
                              (:target-wasteland params)
                              (and (some? (:target-board-index params))
                                   (:orientation params)))
        normal-wasteland-cup? (and (:target-wasteland params)
                                   (:sun-disc-mode params)
                                   (not= :created-territory
                                         (:sun-disc-mode params)))]
    [:<>
     [target-choice-grid board
      target-wasteland-options
      (:target-board-index params)
      (:target-wasteland params)]
     (when (and (not (:sun-disc-mode params))
                (not (:target-board-index params))
                (not (:target-wasteland params)))
       [target-piece-choices board target-piece-options (:target-piece-id params)])
     (when (:target-board-index params)
       [orientation-choices orientation-options (:orientation params)])
     (when cup-target-ready?
       [sun-disc-mode-choices sun-disc-mode-options (:sun-disc-mode params)])
     (when normal-wasteland-cup?
       [one-point-card-choices one-point-card-options (:one-point-card-id params)])
     (case (:sun-disc-mode params)
       :created-piece
       nil

       :created-territory
       [replacement-card-choices replacement-card-options
       (:sun-disc-replacement-card-id params)]

       :piece
       [:<>
        [target-piece-choices board
         target-piece-options
         (:sun-disc-target-piece-id params)]
        (when (and (:sun-disc-target-piece-id params)
                   sun-disc-orientation-available?)
          [orientation-choices "Disc orientation"
           events/set-move-sun-disc-orientation
           orientation-options
           (:sun-disc-orientation params)])]

       :territory
       [:<>
        [board-choice-grid "Disc target territory"
         target-board-options
         (:sun-disc-target-board-index params)]
        (when (:sun-disc-target-board-index params)
          [replacement-card-choices replacement-card-options
           (:sun-disc-replacement-card-id params)])]

       nil)]))

(defn- disc-move-controls
  [params board disc-action-count-options disc-minion-orientation-required?
   disc-target-kind-options target-piece-options target-board-options
   replacement-source-options replacement-card-options orientation-options
   orientation-available?]
  (let [action-count-ready? (or (empty? disc-action-count-options)
                                (:disc-action-count params))
        minion-orientation-ready? (or (not disc-minion-orientation-required?)
                                      (:minion-orientation params))
        target-ready? (and action-count-ready? minion-orientation-ready?)]
    [:<>
     (when (seq disc-action-count-options)
       [disc-action-count-choices disc-action-count-options (:disc-action-count params)])
     (when disc-minion-orientation-required?
       [orientation-choices "Minion orientation"
        events/set-move-minion-orientation
        orientation-options
        (:minion-orientation params)])
     (when target-ready?
       [disc-target-kind-choices disc-target-kind-options (:disc-target-kind params)])
     (when target-ready?
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

         nil))]))

(defn- sword-move-controls
  [params board sword-target-kind-options target-piece-options target-board-options
   replacement-source-options replacement-card-options orientation-options
   orientation-available? damage-options]
  [:<>
   [sword-target-kind-choices sword-target-kind-options (:sword-target-kind params)]
   (case (:sword-target-kind params)
     :piece
     [:<>
      [target-piece-choices board target-piece-options (:target-piece-id params)]
      (when (:target-piece-id params)
        [damage-choices damage-options (:damage params)])
      (when (and orientation-available? (:damage params))
        [orientation-choices orientation-options (:orientation params)])]

     :territory
     [:<>
      [board-choice-grid "Target territory" target-board-options (:target-board-index params)]
      (when (:target-board-index params)
        [damage-choices damage-options (:damage params)])
      (when (and (:target-board-index params)
                 (:damage params))
        [territory-card-source-choices "Replacement source"
         replacement-source-options
         (:replacement-card-source params)])
      (when (and (:target-board-index params)
                 (:damage params)
                 (or (= 1 (count replacement-source-options))
                     (:replacement-card-source params)))
        [replacement-card-choices replacement-card-options (:replacement-card-id params)])]

     nil)])

(defn- piece-orientation-major-controls
  [label params board target-piece-options orientation-options]
  [:<>
   [target-piece-choices board target-piece-options (:target-piece-id params)]
   (when (:target-piece-id params)
     [orientation-choices label orientation-options (:orientation params)])])

(defn- hermit-move-controls
  [params board target-piece-options target-board-options target-wasteland-options
   orientation-options orientation-required?]
  (let [target-selected? (or (:target-piece-id params)
                             (some? (:target-board-index params)))]
    [:<>
     (if target-selected?
       [:div.move-step
        [:div.move-step__header
         [:span "Hermit target"]
         [:strong
          (if-let [piece (some #(when (= (:target-piece-id params) (:id %)) %)
                               (concat target-piece-options []))]
            (piece-choice-label board piece)
            (if-let [cell (some #(when (= (:target-board-index params) (:index %)) %)
                                board)]
              (:title (:card cell))
              "Selected"))]]]
       [:<>
        [target-choice-grid "Hermit target" target-board-options [] (:target-board-index params) nil]
        [target-piece-choices board target-piece-options (:target-piece-id params)]])
     (when target-selected?
       [target-choice-grid "Destination"
        target-board-options
        target-wasteland-options
        (:hermit-destination-board-index params)
        (:hermit-destination-wasteland params)])
     (when (and orientation-required?
                (or (:hermit-destination-board-index params)
                    (:hermit-destination-wasteland params)))
       [orientation-choices orientation-options (:orientation params)])]))

(defn- composite-active-action-power [power params]
  (case power
    :empress (if (zero? (count (:major-actions params))) :orient-minion :cup)
    :emperor (if (zero? (count (:major-actions params))) :orient-minion :rod)
    :lovers (if (zero? (count (:major-actions params))) :rod :cup)
    :chariot :rod
    :hanged-man (if (zero? (count (:major-actions params))) :rod :trade-hand)
    :temperance :cup
    nil))

(defn- sword-major-active-action-power [power params]
  (case power
    :justice (if (zero? (count (:major-actions params))) :trade-hand :sword)
    :death :sword
    :tower (if (zero? (count (:major-actions params))) :orient-minion :sword)
    :moon (if (zero? (count (:major-actions params))) :rod :sword)
    nil))

(defn- composite-major-controls
  [power params board rod-mode-options target-piece-options target-board-options
   target-wasteland-options distance-options territory-card-source-options
   one-point-card-options orientation-options orientation-required?]
  (case (composite-active-action-power power params)
    :orient-minion
    [orientation-choices "Minion orientation"
     events/set-move-minion-orientation
     orientation-options
     (:minion-orientation params)]

    :rod
    [rod-move-controls params
     board
     rod-mode-options
     target-piece-options
     target-board-options
     distance-options
     orientation-options
     orientation-required?]

    :cup
    [cup-move-controls params
     board
     target-piece-options
     target-board-options
     target-wasteland-options
     territory-card-source-options
     one-point-card-options
     orientation-options]

    :trade-hand
    [target-piece-choices board target-piece-options (:target-piece-id params)]

    nil))

(defn- sword-major-controls
  [power params board rod-mode-options sword-action-count-options sword-target-kind-options
   target-piece-options target-board-options distance-options replacement-source-options
   replacement-card-options orientation-options orientation-required? sword-orientation-available?
   damage-options]
  [:<>
   (when (= :death power)
     [sword-action-count-choices sword-action-count-options (:sword-action-count params)])
   (when (or (not= :death power)
             (:sword-action-count params))
     (case (sword-major-active-action-power power params)
       :trade-hand
       [target-piece-choices board target-piece-options (:target-piece-id params)]

       :orient-minion
       [orientation-choices "Minion orientation"
        events/set-move-minion-orientation
        orientation-options
        (:minion-orientation params)]

       :rod
       [rod-move-controls params
        board
        rod-mode-options
        target-piece-options
        target-board-options
        distance-options
        orientation-options
        orientation-required?]

       :sword
       [sword-move-controls params
        board
        sword-target-kind-options
        target-piece-options
        target-board-options
        replacement-source-options
        replacement-card-options
        orientation-options
        sword-orientation-available?
        damage-options]

       nil))])

(defn- world-move-controls
  [params board world-copy-options copied-power-options copied-power
   rod-mode-options disc-action-count-options sword-action-count-options
   sun-disc-mode-options fool-reveal-count-options
   high-priestess-redraw-count-options high-priestess-redraw-options
   judgement-card-options judgement-card-maximum
   disc-minion-orientation-required?
   disc-target-kind-options sword-target-kind-options target-piece-options
   target-board-options target-wasteland-options territory-card-source-options
   one-point-card-options replacement-card-options orientation-options
   orientation-required? disc-orientation-available? sun-disc-orientation-available?
   sword-orientation-available? distance-options damage-options]
  [:<>
   [world-copy-choices world-copy-options (:copied-board-index params)]
   (when (:copied-board-index params)
     [power-choices copied-power-options copied-power])
   (when copied-power
     (case copied-power
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
              disc-action-count-options
              disc-minion-orientation-required?
              disc-target-kind-options
              target-piece-options
              target-board-options
              territory-card-source-options
              replacement-card-options
              orientation-options
              disc-orientation-available?]
       :sun [sun-move-controls params
             board
             sun-disc-mode-options
             target-piece-options
             target-board-options
             target-wasteland-options
             one-point-card-options
             replacement-card-options
             orientation-options
             sun-disc-orientation-available?]
       :sword [sword-move-controls params
               board
               sword-target-kind-options
               target-piece-options
               target-board-options
               territory-card-source-options
               replacement-card-options
               orientation-options
               sword-orientation-available?
               damage-options]
       :fool [fool-reveal-count-choices
              fool-reveal-count-options
              (:fool-reveal-count params)]
       :high-priestess [:<>
                        [high-priestess-redraw-count-choices
                         high-priestess-redraw-count-options
                         (:high-priestess-redraw-count params)]
                        [high-priestess-redraw-controls
                         high-priestess-redraw-options]]
       :judgement [judgement-card-choices
                   judgement-card-options
                   (:judgement-card-ids params)
                   judgement-card-maximum]
       :hierophant [piece-orientation-major-controls
                    "Replacement orientation"
                    params
                    board
                    target-piece-options
                    orientation-options]
       :hermit [hermit-move-controls params
                board
                target-piece-options
                target-board-options
                target-wasteland-options
                orientation-options
                orientation-required?]
       :devil [piece-orientation-major-controls
               "Target orientation"
               params
               board
               target-piece-options
               orientation-options]
       (:empress :emperor :lovers :chariot :hanged-man :temperance)
       [composite-major-controls
        copied-power
        params
        board
        rod-mode-options
        target-piece-options
        target-board-options
        target-wasteland-options
        distance-options
        territory-card-source-options
        one-point-card-options
        orientation-options
        orientation-required?]
       (:justice :death :tower :moon)
       [sword-major-controls
        copied-power
        params
        board
        rod-mode-options
        sword-action-count-options
        sword-target-kind-options
        target-piece-options
        target-board-options
        distance-options
        territory-card-source-options
        replacement-card-options
        orientation-options
        orientation-required?
        sword-orientation-available?
        damage-options]
       nil))])

(defn- move-active-controls [selection controls]
  (let [{:keys [source params]} selection
        {:keys [board power power-options rod-mode-options piece-options
                world-copy-options world-copied-power-options world-copied-power
                disc-action-count-options sword-action-count-options sun-disc-mode-options
                fool-reveal-count-options high-priestess-redraw-count-options
                high-priestess-redraw-options judgement-card-options
                judgement-card-maximum
                disc-minion-orientation-required?
                disc-target-kind-options sword-target-kind-options
                target-piece-options hand-options discard-card-options source-board-options
                target-board-options target-wasteland-options
                territory-card-source-options one-point-card-options
                replacement-card-options orientation-options orientation-required?
                disc-orientation-available? sun-disc-orientation-available?
                sword-orientation-available?
                distance-options damage-options draw-options]} controls]
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
                   disc-action-count-options
                   disc-minion-orientation-required?
                   disc-target-kind-options
                   target-piece-options
                   target-board-options
                   territory-card-source-options
                   replacement-card-options
                   orientation-options
                   disc-orientation-available?]
            :sun [sun-move-controls params
                  board
                  sun-disc-mode-options
                  target-piece-options
                  target-board-options
                  target-wasteland-options
                  one-point-card-options
                  replacement-card-options
                  orientation-options
                  sun-disc-orientation-available?]
            :sword [sword-move-controls params
                    board
                    sword-target-kind-options
                    target-piece-options
                    target-board-options
                    territory-card-source-options
                    replacement-card-options
                    orientation-options
                    sword-orientation-available?
                    damage-options]
            :fool [fool-reveal-count-choices
                   fool-reveal-count-options
                   (:fool-reveal-count params)]
            :high-priestess [:<>
                             [high-priestess-redraw-count-choices
                              high-priestess-redraw-count-options
                              (:high-priestess-redraw-count params)]
                             [high-priestess-redraw-controls
                              high-priestess-redraw-options]]
            :judgement [judgement-card-choices
                        judgement-card-options
                        (:judgement-card-ids params)
                        judgement-card-maximum]
            :hierophant [piece-orientation-major-controls
                         "Replacement orientation"
                         params
                         board
                         target-piece-options
                         orientation-options]
            :hermit [hermit-move-controls params
                     board
                     target-piece-options
                     target-board-options
                     target-wasteland-options
                     orientation-options
                     orientation-required?]
            :devil [piece-orientation-major-controls
                    "Target orientation"
                    params
                    board
                    target-piece-options
                    orientation-options]
            :world [world-move-controls
                    params
                    board
                    world-copy-options
                    world-copied-power-options
                    world-copied-power
                    rod-mode-options
                    disc-action-count-options
                    sword-action-count-options
                    sun-disc-mode-options
                    fool-reveal-count-options
                    high-priestess-redraw-count-options
                    high-priestess-redraw-options
                    judgement-card-options
                    judgement-card-maximum
                    disc-minion-orientation-required?
                    disc-target-kind-options
                    sword-target-kind-options
                    target-piece-options
                    target-board-options
                    target-wasteland-options
                    territory-card-source-options
                    one-point-card-options
                    replacement-card-options
                    orientation-options
                    orientation-required?
                    disc-orientation-available?
                    sun-disc-orientation-available?
                    sword-orientation-available?
                    distance-options
                    damage-options]
            (:empress :emperor :lovers :chariot :hanged-man :temperance)
            [composite-major-controls
             power
             params
             board
             rod-mode-options
             target-piece-options
             target-board-options
             target-wasteland-options
             distance-options
             territory-card-source-options
             one-point-card-options
             orientation-options
             orientation-required?]
            (:justice :death :tower :moon)
            [sword-major-controls
             power
             params
             board
             rod-mode-options
             sword-action-count-options
             sword-target-kind-options
             target-piece-options
             target-board-options
             distance-options
             territory-card-source-options
             replacement-card-options
             orientation-options
             orientation-required?
             sword-orientation-available?
             damage-options]
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
                   disc-action-count-options
                   disc-minion-orientation-required?
                   disc-target-kind-options
                   target-piece-options
                   target-board-options
                   territory-card-source-options
                   replacement-card-options
                   orientation-options
                   disc-orientation-available?]
            :sun [sun-move-controls params
                  board
                  sun-disc-mode-options
                  target-piece-options
                  target-board-options
                  target-wasteland-options
                  one-point-card-options
                  replacement-card-options
                  orientation-options
                  sun-disc-orientation-available?]
            :sword [sword-move-controls params
                    board
                    sword-target-kind-options
                    target-piece-options
                    target-board-options
                    territory-card-source-options
                    replacement-card-options
                    orientation-options
                    sword-orientation-available?
                    damage-options]
            :fool [fool-reveal-count-choices
                   fool-reveal-count-options
                   (:fool-reveal-count params)]
            :high-priestess [:<>
                             [high-priestess-redraw-count-choices
                              high-priestess-redraw-count-options
                              (:high-priestess-redraw-count params)]
                             [high-priestess-redraw-controls
                              high-priestess-redraw-options]]
            :judgement [judgement-card-choices
                        judgement-card-options
                        (:judgement-card-ids params)
                        judgement-card-maximum]
            :hierophant [piece-orientation-major-controls
                         "Replacement orientation"
                         params
                         board
                         target-piece-options
                         orientation-options]
            :hermit [hermit-move-controls params
                     board
                     target-piece-options
                     target-board-options
                     target-wasteland-options
                     orientation-options
                     orientation-required?]
            :devil [piece-orientation-major-controls
                    "Target orientation"
                    params
                    board
                    target-piece-options
                    orientation-options]
            :world [world-move-controls
                    params
                    board
                    world-copy-options
                    world-copied-power-options
                    world-copied-power
                    rod-mode-options
                    disc-action-count-options
                    sword-action-count-options
                    sun-disc-mode-options
                    fool-reveal-count-options
                    high-priestess-redraw-count-options
                    high-priestess-redraw-options
                    judgement-card-options
                    judgement-card-maximum
                    disc-minion-orientation-required?
                    disc-target-kind-options
                    sword-target-kind-options
                    target-piece-options
                    target-board-options
                    target-wasteland-options
                    territory-card-source-options
                    one-point-card-options
                    replacement-card-options
                    orientation-options
                    orientation-required?
                    disc-orientation-available?
                    sun-disc-orientation-available?
                    sword-orientation-available?
                    distance-options
                    damage-options]
            (:empress :emperor :lovers :chariot :hanged-man :temperance)
            [composite-major-controls
             power
             params
             board
             rod-mode-options
             target-piece-options
             target-board-options
             target-wasteland-options
             distance-options
             territory-card-source-options
             one-point-card-options
             orientation-options
             orientation-required?]
            (:justice :death :tower :moon)
            [sword-major-controls
             power
             params
             board
             rod-mode-options
             sword-action-count-options
             sword-target-kind-options
             target-piece-options
             target-board-options
             distance-options
             territory-card-source-options
             replacement-card-options
             orientation-options
             orientation-required?
             sword-orientation-available?
             damage-options]
            nil)])]

      :draw-cards
      [:<>
       [discard-card-choices discard-card-options (:discard-card-ids params)]
       [draw-count-choices draw-options (:draw-count params)]]

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
