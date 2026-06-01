(ns gnostica.ui.move-panel.choices
  (:require [gnostica.app.events :as events]
            [gnostica.board-layout :as layout]
            [gnostica.legal-targets :as legal-targets]
            [gnostica.pieces :as pieces]
            [gnostica.ui.card :as card-ui]
            [gnostica.ui.common :as ui]
            [re-frame.core :as rf]))

(defn- board-cell-label [{:keys [row col card]}]
  (str (:title card)
       ", row "
       (inc row)
       ", column "
       (inc col)))

(defn- territory-choice-descriptors [cells descriptors]
  (if (legal-targets/any-active? descriptors)
    descriptors
    (mapv (fn [cell]
            {:cell cell
             :board-index (:index cell)
             :enabled? true
             :status :legal})
          cells)))

(defn- wasteland-choice-descriptors [wastelands descriptors]
  (if (legal-targets/any-active? descriptors)
    descriptors
    (mapv (fn [space]
            {:space space
             :row (:row space)
             :col (:col space)
             :enabled? true
             :status :legal})
          wastelands)))

(defn board-choice-grid
  ([label cells selected-index]
   (board-choice-grid label cells selected-index nil))
  ([label cells selected-index territory-descriptors]
   [:div.move-step
    [:div.move-step__header
     [:span label]
     [:strong
      (if-let [selected-cell (some #(when (= selected-index (:index %)) %) cells)]
        (:title (:card selected-cell))
        "None")]]
    [:div.move-board-choice-grid
     (for [{:keys [cell board-index enabled?] :as descriptor}
           (territory-choice-descriptors cells territory-descriptors)
           :let [cell (or cell (layout/cell-by-index cells board-index))
                 selected? (= selected-index (:index cell))]]
       ^{:key (:index cell)}
       [:button.move-chip
        {:type "button"
         :class (str (when selected? "is-selected")
                     (legal-targets/status-class descriptor))
         :disabled (false? enabled?)
         :title (legal-targets/reason descriptor)
         :aria-pressed selected?
         :on-click #(rf/dispatch [events/select-board-card (:index cell)])}
        (board-cell-label cell)])]]))

(defn world-copy-choices
  ([cells selected-index]
   (world-copy-choices cells selected-index nil))
  ([cells selected-index territory-descriptors]
   [:div.move-step
    [:div.move-step__header
     [:span "World copy"]
     [:strong
      (if-let [selected-cell (some #(when (= selected-index (:index %)) %) cells)]
        (:title (:card selected-cell))
        "None")]]
    (if (seq (or cells territory-descriptors))
      [:div.move-board-choice-grid
       (for [{:keys [cell board-index enabled?] :as descriptor}
             (territory-choice-descriptors cells territory-descriptors)
             :let [cell (or cell (layout/cell-by-index cells board-index))
                   selected? (= selected-index (:index cell))]]
         ^{:key (:index cell)}
         [:button.move-chip
          {:type "button"
           :class (str (when selected? "is-selected")
                       (legal-targets/status-class descriptor))
           :disabled (false? enabled?)
           :title (legal-targets/reason descriptor)
           :aria-pressed selected?
           :on-click #(rf/dispatch [events/select-move-world-copy (:index cell)])}
          (board-cell-label cell)])]
      [:p.move-step__empty "No major territories available."])]))

(defn- same-wasteland? [selected-space space]
  (and selected-space
       (= (:row selected-space) (:row space))
       (= (:col selected-space) (:col space))))

(defn target-choice-grid
  ([cells wastelands selected-index selected-wasteland]
   (target-choice-grid "Target" cells wastelands selected-index selected-wasteland nil))
  ([cells wastelands selected-index selected-wasteland legal-targets]
   (target-choice-grid "Target" cells wastelands selected-index selected-wasteland legal-targets))
  ([label cells wastelands selected-index selected-wasteland legal-targets]
   [:div.move-step
    [:div.move-step__header
     [:span label]
     [:strong
      (if-let [selected-cell (some #(when (= selected-index (:index %)) %) cells)]
        (:title (:card selected-cell))
        (if selected-wasteland
          (ui/wasteland-label selected-wasteland)
          "None"))]]
    [:div.move-board-choice-grid
     (for [{:keys [cell board-index enabled?] :as descriptor}
           (territory-choice-descriptors cells (:territories legal-targets))
           :let [cell (or cell (layout/cell-by-index cells board-index))
                 selected? (= selected-index (:index cell))]]
       ^{:key (str "territory-" (:index cell))}
       [:button.move-chip
        {:type "button"
         :class (str (when selected? "is-selected")
                     (legal-targets/status-class descriptor))
         :disabled (false? enabled?)
         :title (legal-targets/reason descriptor)
         :aria-pressed selected?
         :on-click #(rf/dispatch [events/select-board-card (:index cell)])}
        (str "Territory: " (board-cell-label cell))])
     (for [{:keys [space row col enabled?] :as descriptor}
           (wasteland-choice-descriptors wastelands (:wastelands legal-targets))
           :let [space (or space {:row row :col col})
                 selected? (same-wasteland? selected-wasteland space)]]
       ^{:key (:id space)}
       [:button.move-chip
        {:type "button"
         :class (str (when selected? "is-selected")
                     (legal-targets/status-class descriptor))
         :disabled (false? enabled?)
         :title (legal-targets/reason descriptor)
         :aria-pressed selected?
         :on-click #(rf/dispatch [events/select-move-wasteland-target (:row space) (:col space)])}
        (ui/wasteland-label space)])]]))

(defn hand-card-choices [cards selected-card-id]
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

(defn one-point-card-choices [cards selected-card-id]
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

(defn replacement-card-choices [cards selected-card-id]
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

(defn discard-card-choices [cards selected-card-ids]
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

(defn territory-card-source-choices
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

(defn power-choices [options selected-power]
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

(defn rod-mode-choices [options selected-mode]
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

(defn target-kind-choices [label event-id options selected-kind]
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

(defn disc-target-kind-choices [options selected-kind]
  [target-kind-choices "Disc move" events/select-move-disc-target-kind options selected-kind])

(defn sword-target-kind-choices [options selected-kind]
  [target-kind-choices "Sword move" events/select-move-sword-target-kind options selected-kind])

(defn piece-choice-label [board piece]
  (let [cell (layout/cell-by-index board (:space-index piece))]
    (str (ui/piece-summary piece)
         " on "
         (cond
           cell (:title (:card cell))
           (:space piece) (ui/wasteland-label (:space piece))
           :else "unknown space"))))

(defn piece-choices [board pieces selected-piece-id]
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

(defn target-piece-choices
  ([board pieces selected-piece-id]
   (target-piece-choices "Target piece" board pieces selected-piece-id))
  ([label board pieces selected-piece-id]
   [:div.move-step
    [:div.move-step__header
     [:span label]
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
      [:p.move-step__empty "No target pieces available."])]))

(defn target-piece-summary [label board target-piece]
  (let [piece (:piece target-piece)]
    [:div.move-step
     [:div.move-step__header
      [:span label]
      [:strong
       (if piece
         (ui/piece-summary piece)
         "None")]]
     [:p.move-step__summary
      (if piece
        (piece-choice-label board piece)
        "No target piece selected.")]]))

(defn selected-orientation-label [orientation]
  (if (some? orientation)
    (pieces/orientation-label orientation)
    "None"))

(defn orientation-choices
  ([options selected-orientation]
   (orientation-choices "Orientation" events/set-move-orientation options selected-orientation))
  ([label event-id options selected-orientation]
   [:div.move-step
    [:div.move-step__header
     [:span label]
     [:strong (selected-orientation-label selected-orientation)]]
    [:div.move-choice-list.is-compact
     (for [{:keys [id label]} options]
       ^{:key id}
       [:button.move-chip
        {:type "button"
         :class (when (= selected-orientation id) "is-selected")
         :aria-pressed (= selected-orientation id)
         :on-click #(rf/dispatch [event-id id])}
        label])]]))

(defn disc-action-count-choices [options selected-count]
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

(defn sword-action-count-choices [options selected-count]
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

(defn major-action-count-choices [options selected-count]
  [:div.move-step
   [:div.move-step__header
    [:span "Major actions"]
    [:strong (or (some #(when (= selected-count (:id %)) (:label %)) options)
                 "None")]]
   [:div.move-choice-list.is-compact
    (for [{:keys [id label]} options]
      ^{:key id}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-count id) "is-selected")
        :aria-pressed (= selected-count id)
        :on-click #(rf/dispatch [events/set-move-major-action-count id])}
       label])]])

(defn devil-action-count-choices [options selected-count]
  [:div.move-step
   [:div.move-step__header
    [:span "Devil orientations"]
    [:strong (or selected-count "None")]]
   [:div.move-choice-list.is-compact
    (for [action-count options]
      ^{:key action-count}
      [:button.move-chip
       {:type "button"
        :class (when (= selected-count action-count) "is-selected")
        :aria-pressed (= selected-count action-count)
        :on-click #(rf/dispatch [events/set-move-devil-action-count action-count])}
       action-count])]])

(defn sun-disc-mode-choices [options selected-mode]
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

(defn draw-count-choices [options selected-count]
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

(defn count-choices [label event-id options selected-count]
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

(defn fool-reveal-count-choices [options selected-count]
  [count-choices "Reveals" events/set-move-fool-reveal-count options selected-count])

(defn fool-reveal-card-control [{:keys [completed-count can-reveal?]}]
  [:div.move-step
   [:div.move-step__header
    [:span (str "Reveal " (inc (or completed-count 0)))]
    [:strong "Next card"]]
   [:div.move-choice-list.is-compact
    [:button.move-chip
     {:type "button"
      :disabled (not can-reveal?)
      :on-click #(rf/dispatch [events/reveal-move-fool-card])}
     "Reveal"]]])

(defn fool-reveal-decision-control [{:keys [active-card active-reveal]}]
  [:div.move-step
   [:div.move-step__header
    [:span (str "Reveal " (:index active-reveal))]
    [:strong (:title active-card)]]
   (when active-card
     [:div.fool-reveal-card
      [card-ui/card-face active-card "fool-reveal-card__face" :always]
      [:div.fool-reveal-card__meta
       [:strong (:title active-card)]
       [:span (name (:arcana active-card))]]])
   [:div.move-choice-list.is-compact
    [:button.move-chip
     {:type "button"
      :class (when (= :skip (:choice active-reveal)) "is-selected")
      :aria-pressed (= :skip (:choice active-reveal))
      :on-click #(rf/dispatch [events/skip-move-fool-reveal])}
     "Skip"]
    [:button.move-chip
     {:type "button"
      :class (when (= :play (:choice active-reveal)) "is-selected")
      :aria-pressed (= :play (:choice active-reveal))
      :on-click #(rf/dispatch [events/play-move-fool-reveal])}
     "Play"]]])

(defn fool-play-power-choices [options selected-power]
  (let [selected-option (some #(when (= selected-power (:id %)) %) options)]
    [:div.move-step
     [:div.move-step__header
      [:span "Revealed power"]
      [:strong (or (:label selected-option) "None")]]
     [:div.move-choice-list.is-compact
      (for [{:keys [id label]} options]
        ^{:key id}
        [:button.move-chip
         {:type "button"
          :class (when (= selected-power id) "is-selected")
          :aria-pressed (= selected-power id)
          :on-click #(rf/dispatch [events/select-move-fool-play-power id])}
         label])]]))

(defn high-priestess-redraw-count-choices [options selected-count]
  [count-choices "Redraw passes" events/set-move-high-priestess-redraw-count options selected-count])

(defn high-priestess-pass-controls
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

(defn high-priestess-redraw-controls [redraw-options]
  [:<>
   (for [redraw-option redraw-options]
     ^{:key (:pass-index redraw-option)}
     [high-priestess-pass-controls redraw-option])])

(defn judgement-card-choices [cards selected-card-ids maximum]
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

(defn distance-choices [options selected-distance]
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

(defn damage-choices [options selected-damage]
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
