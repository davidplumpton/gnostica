(ns gnostica.ui.move-panel.ribbon)

(defn- action-ribbon-status-label [status]
  (case status
    :done "Done"
    :ready "Ready"
    :active "Active"
    :pending "Next"
    :skipped "Skipped"
    "Step"))

(defn- action-ribbon-step [{:keys [id label status detail board-index]}]
  ^{:key id}
  [:li.action-ribbon__step
   {:class (str "is-" (name status))}
   [:span.action-ribbon__status
    (action-ribbon-status-label status)]
   [:span.action-ribbon__label label]
   [:span.action-ribbon__detail
    (cond-> (or detail "")
      board-index
      (str " · board " board-index))]])

(defn action-ribbon-view [{:keys [visible? summary steps prompt ready?]}]
  (when visible?
    [:section.action-ribbon
     {:aria-label "Action sequence"}
     [:div.action-ribbon__heading
      [:p.eyebrow "Sequence"]
      [:strong (or summary "Major power")]]
     [:ol.action-ribbon__steps
      (for [step steps]
        [action-ribbon-step step])]
     [:p.action-ribbon__prompt
      (if ready? "Ready to confirm." prompt)]]))
