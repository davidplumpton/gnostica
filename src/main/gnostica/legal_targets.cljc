(ns gnostica.legal-targets)

(def target-status-keys
  [:target-status :target-enabled? :target-reason])

(defn active? [descriptor]
  (true? (:active? descriptor)))

(defn enabled? [descriptor]
  (true? (:enabled? descriptor)))

(defn selected? [descriptor]
  (true? (:selected? descriptor)))

(defn status [descriptor]
  (:status descriptor))

(defn status-name [descriptor]
  (some-> (status descriptor) name))

(defn status->class [status]
  (case status
    :legal " is-legal-target"
    :disabled " is-disabled-target"
    ""))

(defn status-class
  ([descriptor]
   (status-class true descriptor))
  ([show? descriptor]
   (when (and show? (active? descriptor))
     (status->class (status descriptor)))))

(defn reason [descriptor]
  (or (:reason descriptor)
      (get-in descriptor [:error :message])))

(defn any-active? [descriptors]
  (boolean (some active? descriptors)))

(defn descriptors-by [descriptor-key descriptors]
  (into {}
        (map (juxt descriptor-key identity))
        descriptors))

(defn active-descriptors-by [descriptor-key descriptors]
  (into {}
        (map (juxt descriptor-key identity))
        (filter active? descriptors)))

(defn descriptors-by-card-id [descriptors]
  (active-descriptors-by :card-id descriptors))

(defn descriptor-for-card [descriptors card]
  (get (descriptors-by-card-id descriptors) (:id card)))

(defn territory-targets-by-index [legal-targets]
  (descriptors-by :board-index (:territories legal-targets)))

(defn wasteland-targets-by-coordinate [legal-targets]
  (into {}
        (map (fn [{:keys [row col] :as descriptor}]
               [[row col] descriptor]))
        (:wastelands legal-targets)))

(defn piece-targets-by-id [legal-targets]
  (descriptors-by :piece-id (:pieces legal-targets)))

(defn target-indexes [legal-targets]
  {:territories (territory-targets-by-index legal-targets)
   :wastelands (wasteland-targets-by-coordinate legal-targets)
   :pieces (piece-targets-by-id legal-targets)})

(defn descriptor-for-indexed-target
  [{:keys [territories wastelands pieces]}
   {:keys [kind board-index row col piece-id]}]
  (case kind
    :territory (get territories board-index)
    :wasteland (get wastelands [row col])
    :piece (get pieces piece-id)
    nil))

(defn descriptor-for-target [legal-targets target]
  (descriptor-for-indexed-target (target-indexes legal-targets) target))

(defn with-target-status [m descriptor]
  (if descriptor
    (assoc m
           :target-status (status descriptor)
           :target-enabled? (enabled? descriptor)
           :target-reason (reason descriptor))
    (apply dissoc m target-status-keys)))
