(ns gnostica.move-selection.staging)

(defn empty-selection []
  {:stage :source
   :source nil
   :params {}
   :error nil
   :last-result nil})

(def param-clear-keys
  {:current-major-action
   [:target-board-index
    :target-wasteland
    :target-piece-id
    :territory-card-source
    :one-point-card-id
    :replacement-card-source
    :replacement-card-id
    :orientation
    :distance
    :damage
    :rod-mode
    :sword-target-kind
    :minion-orientation]

   :fool-play
   [:target-board-index
    :target-wasteland
    :target-piece-id
    :territory-card-source
    :one-point-card-id
    :replacement-card-source
    :replacement-card-id
    :orientation
    :distance
    :damage
    :rod-mode
    :disc-target-kind
    :sword-target-kind
    :disc-action-count
    :sword-action-count
    :devil-action-count
    :major-action-count
    :minion-orientation
    :sun-disc-mode
    :sun-disc-target-piece-id
    :sun-disc-target-board-index
    :sun-disc-replacement-card-id
    :sun-disc-orientation
    :major-actions
    :fool-play-power]

   :sun-disc
   [:sun-disc-mode
    :sun-disc-target-piece-id
    :sun-disc-target-board-index
    :sun-disc-replacement-card-id
    :sun-disc-orientation]

   :hermit-destination
   [:hermit-destination-board-index
    :hermit-destination-wasteland]

   :base-target
   [:target-board-index
    :target-wasteland
    :target-piece-id
    :territory-card-source
    :one-point-card-id
    :replacement-card-source
    :replacement-card-id
    :orientation
    :distance
    :damage]

   :power-target-extra
   [:sword-action-count
    :devil-action-count
    :major-action-count
    :fool-reveal-count
    :fool-reveals
    :fool-active-reveal
    :fool-shuffle-fn
    :fool-play-power
    :high-priestess-redraw-count
    :redraws
    :judgement-card-ids
    :major-actions]

   :child-power-extra
   [:rod-mode
    :disc-target-kind
    :sword-target-kind
    :disc-action-count
    :sword-action-count
    :devil-action-count
    :major-action-count
    :minion-orientation
    :fool-reveal-count
    :fool-reveals
    :fool-active-reveal
    :fool-shuffle-fn
    :fool-play-power
    :high-priestess-redraw-count
    :redraws
    :judgement-card-ids
    :major-actions]

   :source-change-extra
   [:piece-id
    :power
    :copied-board-index
    :copied-power
    :rod-mode
    :disc-target-kind
    :sword-target-kind
    :disc-action-count
    :sword-action-count
    :devil-action-count
    :major-action-count
    :minion-orientation
    :fool-reveal-count
    :fool-reveals
    :fool-active-reveal
    :fool-shuffle-fn
    :fool-play-power
    :high-priestess-redraw-count
    :redraws
    :judgement-card-ids
    :major-actions]

   :hand-card-source-extra
   [:piece-id
    :power
    :rod-mode
    :disc-target-kind
    :sword-target-kind
    :disc-action-count
    :sword-action-count
    :devil-action-count
    :major-action-count
    :minion-orientation
    :fool-reveal-count
    :fool-reveals
    :fool-active-reveal
    :fool-shuffle-fn
    :fool-play-power
    :high-priestess-redraw-count
    :redraws
    :judgement-card-ids
    :copied-board-index
    :copied-power]

   :power-change-extra
   [:rod-mode
    :disc-target-kind
    :sword-target-kind
    :disc-action-count
    :sword-action-count
    :major-action-count
    :minion-orientation
    :fool-reveal-count
    :fool-reveals
    :fool-active-reveal
    :fool-shuffle-fn
    :fool-play-power
    :high-priestess-redraw-count
    :redraws
    :judgement-card-ids
    :major-actions
    :copied-board-index
    :copied-power]

   :target-kind-change
   [:target-board-index
    :target-wasteland
    :target-piece-id
    :territory-card-source
    :one-point-card-id
    :replacement-card-source
    :replacement-card-id
    :orientation
    :damage]

   :rod-mode-change
   [:target-board-index
    :target-wasteland
    :target-piece-id
    :territory-card-source
    :one-point-card-id
    :replacement-card-source
    :replacement-card-id
    :orientation
    :damage]

   :sword-action-count-change
   [:target-board-index
    :target-wasteland
    :target-piece-id
    :replacement-card-source
    :replacement-card-id
    :orientation
    :damage
    :sword-target-kind]

   :devil-action-count-change
   [:target-board-index
    :target-wasteland
    :target-piece-id
    :orientation]

   :minion-orientation-change
   [:target-board-index
    :target-wasteland
    :target-piece-id
    :replacement-card-id
    :orientation
    :damage]

   :target-piece-change
   [:target-board-index
    :target-wasteland
    :territory-card-source
    :one-point-card-id
    :replacement-card-source
    :replacement-card-id
    :orientation
    :damage]

   :damage-change
   [:replacement-card-source
    :replacement-card-id
    :orientation]})

(defn clear-params [params group-key]
  (apply dissoc params (get param-clear-keys group-key)))

(defn clear-current-major-action-params [params]
  (clear-params params :current-major-action))

(defn clear-fool-play-params [params]
  (clear-params params :fool-play))

(defn clear-sun-disc-params [params]
  (clear-params params :sun-disc))

(defn clear-hermit-destination-params [params]
  (clear-params params :hermit-destination))

(defn clear-power-target-params [params]
  (-> params
      (clear-params :base-target)
      (clear-params :power-target-extra)
      clear-sun-disc-params
      clear-hermit-destination-params))

(defn clear-child-power-params [params]
  (-> params
      clear-power-target-params
      (clear-params :child-power-extra)))

(defn commit-fool-active-reveal [params reveal]
  (-> params
      clear-fool-play-params
      (dissoc :fool-active-reveal)
      (update :fool-reveals (fnil conj []) reveal)))

(defn set-source-board-index [params board-index]
  (let [next-params (assoc params :source-board-index board-index)]
    (if (= (:source-board-index params) board-index)
      next-params
      (-> next-params
          clear-power-target-params
          (clear-params :source-change-extra)))))

(defn set-territory-target [params board-index]
  (-> params
      (assoc :target-board-index board-index)
      (dissoc :target-wasteland
              :target-piece-id
              :territory-card-source
              :one-point-card-id
              :replacement-card-source
              :replacement-card-id
              :damage)
      clear-sun-disc-params
      clear-hermit-destination-params))

(defn set-wasteland-target [params space]
  (-> params
      (assoc :target-wasteland (select-keys space [:kind :row :col]))
      (dissoc :target-board-index
              :target-piece-id
              :orientation
              :territory-card-source
              :one-point-card-id
              :replacement-card-source
              :replacement-card-id
              :damage)
      clear-sun-disc-params
      clear-hermit-destination-params))

(defn set-hand-card-source [params card-id]
  (-> params
      (assoc :hand-card-id card-id)
      clear-power-target-params
      (clear-params :hand-card-source-extra)))

(defn set-acting-piece [params piece-id]
  (let [next-params (assoc params :piece-id piece-id)]
    (if (= (:piece-id params) piece-id)
      next-params
      (-> next-params
          clear-power-target-params
          (dissoc :minion-orientation)))))

(defn set-power [params power]
  (let [next-params (assoc params :power power)]
    (if (= (:power params) power)
      next-params
      (-> next-params
          clear-power-target-params
          (clear-params :power-change-extra)))))

(defn set-world-copy [params board-index]
  (let [next-params (assoc params :copied-board-index board-index)]
    (if (= (:copied-board-index params) board-index)
      next-params
      (-> next-params
          clear-child-power-params
          (dissoc :copied-power)))))

(defn set-world-copied-power [params power]
  (let [next-params (assoc params :copied-power power)]
    (if (= (:copied-power params) power)
      next-params
      (clear-child-power-params next-params))))

(defn set-rod-mode [params mode]
  (let [next-params (assoc params :rod-mode mode)]
    (if (= (:rod-mode params) mode)
      next-params
      (clear-params next-params :rod-mode-change))))

(defn set-disc-target-kind [params target-kind]
  (let [next-params (assoc params :disc-target-kind target-kind)]
    (if (= (:disc-target-kind params) target-kind)
      next-params
      (clear-params next-params :target-kind-change))))

(defn set-sword-target-kind [params target-kind]
  (let [next-params (assoc params :sword-target-kind target-kind)]
    (if (= (:sword-target-kind params) target-kind)
      next-params
      (clear-params next-params :target-kind-change))))

(defn set-disc-action-count [params action-count]
  (let [next-params (assoc params :disc-action-count action-count)]
    (if (= (:disc-action-count params) action-count)
      next-params
      (dissoc next-params :replacement-card-id))))

(defn set-sword-action-count [params action-count]
  (let [next-params (assoc params :sword-action-count action-count)]
    (if (= (:sword-action-count params) action-count)
      next-params
      (-> next-params
          (clear-params :sword-action-count-change)
          (assoc :major-actions [])))))

(defn set-major-action-count [params action-count]
  (let [next-params (assoc params :major-action-count action-count)]
    (if (= (:major-action-count params) action-count)
      next-params
      (-> next-params
          clear-current-major-action-params
          (assoc :major-actions [])))))

(defn set-devil-action-count [params action-count]
  (let [next-params (assoc params :devil-action-count action-count)]
    (if (= (:devil-action-count params) action-count)
      next-params
      (-> next-params
          (clear-params :devil-action-count-change)
          (assoc :major-actions [])))))

(defn set-damage [params damage]
  (let [next-params (assoc params :damage damage)]
    (if (= (:damage params) damage)
      next-params
      (clear-params next-params :damage-change))))

(defn set-minion-orientation [params orientation]
  (let [next-params (assoc params :minion-orientation orientation)]
    (if (= (:minion-orientation params) orientation)
      next-params
      (clear-params next-params :minion-orientation-change))))

(defn set-target-piece [params piece-id]
  (let [next-params (assoc params :target-piece-id piece-id)]
    (if (= (:target-piece-id params) piece-id)
      next-params
      (-> next-params
          (clear-params :target-piece-change)
          clear-sun-disc-params
          clear-hermit-destination-params))))

(defn set-hermit-destination-territory [params board-index]
  (-> params
      (assoc :hermit-destination-board-index board-index)
      (dissoc :hermit-destination-wasteland
              :orientation)))

(defn set-hermit-destination-wasteland [params space]
  (-> params
      (assoc :hermit-destination-wasteland (select-keys space [:kind :row :col]))
      (dissoc :hermit-destination-board-index
              :orientation)))

(defn set-sun-disc-mode [params mode]
  (let [next-params (assoc params :sun-disc-mode mode)]
    (if (= (:sun-disc-mode params) mode)
      next-params
      (dissoc next-params
              :sun-disc-target-piece-id
              :sun-disc-target-board-index
              :sun-disc-replacement-card-id
              :sun-disc-orientation))))

(defn set-sun-disc-target-piece [params piece-id]
  (let [next-params (assoc params :sun-disc-target-piece-id piece-id)]
    (if (= (:sun-disc-target-piece-id params) piece-id)
      next-params
      (dissoc next-params
              :sun-disc-target-board-index
              :sun-disc-replacement-card-id
              :sun-disc-orientation))))

(defn set-sun-disc-target-territory [params board-index]
  (let [next-params (assoc params :sun-disc-target-board-index board-index)]
    (if (= (:sun-disc-target-board-index params) board-index)
      next-params
      (dissoc next-params
              :sun-disc-target-piece-id
              :sun-disc-replacement-card-id
              :sun-disc-orientation))))

(defn set-fool-reveal-count [params reveal-count]
  (let [next-params (assoc params :fool-reveal-count reveal-count)]
    (if (= (:fool-reveal-count params) reveal-count)
      next-params
      (-> next-params
          clear-fool-play-params
          (dissoc :fool-reveals
                  :fool-active-reveal
                  :fool-shuffle-fn)))))

(defn set-fool-play-power [params power]
  (-> params
      clear-fool-play-params
      (assoc :fool-play-power power)))

(defn set-territory-card-source [params territory-card-source]
  (cond-> (assoc params :territory-card-source territory-card-source)
    (= :draw-pile-top territory-card-source)
    (dissoc :one-point-card-id)))
