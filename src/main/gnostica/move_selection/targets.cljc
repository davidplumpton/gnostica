(ns gnostica.move-selection.targets
  (:require [gnostica.board-layout :as layout]
            [gnostica.cards :as cards]
            [gnostica.move-selection.context :as context]
            [gnostica.pieces :as pieces]))

(def required-context-keys
  #{:active-power
    :board
    :board-pieces
    :cup-move?
    :current-player-hand
    :current-player-id
    :current-player-piece?
    :discard-pile
    :disc-move?
    :disc-replacement-card-source-definitions
    :max-draw-count
    :move-discard-card-options
    :move-hand-card-options
    :move-judgement-card-options
    :move-one-point-card-options
    :move-piece-options
    :move-selection
    :move-source-board-options
    :move-target-board-options
    :move-target-piece-options
    :move-target-wasteland-options
    :move-world-copy-options
    :replacement-card-options-for-source
    :replacement-card-source-option-ids
    :rod-move?
    :selected-disc-action-count
    :selected-replacement-card-source
    :selected-sun-disc-mode
    :small-stash-count
    :source-unavailable-reason
    :sun-disc-target-cell
    :sun-move?
    :sword-move?
    :target-board-cell})

(defn make-context [deps]
  (context/make "gnostica.move-selection.targets" required-context-keys deps))

(defn- call [ctx key & args]
  (apply context/call ctx key args))

(defn- value [ctx key]
  (context/value ctx key))

(defn- target-status [active? enabled?]
  (cond
    enabled? :legal
    active? :disabled
    :else :idle))

(defn- descriptor-error [code message data]
  {:code code
   :message message
   :data data})

(defn- replacement-card-source-label [ctx card-source]
  (get-in (value ctx :disc-replacement-card-source-definitions)
          [card-source :label]
          "card source"))

(defn replacement-card-expected-value [ctx db source params]
  (cond
    (call ctx :sun-move? db source params)
    (case (call ctx :selected-sun-disc-mode db source params)
      :created-territory 2
      :territory (some-> (call ctx :sun-disc-target-cell db params)
                         :card
                         cards/card-point-value
                         inc)
      nil)

    (call ctx :disc-move? db source params)
    (some-> (call ctx :target-board-cell db params)
            :card
            cards/card-point-value
            (+ (call ctx :selected-disc-action-count db source params)))

    (call ctx :sword-move? db source params)
    (when-let [target-value (some-> (call ctx :target-board-cell db params)
                                    :card
                                    cards/card-point-value)]
      (when-let [damage (:damage params)]
        (let [replacement-value (- target-value damage)]
          (when (pos? replacement-value)
            replacement-value))))

    :else
    nil))

(defn- replacement-card-disabled-error [ctx db card-source]
  (let [{:keys [source params]} (call ctx :move-selection db)
        source-options (set (call ctx :replacement-card-source-option-ids db source params))
        selected-source (call ctx :selected-replacement-card-source db source params)
        expected-value (replacement-card-expected-value ctx db source params)]
    (cond
      (not (contains? source-options card-source))
      (descriptor-error :replacement-card-source-unavailable
                        (str (replacement-card-source-label ctx card-source)
                             " replacements are not available for the active source.")
                        {:source source
                         :replacement-card-source card-source
                         :available-sources (vec source-options)})

      (and selected-source
           (not= selected-source card-source))
      (descriptor-error :replacement-card-source-unavailable
                        (str "The active replacement source is "
                             (replacement-card-source-label ctx selected-source)
                             ".")
                        {:source source
                         :replacement-card-source card-source
                         :selected-replacement-card-source selected-source})

      expected-value
      (descriptor-error :invalid-replacement-card
                        (str "Choose a replacement card worth "
                             expected-value
                             " point"
                             (when (not= 1 expected-value) "s")
                             ".")
                        {:source source
                         :replacement-card-source card-source
                         :required-point-value expected-value})

      :else
      (descriptor-error :invalid-replacement-card
                        "Choose an available replacement card."
                        {:source source
                         :replacement-card-source card-source}))))

(defn- selected-territory-indexes [params]
  (set (keep params
             [:source-board-index
              :copied-board-index
              :target-board-index
              :sun-disc-target-board-index
              :hermit-destination-board-index])))

(defn- selected-wasteland? [params {:keys [row col]}]
  (boolean
   (some (fn [key]
           (let [space (get params key)]
             (and (= row (:row space))
                  (= col (:col space)))))
         [:target-wasteland :hermit-destination-wasteland])))

(defn- selected-piece-ids [params]
  (set (keep params
             [:piece-id :target-piece-id :sun-disc-target-piece-id])))

(defn- target-highlight? [source params {:keys [kind board-index row col]}]
  (if (= :place-initial-small source)
    (case kind
      :territory (= board-index (:target-board-index params))
      :wasteland (selected-wasteland? params {:row row :col col})
      false)
    true))

(defn- current-target-role [stage]
  (case stage
    :source-territory :source
    :hand-card :source
    :piece :minion
    :world-copy :copy
    :hermit-destination :destination
    :one-point-card :territory-card
    :replacement-card :replacement-card
    :target-piece :target
    :target :target
    :draw-count :draw
    :orientation :orientation
    :idle))

(defn- territory-disabled-error [ctx db stage]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (case stage
      :source-territory
      (descriptor-error :invalid-source-territory
                        "Choose a territory with one of the current player's pieces."
                        {:source source})

      :world-copy
      (descriptor-error :invalid-world-copy
                        "Choose a non-World major territory for World to copy."
                        {:source source})

      :hermit-destination
      (descriptor-error :invalid-hermit-destination
                        "Choose an eligible Hermit destination territory."
                        {:source source})

      :target
      (cond
        (= :place-initial-small source)
        (descriptor-error :target-space-occupied
                          "Choose an empty territory or wasteland."
                          {:source source})

        (call ctx :rod-move? db source params)
        (descriptor-error :invalid-rod-target
                          "Choose a Rod-targetable territory."
                          {:source source
                           :rod-mode (:rod-mode params)})

        (call ctx :disc-move? db source params)
        (descriptor-error :invalid-disc-target
                          "Choose a Disc-targetable territory."
                          {:source source
                           :disc-target-kind (:disc-target-kind params)})

        (call ctx :sword-move? db source params)
        (descriptor-error :invalid-sword-target
                          "Choose a Sword-targetable territory."
                          {:source source
                           :sword-target-kind (:sword-target-kind params)})

        (or (call ctx :cup-move? db source params)
            (call ctx :sun-move? db source params))
        (descriptor-error :invalid-cup-target
                          "Choose a Cup-targetable territory."
                          {:source source})

        :else
        (descriptor-error :invalid-target-territory
                          "Choose an available target territory."
                          {:source source}))

      nil)))

(defn- territory-descriptor-context [ctx db]
  (let [{:keys [source stage]} (call ctx :move-selection db)]
    (cond
      (and (= :place-initial-small source)
           (contains? #{:orientation :confirm :rejected} stage))
      {:active? true
       :role :target
       :options (call ctx :move-target-board-options db)
       :disabled-error (territory-disabled-error ctx db :target)}

      :else
      (case stage
        :source-territory
        {:active? true
         :role :source
         :options (call ctx :move-source-board-options db)
         :disabled-error (territory-disabled-error ctx db stage)}

        :world-copy
        {:active? true
         :role :copy
         :options (call ctx :move-world-copy-options db)
         :disabled-error (territory-disabled-error ctx db stage)}

        :target
        {:active? true
         :role :target
         :options (call ctx :move-target-board-options db)
         :disabled-error (territory-disabled-error ctx db stage)}

        :hermit-destination
        {:active? true
         :role :destination
         :options (call ctx :move-target-board-options db)
         :disabled-error (territory-disabled-error ctx db stage)}

        nil))))

(defn- territory-descriptors [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)
        {:keys [active? role options disabled-error]} (or (territory-descriptor-context ctx db)
                                                          {})
        legal-indexes (set (map :index options))
        selected-indexes (selected-territory-indexes params)]
    (mapv (fn [{:keys [index row col orientation card] :as cell}]
            (let [enabled? (contains? legal-indexes index)
                  status (target-status active? enabled?)]
              (cond-> {:kind :territory
                       :role role
                       :board-index index
                       :space-key (pieces/territory-space index)
                       :row row
                       :col col
                       :orientation orientation
                       :card-id (:id card)
                       :label (:title card)
                       :cell cell
                       :active? (true? active?)
                       :enabled? enabled?
                       :status status
                       :highlight? (target-highlight? source
                                                      params
                                                      {:kind :territory
                                                       :board-index index})
                       :selected? (contains? selected-indexes index)}
                (and active? (not enabled?) disabled-error)
                (assoc :error (assoc-in disabled-error [:data :board-index] index)
                       :reason (:message disabled-error)))))
          (call ctx :board db))))

(defn- wasteland-disabled-error [ctx db stage]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (case stage
      :target
      (if (= :place-initial-small source)
        (descriptor-error :target-space-occupied
                          "Choose an empty territory or wasteland."
                          {:source source})
        (descriptor-error :invalid-wasteland-target
                          "Choose a wasteland targeted by the active move."
                          {:source source
                           :power (call ctx :active-power db source params)}))

      :hermit-destination
      (descriptor-error :invalid-hermit-destination
                        "Choose an eligible Hermit destination wasteland."
                        {:source source})

      nil)))

(defn- wasteland-descriptor-context [ctx db]
  (let [{:keys [source stage]} (call ctx :move-selection db)]
    (cond
      (and (= :place-initial-small source)
           (contains? #{:orientation :confirm :rejected} stage))
      {:active? true
       :role :target
       :options (call ctx :move-target-wasteland-options db)
       :disabled-error (wasteland-disabled-error ctx db :target)}

      :else
      (case stage
        :target
        {:active? true
         :role :target
         :options (call ctx :move-target-wasteland-options db)
         :disabled-error (wasteland-disabled-error ctx db stage)}

        :hermit-destination
        {:active? true
         :role :destination
         :options (call ctx :move-target-wasteland-options db)
         :disabled-error (wasteland-disabled-error ctx db stage)}

        nil))))

(defn- wasteland-descriptors [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)
        {:keys [active? role options disabled-error]} (or (wasteland-descriptor-context ctx db)
                                                          {})
        legal-coordinates (set (map (juxt :row :col) options))]
    (mapv (fn [{:keys [row col] :as space}]
            (let [enabled? (contains? legal-coordinates [row col])
                  status (target-status active? enabled?)]
              (cond-> {:kind :wasteland
                       :role role
                       :space-key (pieces/wasteland-space row col)
                       :row row
                       :col col
                       :orientation (:orientation space)
                       :label (:id space)
                       :space space
                       :active? (true? active?)
                       :enabled? enabled?
                       :status status
                       :highlight? (target-highlight? source
                                                      params
                                                      {:kind :wasteland
                                                       :row row
                                                       :col col})
                       :selected? (selected-wasteland? params space)}
                (and active? (not enabled?) disabled-error)
                (assoc :error (assoc disabled-error
                                     :data (assoc (:data disabled-error)
                                                  :row row
                                                  :col col))
                       :reason (:message disabled-error)))))
          (layout/wasteland-spaces (call ctx :board db)))))

(defn- piece-disabled-error [ctx db stage]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (case stage
      :piece
      (descriptor-error :invalid-piece
                        "Choose one of the current player's pieces."
                        {:source source})

      :target-piece
      (cond
        (call ctx :rod-move? db source params)
        (descriptor-error :invalid-target-piece
                          "Choose a Rod-targetable piece."
                          {:source source
                           :rod-mode (:rod-mode params)})

        (call ctx :disc-move? db source params)
        (descriptor-error :invalid-target-piece
                          "Choose a Disc-targetable piece."
                          {:source source
                           :disc-target-kind (:disc-target-kind params)})

        (call ctx :sword-move? db source params)
        (descriptor-error :invalid-target-piece
                          "Choose a Sword-targetable piece."
                          {:source source
                           :sword-target-kind (:sword-target-kind params)})

        (or (call ctx :cup-move? db source params)
            (call ctx :sun-move? db source params))
        (descriptor-error :invalid-target-piece
                          "Choose an enemy piece targeted by the minion."
                          {:source source})

        :else
        (descriptor-error :invalid-target-piece
                          "Choose an available target piece."
                          {:source source}))

      nil)))

(defn- piece-descriptor-context [ctx db]
  (let [{:keys [stage]} (call ctx :move-selection db)]
    (case stage
      :piece
      {:active? true
       :role :minion
       :options (call ctx :move-piece-options db)
       :disabled-error (piece-disabled-error ctx db stage)}

      :target-piece
      {:active? true
       :role :target
       :options (call ctx :move-target-piece-options db)
       :disabled-error (piece-disabled-error ctx db stage)}

      nil)))

(defn- piece-descriptors [ctx db]
  (let [{:keys [params]} (call ctx :move-selection db)
        {:keys [active? role options disabled-error]} (or (piece-descriptor-context ctx db)
                                                          {})
        legal-piece-ids (set (map :id options))
        selected-ids (selected-piece-ids params)
        source-enabled? (nil? (call ctx :source-unavailable-reason db :orient-piece))]
    (mapv (fn [{:keys [id player-id space-index space size orientation] :as piece}]
            (let [current-player? (call ctx :current-player-piece? db piece)
                  enabled? (contains? legal-piece-ids id)
                  status (target-status active? enabled?)]
              (cond-> {:kind :piece
                       :role role
                       :piece-id id
                       :player-id player-id
                       :current-player? current-player?
                       :source-enabled? (and current-player? source-enabled?)
                       :space-key (pieces/piece-space-key piece)
                       :space-index space-index
                       :space space
                       :size size
                       :orientation orientation
                       :piece piece
                       :active? (true? active?)
                       :enabled? enabled?
                       :status status
                       :selected? (contains? selected-ids id)}
                (and active? (not enabled?) disabled-error)
                (assoc :error (assoc-in disabled-error [:data :piece-id] id)
                       :reason (:message disabled-error)))))
          (call ctx :board-pieces db))))

(defn- replacement-card-descriptor-context [ctx db card-source]
  (let [{:keys [source params]} (call ctx :move-selection db)
        selected-source (call ctx :selected-replacement-card-source db source params)
        source-active? (or (nil? selected-source)
                           (= selected-source card-source))
        source-options (vec (call ctx
                                  :replacement-card-options-for-source
                                  db
                                  source
                                  params
                                  card-source))
        hand-card-ids (set (map :id (call ctx :current-player-hand db)))
        discard-source-active? (and (= :hand card-source)
                                    (or (nil? selected-source)
                                        (= :discard-pile selected-source))
                                    (contains? (set (call ctx
                                                          :replacement-card-source-option-ids
                                                          db
                                                          source
                                                          params))
                                               :discard-pile))
        just-discarded-hand-options (when discard-source-active?
                                      (filterv #(contains? hand-card-ids (:id %))
                                               (call ctx
                                                     :replacement-card-options-for-source
                                                     db
                                                     source
                                                     params
                                                     :discard-pile)))
        options (if source-active?
                  (vec (concat source-options just-discarded-hand-options))
                  (vec just-discarded-hand-options))
        source-by-card-id (merge
                           (into {} (map (fn [card]
                                           [(:id card) card-source])
                                         source-options))
                           (into {} (map (fn [card]
                                           [(:id card) :discard-pile])
                                         just-discarded-hand-options)))]
    {:active? true
     :role :replacement-card
     :replacement-card-source card-source
     :replacement-card-source-by-card-id source-by-card-id
     :options options
     :disabled-error (replacement-card-disabled-error ctx db card-source)}))

(defn- card-descriptor-context [ctx db]
  (let [{:keys [source stage]} (call ctx :move-selection db)]
    (cond
      (= :hand-card stage)
      {:hand {:active? true
              :role :source
              :options (call ctx :move-hand-card-options db)
              :disabled-error (descriptor-error :invalid-hand-card
                                                "Choose a card from the current player's hand."
                                                {:source source})}}

      (and (= :draw-cards source)
           (contains? #{:draw-count :confirm} stage))
      {:hand {:active? true
              :role :discard
              :options (call ctx :move-discard-card-options db)
              :disabled-error nil}
       :draw-pile {:active? true
                   :role :draw}}

      (= :one-point-card stage)
      {:hand {:active? true
              :role :territory-card
              :options (call ctx :move-one-point-card-options db)
              :disabled-error (descriptor-error :invalid-one-point-card
                                                "Choose a one-point card from the current player's hand."
                                                {:source source})}}

      (contains? #{:replacement-card-source :replacement-card} stage)
      {:hand (replacement-card-descriptor-context ctx db :hand)
       :discard (replacement-card-descriptor-context ctx db :discard-pile)}

      (= :judgement-card-selection stage)
      {:discard {:active? true
                 :role :judgement-card
                 :options (call ctx :move-judgement-card-options db)
                 :disabled-error (descriptor-error :invalid-judgement-card
                                                   "Choose a card from the discard pile."
                                                   {:source source})}}

      :else
      {})))

(defn- card-descriptors-for [cards {:keys [active? role options disabled-error]
                                    :as context}
                             selected-card-ids]
  (let [legal-card-ids (set (map :id options))
        descriptor-context (select-keys context [:replacement-card-source])]
    (mapv (fn [{:keys [id title] :as card}]
            (let [enabled? (contains? legal-card-ids id)
                  status (target-status active? enabled?)
                  card-source (get-in context [:replacement-card-source-by-card-id id]
                                      (:replacement-card-source context))]
              (cond-> (merge {:kind :card
                              :role role
                              :card-id id
                              :label title
                              :card card
                              :active? (true? active?)
                              :enabled? enabled?
                              :status status
                              :selected? (contains? selected-card-ids id)}
                             (cond-> descriptor-context
                               card-source
                               (assoc :replacement-card-source card-source)))
                (and active? (not enabled?) disabled-error)
                (assoc :error (assoc-in disabled-error [:data :card-id] id)
                       :reason (:message disabled-error)))))
          cards)))

(defn- hand-card-descriptors [ctx db]
  (let [{:keys [params]} (call ctx :move-selection db)
        context (get (card-descriptor-context ctx db) :hand)
        selected-ids (set (concat [(:hand-card-id params)
                                   (:one-point-card-id params)
                                   (:replacement-card-id params)
                                   (:sun-disc-replacement-card-id params)]
                                  (:discard-card-ids params)))]
    (card-descriptors-for (call ctx :current-player-hand db) context selected-ids)))

(defn- discard-card-descriptors [ctx db]
  (let [{:keys [params]} (call ctx :move-selection db)
        context (get (card-descriptor-context ctx db) :discard)
        selected-ids (set (concat [(:replacement-card-id params)]
                                  (:judgement-card-ids params)))]
    (card-descriptors-for (call ctx :discard-pile db) context selected-ids)))

(defn- draw-pile-descriptor [ctx db]
  (let [context (get (card-descriptor-context ctx db) :draw-pile)
        active? (:active? context)
        enabled? (and active? (pos? (call ctx :max-draw-count db)))]
    (cond-> {:kind :draw-pile
             :role (:role context)
             :active? (true? active?)
             :enabled? (boolean enabled?)
             :status (target-status active? enabled?)
             :count (count (get-in db [:game :draw-pile] []))}
      (and active? (not enabled?))
      (assoc :reason "The current player cannot draw more cards."
             :error (descriptor-error :draw-pile-unavailable
                                      "The current player cannot draw more cards."
                                      {})))))

(defn- stash-piece-descriptors [ctx db]
  (let [{:keys [source]} (call ctx :move-selection db)
        active? (= :place-initial-small source)
        player-id (call ctx :current-player-id db)
        count (call ctx :small-stash-count db)
        enabled? (and active? (pos? count))]
    (cond-> []
      active?
      (conj (cond-> {:kind :stash-piece
                     :role :source
                     :player-id player-id
                     :size :small
                     :count count
                     :active? true
                     :enabled? enabled?
                     :status (target-status active? enabled?)}
              (not enabled?)
              (assoc :reason "The current player has no small pieces in stash."
                     :error (descriptor-error :stash-piece-unavailable
                                              "The current player has no small pieces in stash."
                                              {:player-id player-id
                                               :size :small})))))))

(defn move-legal-targets [ctx db]
  (let [targets {:territories (territory-descriptors ctx db)
                 :wastelands (wasteland-descriptors ctx db)
                 :pieces (piece-descriptors ctx db)
                 :hand-cards (hand-card-descriptors ctx db)
                 :discard-cards (discard-card-descriptors ctx db)
                 :draw-pile (draw-pile-descriptor ctx db)
                 :stash-pieces (stash-piece-descriptors ctx db)}
        active? (boolean
                 (or (some :active? (:territories targets))
                     (some :active? (:wastelands targets))
                     (some :active? (:pieces targets))
                     (some :active? (:hand-cards targets))
                     (some :active? (:discard-cards targets))
                     (:active? (:draw-pile targets))
                     (some :active? (:stash-pieces targets))))
        stage (:stage (call ctx :move-selection db))]
    (assoc targets
           :active? active?
           :stage stage
           :role (current-target-role stage))))
