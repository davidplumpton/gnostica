(ns gnostica.app-state.lobby-setup
  (:require [clojure.string :as str]
            [gnostica.app-state.db :as db]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as pieces]))

(defn player-colour [player-id]
  (get pieces/players-by-id player-id))

(defn known-player-id? [player-id]
  (contains? pieces/players-by-id player-id))

(defn normalize-player-id [player-id]
  (if (string? player-id)
    (keyword player-id)
    player-id))

(defn trim-name [name]
  (str/trim (str name)))

(defn- normalize-controller-id [controller-id]
  (let [controller-id (trim-name (if (keyword? controller-id)
                                   (name controller-id)
                                   controller-id))]
    (when-not (str/blank? controller-id)
      controller-id)))

(defn normalize-local-controller [controller]
  (when-let [controller-id (normalize-controller-id (:id controller))]
    {:id controller-id
     :name (let [controller-name (trim-name (:name controller))]
             (if (str/blank? controller-name)
               "Local browser"
               controller-name))}))

(defn- normalize-target-score [target-score]
  (let [score (cond
                (int? target-score)
                target-score

                (string? target-score)
                (some #(when (= target-score (str %)) %)
                      db/target-score-options))]
    (when (contains? game-state/allowed-target-scores score)
      score)))

(defn lobby-error [code message data]
  {:code code
   :message message
   :data data})

(defn clear-lobby-error-codes [app-db error-codes]
  (if (contains? error-codes (get-in app-db [:lobby :error :code]))
    (update app-db :lobby dissoc :error)
    app-db))

(defn duplicate-lobby-player-ids [players]
  (->> players
       (map :id)
       frequencies
       (keep (fn [[player-id n]]
               (when (< 1 n)
                 player-id)))
       vec))

(defn first-available-player-id [players]
  (let [used (set (map :id players))]
    (some #(when-not (contains? used (:id %))
             (:id %))
          pieces/players)))

(defn- normalize-player-controller [player-spec local-controller]
  (or local-controller
      (normalize-local-controller {:id (:controller-id player-spec)
                                   :name (:controller-name player-spec)})))

(defn with-player-controller [player controller]
  (cond-> player
    controller
    (assoc :controller-id (:id controller)
           :controller-name (:name controller))))

(defn normalize-lobby-player [local-controller slot-id player-spec]
  (let [requested-id (:id player-spec)
        player-id (if (known-player-id? requested-id)
                    requested-id
                    (:id (first pieces/players)))
        colour (player-colour player-id)
        name (trim-name (or (:name player-spec)
                            (:name colour)))
        controller (normalize-player-controller player-spec local-controller)]
    (with-player-controller {:slot-id slot-id
                             :id player-id
                             :name name}
      controller)))

(defn normalize-lobby-players [player-specs local-controller]
  (mapv (partial normalize-lobby-player local-controller)
        (range 1 (inc (count player-specs)))
        player-specs))

(defn next-lobby-slot-id [players]
  (inc (reduce max 0 (map :slot-id players))))

(defn lobby-player-specs [lobby]
  (mapv (fn [{:keys [id name]}]
          {:id id
           :name (trim-name name)})
        (:players lobby)))

(defn lobby-validation-error [lobby]
  (let [players (:players lobby)
        player-specs (lobby-player-specs lobby)
        duplicate-ids (duplicate-lobby-player-ids players)
        unknown-ids (->> players
                         (map :id)
                         (remove known-player-id?)
                         vec)
        blank-name-slot-ids (->> players
                                 (filter #(str/blank? (trim-name (:name %))))
                                 (mapv :slot-id))]
    (cond
      (< (count players) game-state/min-players)
      (lobby-error :too-few-players
                   "Add at least two players before starting."
                   {:count (count players)
                    :minimum game-state/min-players})

      (< game-state/max-players (count players))
      (lobby-error :too-many-players
                   "Gnostica supports no more than six players."
                   {:count (count players)
                    :maximum game-state/max-players})

      (seq unknown-ids)
      (lobby-error :unknown-player-colours
                   "Every player must use a known player colour."
                   {:player-ids unknown-ids
                    :known-ids (mapv :id pieces/players)})

      (seq duplicate-ids)
      (lobby-error :duplicate-player-colours
                   "Each player colour can only be used once."
                   {:player-ids duplicate-ids})

      (seq blank-name-slot-ids)
      (lobby-error :blank-player-names
                   "Every player needs a display name."
                   {:slot-ids blank-name-slot-ids})

      (not (game-state/valid-player-count? player-specs))
      (lobby-error :invalid-player-count
                   "Gnostica requires between two and six players."
                   {:count (count player-specs)
                    :minimum game-state/min-players
                    :maximum game-state/max-players}))))

(defn lobby-valid? [lobby]
  (nil? (lobby-validation-error lobby)))

(defn create-lobby [{:keys [player-specs lobby-player-specs game-options demo-board-pieces
                            local-controller]
                     :as opts
                     :or {player-specs db/default-lobby-player-specs
                          game-options {}}}]
  (let [local-controller (normalize-local-controller local-controller)
        players (normalize-lobby-players (or lobby-player-specs player-specs)
                                         local-controller)]
    (cond-> {:players players
             :next-slot-id (next-lobby-slot-id players)
             :start-options (cond-> {:game-options (or game-options {})}
                              (contains? opts :demo-board-pieces)
                              (assoc :demo-board-pieces demo-board-pieces))}
      local-controller
      (assoc :local-controller local-controller)

      (lobby-validation-error {:players players})
      (assoc :error (lobby-validation-error {:players players})))))

(defn lobby [app-db]
  (:lobby app-db))

(defn lobby-active? [app-db]
  (some? (lobby app-db)))

(defn lobby-players [app-db]
  (get-in app-db [:lobby :players] []))

(defn clear-lobby-starting-bid [app-db]
  (if (lobby-active? app-db)
    (update app-db :lobby dissoc :starting-bid)
    app-db))

(defn refresh-lobby-error [app-db]
  (if-let [lobby (lobby app-db)]
    (let [validation-error (lobby-validation-error lobby)]
      (if validation-error
        (assoc-in app-db [:lobby :error] validation-error)
        (update app-db :lobby dissoc :error)))
    app-db))

(defn add-lobby-player [app-db]
  (let [app-db (clear-lobby-starting-bid app-db)
        players (lobby-players app-db)]
    (cond
      (not (lobby-active? app-db))
      app-db

      (<= game-state/max-players (count players))
      (assoc-in app-db [:lobby :error]
                (lobby-error :too-many-players
                             "Gnostica supports no more than six players."
                             {:count (count players)
                              :maximum game-state/max-players}))

      :else
      (if-let [player-id (first-available-player-id players)]
        (let [slot-id (get-in app-db [:lobby :next-slot-id]
                              (next-lobby-slot-id players))
              colour (player-colour player-id)
              player (with-player-controller {:slot-id slot-id
                                              :id player-id
                                              :name (:name colour)}
                       (get-in app-db [:lobby :local-controller]))]
          (-> app-db
              (update-in [:lobby :players] conj player)
              (assoc-in [:lobby :next-slot-id] (inc slot-id))
              refresh-lobby-error))
        (assoc-in app-db [:lobby :error]
                  (lobby-error :no-player-colours-available
                               "No unused player colours are available."
                               {:known-ids (mapv :id pieces/players)}))))))

(defn remove-lobby-player [app-db slot-id]
  (if-not (lobby-active? app-db)
    app-db
    (-> app-db
        clear-lobby-starting-bid
        (update-in [:lobby :players]
                   (fn [players]
                     (vec (remove #(= slot-id (:slot-id %)) players))))
        refresh-lobby-error)))

(defn set-lobby-player-name [app-db slot-id name]
  (if-not (lobby-active? app-db)
    app-db
    (-> app-db
        clear-lobby-starting-bid
        (update-in [:lobby :players]
                   (fn [players]
                     (mapv (fn [player]
                             (if (= slot-id (:slot-id player))
                               (assoc player :name (str name))
                               player))
                           players)))
        refresh-lobby-error)))

(defn set-lobby-player-colour [app-db slot-id player-id]
  (let [player-id (normalize-player-id player-id)
        app-db (clear-lobby-starting-bid app-db)
        players (lobby-players app-db)
        existing-player (some #(when (= slot-id (:slot-id %)) %) players)
        duplicate? (some #(and (not= slot-id (:slot-id %))
                               (= player-id (:id %)))
                         players)]
    (cond
      (not (lobby-active? app-db))
      app-db

      (nil? existing-player)
      (assoc-in app-db [:lobby :error]
                (lobby-error :unknown-lobby-player
                             "The selected lobby player no longer exists."
                             {:slot-id slot-id}))

      (not (known-player-id? player-id))
      (assoc-in app-db [:lobby :error]
                (lobby-error :unknown-player-colour
                             "Choose one of the known player colours."
                             {:player-id player-id
                              :known-ids (mapv :id pieces/players)}))

      duplicate?
      (assoc-in app-db [:lobby :error]
                (lobby-error :duplicate-player-colour
                             "Each player colour can only be used once."
                             {:player-id player-id}))

      :else
      (let [old-default-name (:name (player-colour (:id existing-player)))
            new-default-name (:name (player-colour player-id))]
        (-> app-db
            (update-in [:lobby :players]
                       (fn [players]
                         (mapv (fn [player]
                                 (if (= slot-id (:slot-id player))
                                   (cond-> (assoc player :id player-id)
                                     (= old-default-name (:name player))
                                     (assoc :name new-default-name))
                                   player))
                               players)))
            refresh-lobby-error)))))

(defn set-lobby-target-score [app-db target-score]
  (cond
    (not (lobby-active? app-db))
    app-db

    :else
    (if-let [target-score (normalize-target-score target-score)]
      (-> app-db
          clear-lobby-starting-bid
          (assoc-in [:lobby :start-options :game-options :target-score]
                    target-score)
          refresh-lobby-error)
      (assoc-in app-db [:lobby :error]
                (lobby-error :invalid-target-score
                             "Choose an 8, 9, or 10 point game."
                             {:target-score target-score
                              :allowed-target-scores db/target-score-options})))))
