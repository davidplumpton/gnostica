(ns gnostica.app.handlers
  (:require [gnostica.app-state :as app-state]
            [gnostica.deterministic-shuffle :as deterministic-shuffle]))

(defn- injected-shuffle-fn [{:keys [shuffle-fn shuffle-seed]}]
  (or shuffle-fn
      (when (some? shuffle-seed)
        (deterministic-shuffle/shuffle-fn shuffle-seed))))

(defn- add-initialize-shuffle [game-options injections]
  (let [game-options (or game-options {})
        shuffle-fn (injected-shuffle-fn injections)]
    (cond-> game-options
      (and shuffle-fn
           (not (contains? game-options :deck-order))
           (not (contains? game-options :shuffle-fn)))
      (assoc :shuffle-fn shuffle-fn))))

(defn initialize-options [opts injections]
  (let [opts (or opts {})]
    (if (and (:start-in-lobby? opts)
             (not (:bypass-lobby? opts)))
      opts
      (update opts
              :game-options
              add-initialize-shuffle
              injections))))

(defn initialize-db
  ([opts] (app-state/initialize opts))
  ([opts injections]
   (app-state/initialize (initialize-options opts injections))))

(defn transition-options [injections]
  (let [shuffle-fn (injected-shuffle-fn injections)]
    (cond-> {}
      shuffle-fn
      (assoc :shuffle-fn shuffle-fn))))

(defn confirm-move-db
  ([db] (app-state/confirm-move db))
  ([db injections]
   (app-state/confirm-move db (transition-options injections))))

(defn reveal-move-fool-card-db
  ([db] (app-state/reveal-move-fool-card db))
  ([db injections]
   (app-state/reveal-move-fool-card db (transition-options injections))))

(defn start-lobby-game-db
  ([db] (app-state/start-lobby-game db))
  ([db injections]
   (app-state/start-lobby-game db (transition-options injections))))

(defn start-lobby-bidding-db
  ([db] (app-state/start-lobby-bidding db))
  ([db injections]
   (app-state/start-lobby-bidding db (transition-options injections))))
