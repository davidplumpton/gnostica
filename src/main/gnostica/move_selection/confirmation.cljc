(ns gnostica.move-selection.confirmation
  (:require [gnostica.game-state :as game-state]
            [gnostica.move-selection.context :as context]
            [gnostica.move-selection.registry :as registry]))

(def required-context-keys
  #{:empty-move-selection
    :game
    :game-turn-key
    :move-command
    :move-error
    :move-power
    :move-ready?
    :move-selection
    :move-source
    :selected-world-copied-power
    :source-unavailable-reason
    :update-move-selection
    :world-move?})

(defn make-context [deps]
  (context/make "gnostica.move-selection.confirmation" required-context-keys deps))

(defn- call [ctx key & args]
  (apply context/call ctx key args))

(defn- command-uses-draw-pile-territory-card? [value]
  (cond
    (map? value)
    (or (= :draw-pile-top (:territory-card-source value))
        (some command-uses-draw-pile-territory-card? (vals value)))

    (sequential? value)
    (some command-uses-draw-pile-territory-card? value)

    :else
    false))

(defn- command-with-transition-options [command transition-options power]
  (cond-> command
    (and (or (= :draw-cards (:source command))
             (contains? #{:fool :high-priestess} power)
             (command-uses-draw-pile-territory-card? command))
         (:shuffle-fn transition-options)
         (not (contains? command :shuffle-fn)))
    (assoc :shuffle-fn (:shuffle-fn transition-options))))

(defn- transition-power [ctx db]
  (let [{:keys [source params]} (call ctx :move-selection db)]
    (if (call ctx :world-move? db source params)
      (call ctx :selected-world-copied-power db source params)
      (call ctx :move-power db))))

(defn confirmed-move-result [ctx db command transition-options]
  (let [power (transition-power ctx db)
        command (command-with-transition-options command transition-options power)
        move-power (call ctx :move-power db)
        transition-fn (registry/power-transition-fn move-power)]
    (cond
      (= :draw-cards (:source command))
      (game-state/apply-draw-move (call ctx :game db) command)

      (= :orient-piece (:source command))
      (game-state/apply-orient-move (call ctx :game db) command)

      (= :place-initial-small (:source command))
      (game-state/apply-initial-placement (call ctx :game db) command)

      transition-fn
      (transition-fn (call ctx :game db) command)

      :else
      (game-state/failure :move-transition-unavailable
                          "Move selection is complete, but this gameplay rule transition is not implemented yet."
                          {:command command}))))

(defn- previewable-move? [ctx db]
  (let [source (call ctx :move-source db)
        power (transition-power ctx db)]
    (or (contains? #{:orient-piece :place-initial-small} source)
        (registry/previewable-power? power))))

(defn move-preview-result
  ([ctx db] (move-preview-result ctx db {}))
  ([ctx db transition-options]
   (when (and (call ctx :move-ready? db)
              (previewable-move? ctx db))
     (let [result (confirmed-move-result
                   ctx
                   db
                   (call ctx :move-command db)
                   transition-options)]
       (dissoc result :state)))))

(defn- consumed-turn-action [ctx db state]
  {:consumed? true
   :turn-key (call ctx :game-turn-key state)
   :source (call ctx :move-source db)})

(defn- apply-confirmed-move-result [ctx db result]
  (if (:ok? result)
    (assoc db
           :game (:state result)
           :turn-action (consumed-turn-action ctx db (:state result))
           :move-selection (assoc (call ctx :empty-move-selection)
                                  :last-result result))
    (assoc db :move-selection
           (assoc (call ctx :move-selection db)
                  :stage :rejected
                  :error (:error result)
                  :last-result result))))

(defn confirm-move
  ([ctx db] (confirm-move ctx db {}))
  ([ctx db transition-options]
   (if-not (call ctx :move-ready? db)
     (call ctx
           :update-move-selection
           db
           assoc
           :error
           (call ctx
                 :move-error
                 :incomplete-move
                 "Complete the move selection before confirming."
                 {:stage (:stage (call ctx :move-selection db))}))
     (if-let [reason (call ctx :source-unavailable-reason db (call ctx :move-source db))]
       (apply-confirmed-move-result
        ctx
        db
        (game-state/failure :move-source-unavailable
                            reason
                            {:source (call ctx :move-source db)}))
       (let [command (call ctx :move-command db)
             result (confirmed-move-result ctx db command transition-options)]
         (apply-confirmed-move-result ctx db result))))))
