(ns gnostica.game-state.disc-major
  (:require [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.game-state.core :as core]
            [gnostica.game-state.cup :as cup]
            [gnostica.game-state.disc :as disc]
            [gnostica.game-state.major :as major]
            [gnostica.game-state.major-power :as major-power]
            [gnostica.game-state.placement :as placement]
            [gnostica.pieces :as pieces]))

(defn- resolve-disc-command* [state command source-opts]
  (disc/resolve-disc-command* state command source-opts))

(defn- resolve-disc-source-command
  ([state command]
   (resolve-disc-source-command state command {}))
  ([state command source-opts]
   (disc/resolve-disc-source-command state command source-opts)))

(defn- source-cost-state [state player-id source-result]
  (disc/source-cost-state state player-id source-result))

(defn- source-power-card-id [source-result]
  (disc/source-power-card-id source-result))

(defn- paid-disc-source-opts [source-result]
  (disc/paid-disc-source-opts source-result))

(defn- apply-resolved-disc-action [state player-id result opts]
  (disc/apply-resolved-disc-action state player-id result opts))

(defn- apply-single-disc-move
  ([state command]
   (apply-single-disc-move state command {}))
  ([state command opts]
   (disc/apply-single-disc-move state command opts)))

(defn- disc-action-command [command action]
  (let [source (cond-> (:source command)
                 (:piece-id action)
                 (assoc :piece-id (:piece-id action)))]
    (merge (select-keys command [:player-id :disc-variant])
           (dissoc action :piece-id)
           {:source source})))

(defn- same-disc-target? [left-result right-result]
  (let [left (get-in left-result [:command :target])
        right (get-in right-result [:command :target])]
    (and (= (:kind left) (:kind right))
         (case (:kind left)
           :piece (= (:piece-id left) (:piece-id right))
           :territory (= (:board-index left) (:board-index right))
           false))))

(defn- strength-disc-actions [command]
  (let [actions (:disc-actions command)]
    (cond
      (not (sequential? actions))
      (core/failure :invalid-disc-actions
                    "Strength Disc moves require a sequential :disc-actions collection."
                    {:disc-actions actions})

      (not (<= 1 (count actions) 2))
      (core/failure :invalid-disc-actions
                    "Strength can apply one or two Disc actions."
                    {:action-count (count actions)
                     :maximum 2})

      :else
      {:ok? true
       :actions (mapv #(disc-action-command command %) actions)})))

(defn- validate-strength-action-minion [state player-id minion-ids action]
  (let [piece-id (get-in action [:source :piece-id])
        piece (when piece-id
                (core/piece-by-id state piece-id))]
    (cond
      (nil? piece-id)
      (core/failure :missing-major-minion
                    "Strength Disc actions require an acting minion."
                    {:action action})

      (not (contains? minion-ids piece-id))
      (core/failure :invalid-major-minion
                    "The acting piece is not a minion for this Strength sequence."
                    {:piece-id piece-id
                     :available-minion-ids (vec (sort-by str minion-ids))
                     :action action})

      (nil? piece)
      (core/failure :invalid-piece
                    "The acting Strength minion must still be on the board."
                    {:piece-id piece-id})

      (not= player-id (:player-id piece))
      (core/failure :invalid-piece
                    "Strength minions must belong to the move's player."
                    {:piece-id piece-id
                     :player-id player-id
                     :piece-player-id (:player-id piece)})

      :else
      {:ok? true
       :piece piece
       :piece-id piece-id})))

(defn- strength-affected-piece-ids [player-id result]
  (vec
   (concat
    (:affected-piece-ids result)
    (map :id (:affected-pieces result))
    (keep (fn [event]
            (let [piece (:piece event)]
              (when (= player-id (:player-id piece))
                (:id piece))))
          (:events result)))))

(defn- shortcut-result [left-result right-result]
  (let [right-command (:command right-result)]
    (cond-> (assoc left-result
                   :command (merge (:command left-result)
                                   (select-keys right-command
                                                [:replacement-card-source
                                                 :replacement-card-id
                                                 :orientation])))
      (:orientation right-command)
      (assoc-in [:command :target :orientation] (:orientation right-command)))))

(defn- apply-strength-shortcut [state player-id left-result right-result]
  (let [result (shortcut-result left-result right-result)]
    (apply-resolved-disc-action state
                                player-id
                                result
                                {:action-count 2
                                 :charge-source? false
                                 :shortcut? true})))

(defn- apply-strength-actions-sequential [state player-id actions source-opts minion-ids]
  (loop [current-state state
         current-minion-ids minion-ids
         remaining actions
         events []]
    (if-let [action (first remaining)]
      (let [minion-result (validate-strength-action-minion current-state
                                                           player-id
                                                           current-minion-ids
                                                           action)]
        (if-not (:ok? minion-result)
          minion-result
          (let [result (resolve-disc-command* current-state action source-opts)]
            (if-not (:ok? result)
              result
              (let [applied (apply-resolved-disc-action current-state
                                                        player-id
                                                        result
                                                        {:charge-source? false})]
                (if-not (:ok? applied)
                  applied
                  (recur (:state applied)
                         (into current-minion-ids
                               (strength-affected-piece-ids player-id applied))
                         (rest remaining)
                         (into events (:events applied)))))))))
      (core/success current-state events))))

(defn- apply-strength-disc-move
  ([state command]
   (apply-strength-disc-move state command {}))
  ([state command source-opts]
   (let [source-result (resolve-disc-source-command state command source-opts)]
     (if-not (:ok? source-result)
       source-result
       (let [actions-result (strength-disc-actions command)
             player-id (:player-id command)]
         (cond
           (not= "strength" (source-power-card-id source-result))
           (core/failure :disc-actions-unavailable
                         "Only Strength can apply multiple Disc actions."
                         {:card-id (source-power-card-id source-result)
                          :source (:source command)})

           (not (:ok? actions-result))
           actions-result

           :else
           (let [major-source-result (major/resolve-major-source state
                                                                 command
                                                                 source-opts)]
             (if-not (:ok? major-source-result)
               major-source-result
               (let [cost-state (source-cost-state state player-id source-result)
                     actions (:actions actions-result)
                     source-opts (paid-disc-source-opts source-result)
                     minion-ids (set (:minion-ids major-source-result))]
                 (if (= 2 (count actions))
                   (let [left-minion-result
                         (validate-strength-action-minion cost-state
                                                          player-id
                                                          minion-ids
                                                          (first actions))
                         left-result (when (:ok? left-minion-result)
                                       (resolve-disc-command* cost-state
                                                              (first actions)
                                                              source-opts))
                         right-minion-result
                         (when (and (:ok? left-result)
                                    (contains? minion-ids
                                               (get-in (second actions)
                                                       [:source :piece-id])))
                           (validate-strength-action-minion cost-state
                                                            player-id
                                                            minion-ids
                                                            (second actions)))
                         right-result (when (and (:ok? left-result)
                                                 (:ok? right-minion-result))
                                        (resolve-disc-command* cost-state
                                                               (second actions)
                                                               source-opts))]
                     (cond
                       (not (:ok? left-minion-result))
                       left-minion-result

                       (not (:ok? left-result))
                       left-result

                       (and (:ok? right-result)
                            (same-disc-target? left-result right-result))
                       (apply-strength-shortcut cost-state
                                                player-id
                                                left-result
                                                right-result)

                       :else
                       (apply-strength-actions-sequential cost-state
                                                          player-id
                                                          actions
                                                          source-opts
                                                          minion-ids)))
                   (apply-strength-actions-sequential cost-state
                                                      player-id
                                                      actions
                                                      source-opts
                                                      minion-ids)))))))))))

(defn- apply-star-disc-move
  ([state command]
   (apply-star-disc-move state command {}))
  ([state command source-opts]
   (let [source-result (resolve-disc-source-command state command source-opts)]
     (if-not (:ok? source-result)
       source-result
       (if-not (= "star" (source-power-card-id source-result))
         (core/failure :disc-orient-unavailable
                       "Only Star Disc can orient a minion before applying Disc."
                       {:card-id (source-power-card-id source-result)
                        :source (:source command)})
         (let [orientation (:minion-orientation command)
               orient-result (placement/apply-orient-move
                              state
                              {:player-id (:player-id command)
                               :piece-id (get-in command [:source :piece-id])
                               :orientation orientation})]
           (if-not (:ok? orient-result)
             orient-result
             (let [disc-result (apply-single-disc-move
                                (:state orient-result)
                                (dissoc command :minion-orientation)
                                {:source-opts source-opts})]
               (if-not (:ok? disc-result)
                 disc-result
                 (core/success (:state disc-result)
                               (concat (:events orient-result)
                                       (:events disc-result))))))))))))

(defn- cup-created-piece-id [cup-result]
  (some (fn [event]
          (when (contains? #{:cup/small-piece-created}
                           (:type event))
            (get-in event [:piece :id])))
        (:events cup-result)))

(defn- cup-created-territory-index [cup-result]
  (some (fn [event]
          (when (= :cup/territory-created (:type event))
            (:board-index event)))
        (:events cup-result)))

(defn- sun-cup-command [command]
  (merge {:player-id (:player-id command)
          :source (:source command)
          :cup-variant :cup}
         (:cup command)))

(defn- sun-disc-target [disc-action cup-result]
  (case (get-in disc-action [:target :kind])
    :created-piece
    (when-let [piece-id (cup-created-piece-id cup-result)]
      {:kind :piece
       :piece-id piece-id})

    :created-territory
    (when-let [board-index (cup-created-territory-index cup-result)]
      {:kind :territory
       :board-index board-index})

    (:target disc-action)))

(defn- sun-disc-source [command disc-action cup-result]
  (let [created-piece-id (cup-created-piece-id cup-result)
        piece-id (or (:piece-id disc-action)
                     (when (= :created-piece (get-in disc-action [:target :kind]))
                       created-piece-id)
                     (get-in command [:source :piece-id]))]
    (assoc (:source command) :piece-id piece-id)))

(defn- sun-disc-command [command cup-result]
  (let [disc-action (:disc command)
        disc-target (sun-disc-target disc-action cup-result)]
    (when disc-target
      (-> (merge {:player-id (:player-id command)
                  :disc-variant :disc}
                 (dissoc disc-action :piece-id :target))
          (assoc :source (sun-disc-source command
                                          disc-action
                                          cup-result)
                 :target disc-target)))))

(defn- sun-created-piece-shortcut? [command]
  (and (= :territory (get-in command [:cup :target :kind]))
       (= :created-piece (get-in command [:disc :target :kind]))))

(defn- sun-created-territory-shortcut? [command]
  (and (= :wasteland (get-in command [:cup :target :kind]))
       (= :created-territory (get-in command [:disc :target :kind]))))

(defn- apply-sun-created-piece-shortcut [state command source-result]
  (let [player-id (:player-id command)
        target (get-in command [:cup :target])
        board-index (:board-index target)
        cell (core/board-cell-by-index state board-index)
        orientation (or (get-in command [:disc :orientation])
                        (get-in command [:cup :orientation]))
        target-result (when cell
                        (cup/validate-target-coordinate
                         state
                         source-result
                         target
                         (select-keys cell [:row :col])))
        target-pieces (when cell
                        (core/pieces-at-board-index state board-index))]
    (cond
      (nil? cell)
      (core/failure :invalid-target-territory
                    "Sun piece shortcuts must target an existing territory."
                    {:target target})

      (not (:ok? target-result))
      target-result

      (not (contains? pieces/legal-orientations orientation))
      (core/failure :invalid-orientation
                    "Sun piece shortcuts require a legal final orientation."
                    {:orientation orientation
                     :legal-orientations pieces/legal-orientations})

      (<= pieces/max-pieces-per-space (count target-pieces))
      (core/failure :target-territory-full
                    "Sun piece shortcuts require fewer than three pieces on the target territory."
                    {:board-index board-index
                     :maximum pieces/max-pieces-per-space})

      (not (pos? (core/stash-count state player-id :medium)))
      (core/failure :no-larger-piece-available
                    "Sun piece shortcuts require a medium piece in the player's stash."
                    {:player-id player-id
                     :to-size :medium})

      :else
      (let [piece {:id (core/next-piece-id state player-id :medium)
                   :player-id player-id
                   :space-index board-index
                   :size :medium
                   :orientation orientation}
            event {:type :sun/piece-created-and-grown
                   :player-id player-id
                   :source (core/source-summary (:source source-result))
                   :cup-target {:kind :territory
                                :board-index board-index}
                   :disc-target {:kind :created-piece}
                   :shortcut? true
                   :piece piece}
            cost-state (source-cost-state state player-id source-result)
            next-state (-> cost-state
                           (core/decrement-stash player-id :medium)
                           (update-in [:pieces :on-board] conj piece)
                           (core/append-history event))]
        (core/success next-state [event])))))

(defn- apply-sun-created-territory-shortcut [state command source-result]
  (let [player-id (:player-id command)
        target (get-in command [:cup :target])
        {:keys [row col] :as normalized-target} (core/wasteland-target target)
        replacement-card-source (or (get-in command [:disc :replacement-card-source])
                                    :hand)
        replacement-card-id (get-in command [:disc :replacement-card-id])
        source-card (:source-card source-result)
        target-result (when normalized-target
                        (cup/validate-target-coordinate
                         state
                         source-result
                         normalized-target
                         normalized-target))]
    (cond
      (nil? normalized-target)
      (core/failure :invalid-cup-target
                    "Sun territory shortcuts target an explicit wasteland coordinate."
                    {:target target})

      (some? (core/board-cell-at state row col))
      (core/failure :target-not-wasteland
                    "Sun territory shortcuts must target an empty wasteland space."
                    {:target normalized-target})

      (not (core/wasteland-target? state normalized-target))
      (core/failure :target-not-wasteland
                    "Sun territory shortcuts cannot target the void."
                    {:target normalized-target})

      (not (:ok? target-result))
      target-result

      (seq (core/enemy-pieces-at-coordinate state player-id row col))
      (core/failure :wasteland-occupied-by-enemy
                    "Sun territory shortcuts cannot target a wasteland occupied by enemy pieces."
                    {:target normalized-target
                     :enemy-piece-ids (mapv :id (core/enemy-pieces-at-coordinate
                                                 state
                                                 player-id
                                                 row
                                                 col))})

      (not= :hand replacement-card-source)
      (core/failure :invalid-disc-replacement-card-source
                    "Sun territory shortcuts currently require a hand replacement card."
                    {:replacement-card-source replacement-card-source
                     :valid-sources #{:hand}})

      (= (:id source-card) replacement-card-id)
      (core/failure :card-already-used
                    "A played source card cannot also become the shortcut territory."
                    {:card-id replacement-card-id})

      :else
      (let [cost-state (source-cost-state state player-id source-result)
            replacement-card (core/player-hand-card cost-state
                                                    player-id
                                                    replacement-card-id)
            replacement-value (cards/card-point-value replacement-card)]
        (cond
          (nil? replacement-card)
          (core/failure :invalid-disc-replacement-card
                        "Sun territory shortcuts require a replacement card from the player's hand."
                        {:card-id replacement-card-id
                         :player-id player-id})

          (not= 2 replacement-value)
          (core/failure :invalid-disc-replacement-card
                        "Sun territory shortcuts require a royalty replacement card."
                        {:replacement-card-id (:id replacement-card)
                         :replacement-value replacement-value})

          :else
          (let [board-index (core/next-board-index state)
                cell {:index board-index
                      :row row
                      :col col
                      :orientation (board/orientation-for row col)
                      :face :up
                      :card replacement-card}
                event {:type :sun/territory-created-and-grown
                       :player-id player-id
                       :source (core/source-summary (:source source-result))
                       :target normalized-target
                       :board-index board-index
                       :replacement-card-id (:id replacement-card)
                       :to-value replacement-value
                       :shortcut? true
                       :territory cell}
                next-state (-> cost-state
                               (core/remove-card-from-hand player-id replacement-card-id)
                               (update :board conj cell)
                               (core/move-wasteland-pieces-to-board-index row col board-index)
                               (core/append-history event))]
            (core/success next-state [event])))))))

(defn- apply-sun-shortcut [state command source-result]
  (cond
    (sun-created-piece-shortcut? command)
    (apply-sun-created-piece-shortcut state command source-result)

    (sun-created-territory-shortcut? command)
    (apply-sun-created-territory-shortcut state command source-result)

    :else
    nil))

(defn apply-sun-move-with-opts
  ([state command]
   (apply-sun-move-with-opts state command {}))
  ([state command {:keys [source-opts]
                   :or {source-opts {}}}]
   (let [source-result (resolve-disc-source-command state command source-opts)]
     (if-not (:ok? source-result)
       source-result
       (cond
         (not= "sun" (source-power-card-id source-result))
         (core/failure :sun-actions-unavailable
                       "Only Sun can apply Cup followed by Disc."
                       {:card-id (source-power-card-id source-result)
                        :source (:source command)})

         (nil? (:cup command))
         (core/failure :invalid-sun-command
                       "Sun moves require a :cup action map."
                       {:command command})

         :else
         (if-let [shortcut-result (apply-sun-shortcut state command source-result)]
           shortcut-result
           (let [cup-result (cup/apply-cup-move-with-opts
                             state
                             (sun-cup-command command)
                             {:source-opts source-opts})]
             (if-not (:ok? cup-result)
               cup-result
               (if-not (:disc command)
                 cup-result
                 (if-let [disc-command (sun-disc-command command cup-result)]
                   (let [disc-result (apply-single-disc-move
                                      (:state cup-result)
                                      disc-command
                                      {:source-opts (paid-disc-source-opts
                                                     source-result)
                                       :charge-source? false})]
                     (if-not (:ok? disc-result)
                       disc-result
                       (core/success (:state disc-result)
                                     (concat (:events cup-result)
                                             (:events disc-result)))))
                   (core/failure :invalid-sun-disc-target
                                 "Sun Disc targets that refer to a newly-created Cup target require that Cup action to create that target."
                                 {:disc (:disc command)
                                  :cup-events (:events cup-result)})))))))))))

(defn apply-sun-move [state command]
  (apply-sun-move-with-opts state command {}))

(defn apply-disc-move-with-opts
  ([state command]
   (apply-disc-move-with-opts state command {}))
  ([state command {:keys [source-opts] :as opts
                   :or {source-opts {}}}]
   (cond
     (contains? command :disc-actions)
     (apply-strength-disc-move state command source-opts)

     (contains? command :minion-orientation)
     (apply-star-disc-move state command source-opts)

     :else
     (disc/apply-disc-move-with-opts state command opts))))

(defn apply-disc-move [state command]
  (apply-disc-move-with-opts state command {}))

(defmethod major-power/apply-card-power "sun"
  [state command _card {:keys [source-opts]}]
  (apply-sun-move-with-opts state command {:source-opts source-opts}))

(defmethod major-power/apply-card-power "strength"
  [state command _card {:keys [source-opts]}]
  (apply-disc-move-with-opts state command {:source-opts source-opts}))

(defmethod major-power/apply-card-power "star"
  [state command _card {:keys [source-opts]}]
  (apply-disc-move-with-opts state command {:source-opts source-opts}))
