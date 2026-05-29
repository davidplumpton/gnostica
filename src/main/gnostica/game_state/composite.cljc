(ns gnostica.game-state.composite
  (:require [gnostica.game-state.core :as core]
            [gnostica.game-state.cup :as cup]
            [gnostica.game-state.major :as major]
            [gnostica.game-state.major-power :as major-power]
            [gnostica.game-state.manipulation :as manipulation]
            [gnostica.game-state.placement :as placement]
            [gnostica.game-state.rod :as rod]
            [gnostica.pieces :as pieces]))

(defn- action-piece-id [command action]
  (or (:piece-id action)
      (get-in command [:source :piece-id])))

(defn- with-default-piece [command power action]
  (when action
    (assoc action
           :power power
           :piece-id (action-piece-id command action))))

(defn- selected-action [command keys]
  (let [action (select-keys command keys)]
    (when (seq action)
      action)))

(defn- cup-action [command]
  (or (:cup command)
      (selected-action command
                       [:target :orientation :one-point-card-id
                        :territory-card-source])))

(defn- rod-action [command]
  (or (:rod command)
      (selected-action command
                       [:mode :target :distance :orientation])))

(defn- orient-action [command]
  (when (contains? command :minion-orientation)
    {:orientation (:minion-orientation command)}))

(defn- trade-action [command]
  (or (when-let [target (:hand-trade-target command)]
        {:target target})
      (when-let [piece-id (:hand-trade-target-piece-id command)]
        {:target {:kind :piece
                  :piece-id piece-id}})))

(defn- command-with-actions [command actions]
  (if (contains? command :actions)
    command
    (assoc command :actions (vec (remove nil? actions)))))

(defn- command-with-empress-actions [command]
  (command-with-actions
   command
   [(with-default-piece command :orient-minion (orient-action command))
    (with-default-piece command :cup (cup-action command))]))

(defn- command-with-emperor-actions [command]
  (command-with-actions
   command
   [(with-default-piece command :orient-minion (orient-action command))
    (with-default-piece command :rod (rod-action command))]))

(defn- command-with-lovers-actions [command]
  (command-with-actions
   command
   [(with-default-piece command :rod (:rod command))
    (with-default-piece command :cup (:cup command))]))

(defn- command-with-chariot-actions [command]
  (if (contains? command :actions)
    command
    (assoc command
           :actions
           (mapv #(with-default-piece command :rod %)
                 (:rod-actions command)))))

(defn- command-with-hanged-man-actions [command]
  (let [rod (with-default-piece command :rod (:rod command))
        trade (with-default-piece command :trade-hand (trade-action command))]
    (command-with-actions
     command
     [rod
      (cond-> trade
        (and trade (nil? (:piece-id trade)) (:piece-id rod))
        (assoc :piece-id (:piece-id rod)))])))

(defn- command-with-temperance-actions [command]
  (if (contains? command :actions)
    command
    (assoc command
           :actions
           (mapv #(with-default-piece command :cup %)
                 (:cup-actions command)))))

(defn- source-opts [context]
  (assoc (:source-opts context) :allow-major-minion? true))

(defn- owned-event-piece-ids [player-id events]
  (->> events
       (keep (fn [event]
               (let [piece (:piece event)]
                 (when (= player-id (:player-id piece))
                   (:id piece)))))
       distinct
       vec))

(defn- with-affected-pieces [result player-id]
  (if-not (:ok? result)
    result
    (let [affected-piece-ids (owned-event-piece-ids player-id (:events result))]
      (cond-> result
        (seq affected-piece-ids)
        (assoc :affected-piece-ids affected-piece-ids)))))

(defn- clean-action [action]
  (dissoc action :power :piece-id :major/power-index))

(defn- apply-orient-action [state context action]
  (let [player-id (:player-id context)
        result (placement/apply-orient-move
                state
                {:player-id player-id
                 :piece-id (:piece-id action)
                 :orientation (:orientation action)})]
    (with-affected-pieces result player-id)))

(defn- apply-cup-action [state context action]
  (let [player-id (:player-id context)
        command (merge (clean-action action)
                       {:player-id player-id
                        :source (:action-source context)
                        :cup-variant (:cup-variant (:spec context))})
        result (cup/apply-cup-move-with-opts
                state
                command
                {:source-opts (source-opts context)})]
    (with-affected-pieces result player-id)))

(defn- apply-rod-action
  ([state context action]
   (apply-rod-action state context action {}))
  ([state context action opts]
   (let [player-id (:player-id context)
         command (merge (clean-action action)
                        {:player-id player-id
                         :source (:action-source context)
                         :rod-variant (:rod-variant (:spec context))})
         result (rod/apply-rod-move-with-opts
                 state
                 command
                 (merge {:source-opts (source-opts context)}
                        opts))]
     (with-affected-pieces result player-id))))

(defn- coordinate-map [coordinate]
  (cond
    (map? coordinate)
    (when (and (int? (:row coordinate))
               (int? (:col coordinate)))
      (select-keys coordinate [:row :col]))

    (sequential? coordinate)
    (let [[row col] coordinate]
      (when (and (int? row)
                 (int? col))
        {:row row
         :col col}))))

(defn- same-coordinate? [left right]
  (= (coordinate-map left)
     (coordinate-map right)))

(defn- targetable-coordinate? [actor-coordinate target-coordinate orientation target-self?]
  (or target-self?
      (same-coordinate? target-coordinate
                        (manipulation/target-coordinate actor-coordinate
                                                        orientation))))

(defn- hand-trade-target [action]
  (or (:target action)
      (when-let [piece-id (:target-piece-id action)]
        {:kind :piece
         :piece-id piece-id})))

(defn- resolve-hand-trade-target [state context action]
  (let [target (hand-trade-target action)
        minion (:action-minion context)
        target-piece-id (:piece-id target)
        target-piece (when target-piece-id
                       (core/piece-by-id state target-piece-id))
        source-coordinate (coordinate-map (core/piece-coordinate state minion))
        target-coordinate (when target-piece
                            (coordinate-map (core/piece-coordinate state target-piece)))
        target-self? (= (:id minion) (:id target-piece))]
    (cond
      (not (map? target))
      (core/failure :invalid-hand-trade-target
                    "Hanged Man hand trades require a target piece map."
                    {:target target})

      (not= :piece (:kind target))
      (core/failure :invalid-hand-trade-target
                    "Hanged Man hand trades target :kind :piece."
                    {:target target})

      (nil? target-piece-id)
      (core/failure :invalid-hand-trade-target
                    "Hanged Man hand trades require a target piece id."
                    {:target target})

      (nil? target-piece)
      (core/failure :invalid-hand-trade-target
                    "Hanged Man hand trades must reference a piece on the board."
                    {:target target})

      (nil? source-coordinate)
      (core/failure :invalid-piece-space
                    "Hanged Man hand trades require an acting minion with a board coordinate."
                    {:piece-id (:id minion)
                     :space-index (:space-index minion)
                     :space (:space minion)})

      (nil? target-coordinate)
      (core/failure :invalid-piece-space
                    "Hanged Man hand-trade targets must have a board coordinate."
                    {:piece-id (:id target-piece)
                     :space-index (:space-index target-piece)
                     :space (:space target-piece)})

      (not (contains? pieces/legal-orientations (:orientation minion)))
      (core/failure :invalid-major-target-orientation
                    "Hanged Man hand trades require the acting minion to have a legal orientation."
                    {:piece-id (:id minion)
                     :orientation (:orientation minion)
                     :legal-orientations pieces/legal-orientations})

      (not (targetable-coordinate? source-coordinate
                                   target-coordinate
                                   (:orientation minion)
                                   target-self?))
      (core/failure :invalid-hand-trade-target
                    "Hanged Man can trade only with the owner of a piece targeted by the minion."
                    {:piece-id (:id target-piece)
                     :orientation (:orientation minion)
                     :source-coordinate source-coordinate
                     :target-coordinate target-coordinate
                     :expected-coordinate (manipulation/target-coordinate
                                           source-coordinate
                                           (:orientation minion))})

      :else
      {:ok? true
       :target-piece target-piece
       :target {:kind :piece
                :piece-id (:id target-piece)
                :player-id (:player-id target-piece)
                :row (:row target-coordinate)
                :col (:col target-coordinate)}})))

(defn- card-ids [cards]
  (mapv :id cards))

(defn- swap-player-hands [state left-player-id right-player-id]
  (let [left-hand (get-in state [:players-by-id left-player-id :hand])
        right-hand (get-in state [:players-by-id right-player-id :hand])]
    (-> state
        (core/update-player left-player-id assoc :hand (vec right-hand))
        (core/update-player right-player-id assoc :hand (vec left-hand)))))

(defn- apply-hand-trade-action [state context action]
  (let [target-result (resolve-hand-trade-target state context action)]
    (if-not (:ok? target-result)
      target-result
      (let [player-id (:player-id context)
            other-player-id (get-in target-result [:target-piece :player-id])
            player-hand (get-in state [:players-by-id player-id :hand])
            other-hand (get-in state [:players-by-id other-player-id :hand])
            event {:type :hanged-man/hands-traded
                   :player-id player-id
                   :source (core/source-summary (:action-source context))
                   :target (:target target-result)
                   :with-player-id other-player-id
                   :player-hand-card-ids (card-ids player-hand)
                   :other-hand-card-ids (card-ids other-hand)}
            next-state (-> state
                           (swap-player-hands player-id other-player-id)
                           (core/append-history event))]
        (core/success next-state [event])))))

(defn- apply-composite-action [state context action]
  (case (:power action)
    :orient-minion
    (apply-orient-action state context action)

    :cup
    (apply-cup-action state context action)

    :rod
    (apply-rod-action state context action)

    :trade-hand
    (apply-hand-trade-action state context action)

    (core/failure :invalid-composite-major-action
                  "Composite suit major powers support orient, Cup, Rod, and hand-trade actions."
                  {:action action})))

(defn- rod-piece-action? [action]
  (contains? #{:move-minion :push-piece} (:mode action)))

(defn- rod-moved-piece-id [action]
  (when (rod-piece-action? action)
    (case (:mode action)
      :move-minion (:piece-id action)
      :push-piece (get-in action [:target :piece-id]))))

(defn- chariot-shortcut-key [action]
  (when (= :rod (:power action))
    (rod-moved-piece-id action)))

(defn- valid-rod-distance [piece distance]
  (let [maximum (or (pieces/pips piece) 0)]
    (cond
      (not (int? distance))
      (core/failure :invalid-rod-distance
                    "Chariot Rod shortcuts require integer distances."
                    {:distance distance
                     :maximum maximum})

      (not (pos? distance))
      (core/failure :invalid-rod-distance
                    "Chariot Rod shortcuts cannot move zero spaces."
                    {:distance distance
                     :maximum maximum})

      (< maximum distance)
      (core/failure :invalid-rod-distance
                    "Each Chariot Rod action cannot exceed its acting minion's pip count."
                    {:distance distance
                     :maximum maximum})

      :else
      {:ok? true
       :distance distance})))

(defn- validate-orientation [orientation message data]
  (if (contains? pieces/legal-orientations orientation)
    {:ok? true
     :orientation orientation}
    (core/failure :invalid-orientation
                  message
                  (assoc data
                         :orientation orientation
                         :legal-orientations pieces/legal-orientations))))

(defn- validate-rod-direction [piece direction]
  (cond
    (= :up direction)
    (core/failure :rod-minion-upright
                  "A piece standing upright may not use a Rod."
                  {:piece-id (:id piece)
                   :orientation direction})

    (not (contains? pieces/cardinal-orientations direction))
    (core/failure :invalid-rod-direction
                  "Rod moves require the acting minion to point in a cardinal direction."
                  {:piece-id (:id piece)
                   :orientation direction
                   :legal-directions pieces/cardinal-orientations})

    :else
    {:ok? true
     :direction direction}))

(defn- chariot-final-piece-space [state moved-piece destination]
  (let [{:keys [row col]} destination]
    (cond
      (not (and (int? row) (int? col)))
      (core/failure :invalid-rod-destination
                    "Chariot Rod shortcuts require a final destination coordinate."
                    {:piece-id (:id moved-piece)
                     :destination destination})

      :else
      (if-let [cell (core/board-cell-at state row col)]
        (let [space-pieces (remove #(= (:id moved-piece) (:id %))
                                   (core/pieces-at-coordinate state row col))]
          (if (<= pieces/max-pieces-per-space (count space-pieces))
            (core/failure :target-territory-full
                          "Chariot Rod shortcuts may pass through full territories but may not end in one."
                          {:piece-id (:id moved-piece)
                           :board-index (:index cell)
                           :row row
                           :col col
                           :piece-ids (mapv :id space-pieces)
                           :maximum pieces/max-pieces-per-space})
            {:ok? true
             :piece-space {:space-index (:index cell)}
             :destination {:kind :territory
                           :board-index (:index cell)
                           :row row
                           :col col}}))
        (let [target {:kind :wasteland
                      :row row
                      :col col}]
          (if (core/wasteland-target? state target)
            {:ok? true
             :piece-space {:space target}
             :destination target}
            (core/failure :rod-destination-void
                          "Chariot Rod shortcuts may pass through void spaces but may not end in the void."
                          {:piece-id (:id moved-piece)
                           :destination destination})))))))

(defn- move-piece-to-space [piece piece-space orientation]
  (let [piece (cond-> piece
                orientation (assoc :orientation orientation))]
    (if-let [space-index (:space-index piece-space)]
      (-> piece
          (dissoc :space)
          (assoc :space-index space-index))
      (-> piece
          (dissoc :space-index)
          (assoc :space (:space piece-space))))))

(defn- supported-chariot-shortcut? [context left-action right-action moved-piece]
  (and (= (:player-id context) (:player-id moved-piece))
       (= (:piece-id right-action) (:id moved-piece))
       (rod-piece-action? left-action)
       (rod-piece-action? right-action)))

(defn- apply-chariot-rod-shortcut [state context left-action right-action]
  (let [moved-piece-id (rod-moved-piece-id left-action)
        moved-piece (when moved-piece-id
                      (core/piece-by-id state moved-piece-id))]
    (when (and moved-piece
               (= moved-piece-id (rod-moved-piece-id right-action))
               (supported-chariot-shortcut? context
                                            left-action
                                            right-action
                                            moved-piece))
      (let [left-minion (:action-minion context)
            left-direction (:orientation left-minion)
            moved-coordinate (coordinate-map (core/piece-coordinate state moved-piece))
            left-minion-coordinate (coordinate-map (core/piece-coordinate state left-minion))
            target-self? (= (:id left-minion) (:id moved-piece))
            first-orientation (or (:orientation left-action)
                                  (:orientation moved-piece))
            final-orientation (or (:orientation right-action)
                                  first-orientation)
            left-distance-result (valid-rod-distance left-minion (:distance left-action))
            right-distance-result (valid-rod-distance moved-piece (:distance right-action))
            first-orientation-result (validate-orientation
                                      first-orientation
                                      "Chariot first Rod action requires a legal piece orientation."
                                      {:piece-id (:id moved-piece)})
            final-orientation-result (validate-orientation
                                      final-orientation
                                      "Chariot second Rod action requires a legal piece orientation."
                                      {:piece-id (:id moved-piece)})
            left-direction-result (validate-rod-direction left-minion left-direction)
            right-direction-result (validate-rod-direction
                                    (assoc moved-piece :orientation first-orientation)
                                    first-orientation)]
        (cond
          (nil? moved-coordinate)
          (core/failure :invalid-piece-space
                        "Chariot Rod shortcuts require the moved piece to have a board coordinate."
                        {:piece-id (:id moved-piece)})

          (nil? left-minion-coordinate)
          (core/failure :invalid-piece-space
                        "Chariot Rod shortcuts require the acting minion to have a board coordinate."
                        {:piece-id (:id left-minion)})

          (not (targetable-coordinate? left-minion-coordinate
                                       moved-coordinate
                                       left-direction
                                       target-self?))
          (core/failure :invalid-rod-target
                        "Chariot Rod shortcuts require the first Rod action to move the acting minion or an adjacent target piece."
                        {:piece-id (:id moved-piece)
                         :direction left-direction
                         :source-coordinate left-minion-coordinate
                         :target-coordinate moved-coordinate
                         :expected-coordinate (manipulation/target-coordinate
                                               left-minion-coordinate
                                               left-direction)})

          (not (:ok? left-distance-result))
          left-distance-result

          (not (:ok? right-distance-result))
          right-distance-result

          (not (:ok? first-orientation-result))
          first-orientation-result

          (not (:ok? final-orientation-result))
          final-orientation-result

          (not (:ok? left-direction-result))
          left-direction-result

          (not (:ok? right-direction-result))
          right-direction-result

          :else
          (let [intermediate (rod/rod-destination-coordinate
                              moved-coordinate
                              left-direction
                              (:distance left-action))
                final-destination (rod/rod-destination-coordinate
                                   intermediate
                                   first-orientation
                                   (:distance right-action))
                destination-result (chariot-final-piece-space
                                    state
                                    moved-piece
                                    final-destination)]
            (if-not (:ok? destination-result)
              destination-result
              (let [moved-piece (move-piece-to-space
                                 moved-piece
                                 (:piece-space destination-result)
                                 final-orientation)
                    event {:type :chariot/rod-shortcut
                           :player-id (:player-id context)
                           :source (core/source-summary (:action-source context))
                           :rod-variant (:rod-variant (:spec context))
                           :target {:kind :piece
                                    :piece-id (:id moved-piece)
                                    :player-id (:player-id moved-piece)
                                    :row (:row moved-coordinate)
                                    :col (:col moved-coordinate)}
                           :intermediate intermediate
                           :destination (:destination destination-result)
                           :action-count 2
                           :shortcut? true
                           :piece moved-piece}
                    next-state (-> state
                                   (core/replace-piece moved-piece)
                                   (core/append-history event))]
                (assoc (core/success next-state [event])
                       :affected-piece-ids [(:id moved-piece)])))))))))

(defn- composite-spec [card-id power-order opts]
  (merge {:card-id card-id
          :power-order power-order
          :cup-variant :cup
          :rod-variant :rod
          :apply-action-fn apply-composite-action}
         opts))

(defn- with-power-card-and-source-opts [opts power-card source-opts]
  (cond-> opts
    power-card
    (assoc :power-card power-card)

    source-opts
    (assoc :source-opts source-opts)))

(defn apply-empress-move-with-source-card-id
  ([state command source-card-id]
   (apply-empress-move-with-source-card-id state command source-card-id nil))
  ([state command source-card-id power-card]
   (apply-empress-move-with-source-card-id state command source-card-id power-card {}))
  ([state command source-card-id power-card {:keys [source-opts]}]
   (major/apply-major-sequence
    state
    (command-with-empress-actions command)
    (composite-spec source-card-id
                    [:orient-minion :cup]
                    (with-power-card-and-source-opts
                      {:cup-variant :cup-unbounded}
                      power-card
                      source-opts)))))

(defn apply-empress-move [state command]
  (apply-empress-move-with-source-card-id state command "empress"))

(defn apply-emperor-move-with-source-card-id
  ([state command source-card-id]
   (apply-emperor-move-with-source-card-id state command source-card-id nil))
  ([state command source-card-id power-card]
   (apply-emperor-move-with-source-card-id state command source-card-id power-card {}))
  ([state command source-card-id power-card {:keys [source-opts]}]
   (major/apply-major-sequence
    state
    (command-with-emperor-actions command)
    (composite-spec source-card-id
                    [:orient-minion :rod]
                    (with-power-card-and-source-opts
                      {:rod-variant :rod-unbounded}
                      power-card
                      source-opts)))))

(defn apply-emperor-move [state command]
  (apply-emperor-move-with-source-card-id state command "emperor"))

(defn apply-lovers-move-with-source-card-id
  ([state command source-card-id]
   (apply-lovers-move-with-source-card-id state command source-card-id nil))
  ([state command source-card-id power-card]
   (apply-lovers-move-with-source-card-id state command source-card-id power-card {}))
  ([state command source-card-id power-card {:keys [source-opts]}]
   (major/apply-major-sequence
    state
    (command-with-lovers-actions command)
    (composite-spec source-card-id
                    [:rod :cup]
                    (with-power-card-and-source-opts
                      {}
                      power-card
                      source-opts)))))

(defn apply-lovers-move [state command]
  (apply-lovers-move-with-source-card-id state command "lovers"))

(defn apply-chariot-move-with-source-card-id
  ([state command source-card-id]
   (apply-chariot-move-with-source-card-id state command source-card-id nil))
  ([state command source-card-id power-card]
   (apply-chariot-move-with-source-card-id state command source-card-id power-card {}))
  ([state command source-card-id power-card {:keys [source-opts]}]
   (major/apply-major-sequence
    state
    (command-with-chariot-actions command)
    (composite-spec source-card-id
                    [:rod :rod]
                    (with-power-card-and-source-opts
                      {:shortcut-key-fn chariot-shortcut-key
                       :shortcut-fn apply-chariot-rod-shortcut}
                      power-card
                      source-opts)))))

(defn apply-chariot-move [state command]
  (apply-chariot-move-with-source-card-id state command "chariot"))

(defn apply-hanged-man-move-with-source-card-id
  ([state command source-card-id]
   (apply-hanged-man-move-with-source-card-id state command source-card-id nil))
  ([state command source-card-id power-card]
   (apply-hanged-man-move-with-source-card-id state command source-card-id power-card {}))
  ([state command source-card-id power-card {:keys [source-opts]}]
   (major/apply-major-sequence
    state
    (command-with-hanged-man-actions command)
    (composite-spec source-card-id
                    [:rod :trade-hand]
                    (with-power-card-and-source-opts
                      {}
                      power-card
                      source-opts)))))

(defn apply-hanged-man-move [state command]
  (apply-hanged-man-move-with-source-card-id state command "hangedman"))

(defn apply-temperance-move-with-source-card-id
  ([state command source-card-id]
   (apply-temperance-move-with-source-card-id state command source-card-id nil))
  ([state command source-card-id power-card]
   (apply-temperance-move-with-source-card-id state command source-card-id power-card {}))
  ([state command source-card-id power-card {:keys [source-opts]}]
   (major/apply-major-sequence
    state
    (command-with-temperance-actions command)
    (composite-spec source-card-id
                    [:cup :cup]
                    (with-power-card-and-source-opts
                      {}
                      power-card
                      source-opts)))))

(defn apply-temperance-move [state command]
  (apply-temperance-move-with-source-card-id state command "temperance"))

(defmethod major-power/apply-card-power "empress"
  [state command card {:keys [source-opts]}]
  (apply-empress-move-with-source-card-id state command "empress" card {:source-opts source-opts}))

(defmethod major-power/apply-card-power "emperor"
  [state command card {:keys [source-opts]}]
  (apply-emperor-move-with-source-card-id state command "emperor" card {:source-opts source-opts}))

(defmethod major-power/apply-card-power "lovers"
  [state command card {:keys [source-opts]}]
  (apply-lovers-move-with-source-card-id state command "lovers" card {:source-opts source-opts}))

(defmethod major-power/apply-card-power "chariot"
  [state command card {:keys [source-opts]}]
  (apply-chariot-move-with-source-card-id state command "chariot" card {:source-opts source-opts}))

(defmethod major-power/apply-card-power "hangedman"
  [state command card {:keys [source-opts]}]
  (apply-hanged-man-move-with-source-card-id state command "hangedman" card {:source-opts source-opts}))

(defmethod major-power/apply-card-power "temperance"
  [state command card {:keys [source-opts]}]
  (apply-temperance-move-with-source-card-id state command "temperance" card {:source-opts source-opts}))
