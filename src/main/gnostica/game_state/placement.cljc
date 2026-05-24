(ns gnostica.game-state.placement
  (:require [gnostica.game-state.core :as core]
            [gnostica.pieces :as pieces]))

(defn apply-orient-move [state command]
  (let [{:keys [player-id piece-id orientation]} command
        piece (core/piece-by-id state piece-id)]
    (cond
      (not (map? command))
      (core/failure :invalid-orient-command
               "Orient moves require a command map."
               {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (core/failure :unknown-player
               "Orient moves require a participating player."
               {:player-id player-id})

      (not (core/current-player-id? state player-id))
      (core/failure :not-current-player
               "Only the current player can orient a piece."
               {:player-id player-id
                :current-player-id (get-in state [:turn :current-player-id])})

      (nil? piece)
      (core/failure :invalid-piece
               "Orient moves require a piece on the board."
               {:piece-id piece-id})

      (not= player-id (:player-id piece))
      (core/failure :invalid-piece
               "Orient moves can only target one of the current player's pieces."
               {:piece-id piece-id
                :player-id player-id
                :piece-player-id (:player-id piece)})

      (not (contains? pieces/legal-orientations orientation))
      (core/failure :invalid-orientation
               "Orient moves require a legal orientation."
               {:piece-id piece-id
                :orientation orientation
                :legal-orientations pieces/legal-orientations})

      :else
      (let [oriented-piece (assoc piece :orientation orientation)
            event {:type :piece/oriented
                   :player-id player-id
                   :piece-id piece-id
                   :from-orientation (:orientation piece)
                   :to-orientation orientation
                   :piece oriented-piece}
            next-state (-> state
                           (core/replace-piece oriented-piece)
                           (core/append-history event))]
        (core/success next-state [event])))))

(defn- initial-placement-territory-target [state target]
  (let [board-index (:board-index target)
        cell (core/board-cell-by-index state board-index)
        pieces (when cell
                 (core/pieces-at-coordinate state (:row cell) (:col cell)))]
    (cond
      (nil? cell)
      (core/failure :invalid-target-territory
               "Initial small placement territory targets must reference an existing board cell."
               {:target target})

      (seq pieces)
      (core/failure :target-space-occupied
               "Initial small placement requires an empty territory or wasteland."
               {:target {:kind :territory
                         :board-index board-index}
                :piece-ids (mapv :id pieces)})

      :else
      {:ok? true
       :target {:kind :territory
                :board-index board-index}
       :piece-space {:space-index board-index}})))

(defn- initial-placement-wasteland-target [state target]
  (let [{:keys [row col] :as normalized-target} (core/wasteland-target target)
        pieces (when normalized-target
                 (core/pieces-at-coordinate state row col))]
    (cond
      (nil? normalized-target)
      (core/failure :invalid-initial-placement-target
               "Initial small placement wasteland targets require an explicit coordinate."
               {:target target})

      (some? (core/board-cell-at state row col))
      (core/failure :target-not-wasteland
               "Initial small placement wasteland targets must be empty spaces next to a territory."
               {:target normalized-target})

      (not (core/wasteland-target? state normalized-target))
      (core/failure :target-not-wasteland
               "Initial small placement cannot target the void."
               {:target normalized-target})

      (seq pieces)
      (core/failure :target-space-occupied
               "Initial small placement requires an empty territory or wasteland."
               {:target normalized-target
                :piece-ids (mapv :id pieces)})

      :else
      {:ok? true
       :target normalized-target
       :piece-space {:space normalized-target}})))

(defn- initial-placement-target [state target]
  (cond
    (not (map? target))
    (core/failure :invalid-initial-placement-target
             "Initial small placement requires a target map."
             {:target target})

    (= :territory (:kind target))
    (initial-placement-territory-target state target)

    (= :wasteland (:kind target))
    (initial-placement-wasteland-target state target)

    :else
    (core/failure :invalid-initial-placement-target
             "Initial small placement targets must be either :territory or :wasteland."
             {:target target})))

(defn apply-initial-placement [state command]
  (let [{:keys [player-id target orientation]} command]
    (cond
      (not (map? command))
      (core/failure :invalid-initial-placement-command
               "Initial small placement requires a command map."
               {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (core/failure :unknown-player
               "Initial small placement requires a participating player."
               {:player-id player-id})

      (not (core/current-player-id? state player-id))
      (core/failure :not-current-player
               "Only the current player can place an initial small piece."
               {:player-id player-id
                :current-player-id (get-in state [:turn :current-player-id])})

      (seq (core/player-pieces state player-id))
      (core/failure :initial-placement-has-pieces
               "Initial small placement is only available when the player has no pieces on the board."
               {:player-id player-id
                :piece-ids (mapv :id (core/player-pieces state player-id))})

      (not (contains? pieces/legal-orientations orientation))
      (core/failure :invalid-orientation
               "Initial small placement requires a legal orientation."
               {:orientation orientation
                :legal-orientations pieces/legal-orientations})

      (not (pos? (core/small-stash-count state player-id)))
      (core/failure :no-small-piece-available
               "The player has no small pieces available in stash."
               {:player-id player-id})

      :else
      (let [target-result (initial-placement-target state target)]
        (if-not (:ok? target-result)
          target-result
          (let [piece (merge {:id (core/next-piece-id state player-id :small)
                              :player-id player-id
                              :size :small
                              :orientation orientation}
                             (:piece-space target-result))
                event {:type :initial-placement/small-piece-placed
                       :player-id player-id
                       :target (:target target-result)
                       :piece piece}
                next-state (-> state
                               (core/decrement-small-stash player-id)
                               (update-in [:pieces :on-board] conj piece)
                               (core/append-history event))]
            (core/success next-state [event])))))))
