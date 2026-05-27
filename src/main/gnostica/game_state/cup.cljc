(ns gnostica.game-state.cup
  (:require [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.game-state.core :as core]
            [gnostica.pieces :as pieces]))

(def cup-territory-card-sources #{:hand :draw-pile-top})

(defn- discard-pile-card [state card-id]
  (some (fn [card]
          (when (= card-id (:id card))
            card))
        (:discard-pile state)))

(defn- resolve-cup-variant [card requested-variant source]
  (let [variants (cards/cup-variants card)
        variant-set (set variants)]
    (cond
      (empty? variants)
      (core/failure :source-card-not-cup
               "The source card does not provide a Cup power."
               {:card-id (:id card)
                :source source})

      (nil? requested-variant)
      {:ok? true
       :cup-variant (first variants)}

      (not (contains? cards/cup-variant-ids requested-variant))
      (core/failure :invalid-cup-variant
               "Cup moves require a known Cup variant."
               {:cup-variant requested-variant
                :valid-variants cards/cup-variant-ids})

      (contains? variant-set requested-variant)
      {:ok? true
       :cup-variant requested-variant}

      :else
      (core/failure :cup-variant-unavailable
               "The source card does not provide the selected Cup variant."
               {:card-id (:id card)
                :cup-variant requested-variant
                :available-variants variants}))))

(defn- resolve-cup-source
  ([state player-id source cup-variant]
   (resolve-cup-source state player-id source cup-variant {}))
  ([state player-id source cup-variant
    {:keys [source-card source-card-already-discarded?]}]
   (let [piece (core/piece-by-id state (:piece-id source))]
    (cond
      (not (map? source))
      (core/failure :invalid-cup-command
               "Cup moves require a source map."
               {:source source})

      (nil? piece)
      (core/failure :invalid-piece
               "Cup moves require one of the player's pieces as the acting minion."
               {:piece-id (:piece-id source)})

      (not= player-id (:player-id piece))
      (core/failure :invalid-piece
               "The acting minion must belong to the move's player."
               {:piece-id (:piece-id source)
                :player-id player-id
                :piece-player-id (:player-id piece)})

      (= :territory (:kind source))
      (let [cell (core/board-cell-by-index state (:board-index source))]
        (cond
          (nil? cell)
          (core/failure :invalid-source-territory
                   "Cup territory sources must reference an existing board cell."
                   {:board-index (:board-index source)})

          (not= (:board-index source) (:space-index piece))
          (core/failure :source-piece-mismatch
                   "The acting minion must occupy the source territory."
                   {:piece-id (:piece-id source)
                    :piece-space-index (:space-index piece)
                    :source-board-index (:board-index source)})

          :else
          (let [variant-result (resolve-cup-variant (:card cell)
                                                    cup-variant
                                                    source)]
            (if (:ok? variant-result)
              {:ok? true
               :source source
               :source-card (:card cell)
               :cup-variant (:cup-variant variant-result)
               :piece piece}
              variant-result))))

      (= :hand-card (:kind source))
      (let [card (or source-card
                     (core/player-hand-card state player-id (:card-id source))
                     (when source-card-already-discarded?
                       (discard-pile-card state (:card-id source))))]
        (cond
          (nil? card)
          (core/failure :invalid-hand-card
                   "Cup hand-card sources must reference a card in the player's hand."
                   {:card-id (:card-id source)
                    :player-id player-id})

          (and source-card
               (not= (:card-id source) (:id source-card)))
          (core/failure :invalid-hand-card
                   "Cup paid source cards must match the command source card."
                   {:card-id (:card-id source)
                    :source-card-id (:id source-card)})

          :else
          (let [variant-result (resolve-cup-variant card cup-variant source)]
            (if (:ok? variant-result)
              {:ok? true
               :source source
               :source-card card
               :cup-variant (:cup-variant variant-result)
               :discard-source-card? (not source-card-already-discarded?)
               :piece piece}
              variant-result))))

      :else
      (core/failure :invalid-cup-command
               "Cup move sources must be either :territory or :hand-card."
               {:source source})))))

(defn- cup-unbounded? [source]
  (= :cup-unbounded (:cup-variant source)))

(defn- place-small-piece [state player-id source target orientation]
  (cond
    (not (map? target))
    (core/failure :invalid-cup-target
             "Cup small-piece targets must be territory target maps."
             {:target target})

    (not= :territory (:kind target))
    (core/failure :invalid-cup-target
             "Cup small-piece placement targets an existing territory."
             {:target target})

    (nil? (core/board-cell-by-index state (:board-index target)))
    (core/failure :invalid-target-territory
             "Cup small-piece targets must reference an existing board cell."
             {:board-index (:board-index target)})

    (not (contains? pieces/legal-orientations orientation))
    (core/failure :invalid-orientation
             "Cup small-piece placement requires a legal orientation."
             {:orientation orientation
              :legal-orientations pieces/legal-orientations})

    (and (not (cup-unbounded? source))
         (<= pieces/max-pieces-per-space
             (count (core/pieces-at-board-index state (:board-index target)))))
    (core/failure :target-territory-full
             "Cup small-piece placement requires fewer than three pieces on the target territory."
             {:board-index (:board-index target)
              :maximum pieces/max-pieces-per-space})

    (not (pos? (core/small-stash-count state player-id)))
    (core/failure :no-small-piece-available
             "The player has no small pieces available in stash."
             {:player-id player-id})

    :else
    (let [piece {:id (core/next-piece-id state player-id :small)
                 :player-id player-id
                 :space-index (:board-index target)
                 :size :small
                 :orientation orientation}
          event {:type :cup/small-piece-created
                 :player-id player-id
                 :cup-variant (:cup-variant source)
                 :source (core/source-summary (:source source))
                 :target {:kind :territory
                          :board-index (:board-index target)}
                 :piece piece}
          next-state (-> state
                         (core/apply-source-cost player-id source)
                         (core/decrement-small-stash player-id)
                         (update-in [:pieces :on-board] conj piece)
                         (core/append-history event))]
      (core/success next-state [event]))))

(defn- create-enemy-small-piece [state player-id source target orientation]
  (let [target-piece (core/piece-by-id state (:piece-id target))
        target-cell (when target-piece
                      (core/target-piece-territory-cell state target-piece))
        target-player-id (:player-id target-piece)
        target-space-pieces (when target-cell
                              (core/pieces-at-coordinate state
                                                    (:row target-cell)
                                                    (:col target-cell)))]
    (cond
      (not (map? target))
      (core/failure :invalid-cup-target
               "Cup enemy-piece creation targets must be piece target maps."
               {:target target})

      (not= :piece (:kind target))
      (core/failure :invalid-cup-target
               "Cup enemy-piece creation targets an enemy piece."
               {:target target})

      (some? orientation)
      (core/failure :invalid-orientation
               "Cup enemy-piece creation preserves the target piece orientation."
               {:orientation orientation
                :piece-id (:piece-id target)})

      (nil? target-piece)
      (core/failure :invalid-target-piece
               "Cup enemy-piece creation must reference a piece on the board."
               {:target target})

      (= player-id target-player-id)
      (core/failure :target-piece-not-enemy
               "Cup enemy-piece creation must target an enemy piece."
               {:piece-id (:id target-piece)
                :player-id player-id})

      (nil? target-cell)
      (core/failure :invalid-target-piece-space
               "Cup enemy-piece creation must target an enemy piece on an existing territory."
               {:piece-id (:id target-piece)
                :space-index (:space-index target-piece)
                :space (:space target-piece)})

      (and (not (cup-unbounded? source))
           (<= pieces/max-pieces-per-space (count target-space-pieces)))
      (core/failure :target-territory-full
               "Cup enemy-piece creation requires fewer than three pieces on the target territory."
               {:board-index (:index target-cell)
                :piece-ids (mapv :id target-space-pieces)
                :maximum pieces/max-pieces-per-space})

      (not (pos? (core/small-stash-count state target-player-id)))
      (core/failure :no-small-piece-available
               "The target piece's player has no small pieces available in stash."
               {:player-id target-player-id
                :target-piece-id (:id target-piece)})

      :else
      (let [piece {:id (core/next-piece-id state target-player-id :small)
                   :player-id target-player-id
                   :space-index (:index target-cell)
                   :size :small
                   :orientation (:orientation target-piece)}
            normalized-target {:kind :piece
                               :piece-id (:id target-piece)
                               :board-index (:index target-cell)}
            event {:type :cup/enemy-small-piece-created
                   :player-id player-id
                   :cup-variant (:cup-variant source)
                   :source (core/source-summary (:source source))
                   :target normalized-target
                   :target-piece target-piece
                   :piece piece}
            next-state (-> state
                           (core/apply-source-cost player-id source)
                           (core/decrement-small-stash target-player-id)
                           (update-in [:pieces :on-board] conj piece)
                           (core/append-history event))]
        (core/success next-state [event])))))

(defn- create-wasteland-territory
  [state player-id source target one-point-card-id territory-card-source]
  (let [{:keys [row col] :as normalized-target} (core/wasteland-target target)
        source-card-id (get-in source [:source-card :id])
        one-point-card (core/player-hand-card state player-id one-point-card-id)
        territory-card-source (or territory-card-source :hand)
        draw-pile-card (first (:draw-pile state))]
    (cond
      (nil? normalized-target)
      (core/failure :invalid-cup-target
               "Cup territory creation targets an explicit wasteland coordinate."
               {:target target})

      (some? (core/board-cell-at state row col))
      (core/failure :target-not-wasteland
               "Cup territory creation must target an empty wasteland space."
               {:target normalized-target})

      (not (core/wasteland-target? state normalized-target))
      (core/failure :target-not-wasteland
               "Cup territory creation cannot target the void."
               {:target normalized-target})

      (seq (core/enemy-pieces-at-coordinate state player-id row col))
      (core/failure :wasteland-occupied-by-enemy
               "Cup territory creation cannot target a wasteland occupied by enemy pieces."
               {:target normalized-target
                :enemy-piece-ids (mapv :id (core/enemy-pieces-at-coordinate state player-id row col))})

      (not (contains? cup-territory-card-sources territory-card-source))
      (core/failure :invalid-cup-territory-card-source
               "Cup territory creation requires a supported card source."
               {:territory-card-source territory-card-source
                :valid-sources cup-territory-card-sources})

      (and (= :draw-pile-top territory-card-source)
           (not= :wheel-cup (:cup-variant source)))
      (core/failure :cup-variant-option-unavailable
               "Only Wheel Cup can create territory from the top draw-pile card."
               {:cup-variant (:cup-variant source)
                :territory-card-source territory-card-source})

      (and (= :draw-pile-top territory-card-source)
           (some? one-point-card-id))
      (core/failure :invalid-cup-territory-card-source
               "Draw-pile territory creation does not use a selected hand card."
               {:territory-card-source territory-card-source
                :one-point-card-id one-point-card-id})

      (and (= :hand territory-card-source)
           (nil? one-point-card))
      (core/failure :invalid-one-point-card
               "Cup territory creation requires a selected card from the player's hand."
               {:card-id one-point-card-id
                :player-id player-id})

      (and (= :hand territory-card-source)
           (= source-card-id one-point-card-id))
      (core/failure :card-already-used
               "A played source card cannot also become the new territory."
               {:card-id one-point-card-id})

      (and (= :hand territory-card-source)
           (not (cards/one-point-card? one-point-card)))
      (core/failure :invalid-one-point-card
               "Cup territory creation requires a one-point spot card."
               {:card-id one-point-card-id})

      (and (= :draw-pile-top territory-card-source)
           (nil? draw-pile-card))
      (core/failure :draw-pile-empty
               "Wheel Cup territory creation requires a card in the draw pile."
               {:territory-card-source territory-card-source})

      :else
      (let [board-index (core/next-board-index state)
            territory-card (case territory-card-source
                             :draw-pile-top draw-pile-card
                             one-point-card)
            cell {:index board-index
                  :row row
                  :col col
                  :orientation (board/orientation-for row col)
                  :face :up
                  :card territory-card}
            event {:type :cup/territory-created
                   :player-id player-id
                   :cup-variant (:cup-variant source)
                   :source (core/source-summary (:source source))
                   :target normalized-target
                   :board-index board-index
                   :card-id (:id territory-card)
                   :territory-card-source territory-card-source}
            cost-state (core/apply-source-cost state player-id source)
            card-source-state (case territory-card-source
                                :draw-pile-top
                                (assoc cost-state
                                       :draw-pile (vec (rest (:draw-pile cost-state))))

                                (core/remove-card-from-hand cost-state
                                                       player-id
                                                       one-point-card-id))
            next-state (-> card-source-state
                           (update :board conj cell)
                           (core/move-wasteland-pieces-to-board-index row col board-index)
                           (core/append-history event))]
        (core/success next-state [event])))))

(defn apply-cup-move-with-opts
  ([state command]
   (apply-cup-move-with-opts state command {}))
  ([state command {:keys [source-opts]
                   :or {source-opts {}}}]
  (let [{:keys [player-id source target orientation one-point-card-id
                cup-variant territory-card-source]} command]
    (cond
      (not (map? command))
      (core/failure :invalid-cup-command
               "Cup moves require a command map."
               {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (core/failure :unknown-player
               "Cup moves require a participating player."
               {:player-id player-id})

      (not (core/current-player-id? state player-id))
      (core/failure :not-current-player
               "Only the current player can apply a Cup move."
               {:player-id player-id
                :current-player-id (get-in state [:turn :current-player-id])})

      :else
      (let [source-result (resolve-cup-source state
                                              player-id
                                              source
                                              cup-variant
                                              source-opts)]
        (if (:ok? source-result)
          (case (:kind target)
            :territory
            (place-small-piece state player-id source-result target orientation)

            :piece
            (create-enemy-small-piece state player-id source-result target orientation)

            :wasteland
            (create-wasteland-territory state
                                        player-id
                                        source-result
                                        target
                                        one-point-card-id
                                        territory-card-source)

            (core/failure :invalid-cup-target
                     "Cup move targets must be :territory, :piece, or :wasteland."
                     {:target target}))
          source-result))))))

(defn apply-cup-move [state command]
  (apply-cup-move-with-opts state command {}))
