(ns gnostica.game-state.manipulation
  (:require [gnostica.board :as board]
            [gnostica.game-state.core :as core]
            [gnostica.game-state.major :as major]
            [gnostica.pieces :as pieces]))

(def target-direction-offsets
  {:north [-1 0]
   :east [0 1]
   :south [1 0]
   :west [0 -1]})

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

(defn target-coordinate [coordinate orientation]
  (when-let [{:keys [row col]} (coordinate-map coordinate)]
    (cond
      (= :up orientation)
      {:row row
       :col col}

      :else
      (when-let [[row-offset col-offset] (get target-direction-offsets orientation)]
        {:row (+ row row-offset)
         :col (+ col col-offset)}))))

(defn- same-coordinate? [left right]
  (= (coordinate-map left)
     (coordinate-map right)))

(defn- targetable-coordinate? [actor-coordinate target-coord orientation target-self?]
  (or target-self?
      (same-coordinate? target-coord
                        (target-coordinate actor-coordinate orientation))))

(defn- target-summary [target]
  (select-keys target [:kind :piece-id :board-index :row :col]))

(defn- command-action [command power]
  (assoc (dissoc command :player-id :source)
         :power power
         :piece-id (get-in command [:source :piece-id])))

(defn- command-with-single-action [command power]
  (if (contains? command :actions)
    command
    (assoc command :actions [(command-action command power)])))

(defn- command-with-orientation-actions [command]
  (cond
    (contains? command :actions)
    command

    (sequential? (:orientations command))
    (assoc command
           :actions
           (mapv (fn [action]
                   (assoc action
                          :power :orient-target
                          :piece-id (or (:piece-id action)
                                        (get-in command [:source :piece-id]))))
                 (:orientations command)))

    :else
    (assoc command :actions [(command-action command :orient-target)])))

(defn- apply-specific-major-sequence [state command spec power]
  (major/apply-major-sequence state
                              (command-with-single-action command power)
                              spec))

(defn- invalid-target-piece [target message]
  (core/failure :invalid-target-piece
                message
                {:target target}))

(defn- piece-target-result [state context action action-name]
  (let [target (:target action)
        target-piece-id (:piece-id target)
        minion (:action-minion context)
        source-orientation (:orientation minion)
        source-coordinate (coordinate-map (core/piece-coordinate state minion))
        target-piece (when target-piece-id
                       (core/piece-by-id state target-piece-id))
        target-coord (when target-piece
                       (coordinate-map (core/piece-coordinate state target-piece)))
        target-self? (= (:id minion) (:id target-piece))]
    (cond
      (not (map? target))
      (core/failure :invalid-target-piece
                    (str action-name " requires a target piece map.")
                    {:target target})

      (not= :piece (:kind target))
      (core/failure :invalid-target-piece
                    (str action-name " targets must use :kind :piece.")
                    {:target target})

      (nil? target-piece-id)
      (invalid-target-piece target
                            (str action-name " target maps require a piece id."))

      (nil? target-piece)
      (invalid-target-piece target
                            (str action-name " targets must reference a piece on the board."))

      (nil? source-coordinate)
      (core/failure :invalid-piece-space
                    (str action-name " requires an acting minion with a board coordinate.")
                    {:piece-id (:id minion)
                     :space-index (:space-index minion)
                     :space (:space minion)})

      (nil? target-coord)
      (core/failure :invalid-piece-space
                    (str action-name " targets must have a board coordinate.")
                    {:piece-id (:id target-piece)
                     :space-index (:space-index target-piece)
                     :space (:space target-piece)})

      (not (contains? pieces/legal-orientations source-orientation))
      (core/failure :invalid-major-target-orientation
                    (str action-name " requires the acting minion to have a legal orientation.")
                    {:piece-id (:id minion)
                     :orientation source-orientation
                     :legal-orientations pieces/legal-orientations})

      (not (targetable-coordinate? source-coordinate
                                   target-coord
                                   source-orientation
                                   target-self?))
      (core/failure :invalid-major-target
                    (str action-name " targets must be the minion itself, occupy the current space for upright minions, or occupy the adjacent space in the minion direction.")
                    {:piece-id (:id target-piece)
                     :orientation source-orientation
                     :source-coordinate source-coordinate
                     :target-coordinate target-coord
                     :expected-coordinate (target-coordinate source-coordinate source-orientation)})

      :else
      {:ok? true
       :target-piece target-piece
       :target-coordinate target-coord
       :target-cell (core/target-piece-territory-cell state target-piece)
       :target (cond-> {:kind :piece
                        :piece-id (:id target-piece)
                        :player-id (:player-id target-piece)
                        :row (:row target-coord)
                        :col (:col target-coord)}
                 (core/target-piece-territory-cell state target-piece)
                 (assoc :board-index (:index (core/target-piece-territory-cell state target-piece))))})))

(defn- territory-target-cell [state target action-name]
  (cond
    (not (map? target))
    (core/failure :invalid-target-territory
                  (str action-name " territory targets require a target map.")
                  {:target target})

    (not= :territory (:kind target))
    (core/failure :invalid-target-territory
                  (str action-name " territory targets must use :kind :territory.")
                  {:target target})

    (some? (:board-index target))
    (if-let [cell (core/board-cell-by-index state (:board-index target))]
      {:ok? true
       :cell cell}
      (core/failure :invalid-target-territory
                    (str action-name " territory targets must reference an existing board cell.")
                    {:target target}))

    (and (int? (:row target))
         (int? (:col target)))
    (if-let [cell (core/board-cell-at state (:row target) (:col target))]
      {:ok? true
       :cell cell}
      (core/failure :invalid-target-territory
                    (str action-name " territory targets must reference an existing board cell.")
                    {:target target}))

    :else
    (core/failure :invalid-target-territory
                  (str action-name " territory targets require a board index or row and column.")
                  {:target target})))

(defn- territory-target-result [state context action action-name]
  (let [cell-result (territory-target-cell state (:target action) action-name)]
    (if-not (:ok? cell-result)
      cell-result
      (let [cell (:cell cell-result)
            minion (:action-minion context)
            source-coordinate (coordinate-map (core/piece-coordinate state minion))
            source-orientation (:orientation minion)
            target-coord (select-keys cell [:row :col])]
        (cond
          (nil? source-coordinate)
          (core/failure :invalid-piece-space
                        (str action-name " requires an acting minion with a board coordinate.")
                        {:piece-id (:id minion)
                         :space-index (:space-index minion)
                         :space (:space minion)})

          (not (contains? pieces/legal-orientations source-orientation))
          (core/failure :invalid-major-target-orientation
                        (str action-name " requires the acting minion to have a legal orientation.")
                        {:piece-id (:id minion)
                         :orientation source-orientation
                         :legal-orientations pieces/legal-orientations})

          (not (targetable-coordinate? source-coordinate
                                       target-coord
                                       source-orientation
                                       false))
          (core/failure :invalid-major-target
                        (str action-name " territory targets must occupy the current space for upright minions or the adjacent space in the minion direction.")
                        {:target (target-summary (:target action))
                         :orientation source-orientation
                         :source-coordinate source-coordinate
                         :target-coordinate target-coord
                         :expected-coordinate (target-coordinate source-coordinate source-orientation)})

          :else
          {:ok? true
           :target-cell cell
           :target {:kind :territory
                    :board-index (:index cell)
                    :row (:row cell)
                    :col (:col cell)}})))))

(defn- orientation-result [orientation action-name]
  (if (contains? pieces/legal-orientations orientation)
    {:ok? true
     :orientation orientation}
    (core/failure :invalid-orientation
                  (str action-name " requires a legal orientation.")
                  {:orientation orientation
                   :legal-orientations pieces/legal-orientations})))

(defn- piece-space [piece]
  (if (contains? piece :space-index)
    {:space-index (:space-index piece)}
    {:space (:space piece)}))

(defn- replace-piece-for-player [state player-id target-piece orientation]
  (let [target-owner-id (:player-id target-piece)
        size (:size target-piece)
        stash-count (core/stash-count state player-id size)]
    (cond
      (not (pos? stash-count))
      (core/failure :no-same-size-piece-available
                    "Hierophant replacement requires a same-size piece in the current player's stash."
                    {:player-id player-id
                     :piece-id (:id target-piece)
                     :size size})

      :else
      (let [replacement-piece (merge {:id (core/next-piece-id state player-id size)
                                      :player-id player-id
                                      :size size
                                      :orientation orientation}
                                     (piece-space target-piece))]
        {:ok? true
         :piece replacement-piece
         :target-owner-id target-owner-id
         :size size}))))

(defn- apply-hierophant-action [state context action]
  (let [player-id (:player-id context)
        piece-result (piece-target-result state context action "Hierophant")
        orientation-check (orientation-result (:orientation action) "Hierophant")]
    (cond
      (not (:ok? piece-result))
      piece-result

      (not (:ok? orientation-check))
      orientation-check

      :else
      (let [target-piece (:target-piece piece-result)
            replacement-result (replace-piece-for-player state
                                                         player-id
                                                         target-piece
                                                         (:orientation orientation-check))]
        (if-not (:ok? replacement-result)
          replacement-result
          (let [{replacement-piece :piece
                 target-owner-id :target-owner-id
                 size :size} replacement-result
                event {:type :hierophant/piece-replaced
                       :player-id player-id
                       :source (core/source-summary (:action-source context))
                       :target (:target piece-result)
                       :replaced-piece target-piece
                       :piece replacement-piece}
                next-state (-> state
                               (core/increment-stash target-owner-id size)
                               (core/decrement-stash player-id size)
                               (core/replace-piece-by-id (:id target-piece)
                                                         replacement-piece)
                               (core/append-history event))]
            (assoc (core/success next-state [event])
                   :affected-piece-ids [(:id replacement-piece)])))))))

(defn apply-hierophant-move-with-source-card-id [state command source-card-id]
  (apply-specific-major-sequence
   state
   command
   {:card-id source-card-id
    :power-order [:convert-piece]
    :apply-action-fn apply-hierophant-action}
   :convert-piece))

(defn apply-hierophant-move [state command]
  (apply-hierophant-move-with-source-card-id state command "hierophant"))

(defn- empty-territory-destination [state destination]
  (let [board-index (:board-index destination)
        cell (core/board-cell-by-index state board-index)
        pieces (when cell
                 (core/pieces-at-coordinate state (:row cell) (:col cell)))]
    (cond
      (nil? cell)
      (core/failure :invalid-hermit-destination
                    "Hermit piece movement territory destinations must reference an existing board cell."
                    {:destination destination})

      (seq pieces)
      (core/failure :hermit-destination-occupied
                    "Hermit piece movement requires an empty territory or wasteland destination."
                    {:destination {:kind :territory
                                   :board-index board-index
                                   :row (:row cell)
                                   :col (:col cell)}
                     :piece-ids (mapv :id pieces)})

      :else
      {:ok? true
       :piece-space {:space-index (:index cell)}
       :destination {:kind :territory
                     :board-index (:index cell)
                     :row (:row cell)
                     :col (:col cell)}})))

(defn- empty-wasteland-destination [state destination]
  (let [{:keys [row col] :as normalized-destination} (core/wasteland-target destination)
        pieces (when normalized-destination
                 (core/pieces-at-coordinate state row col))]
    (cond
      (nil? normalized-destination)
      (core/failure :invalid-hermit-destination
                    "Hermit wasteland destinations require an explicit coordinate."
                    {:destination destination})

      (some? (core/board-cell-at state row col))
      (core/failure :target-not-wasteland
                    "Hermit wasteland destinations must be empty spaces next to a territory."
                    {:destination normalized-destination})

      (not (core/wasteland-target? state normalized-destination))
      (core/failure :target-not-wasteland
                    "Hermit cannot move a piece or territory into the void."
                    {:destination normalized-destination})

      (seq pieces)
      (core/failure :hermit-destination-occupied
                    "Hermit piece movement requires an empty territory or wasteland destination."
                    {:destination normalized-destination
                     :piece-ids (mapv :id pieces)})

      :else
      {:ok? true
       :piece-space {:space normalized-destination}
       :destination normalized-destination})))

(defn- piece-destination-result [state destination]
  (cond
    (not (map? destination))
    (core/failure :invalid-hermit-destination
                  "Hermit piece movement requires a destination map."
                  {:destination destination})

    (= :territory (:kind destination))
    (empty-territory-destination state destination)

    (= :wasteland (:kind destination))
    (empty-wasteland-destination state destination)

    :else
    (core/failure :invalid-hermit-destination
                  "Hermit piece movement destinations must be :territory or :wasteland."
                  {:destination destination})))

(defn- territory-destination-result [state player-id destination]
  (let [{:keys [row col] :as normalized-destination} (core/wasteland-target destination)
        enemy-pieces (when normalized-destination
                       (core/enemy-pieces-at-coordinate state player-id row col))]
    (cond
      (nil? normalized-destination)
      (core/failure :invalid-hermit-destination
                    "Hermit territory movement requires a wasteland destination coordinate."
                    {:destination destination})

      (some? (core/board-cell-at state row col))
      (core/failure :target-not-wasteland
                    "Hermit territory movement must land in a wasteland."
                    {:destination normalized-destination})

      (not (core/wasteland-target? state normalized-destination))
      (core/failure :target-not-wasteland
                    "Hermit cannot move a territory into the void."
                    {:destination normalized-destination})

      (seq enemy-pieces)
      (core/failure :wasteland-occupied-by-enemy
                    "Hermit territory movement cannot land on a wasteland occupied by enemy pieces."
                    {:destination normalized-destination
                     :enemy-piece-ids (mapv :id enemy-pieces)})

      :else
      {:ok? true
       :destination normalized-destination})))

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

(defn- resolve-hermit-piece-orientation [player-id target-piece orientation]
  (cond
    (nil? orientation)
    {:ok? true}

    (not= player-id (:player-id target-piece))
    (core/failure :invalid-orientation
                  "Enemy pieces retain their original orientation when moved by the Hermit."
                  {:piece-id (:id target-piece)
                   :piece-player-id (:player-id target-piece)
                   :orientation orientation})

    (not (contains? pieces/legal-orientations orientation))
    (core/failure :invalid-orientation
                  "Hermit can only reorient current-player pieces to a legal orientation."
                  {:piece-id (:id target-piece)
                   :orientation orientation
                   :legal-orientations pieces/legal-orientations})

    :else
    {:ok? true
     :orientation orientation}))

(defn- apply-hermit-piece-action [state context action]
  (let [player-id (:player-id context)
        target-result (piece-target-result state context action "Hermit")
        destination-result (piece-destination-result state (:destination action))]
    (cond
      (not (:ok? target-result))
      target-result

      (not (:ok? destination-result))
      destination-result

      :else
      (let [target-piece (:target-piece target-result)
            orientation-check (resolve-hermit-piece-orientation
                               player-id
                               target-piece
                               (:orientation action))]
        (if-not (:ok? orientation-check)
          orientation-check
          (let [moved-piece (move-piece-to-space target-piece
                                                 (:piece-space destination-result)
                                                 (:orientation orientation-check))
                event (cond-> {:type :hermit/piece-moved
                               :player-id player-id
                               :source (core/source-summary (:action-source context))
                               :target (:target target-result)
                               :destination (:destination destination-result)
                               :piece moved-piece}
                        (:orientation orientation-check)
                        (assoc :from-orientation (:orientation target-piece)
                               :to-orientation (:orientation orientation-check)))
                next-state (-> state
                               (core/replace-piece moved-piece)
                               (core/append-history event))]
            (assoc (core/success next-state [event])
                   :affected-piece-ids [(:id moved-piece)])))))))

(defn- apply-hermit-territory-action [state context action]
  (let [player-id (:player-id context)
        target-result (territory-target-result state context action "Hermit")
        destination-result (territory-destination-result state player-id (:destination action))]
    (cond
      (some? (:orientation action))
      (core/failure :invalid-orientation
                    "Hermit territory movement does not take a piece orientation."
                    {:orientation (:orientation action)
                     :target (:target action)})

      (not (:ok? target-result))
      target-result

      (not (:ok? destination-result))
      destination-result

      :else
      (let [target-cell (:target-cell target-result)
            {:keys [row col]} target-cell
            enemy-pieces (core/enemy-pieces-at-coordinate state player-id row col)]
        (if (seq enemy-pieces)
          (core/failure :target-territory-occupied-by-enemy
                        "Hermit territory movement cannot target a territory occupied by enemy pieces."
                        {:target (:target target-result)
                         :enemy-piece-ids (mapv :id enemy-pieces)})
          (let [destination (:destination destination-result)
                moved-territory (assoc target-cell
                                        :row (:row destination)
                                        :col (:col destination)
                                        :orientation (board/orientation-for
                                                      (:row destination)
                                                      (:col destination)))
                event {:type :hermit/territory-moved
                       :player-id player-id
                       :source (core/source-summary (:action-source context))
                       :target (:target target-result)
                       :destination destination
                       :territory moved-territory}
                next-state (-> state
                               (core/move-board-index-pieces-to-wasteland
                                (:index target-cell)
                                row
                                col)
                               (core/move-territory-cell (:index target-cell)
                                                         (:row destination)
                                                         (:col destination))
                              (core/move-wasteland-pieces-to-board-index
                               (:row destination)
                               (:col destination)
                               (:index target-cell))
                              (core/return-void-pieces-to-stash)
                              (core/append-history event))]
            (core/success next-state [event])))))))

(defn- apply-hermit-action [state context action]
  (case (get-in action [:target :kind])
    :piece (apply-hermit-piece-action state context action)
    :territory (apply-hermit-territory-action state context action)
    (core/failure :invalid-hermit-target
                  "Hermit targets must be :piece or :territory."
                  {:target (:target action)})))

(defn apply-hermit-move-with-source-card-id [state command source-card-id]
  (apply-specific-major-sequence
   state
   command
   {:card-id source-card-id
    :power-order [:relocate]
    :apply-action-fn apply-hermit-action}
   :relocate))

(defn apply-hermit-move [state command]
  (apply-hermit-move-with-source-card-id state command "hermit"))

(defn- apply-devil-action [state context action]
  (let [target-result (piece-target-result state context action "Devil")
        orientation-check (orientation-result (:orientation action) "Devil")]
    (cond
      (not (:ok? target-result))
      target-result

      (not (:ok? orientation-check))
      orientation-check

      :else
      (let [target-piece (:target-piece target-result)
            oriented-piece (assoc target-piece :orientation (:orientation orientation-check))
            event {:type :devil/piece-oriented
                   :player-id (:player-id context)
                   :source (core/source-summary (:action-source context))
                   :target (:target target-result)
                   :from-orientation (:orientation target-piece)
                   :to-orientation (:orientation oriented-piece)
                   :piece oriented-piece}
            next-state (-> state
                           (core/replace-piece oriented-piece)
                           (core/append-history event))]
        (assoc (core/success next-state [event])
               :affected-piece-ids (when (= (:player-id context)
                                            (:player-id oriented-piece))
                                     [(:id oriented-piece)]))))))

(defn apply-devil-move-with-source-card-id [state command source-card-id]
  (major/apply-major-sequence
   state
   (command-with-orientation-actions command)
   {:card-id source-card-id
    :power-order [:orient-target :orient-target :orient-target]
    :apply-action-fn apply-devil-action}))

(defn apply-devil-move [state command]
  (apply-devil-move-with-source-card-id state command "devil"))
