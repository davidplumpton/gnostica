(ns gnostica.app-state
  (:require [gnostica.game-state :as game-state]
            [gnostica.pieces :as pieces]))

(def default-player-specs
  (mapv #(select-keys % [:id :name]) pieces/players))

(def default-selected-board-index 0)

(def default-demo-board-pieces
  pieces/initial-pieces)

(def move-source-order
  [:activate-territory
   :play-hand-card
   :draw-cards
   :orient-piece
   :place-initial-small])

(def move-source-definitions
  {:activate-territory
   {:id :activate-territory
    :label "Activate territory"
    :summary "Use a board card through one of your pieces."
    :requirements [:source-board-index :piece-id :target-board-index]}
   :play-hand-card
   {:id :play-hand-card
    :label "Play hand card"
    :summary "Discard a hand card and use its power through a piece."
    :requirements [:hand-card-id :piece-id :target-board-index]}
   :draw-cards
   {:id :draw-cards
    :label "Draw cards"
    :summary "Draw toward the six-card hand limit."
    :requirements [:draw-count]}
   :orient-piece
   {:id :orient-piece
    :label "Orient piece"
    :summary "Turn one of your pieces to a legal direction."
    :requirements [:piece-id :orientation]}
   :place-initial-small
   {:id :place-initial-small
    :label "Place initial small"
    :summary "Put your first small piece on the board."
    :requirements [:target-board-index :orientation]}})

(def requirement-prompts
  {:source-board-index "Choose a source territory with one of your pieces."
   :hand-card-id "Choose a card from the current player's hand."
   :piece-id "Choose a minion."
   :target-board-index "Choose a target territory."
   :orientation "Choose an orientation."
   :draw-count "Choose how many cards to draw."})

(defn empty-move-selection []
  {:stage :source
   :source nil
   :params {}
   :error nil
   :last-result nil})

(defn- state-with-demo-board-pieces [state demo-board-pieces]
  (cond-> state
    (some? demo-board-pieces)
    (assoc-in [:pieces :on-board] (vec demo-board-pieces))))

(defn initialize
  ([] (initialize {}))
  ([{:keys [player-specs game-options selected-board-index demo-board-pieces]
     :or {player-specs default-player-specs
          game-options {}
          selected-board-index default-selected-board-index
          demo-board-pieces default-demo-board-pieces}}]
     (let [result (game-state/create-game player-specs game-options)
         base-db {:selected-board-index selected-board-index
                  :move-selection (empty-move-selection)
                  :three-texture-errors []}]
     (if (:ok? result)
       (assoc base-db :game (state-with-demo-board-pieces (:state result)
                                                          demo-board-pieces))
       (assoc base-db :setup-error (:error result))))))

(defn game [db]
  (:game db))

(defn setup-error [db]
  (:setup-error db))

(defn board [db]
  (get-in db [:game :board] []))

(defn board-pieces [db]
  (get-in db [:game :pieces :on-board] []))

(defn selected-board-index [db]
  (:selected-board-index db))

(defn selected-board-cell [db]
  (get (board db) (selected-board-index db)))

(defn selected-board-pieces [db]
  (pieces/pieces-for-space (board-pieces db) (selected-board-index db)))

(defn current-player [db]
  (some-> (game db) game-state/current-player))

(defn current-player-id [db]
  (get-in db [:game :turn :current-player-id]))

(defn current-player-hand [db]
  (vec (:hand (current-player db))))

(defn draw-pile [db]
  (vec (get-in db [:game :draw-pile] [])))

(defn discard-pile [db]
  (vec (get-in db [:game :discard-pile] [])))

(defn discard-top-card [db]
  (peek (discard-pile db)))

(defn card-zones [db]
  {:hand (current-player-hand db)
   :draw-pile (draw-pile db)
   :discard-pile (discard-pile db)
   :draw-count (count (draw-pile db))
   :discard-count (count (discard-pile db))
   :discard-top-card (discard-top-card db)})

(defn current-player-pieces [db]
  (let [player-id (current-player-id db)]
    (->> (board-pieces db)
         (filter #(= player-id (:player-id %)))
         vec)))

(defn current-player-piece? [db piece]
  (= (current-player-id db) (:player-id piece)))

(defn piece-by-id [db piece-id]
  (some #(when (= piece-id (:id %)) %)
        (board-pieces db)))

(defn current-player-piece-by-id [db piece-id]
  (let [piece (piece-by-id db piece-id)]
    (when (current-player-piece? db piece)
      piece)))

(defn current-player-pieces-on-space [db space-index]
  (->> (pieces/pieces-for-space (board-pieces db) space-index)
       (filter #(current-player-piece? db %))
       vec))

(defn hand-card-by-id [db card-id]
  (some #(when (= card-id (:id %)) %)
        (current-player-hand db)))

(defn valid-board-index? [db index]
  (contains? (board db) index))

(defn max-draw-count [db]
  (let [hand-slots (- game-state/starting-hand-size
                      (count (current-player-hand db)))
        draw-cards (count (get-in db [:game :draw-pile] []))]
    (max 0 (min hand-slots draw-cards))))

(defn draw-count-options [db]
  (vec (range 1 (inc (max-draw-count db)))))

(defn- small-stash-count [db]
  (or (get-in (current-player db) [:stash :small])
      (get-in db [:game :pieces :stashes (current-player-id db) :small])
      0))

(defn- source-unavailable-reason [db source-id]
  (let [player (current-player db)
        owned-pieces (current-player-pieces db)
        hand (current-player-hand db)
        max-draw (max-draw-count db)]
    (cond
      (nil? player)
      "No current player is available."

      (= :activate-territory source-id)
      (when-not (seq owned-pieces)
        "The current player has no pieces on the board.")

      (= :play-hand-card source-id)
      (cond
        (empty? hand) "The current player has no hand cards."
        (empty? owned-pieces) "The current player needs a piece on the board.")

      (= :draw-cards source-id)
      (when (zero? max-draw)
        "The current player cannot draw more cards.")

      (= :orient-piece source-id)
      (when-not (seq owned-pieces)
        "The current player has no pieces to orient.")

      (= :place-initial-small source-id)
      (cond
        (seq owned-pieces) "The current player already has pieces on the board."
        (not (pos? (small-stash-count db))) "The current player has no small pieces in stash.")

      :else
      "Unknown move source.")))

(defn move-source-options [db]
  (mapv (fn [source-id]
          (let [reason (source-unavailable-reason db source-id)]
            (assoc (get move-source-definitions source-id)
                   :enabled? (nil? reason)
                   :reason reason)))
        move-source-order))

(defn move-selection [db]
  (:move-selection db (empty-move-selection)))

(defn move-source [db]
  (:source (move-selection db)))

(defn move-params [db]
  (:params (move-selection db)))

(defn- move-error [code message data]
  {:code code
   :message message
   :data data})

(defn- requirement-complete? [db source-id params requirement]
  (case requirement
    :source-board-index
    (let [index (:source-board-index params)]
      (and (valid-board-index? db index)
           (seq (current-player-pieces-on-space db index))))

    :hand-card-id
    (some? (hand-card-by-id db (:hand-card-id params)))

    :piece-id
    (let [piece (current-player-piece-by-id db (:piece-id params))]
      (case source-id
        :activate-territory
        (and piece
             (= (:source-board-index params) (:space-index piece)))

        (:play-hand-card :orient-piece)
        (some? piece)

        false))

    :target-board-index
    (valid-board-index? db (:target-board-index params))

    :orientation
    (contains? pieces/legal-orientations (:orientation params))

    :draw-count
    (let [draw-count (:draw-count params)]
      (and (int? draw-count)
           (<= 1 draw-count)
           (<= draw-count (max-draw-count db))))

    false))

(defn- first-missing-requirement [db source-id params]
  (some (fn [requirement]
          (when-not (requirement-complete? db source-id params requirement)
            requirement))
        (:requirements (get move-source-definitions source-id))))

(defn- stage-for-requirement [requirement]
  (case requirement
    :source-board-index :source-territory
    :hand-card-id :hand-card
    :piece-id :piece
    :target-board-index :target
    :orientation :orientation
    :draw-count :draw-count
    :confirm))

(defn- refresh-move-selection [db]
  (let [{:keys [source params] :as selection} (move-selection db)]
    (assoc db :move-selection
           (if source
             (let [missing (first-missing-requirement db source params)]
               (assoc selection
                      :stage (if missing
                               (stage-for-requirement missing)
                               :confirm)))
             (assoc selection :stage :source)))))

(defn- update-move-selection [db f & args]
  (refresh-move-selection
   (update db :move-selection
           (fnil (fn [selection]
                   (apply f selection args))
                 (empty-move-selection)))))

(defn- update-move-selection-success [db f & args]
  (refresh-move-selection
   (update db :move-selection
           (fnil (fn [selection]
                   (assoc (apply f selection args)
                          :error nil
                          :last-result nil))
                 (empty-move-selection)))))

(defn move-ready? [db]
  (= :confirm (:stage (move-selection db))))

(defn move-prompt [db]
  (let [{:keys [source stage]} (move-selection db)]
    (cond
      (nil? source) "Choose a move source."
      (= :confirm stage) "Confirm the selected move."
      (= :rejected stage) "Review or cancel the rejected move."
      :else (get requirement-prompts
                 (some (fn [[requirement requirement-stage]]
                         (when (= stage requirement-stage)
                           requirement))
                       {:source-board-index :source-territory
                        :hand-card-id :hand-card
                        :piece-id :piece
                        :target-board-index :target
                        :orientation :orientation
                        :draw-count :draw-count})
                 "Complete the move selection."))))

(defn select-move-source [db source-id]
  (let [reason (source-unavailable-reason db source-id)]
    (if reason
      (assoc db :move-selection
             (assoc (empty-move-selection)
                    :error (move-error :move-source-unavailable
                                       reason
                                       {:source source-id})))
      (let [selected-index (selected-board-index db)
            base-params (cond-> {}
                          (= source-id :draw-cards)
                          (assoc :draw-count (first (draw-count-options db)))

                          (and (= source-id :activate-territory)
                               (seq (current-player-pieces-on-space db selected-index)))
                          (assoc :source-board-index selected-index)

                          (and (= source-id :place-initial-small)
                               (valid-board-index? db selected-index))
                          (assoc :target-board-index selected-index))]
        (refresh-move-selection
         (assoc db :move-selection
                {:stage :source
                 :source source-id
                 :params base-params
                 :error nil
                 :last-result nil}))))))

(defn cancel-move [db]
  (assoc db :move-selection (empty-move-selection)))

(defn- clear-piece-when-source-changes [params board-index]
  (let [next-params (assoc params :source-board-index board-index)]
    (if (= (:source-board-index params) board-index)
      next-params
      (dissoc next-params :piece-id))))

(defn select-board-for-active-move [db index]
  (if-not (valid-board-index? db index)
    db
    (let [{:keys [source params]} (move-selection db)]
      (case source
        :activate-territory
        (let [has-source? (requirement-complete? db source params :source-board-index)
              has-piece? (requirement-complete? db source params :piece-id)
              current-pieces (current-player-pieces-on-space db index)]
          (cond
            (and has-source? has-piece?)
            (update-move-selection-success db assoc-in [:params :target-board-index] index)

            (seq current-pieces)
            (update-move-selection-success db update :params clear-piece-when-source-changes index)

            :else
            (update-move-selection db assoc
                                   :error
                                   (move-error :invalid-source-territory
                                               "Choose a territory with one of the current player's pieces."
                                               {:board-index index}))))

        :play-hand-card
        (update-move-selection-success db assoc-in [:params :target-board-index] index)

        :place-initial-small
        (update-move-selection-success db assoc-in [:params :target-board-index] index)

        db))))

(defn select-board-card [db index]
  (if (contains? (board db) index)
    (select-board-for-active-move (assoc db :selected-board-index index) index)
    db))

(defn select-move-piece [db piece-id]
  (let [{:keys [source params]} (move-selection db)
        piece (current-player-piece-by-id db piece-id)]
    (cond
      (nil? source)
      db

      (nil? piece)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-piece
                                         "Choose one of the current player's pieces."
                                         {:piece-id piece-id}))

      (= :activate-territory source)
      (let [source-index (:source-board-index params)]
        (cond
          (nil? source-index)
          (-> db
              (update-move-selection-success assoc-in [:params :source-board-index] (:space-index piece))
              (update-move-selection-success assoc-in [:params :piece-id] piece-id))

          (= source-index (:space-index piece))
          (update-move-selection-success db assoc-in [:params :piece-id] piece-id)

          :else
          (update-move-selection db assoc
                                 :error
                                 (move-error :piece-outside-source-territory
                                             "Choose a minion on the selected source territory."
                                             {:piece-id piece-id
                                              :source-board-index source-index}))))

      (contains? #{:play-hand-card :orient-piece} source)
      (update-move-selection-success db assoc-in [:params :piece-id] piece-id)

      :else
      db)))

(defn select-move-hand-card [db card-id]
  (if (and (= :play-hand-card (move-source db))
           (hand-card-by-id db card-id))
    (update-move-selection-success db assoc-in [:params :hand-card-id] card-id)
    (update-move-selection db assoc
                           :error
                           (move-error :invalid-hand-card
                                       "Choose a card from the current player's hand."
                                       {:card-id card-id}))))

(defn set-move-orientation [db orientation]
  (if (contains? pieces/legal-orientations orientation)
    (update-move-selection-success db assoc-in [:params :orientation] orientation)
    (update-move-selection db assoc
                           :error
                           (move-error :invalid-orientation
                                       "Choose a legal piece orientation."
                                       {:orientation orientation}))))

(defn set-move-draw-count [db draw-count]
  (if (some #{draw-count} (draw-count-options db))
    (update-move-selection-success db assoc-in [:params :draw-count] draw-count)
    (update-move-selection db assoc
                           :error
                           (move-error :invalid-draw-count
                                       "Choose a draw count the current player can take."
                                       {:draw-count draw-count
                                        :options (draw-count-options db)}))))

(defn move-piece-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (case source
      :activate-territory
      (if (valid-board-index? db (:source-board-index params))
        (current-player-pieces-on-space db (:source-board-index params))
        [])

      (:play-hand-card :orient-piece)
      (current-player-pieces db)

      [])))

(defn move-hand-card-options [db]
  (if (= :play-hand-card (move-source db))
    (current-player-hand db)
    []))

(defn move-source-board-options [db]
  (let [owned-spaces (set (map :space-index (current-player-pieces db)))]
    (filterv #(contains? owned-spaces (:index %))
             (board db))))

(defn move-target-board-options [db]
  (board db))

(defn move-orientation-options [_db]
  (mapv (fn [orientation]
          {:id orientation
           :label (pieces/orientation-label orientation)})
        [:up :north :east :south :west]))

(defn move-command [db]
  (let [{:keys [source params]} (move-selection db)]
    (when source
      {:source source
       :player-id (current-player-id db)
       :params params})))

(defn confirm-move [db]
  (if-not (move-ready? db)
    (update-move-selection db assoc
                           :error
                           (move-error :incomplete-move
                                       "Complete the move selection before confirming."
                                       {:stage (:stage (move-selection db))}))
    (let [command (move-command db)
          result (game-state/failure :move-transition-unavailable
                                     "Move selection is complete, but gameplay rule transitions are not implemented yet."
                                     {:command command})]
      (assoc db :move-selection
             (assoc (move-selection db)
                    :stage :rejected
                    :error (:error result)
                    :last-result result)))))
