(ns gnostica.move-selection
  (:require [gnostica.board-layout :as layout]
            [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as pieces]))

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
    :requirements [:source-board-index :piece-id :target-space :target-resolution]}
   :play-hand-card
   {:id :play-hand-card
    :label "Play hand card"
    :summary "Discard a hand card and use its power through a piece."
    :requirements [:hand-card-id :piece-id :target-space :target-resolution]}
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
    :summary "Put your first small piece on an empty territory or wasteland."
    :requirements [:target-space :orientation]}})

(def move-power-order
  [:cup :rod :disc])

(def move-power-definitions
  {:cup {:id :cup
         :label "Cup"}
   :rod {:id :rod
         :label "Rod"}
   :disc {:id :disc
          :label "Disc"}})

(def rod-mode-order
  [:move-minion
   :push-piece
   :push-territory])

(def rod-modes
  (set rod-mode-order))

(def rod-mode-definitions
  {:move-minion {:id :move-minion
                 :label "Move minion"}
   :push-piece {:id :push-piece
                :label "Push piece"}
   :push-territory {:id :push-territory
                    :label "Push territory"}})

(def disc-target-kind-order
  [:piece
   :territory])

(def disc-target-kinds
  (set disc-target-kind-order))

(def disc-target-kind-definitions
  {:piece {:id :piece
           :label "Grow piece"}
   :territory {:id :territory
               :label "Grow territory"}})

(def territory-card-source-order
  [:hand :draw-pile-top])

(def territory-card-source-definitions
  {:hand {:id :hand
          :label "Hand one-point card"}
   :draw-pile-top {:id :draw-pile-top
                   :label "Top draw-pile card"}})

(def disc-replacement-card-source-order
  [:hand :discard-pile])

(def disc-replacement-card-source-definitions
  {:hand {:id :hand
          :label "Hand card"}
   :discard-pile {:id :discard-pile
                  :label "Discard pile card"}})

(def requirement-prompts
  {:source-board-index "Choose a source territory with one of your pieces."
   :hand-card-id "Choose a card from the current player's hand."
   :power "Choose the card power."
   :piece-id "Choose a minion."
   :rod-mode "Choose a Rod move."
   :disc-target-kind "Choose piece or territory growth."
   :target-piece-id "Choose a target piece."
   :target-board-index "Choose a target territory."
   :target-space "Choose a target territory, enemy piece, or wasteland."
   :initial-target-space "Choose an empty territory or wasteland."
   :territory-card-source "Choose where the new territory card comes from."
   :one-point-card-id "Choose a one-point card from the current player's hand."
   :replacement-card-source "Choose where the replacement card comes from."
   :replacement-card-id "Choose a replacement card."
   :orientation "Choose an orientation."
   :distance "Choose a distance."
   :draw-count "Choose how many cards to draw."})

(defn empty-move-selection []
  {:stage :source
   :source nil
   :params {}
   :error nil
   :last-result nil})

(defn game [db]
  (:game db))

(defn board [db]
  (get-in db [:game :board] []))

(defn board-pieces [db]
  (get-in db [:game :pieces :on-board] []))

(defn selected-board-index [db]
  (:selected-board-index db))

(defn current-player [db]
  (some-> (game db) game-state/current-player))

(defn current-player-id [db]
  (get-in db [:game :turn :current-player-id]))

(defn current-player-hand [db]
  (vec (:hand (current-player db))))

(defn discard-pile [db]
  (vec (get-in db [:game :discard-pile] [])))

(defn current-player-pieces [db]
  (let [player-id (current-player-id db)]
    (->> (board-pieces db)
         (filter #(= player-id (:player-id %)))
         vec)))

(defn- piece-coordinate [db piece]
  (if-let [{:keys [row col]} (:space piece)]
    [row col]
    (when-let [{:keys [row col]} (get (board db) (:space-index piece))]
      [row col])))

(defn- pieces-at-coordinate [db row col]
  (filterv #(= [row col] (piece-coordinate db %))
           (board-pieces db)))

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

(defn- gameplay-move-source? [source]
  (contains? #{:activate-territory :play-hand-card} source))

(defn- source-hand-card-id [source-id params]
  (when (= :play-hand-card source-id)
    (:hand-card-id params)))

(defn- source-board-card [db params]
  (:card (get (board db) (:source-board-index params))))

(defn- source-card [db source-id params]
  (case source-id
    :activate-territory (source-board-card db params)
    :play-hand-card (hand-card-by-id db (:hand-card-id params))
    nil))

(defn- source-command [source params]
  (case source
    :activate-territory
    {:kind :territory
     :board-index (:source-board-index params)
     :piece-id (:piece-id params)}

    :play-hand-card
    {:kind :hand-card
     :card-id (:hand-card-id params)
     :piece-id (:piece-id params)}))

(defn- move-power-ids-for-card [card]
  (when card
    (cond-> []
      (cards/cup-card? card) (conj :cup)
      (cards/rod-card? card) (conj :rod)
      (cards/disc-card? card) (conj :disc))))

(defn- selected-power [db source-id params]
  (when (gameplay-move-source? source-id)
    (let [card (source-card db source-id params)
          power-options (move-power-ids-for-card card)
          selected (:power params)]
      (cond
        (contains? (set power-options) selected)
        selected

        (= 1 (count power-options))
        (first power-options)

        (and card (empty? power-options))
        :cup

        :else
        nil))))

(defn- cup-move? [db source-id params]
  (= :cup (selected-power db source-id params)))

(defn- selected-cup-variant [db source-id params]
  (when (cup-move? db source-id params)
    (cards/cup-variant (source-card db source-id params))))

(defn- territory-card-source-option-ids [db source-id params]
  (when (cup-move? db source-id params)
    (if (= :wheel-cup (selected-cup-variant db source-id params))
      territory-card-source-order
      [:hand])))

(defn- rod-move? [db source-id params]
  (= :rod (selected-power db source-id params)))

(defn- selected-rod-variant [db source-id params]
  (when (rod-move? db source-id params)
    (cards/rod-variant (source-card db source-id params))))

(defn- disc-move? [db source-id params]
  (= :disc (selected-power db source-id params)))

(defn- selected-disc-variant [db source-id params]
  (when (disc-move? db source-id params)
    (cards/disc-variant (source-card db source-id params))))

(defn- one-point-card-options-for [db source-id params]
  (let [source-card-id (source-hand-card-id source-id params)]
    (->> (current-player-hand db)
         (filter #(and (cards/one-point-card? %)
                       (not= source-card-id (:id %))))
         vec)))

(defn- one-point-card-by-id [db source-id params card-id]
  (some #(when (= card-id (:id %)) %)
        (one-point-card-options-for db source-id params)))

(defn valid-board-index? [db index]
  (contains? (board db) index))

(defn- target-board-cell [db params]
  (get (board db) (:target-board-index params)))

(defn- discard-card-by-id [db card-id]
  (some #(when (= card-id (:id %)) %)
        (discard-pile db)))

(defn- disc-base-command [db source-id params]
  (when (and (disc-move? db source-id params)
             (current-player-id db)
             (:piece-id params))
    (cond-> {:player-id (current-player-id db)
             :source (source-command source-id params)}
      (selected-disc-variant db source-id params)
      (assoc :disc-variant (selected-disc-variant db source-id params)))))

(defn- disc-command-resolves? [db command]
  (boolean
   (and (game db)
        command
        (:ok? (game-state/resolve-disc-command (game db) command)))))

(defn- disc-piece-target? [db source-id params piece]
  (and piece
       (= :piece (:disc-target-kind params))
       (disc-command-resolves?
        db
        (assoc (disc-base-command db source-id params)
               :target {:kind :piece
                        :piece-id (:id piece)}))))

(defn- disc-territory-target? [db source-id params cell]
  (and cell
       (= :territory (:disc-target-kind params))
       (disc-command-resolves?
        db
        (assoc (disc-base-command db source-id params)
               :target {:kind :territory
                        :board-index (:index cell)}))))

(defn- disc-target-piece [db params]
  (let [source (get-in db [:move-selection :source])
        piece (piece-by-id db (:target-piece-id params))]
    (when (disc-piece-target? db source params piece)
      piece)))

(defn- disc-replacement-card-source-option-ids [db source-id params]
  (when (and (disc-move? db source-id params)
             (= :territory (:disc-target-kind params)))
    (if (= :disc-from-discard (selected-disc-variant db source-id params))
      disc-replacement-card-source-order
      [:hand])))

(defn- selected-disc-replacement-card-source [db source-id params]
  (let [options (set (disc-replacement-card-source-option-ids db source-id params))
        selected (:replacement-card-source params)]
    (cond
      (contains? options selected)
      selected

      (= 1 (count options))
      (first options)

      :else
      nil)))

(defn- discard-replacement-options [db source-id params]
  (let [source-card (source-card db source-id params)]
    (cond-> (discard-pile db)
      (and (= :play-hand-card source-id)
           source-card)
      (conj source-card))))

(defn- disc-replacement-card-options-for [db source-id params]
  (let [target-cell (target-board-cell db params)
        original-card (:card target-cell)
        source-card-id (source-hand-card-id source-id params)
        replacement-source (selected-disc-replacement-card-source db source-id params)]
    (if-not original-card
      []
      (->> (case replacement-source
             :hand (current-player-hand db)
             :discard-pile (discard-replacement-options db source-id params)
             [])
           (filter #(and (cards/card-worth-one-more? % original-card)
                         (or (not= :hand replacement-source)
                             (not= source-card-id (:id %)))))
           vec))))

(defn- disc-replacement-card-by-id [db source-id params card-id]
  (some #(when (= card-id (:id %)) %)
        (disc-replacement-card-options-for db source-id params)))

(defn- cup-target-piece? [db piece]
  (and piece
       (not (current-player-piece? db piece))
       (valid-board-index? db (:space-index piece))))

(defn- cup-target-piece [db params]
  (let [piece (piece-by-id db (:target-piece-id params))]
    (when (cup-target-piece? db piece)
      piece)))

(defn- empty-board-target? [db index]
  (when-let [{:keys [row col]} (get (board db) index)]
    (empty? (pieces-at-coordinate db row col))))

(defn- empty-wasteland-target? [db {:keys [row col]}]
  (empty? (pieces-at-coordinate db row col)))

(defn move-target-wasteland-options [db]
  (let [spaces (layout/wasteland-spaces (board db))]
    (if (= :place-initial-small (get-in db [:move-selection :source]))
      (filterv #(empty-wasteland-target? db %) spaces)
      spaces)))

(defn- wasteland-space-by-coordinate [db row col]
  (some #(when (and (= row (:row %))
                    (= col (:col %)))
           %)
        (move-target-wasteland-options db)))

(defn- valid-wasteland-target? [db target]
  (and (= :wasteland (:kind target))
       (int? (:row target))
       (int? (:col target))
       (some? (wasteland-space-by-coordinate db (:row target) (:col target)))))

(defn- target-space-complete? [db source-id params]
  (case source-id
    :place-initial-small
    (or (empty-board-target? db (:target-board-index params))
        (valid-wasteland-target? db (:target-wasteland params)))

    (or (valid-board-index? db (:target-board-index params))
        (some? (cup-target-piece db params))
        (valid-wasteland-target? db (:target-wasteland params)))))

(defn- target-resolution-complete? [db source-id params]
  (cond
    (some? (cup-target-piece db params))
    true

    (valid-board-index? db (:target-board-index params))
    (contains? pieces/legal-orientations (:orientation params))

    (valid-wasteland-target? db (:target-wasteland params))
    (let [cup-variant (selected-cup-variant db source-id params)
          selected-source (:territory-card-source params)
          territory-card-source (or selected-source :hand)
          source-options (set (territory-card-source-option-ids db source-id params))]
      (cond
        (and (= :wheel-cup cup-variant)
             (nil? selected-source))
        false

        (not (contains? source-options territory-card-source))
        false

        (= :draw-pile-top territory-card-source)
        true

        :else
        (some? (one-point-card-by-id db source-id params (:one-point-card-id params)))))

    :else
    false))

(defn- rod-distance-options-for-piece [piece]
  (vec (range 1 (inc (or (pieces/pips piece) 0)))))

(defn max-draw-count [db]
  (let [hand-slots (- game-state/starting-hand-size
                      (count (current-player-hand db)))
        available-cards (+ (count (get-in db [:game :draw-pile] []))
                           (count (get-in db [:game :discard-pile] [])))]
    (max 0 (min hand-slots available-cards))))

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

(defn move-power-options [db]
  (let [{:keys [source params]} (move-selection db)
        options (move-power-ids-for-card (source-card db source params))]
    (mapv move-power-definitions
          (filter #(contains? (set options) %)
                  move-power-order))))

(defn move-power [db]
  (let [{:keys [source params]} (move-selection db)]
    (selected-power db source params)))

(defn move-rod-mode-options [_db]
  (mapv rod-mode-definitions rod-mode-order))

(defn move-distance-options [db]
  (let [piece (piece-by-id db (get-in (move-selection db)
                                      [:params :piece-id]))]
    (rod-distance-options-for-piece piece)))

(defn move-target-piece-options [db]
  (let [source (move-source db)
        params (move-params db)]
    (cond
      (rod-move? db source params)
      (board-pieces db)

      (cup-move? db source params)
      (filterv #(cup-target-piece? db %) (board-pieces db))

      (disc-move? db source params)
      (filterv #(disc-piece-target? db source params %)
               (board-pieces db))

      :else
      [])))

(defn- rod-target-piece [db params]
  (piece-by-id db (:target-piece-id params)))

(defn move-rod-orientation-required? [db]
  (let [{:keys [source params]} (move-selection db)]
    (and (rod-move? db source params)
         (case (:rod-mode params)
           :move-minion true
           :push-piece (current-player-piece? db (rod-target-piece db params))
           false))))

(defn move-disc-orientation-available? [db]
  (let [{:keys [source params]} (move-selection db)]
    (and (disc-move? db source params)
         (= :piece (:disc-target-kind params))
         (current-player-piece? db (disc-target-piece db params)))))

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

    :power
    (some? (selected-power db source-id params))

    :rod-mode
    (and (rod-move? db source-id params)
         (contains? rod-modes (:rod-mode params)))

    :disc-target-kind
    (and (disc-move? db source-id params)
         (contains? disc-target-kinds (:disc-target-kind params)))

    :target-piece-id
    (cond
      (rod-move? db source-id params)
      (some? (rod-target-piece db params))

      (disc-move? db source-id params)
      (some? (disc-target-piece db params))

      :else
      false)

    :target-board-index
    (if (and (disc-move? db source-id params)
             (= :territory (:disc-target-kind params)))
      (some #(= (:target-board-index params) (:index %))
            (filterv #(disc-territory-target? db source-id params %)
                     (board db)))
      (valid-board-index? db (:target-board-index params)))

    :target-space
    (case source-id
      :place-initial-small (target-space-complete? db source-id params)
      (and (cup-move? db source-id params)
           (target-space-complete? db source-id params)))

    :target-resolution
    (and (cup-move? db source-id params)
         (target-resolution-complete? db source-id params))

    :replacement-card-source
    (some? (selected-disc-replacement-card-source db source-id params))

    :replacement-card-id
    (some? (disc-replacement-card-by-id db
                                        source-id
                                        params
                                        (:replacement-card-id params)))

    :orientation
    (contains? pieces/legal-orientations (:orientation params))

    :draw-count
    (let [draw-count (:draw-count params)]
      (and (int? draw-count)
           (<= 1 draw-count)
           (<= draw-count (max-draw-count db))))

    :distance
    (some #{(:distance params)} (move-distance-options db))

    false))

(defn- rod-requirements [db params]
  (let [mode (:rod-mode params)]
    (vec
     (concat [:rod-mode]
             (case mode
               :move-minion [:distance]
               :push-piece [:target-piece-id :distance]
               :push-territory [:target-board-index :distance]
               [])
             (when (and (contains? rod-modes mode)
                        (move-rod-orientation-required? db))
               [:orientation])))))

(defn- disc-requirements [db source-id params]
  (vec
   (concat [:disc-target-kind]
           (case (:disc-target-kind params)
             :piece [:target-piece-id]
             :territory
             (concat [:target-board-index]
                     (when (< 1 (count (disc-replacement-card-source-option-ids
                                         db
                                         source-id
                                         params)))
                       [:replacement-card-source])
                     [:replacement-card-id])
             []))))

(defn- gameplay-source-requirements [db source-id params]
  (let [base (case source-id
               :activate-territory [:source-board-index :piece-id]
               :play-hand-card [:hand-card-id :piece-id])
        card (source-card db source-id params)
        power (selected-power db source-id params)]
    (vec
     (concat base
             (when (and card (nil? power))
               [:power])
             (case power
               :cup [:target-space :target-resolution]
               :rod (rod-requirements db params)
               :disc (disc-requirements db source-id params)
               [])))))

(defn- move-requirements [db source-id params]
  (case source-id
    (:activate-territory :play-hand-card)
    (gameplay-source-requirements db source-id params)

    (:draw-cards :orient-piece :place-initial-small)
    (:requirements (get move-source-definitions source-id))

    []))

(defn- first-missing-requirement [db source-id params]
  (some (fn [requirement]
          (when-not (requirement-complete? db source-id params requirement)
            requirement))
        (move-requirements db source-id params)))

(defn- stage-for-requirement [db source-id params requirement]
  (case requirement
    :source-board-index :source-territory
    :hand-card-id :hand-card
    :power :power
    :piece-id :piece
    :rod-mode :rod-mode
    :disc-target-kind :disc-target-kind
    :target-piece-id :target-piece
    :target-board-index :target
    :target-space :target
    :target-resolution (cond
                         (and (valid-wasteland-target? db (:target-wasteland params))
                              (= :wheel-cup (selected-cup-variant db source-id params))
                              (nil? (:territory-card-source params)))
                         :territory-card-source

                         (valid-wasteland-target? db (:target-wasteland params))
                         :one-point-card

                         :else
                         :orientation)
    :one-point-card-id :one-point-card
    :territory-card-source :territory-card-source
    :replacement-card-source :replacement-card-source
    :replacement-card-id :replacement-card
    :orientation :orientation
    :distance :distance
    :draw-count :draw-count
    :confirm))

(defn- refresh-move-selection [db]
  (let [{:keys [source params] :as selection} (move-selection db)]
    (assoc db :move-selection
           (if source
             (let [missing (first-missing-requirement db source params)]
               (assoc selection
                      :stage (if missing
                               (stage-for-requirement db source params missing)
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
      (= :target stage) (cond
                          (cup-move? db source (move-params db))
                          (:target-space requirement-prompts)

                          (disc-move? db source (move-params db))
                          (:target-board-index requirement-prompts)

                          (= :place-initial-small source)
                          (:initial-target-space requirement-prompts)

                          :else
                          (:target-board-index requirement-prompts))
      (= :territory-card-source stage) (:territory-card-source requirement-prompts)
      (= :one-point-card stage) (:one-point-card-id requirement-prompts)
      (= :replacement-card-source stage) (:replacement-card-source requirement-prompts)
      (= :replacement-card stage) (:replacement-card-id requirement-prompts)
      :else (get {:source-territory (:source-board-index requirement-prompts)
                  :hand-card (:hand-card-id requirement-prompts)
                  :power (:power requirement-prompts)
                  :piece (:piece-id requirement-prompts)
                  :rod-mode (:rod-mode requirement-prompts)
                  :disc-target-kind (:disc-target-kind requirement-prompts)
                  :target-piece (:target-piece-id requirement-prompts)
                  :orientation (:orientation requirement-prompts)
                  :distance (:distance requirement-prompts)
                  :draw-count (:draw-count requirement-prompts)}
                 stage
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
                               (empty-board-target? db selected-index))
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
      (dissoc next-params
              :piece-id
              :power
              :rod-mode
              :disc-target-kind
              :target-board-index
              :target-wasteland
              :target-piece-id
              :territory-card-source
              :one-point-card-id
              :replacement-card-source
              :replacement-card-id
              :orientation
              :distance))))

(defn- set-territory-target [params board-index]
  (-> params
      (assoc :target-board-index board-index)
      (dissoc :target-wasteland
              :target-piece-id
              :territory-card-source
              :one-point-card-id
              :replacement-card-id)))

(defn- set-wasteland-target [params space]
  (-> params
      (assoc :target-wasteland (select-keys space [:kind :row :col]))
      (dissoc :target-board-index
              :target-piece-id
              :orientation
              :territory-card-source
              :one-point-card-id
              :replacement-card-source
              :replacement-card-id)))

(defn- set-hand-card-source [params card-id]
  (-> params
      (assoc :hand-card-id card-id)
      (dissoc :piece-id
              :power
              :rod-mode
              :disc-target-kind
              :target-board-index
              :target-wasteland
              :target-piece-id
              :territory-card-source
              :one-point-card-id
              :replacement-card-source
              :replacement-card-id
              :orientation
              :distance)))

(defn- set-acting-piece [params piece-id]
  (let [next-params (assoc params :piece-id piece-id)]
    (if (= (:piece-id params) piece-id)
      next-params
      (dissoc next-params
              :target-board-index
              :target-wasteland
              :target-piece-id
              :territory-card-source
              :one-point-card-id
              :replacement-card-source
              :replacement-card-id
              :orientation
              :distance))))

(defn- set-power-param [params power]
  (let [next-params (assoc params :power power)]
    (if (= (:power params) power)
      next-params
      (dissoc next-params
              :rod-mode
              :disc-target-kind
              :target-board-index
              :target-wasteland
              :target-piece-id
              :territory-card-source
              :one-point-card-id
              :replacement-card-source
              :replacement-card-id
              :orientation
              :distance))))

(defn- set-rod-mode-param [params mode]
  (let [next-params (assoc params :rod-mode mode)]
    (if (= (:rod-mode params) mode)
      next-params
      (dissoc next-params
              :target-board-index
              :target-wasteland
              :target-piece-id
              :territory-card-source
              :one-point-card-id
              :replacement-card-source
              :replacement-card-id
              :orientation))))

(defn- set-disc-target-kind-param [params target-kind]
  (let [next-params (assoc params :disc-target-kind target-kind)]
    (if (= (:disc-target-kind params) target-kind)
      next-params
      (dissoc next-params
              :target-board-index
              :target-wasteland
              :target-piece-id
              :territory-card-source
              :one-point-card-id
              :replacement-card-source
              :replacement-card-id
              :orientation))))

(defn- set-target-piece [params piece-id]
  (let [next-params (assoc params :target-piece-id piece-id)]
    (if (= (:target-piece-id params) piece-id)
      next-params
      (dissoc next-params
              :target-board-index
              :target-wasteland
              :territory-card-source
              :one-point-card-id
              :replacement-card-source
              :replacement-card-id
              :orientation))))

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
            (and has-source?
                 has-piece?
                 (disc-move? db source params)
                 (= :territory (:disc-target-kind params))
                 (disc-territory-target? db source params (get (board db) index)))
            (update-move-selection-success db update :params set-territory-target index)

            (and has-source?
                 has-piece?
                 (disc-move? db source params)
                 (not= :territory (:disc-target-kind params)))
            db

            (and has-source?
                 has-piece?
                 (not (disc-move? db source params)))
            (update-move-selection-success db update :params set-territory-target index)

            (and has-source?
                 has-piece?
                 (disc-move? db source params))
            (update-move-selection db assoc
                                   :error
                                   (move-error :invalid-disc-target
                                               "Choose a Disc-targetable territory."
                                               {:board-index index}))

            (seq current-pieces)
            (update-move-selection-success db update :params clear-piece-when-source-changes index)

            :else
            (update-move-selection db assoc
                                   :error
                                   (move-error :invalid-source-territory
                                               "Choose a territory with one of the current player's pieces."
                                               {:board-index index}))))

        :play-hand-card
        (cond
          (and (disc-move? db source params)
               (not= :territory (:disc-target-kind params)))
          db

          (and (disc-move? db source params)
               (not (disc-territory-target? db source params (get (board db) index))))
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-disc-target
                                             "Choose a Disc-targetable territory."
                                             {:board-index index}))

          :else
          (update-move-selection-success db update :params set-territory-target index))

        :place-initial-small
        (if (empty-board-target? db index)
          (update-move-selection-success db update :params set-territory-target index)
          (update-move-selection db assoc
                                 :error
                                 (move-error :target-space-occupied
                                             "Choose an empty territory or wasteland."
                                             {:board-index index})))

        db))))

(defn select-move-wasteland-target [db row col]
  (let [source (move-source db)
        params (move-params db)
        space (wasteland-space-by-coordinate db row col)]
    (cond
      (not (or (cup-move? db source params)
               (= :place-initial-small source)))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-wasteland-target
                                         "Wasteland targets are only available for Cup moves or initial placement."
                                         {:row row
                                          :col col
                                          :source source}))

      (nil? space)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-wasteland-target
                                         "Choose an available wasteland space."
                                         {:row row
                                          :col col}))

      :else
      (update-move-selection-success db update :params set-wasteland-target space))))

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
              (update-move-selection-success update :params set-acting-piece piece-id))

          (= source-index (:space-index piece))
          (update-move-selection-success db update :params set-acting-piece piece-id)

          :else
          (update-move-selection db assoc
                                 :error
                                 (move-error :piece-outside-source-territory
                                             "Choose a minion on the selected source territory."
                                             {:piece-id piece-id
                                              :source-board-index source-index}))))

      (contains? #{:play-hand-card :orient-piece} source)
      (update-move-selection-success db update :params set-acting-piece piece-id)

      :else
      db)))

(defn select-move-hand-card [db card-id]
  (if (and (= :play-hand-card (move-source db))
           (hand-card-by-id db card-id))
    (update-move-selection-success db update :params set-hand-card-source card-id)
    (update-move-selection db assoc
                           :error
                           (move-error :invalid-hand-card
                                       "Choose a card from the current player's hand."
                                       {:card-id card-id}))))

(defn select-move-power [db power]
  (let [power-ids (set (map :id (move-power-options db)))]
    (if (contains? power-ids power)
      (update-move-selection-success db update :params set-power-param power)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-move-power
                                         "Choose a power provided by the selected card."
                                         {:power power
                                          :options (vec power-ids)})))))

(defn select-move-rod-mode [db mode]
  (if (contains? rod-modes mode)
    (update-move-selection-success db update :params set-rod-mode-param mode)
    (update-move-selection db assoc
                           :error
                           (move-error :invalid-rod-mode
                                       "Choose a supported Rod move."
                                       {:mode mode
                                        :options rod-mode-order}))))

(defn select-move-disc-target-kind [db target-kind]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (disc-move? db source params)
             (contains? disc-target-kinds target-kind))
      (update-move-selection-success db update :params set-disc-target-kind-param target-kind)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-disc-target-kind
                                         "Choose a supported Disc growth target."
                                         {:target-kind target-kind
                                          :options disc-target-kind-order})))))

(defn select-move-target-piece [db piece-id]
  (let [source (move-source db)
        params (move-params db)
        piece (piece-by-id db piece-id)
        selectable-piece? (some #(= piece-id (:id %))
                                (move-target-piece-options db))]
    (cond
      (nil? piece)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a piece on the board."
                                         {:piece-id piece-id}))

      (rod-move? db source params)
      (update-move-selection-success db update :params set-target-piece (:id piece))

      (and (cup-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params set-target-piece (:id piece))

      (cup-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose an enemy piece on an existing territory."
                                         {:piece-id piece-id}))

      (and (disc-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params set-target-piece (:id piece))

      (disc-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Disc-targetable piece."
                                         {:piece-id piece-id}))

      :else
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Target pieces are only available for Cup, Rod, or Disc moves."
                                         {:piece-id piece-id})))))

(defn- set-territory-card-source [params territory-card-source]
  (cond-> (assoc params :territory-card-source territory-card-source)
    (= :draw-pile-top territory-card-source)
    (dissoc :one-point-card-id)))

(defn select-move-territory-card-source [db territory-card-source]
  (let [{:keys [source params]} (move-selection db)
        option-ids (set (if (disc-move? db source params)
                          (disc-replacement-card-source-option-ids db source params)
                          (territory-card-source-option-ids db source params)))]
    (if (contains? option-ids territory-card-source)
      (update-move-selection-success db
                                     update
                                     :params
                                     (if (disc-move? db source params)
                                       (fn [params card-source]
                                         (-> params
                                             (assoc :replacement-card-source card-source)
                                             (dissoc :replacement-card-id)))
                                       set-territory-card-source)
                                     territory-card-source)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-territory-card-source
                                         "Choose an available card source."
                                         {:territory-card-source territory-card-source
                                          :options (vec option-ids)})))))

(defn select-move-one-point-card [db card-id]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (cup-move? db source params)
             (one-point-card-by-id db source params card-id))
      (update-move-selection-success db assoc-in [:params :one-point-card-id] card-id)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-one-point-card
                                         "Choose a one-point card from the current player's hand."
                                         {:card-id card-id})))))

(defn select-move-replacement-card [db card-id]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (disc-move? db source params)
             (disc-replacement-card-by-id db source params card-id))
      (update-move-selection-success db assoc-in [:params :replacement-card-id] card-id)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-disc-replacement-card
                                         "Choose a replacement card worth exactly one more point."
                                         {:card-id card-id})))))

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

(defn set-move-distance [db distance]
  (if (some #{distance} (move-distance-options db))
    (update-move-selection-success db assoc-in [:params :distance] distance)
    (update-move-selection db assoc
                           :error
                           (move-error :invalid-distance
                                       "Choose a distance the acting minion can move."
                                       {:distance distance
                                        :options (move-distance-options db)}))))

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
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (= :place-initial-small source)
      (filterv #(empty-board-target? db (:index %))
               (board db))

      (and (disc-move? db source params)
           (= :territory (:disc-target-kind params)))
      (filterv #(disc-territory-target? db source params %)
               (board db))

      :else
      (board db))))

(defn move-one-point-card-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (cup-move? db source params)
             (not= :draw-pile-top (:territory-card-source params)))
      (one-point-card-options-for db source params)
      [])))

(defn move-territory-card-source-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (cup-move? db source params)
      (mapv territory-card-source-definitions
            (territory-card-source-option-ids db source params))

      (disc-move? db source params)
      (mapv disc-replacement-card-source-definitions
            (disc-replacement-card-source-option-ids db source params))

      :else
      [])))

(defn move-disc-target-kind-options [db]
  (if (disc-move? db (move-source db) (move-params db))
    (mapv disc-target-kind-definitions disc-target-kind-order)
    []))

(defn move-replacement-card-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (if (disc-move? db source params)
      (disc-replacement-card-options-for db source params)
      [])))

(defn move-orientation-options [_db]
  (mapv (fn [orientation]
          {:id orientation
           :label (pieces/orientation-label orientation)})
        [:up :north :east :south :west]))

(defn- cup-target-command [params]
  (cond
    (:target-wasteland params)
    (let [territory-card-source (or (:territory-card-source params) :hand)]
      (cond-> {:target (select-keys (:target-wasteland params) [:kind :row :col])
               :territory-card-source territory-card-source}
        (= :hand territory-card-source)
        (assoc :one-point-card-id (:one-point-card-id params))))

    (:target-piece-id params)
    {:target {:kind :piece
              :piece-id (:target-piece-id params)}}

    :else
    {:target {:kind :territory
              :board-index (:target-board-index params)}
     :orientation (:orientation params)}))

(defn- cup-command [db source params]
  (assoc (cup-target-command params)
         :cup-variant (selected-cup-variant db source params)))

(defn- initial-placement-target-command [params]
  (if-let [target-wasteland (:target-wasteland params)]
    (select-keys target-wasteland [:kind :row :col])
    {:kind :territory
     :board-index (:target-board-index params)}))

(defn- rod-target-command [params]
  (case (:rod-mode params)
    :move-minion {}
    :push-piece {:target {:kind :piece
                          :piece-id (:target-piece-id params)}}
    :push-territory {:target {:kind :territory
                              :board-index (:target-board-index params)}}))

(defn- rod-command [db source params]
  (let [rod-variant (selected-rod-variant db source params)]
    (cond-> (merge {:mode (:rod-mode params)
                    :distance (:distance params)}
                   (rod-target-command params))
      rod-variant
      (assoc :rod-variant rod-variant)

      (:orientation params)
      (assoc :orientation (:orientation params)))))

(defn- disc-target-command [db source params]
  (case (:disc-target-kind params)
    :piece
    (cond-> {:target {:kind :piece
                      :piece-id (:target-piece-id params)}}
      (:orientation params)
      (assoc :orientation (:orientation params)))

    :territory
    (cond-> {:target {:kind :territory
                      :board-index (:target-board-index params)}
             :replacement-card-source (selected-disc-replacement-card-source db source params)
             :replacement-card-id (:replacement-card-id params)}
      (nil? (:replacement-card-id params))
      (dissoc :replacement-card-id))

    {}))

(defn- disc-command [db source params]
  (assoc (disc-target-command db source params)
         :disc-variant (selected-disc-variant db source params)))

(defn move-command [db]
  (let [{:keys [source params]} (move-selection db)]
    (when source
      (case source
        :activate-territory
        (merge {:player-id (current-player-id db)
                :source (source-command source params)}
               (case (selected-power db source params)
                 :rod (rod-command db source params)
                 :disc (disc-command db source params)
                 :cup (cup-command db source params)
                 (cup-command db source params)))

        :play-hand-card
        (merge {:player-id (current-player-id db)
                :source (source-command source params)}
               (case (selected-power db source params)
                 :rod (rod-command db source params)
                 :disc (disc-command db source params)
                 :cup (cup-command db source params)
                 (cup-command db source params)))

        :draw-cards
        {:source :draw-cards
         :player-id (current-player-id db)
         :discard-card-ids (vec (:discard-card-ids params))
         :draw-count (:draw-count params)}

        :orient-piece
        {:source :orient-piece
         :player-id (current-player-id db)
         :piece-id (:piece-id params)
         :orientation (:orientation params)}

        :place-initial-small
        {:source :place-initial-small
         :player-id (current-player-id db)
         :target (initial-placement-target-command params)
         :orientation (:orientation params)}

        {:source source
         :player-id (current-player-id db)
         :params params}))))

(defn- command-with-transition-options [command transition-options]
  (cond-> command
    (and (= :draw-cards (:source command))
         (:shuffle-fn transition-options)
         (not (contains? command :shuffle-fn)))
    (assoc :shuffle-fn (:shuffle-fn transition-options))))

(defn- confirmed-move-result [db command transition-options]
  (let [command (command-with-transition-options command transition-options)]
    (cond
      (= :draw-cards (:source command))
      (game-state/apply-draw-move (game db) command)

      (= :orient-piece (:source command))
      (game-state/apply-orient-move (game db) command)

      (= :place-initial-small (:source command))
      (game-state/apply-initial-placement (game db) command)

      (= :cup (move-power db))
      (game-state/apply-cup-move (game db) command)

      (= :rod (move-power db))
      (game-state/apply-rod-move (game db) command)

      (= :disc (move-power db))
      (game-state/apply-disc-move (game db) command)

      :else
      (game-state/failure :move-transition-unavailable
                          "Move selection is complete, but this gameplay rule transition is not implemented yet."
                          {:command command}))))

(defn- apply-confirmed-move-result [db result]
  (if (:ok? result)
    (assoc db
           :game (:state result)
           :move-selection (assoc (empty-move-selection)
                                  :last-result result))
    (assoc db :move-selection
           (assoc (move-selection db)
                  :stage :rejected
                  :error (:error result)
                  :last-result result))))

(defn confirm-move
  ([db] (confirm-move db {}))
  ([db transition-options]
   (if-not (move-ready? db)
     (update-move-selection db assoc
                            :error
                            (move-error :incomplete-move
                                        "Complete the move selection before confirming."
                                        {:stage (:stage (move-selection db))}))
     (let [command (move-command db)
           result (confirmed-move-result db command transition-options)]
       (apply-confirmed-move-result db result)))))
