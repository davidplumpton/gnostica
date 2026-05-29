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
    :summary "Discard hand cards, then draw toward the six-card hand limit."
    :requirements [:draw-count]}
   :orient-piece
   {:id :orient-piece
    :label "Orient piece"
    :summary "Turn one of your pieces to a legal direction."
    :requirements [:piece-id :orientation]}
   :place-initial-small
   {:id :place-initial-small
    :label "Place first piece"
    :summary "Special rule: with no pieces, put your first small piece on an empty territory or wasteland."
    :requirements [:target-space :orientation]}})

(def move-power-order
  [:empress :emperor :lovers :chariot :hanged-man :temperance
   :justice :death :tower :moon
   :cup :rod :disc :sun :sword
   :fool :high-priestess :judgement :hierophant :hermit :devil :world])

(def move-power-definitions
  {:empress {:id :empress
             :label "Empress"}
   :emperor {:id :emperor
             :label "Emperor"}
   :lovers {:id :lovers
            :label "Lovers"}
   :chariot {:id :chariot
             :label "Chariot"}
   :hanged-man {:id :hanged-man
                :label "Hanged Man"}
   :temperance {:id :temperance
                :label "Temperance"}
   :justice {:id :justice
             :label "Justice"}
   :death {:id :death
           :label "Death"}
   :tower {:id :tower
           :label "Tower"}
   :moon {:id :moon
          :label "Moon"}
   :cup {:id :cup
         :label "Cup"}
   :rod {:id :rod
         :label "Rod"}
   :disc {:id :disc
          :label "Disc"}
   :sun {:id :sun
         :label "Sun"}
   :sword {:id :sword
           :label "Sword"}
   :fool {:id :fool
          :label "Fool"}
   :high-priestess {:id :high-priestess
                    :label "High Priestess"}
   :judgement {:id :judgement
               :label "Judgement"}
   :hierophant {:id :hierophant
                :label "Hierophant"}
   :hermit {:id :hermit
            :label "Hermit"}
   :devil {:id :devil
           :label "Devil"}
   :world {:id :world
           :label "World"}})

(def copied-suit-powers
  #{:cup :rod :disc :sword})

(def composite-major-card-powers
  {"empress" :empress
   "emperor" :emperor
   "lovers" :lovers
   "chariot" :chariot
   "hangedman" :hanged-man
   "temperance" :temperance})

(def composite-major-powers
  (set (vals composite-major-card-powers)))

(def sword-major-card-powers
  {"justice" :justice
   "death" :death
   "tower" :tower
   "moon" :moon})

(def sword-major-powers
  (set (vals sword-major-card-powers)))

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

(def sun-disc-mode-order
  [:skip
   :created-piece
   :created-territory
   :piece
   :territory])

(def sun-disc-modes
  (set sun-disc-mode-order))

(def sun-disc-mode-definitions
  {:skip {:id :skip
          :label "Cup only"}
   :created-piece {:id :created-piece
                   :label "Grow created piece"}
   :created-territory {:id :created-territory
                       :label "Grow created territory"}
   :piece {:id :piece
           :label "Grow existing piece"}
   :territory {:id :territory
               :label "Grow existing territory"}})

(def sword-target-kind-order
  [:piece
   :territory])

(def sword-target-kinds
  (set sword-target-kind-order))

(def sword-target-kind-definitions
  {:piece {:id :piece
           :label "Attack piece"}
   :territory {:id :territory
               :label "Attack territory"}})

(def fool-reveal-count-order
  [0 1 2])

(def high-priestess-redraw-count-order
  [0 1 2])

(def strength-disc-action-count-order
  [1 2])

(def death-sword-action-count-order
  [1 2])

(def devil-action-count-order
  [1 2 3])

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
   :disc-action-count "Choose how many Disc actions to apply."
   :sword-action-count "Choose how many Sword actions to apply."
   :devil-action-count "Choose how many Devil orientations to apply."
   :minion-orientation "Choose the minion orientation."
   :sun-disc-mode "Choose whether Sun applies Disc."
   :disc-target-kind "Choose piece or territory growth."
   :sword-target-kind "Choose piece or territory attack."
   :fool-reveal-count "Choose how many cards Fool reveals."
   :high-priestess-redraw-count "Choose how many redraw passes High Priestess applies."
   :high-priestess-redraws "Choose cards and draw counts for each redraw pass."
   :judgement-card-selection "Choose discard-pile cards for Judgement."
   :copied-board-index "Choose a major territory for World to copy."
   :copied-power "Choose the copied power."
   :target-piece-id "Choose a target piece."
   :target-board-index "Choose a target territory."
   :target-space "Choose a target territory, enemy piece, or wasteland."
   :hermit-target-space "Choose a targeted piece or territory."
   :hermit-destination-space "Choose a destination territory or wasteland."
   :initial-target-space "Choose an empty territory or wasteland."
   :territory-card-source "Choose where the new territory card comes from."
   :one-point-card-id "Choose a one-point card from the current player's hand."
   :replacement-card-source "Choose where the replacement card comes from."
   :replacement-card-id "Choose a replacement card."
   :orientation "Choose an orientation."
   :distance "Choose a distance."
   :damage "Choose damage."
   :draw-count "Choose cards to discard, then how many cards to draw."})

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

(defn- board-cell-by-index [db index]
  (layout/cell-by-index (board db) index))

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

(defn- game-turn-key [state]
  (select-keys (:turn state)
               [:current-player-id :current-player-index :round]))

(defn turn-action-consumed? [db]
  (let [record (:turn-action db)]
    (boolean
     (and (true? (:consumed? record))
          (= (:turn-key record)
             (game-turn-key (game db)))))))

(defn- piece-coordinate [db piece]
  (if-let [{:keys [row col]} (:space piece)]
    [row col]
    (when-let [{:keys [row col]} (board-cell-by-index db (:space-index piece))]
      [row col])))

(defn- pieces-at-coordinate [db row col]
  (filterv #(= [row col] (piece-coordinate db %))
           (board-pieces db)))

(defn- same-coordinate? [left right]
  (= left right))

(defn- minion-target-coordinate [db piece]
  (when-let [coordinate (piece-coordinate db piece)]
    (when-let [{:keys [row col]} (game-state/target-coordinate coordinate
                                                               (:orientation piece))]
      [row col])))

(defn- targetable-piece? [db minion piece]
  (let [target-coordinate (piece-coordinate db piece)]
    (and minion
         piece
         target-coordinate
         (or (= (:id minion) (:id piece))
             (same-coordinate? target-coordinate
                               (minion-target-coordinate db minion))))))

(defn- targetable-territory? [db minion cell]
  (and minion
       cell
       (same-coordinate? [(:row cell) (:col cell)]
                         (minion-target-coordinate db minion))))

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
  (:card (board-cell-by-index db (:source-board-index params))))

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
      (contains? composite-major-card-powers (:id card))
      (conj (get composite-major-card-powers (:id card)))
      (contains? sword-major-card-powers (:id card))
      (conj (get sword-major-card-powers (:id card)))
      (cards/cup-card? card) (conj :cup)
      (cards/rod-card? card) (conj :rod)
      (cards/disc-card? card) (conj :disc)
      (= "sun" (:id card)) (conj :sun)
      (cards/sword-card? card) (conj :sword)
      (= "fool" (:id card)) (conj :fool)
      (= "high-priestess" (:id card)) (conj :high-priestess)
      (= "judgement" (:id card)) (conj :judgement)
      (= "hierophant" (:id card)) (conj :hierophant)
      (= "hermit" (:id card)) (conj :hermit)
      (= "devil" (:id card)) (conj :devil)
      (= "world" (:id card)) (conj :world))))

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
        :unavailable

        :else
        nil))))

(defn- world-move? [db source-id params]
  (= :world (selected-power db source-id params)))

(defn- world-copy-board-indexes [db]
  (set (map :board-index (game-state/world-major-territories (game db)))))

(defn- world-copy-board-cell [db board-index]
  (when (contains? (world-copy-board-indexes db) board-index)
    (board-cell-by-index db board-index)))

(defn- world-copied-card [db params]
  (:card (world-copy-board-cell db (:copied-board-index params))))

(defn- world-copied-power-ids-for-card [card]
  (vec (remove #{:world} (move-power-ids-for-card card))))

(defn- selected-world-copied-power [db source-id params]
  (when (world-move? db source-id params)
    (let [card (world-copied-card db params)
          power-options (world-copied-power-ids-for-card card)
          selected (:copied-power params)]
      (cond
        (contains? (set power-options) selected)
        selected

        (= 1 (count power-options))
        (first power-options)

        (and card (empty? power-options))
        :unavailable

        :else
        nil))))

(defn- active-power [db source-id params]
  (if (world-move? db source-id params)
    (selected-world-copied-power db source-id params)
    (selected-power db source-id params)))

(defn- active-card [db source-id params]
  (if (world-move? db source-id params)
    (world-copied-card db params)
    (source-card db source-id params)))

(defn- world-source-opts [db source-id params]
  (when (world-move? db source-id params)
    (let [source-result (game-state/resolve-major-source
                         (game db)
                         {:player-id (current-player-id db)
                          :source (source-command source-id params)})
          copied-card (world-copied-card db params)]
      (when (and (:ok? source-result) copied-card)
        (assoc (game-state/major-paid-source-opts source-result)
               :power-card copied-card
               :allow-major-minion? true)))))

(defn- completed-major-actions [params]
  (vec (:major-actions params)))

(defn- completed-major-action-count [params]
  (count (completed-major-actions params)))

(defn- composite-major-move? [db source-id params]
  (contains? composite-major-powers (active-power db source-id params)))

(defn- active-composite-action-power [db source-id params]
  (case (active-power db source-id params)
    :empress (if (zero? (completed-major-action-count params))
               :orient-minion
               :cup)
    :emperor (if (zero? (completed-major-action-count params))
               :orient-minion
               :rod)
    :lovers (if (zero? (completed-major-action-count params))
              :rod
              :cup)
    :chariot :rod
    :hanged-man (if (zero? (completed-major-action-count params))
                  :rod
                  :trade-hand)
    :temperance :cup
    nil))

(defn- composite-action-power? [db source-id params power]
  (= power (active-composite-action-power db source-id params)))

(defn- sword-major-move? [db source-id params]
  (contains? sword-major-powers (active-power db source-id params)))

(defn- active-sword-major-action-power [db source-id params]
  (case (active-power db source-id params)
    :justice (if (zero? (completed-major-action-count params))
               :trade-hand
               :sword)
    :death :sword
    :tower (if (zero? (completed-major-action-count params))
             :orient-minion
             :sword)
    :moon (if (zero? (completed-major-action-count params))
            :rod
            :sword)
    nil))

(defn- sword-major-action-power? [db source-id params power]
  (= power (active-sword-major-action-power db source-id params)))

(defn- hanged-man-trade-stage? [db source-id params]
  (composite-action-power? db source-id params :trade-hand))

(defn- justice-trade-stage? [db source-id params]
  (sword-major-action-power? db source-id params :trade-hand))

(defn- major-orient-step? [db source-id params]
  (or (composite-action-power? db source-id params :orient-minion)
      (sword-major-action-power? db source-id params :orient-minion)))

(defn- cup-move? [db source-id params]
  (or (= :cup (active-power db source-id params))
      (composite-action-power? db source-id params :cup)))

(defn- sun-move? [db source-id params]
  (= :sun (active-power db source-id params)))

(defn- selected-cup-variant [db source-id params]
  (when (cup-move? db source-id params)
    (cards/cup-variant (active-card db source-id params))))

(defn- territory-card-source-option-ids [db source-id params]
  (when (cup-move? db source-id params)
    (if (= :wheel-cup (selected-cup-variant db source-id params))
      territory-card-source-order
      [:hand])))

(defn- rod-move? [db source-id params]
  (or (= :rod (active-power db source-id params))
      (composite-action-power? db source-id params :rod)
      (sword-major-action-power? db source-id params :rod)))

(defn- selected-rod-variant [db source-id params]
  (when (rod-move? db source-id params)
    (cards/rod-variant (active-card db source-id params))))

(defn- disc-move? [db source-id params]
  (= :disc (active-power db source-id params)))

(defn- selected-disc-variant [db source-id params]
  (when (disc-move? db source-id params)
    (cards/disc-variant (active-card db source-id params))))

(defn- strength-disc-source? [db source-id params]
  (and (disc-move? db source-id params)
       (= "strength" (:id (active-card db source-id params)))))

(defn- star-disc-source? [db source-id params]
  (and (disc-move? db source-id params)
       (= "star" (:id (active-card db source-id params)))))

(defn- disc-action-count-option-values [db source-id params]
  (if (strength-disc-source? db source-id params)
    strength-disc-action-count-order
    []))

(defn- selected-disc-action-count [db source-id params]
  (if (strength-disc-source? db source-id params)
    (if (some #{(:disc-action-count params)}
              strength-disc-action-count-order)
      (:disc-action-count params)
      1)
    1))

(defn- card-worth-disc-actions-more? [replacement-card original-card action-count]
  (let [original-value (cards/card-point-value original-card)
        replacement-value (cards/card-point-value replacement-card)]
    (and (some? original-value)
         (= (+ original-value action-count)
            replacement-value))))

(defn- sword-move? [db source-id params]
  (or (= :sword (active-power db source-id params))
      (sword-major-action-power? db source-id params :sword)))

(defn- death-sword-source? [db source-id params]
  (= :death (active-power db source-id params)))

(defn- death-sword-action-count-option-values [db source-id params]
  (if (death-sword-source? db source-id params)
    death-sword-action-count-order
    []))

(defn- selected-death-sword-action-count [params]
  (if (some #{(:sword-action-count params)}
            death-sword-action-count-order)
    (:sword-action-count params)
    1))

(defn- fool-move? [db source-id params]
  (= :fool (active-power db source-id params)))

(defn- high-priestess-move? [db source-id params]
  (= :high-priestess (active-power db source-id params)))

(defn- judgement-move? [db source-id params]
  (= :judgement (active-power db source-id params)))

(defn- hierophant-move? [db source-id params]
  (= :hierophant (active-power db source-id params)))

(defn- hermit-move? [db source-id params]
  (= :hermit (active-power db source-id params)))

(defn- devil-move? [db source-id params]
  (= :devil (active-power db source-id params)))

(defn- devil-action-count-option-values [db source-id params]
  (if (devil-move? db source-id params)
    devil-action-count-order
    []))

(defn- selected-devil-action-count [params]
  (if (some #{(:devil-action-count params)}
            devil-action-count-order)
    (:devil-action-count params)
    1))

(defn- manipulation-piece-power? [db source-id params]
  (or (hierophant-move? db source-id params)
      (devil-move? db source-id params)))

(defn- selected-sword-variant [db source-id params]
  (when (sword-move? db source-id params)
    (cards/sword-variant (active-card db source-id params))))

(defn- card-worth-sword-damage-less? [replacement-card original-card damage]
  (let [original-value (cards/card-point-value original-card)
        replacement-value (cards/card-point-value replacement-card)]
    (and (some? original-value)
         (pos? (- original-value damage))
         (= (- original-value damage)
            replacement-value))))

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
  (some? (board-cell-by-index db index)))

(defn- target-board-cell [db params]
  (board-cell-by-index db (:target-board-index params)))

(defn- disc-base-command [db source-id params]
  (when (and (disc-move? db source-id params)
             (current-player-id db)
             (:piece-id params))
    (cond-> {:player-id (current-player-id db)
             :source (source-command source-id params)}
      (selected-disc-variant db source-id params)
      (assoc :disc-variant (selected-disc-variant db source-id params))

      (and (star-disc-source? db source-id params)
           (:minion-orientation params))
      (assoc :minion-orientation (:minion-orientation params)))))

(defn- disc-resolution-state [state command]
  (if-let [orientation (:minion-orientation command)]
    (let [orient-result (game-state/apply-orient-move
                         state
                         {:player-id (:player-id command)
                          :piece-id (get-in command [:source :piece-id])
                          :orientation orientation})]
      (when (:ok? orient-result)
        (:state orient-result)))
    state))

(defn- disc-command-resolves? [db source-id params command]
  (let [state (game db)]
    (boolean
     (and state
          command
          (when-let [resolution-state (disc-resolution-state state command)]
            (:ok? (game-state/resolve-disc-command
                   resolution-state
                   (dissoc command :minion-orientation)
                   (or (world-source-opts db source-id params) {}))))))))

(defn- disc-piece-target? [db source-id params piece]
  (and piece
       (= :piece (:disc-target-kind params))
       (disc-command-resolves?
        db
        source-id
        params
        (assoc (disc-base-command db source-id params)
               :target {:kind :piece
                        :piece-id (:id piece)}))))

(defn- disc-territory-target? [db source-id params cell]
  (and cell
       (= :territory (:disc-target-kind params))
       (disc-command-resolves?
        db
        source-id
        params
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
           (filter #(and (card-worth-disc-actions-more?
                          %
                          original-card
                          (selected-disc-action-count db source-id params))
                         (or (not= :hand replacement-source)
                             (not= source-card-id (:id %)))))
           vec))))

(defn- disc-replacement-card-by-id [db source-id params card-id]
  (some #(when (= card-id (:id %)) %)
        (disc-replacement-card-options-for db source-id params)))

(defn- completed-major-action-by-power [params power]
  (some #(when (= power (:power %)) %)
        (completed-major-actions params)))

(defn- tower-minion-orientation [params]
  (or (:minion-orientation params)
      (:orientation (completed-major-action-by-power params :orient-minion))))

(defn- sword-base-command [db source-id params]
  (when (and (sword-move? db source-id params)
             (current-player-id db)
             (:piece-id params))
    (cond-> {:player-id (current-player-id db)
             :source (source-command source-id params)}
      (selected-sword-variant db source-id params)
      (assoc :sword-variant (selected-sword-variant db source-id params)))))

(defn- sword-resolution-state [db source-id params command]
  (let [state (game db)]
    (cond
      (nil? state)
      nil

      (and (= :tower (active-power db source-id params))
           (tower-minion-orientation params))
      (let [orient-result (game-state/apply-orient-move
                           state
                           {:player-id (:player-id command)
                            :piece-id (get-in command [:source :piece-id])
                            :orientation (tower-minion-orientation params)})]
        (when (:ok? orient-result)
          (:state orient-result)))

      :else
      state)))

(defn- moon-command-resolves? [db source-id params command]
  (let [rod-action (completed-major-action-by-power params :rod)]
    (boolean
     (and rod-action
          (let [moon-command {:player-id (current-player-id db)
                              :source (source-command source-id params)
                              :rod rod-action
                              :sword (assoc (dissoc command
                                                     :player-id
                                                     :source
                                                     :sword-variant)
                                            :piece-id (:piece-id params))}
                result (if (world-move? db source-id params)
                         (game-state/apply-world-move
                          (game db)
                          (assoc moon-command
                                 :copied-board-index (:copied-board-index params)))
                         (game-state/apply-moon-move (game db) moon-command))]
            (:ok? result))))))

(defn- sword-command-resolves? [db source-id params command]
  (let [state (game db)]
    (boolean
     (and state
          command
          (if (= :moon (active-power db source-id params))
            (moon-command-resolves? db source-id params command)
            (when-let [resolution-state (sword-resolution-state
                                         db
                                         source-id
                                         params
                                         command)]
              (:ok? (game-state/resolve-sword-command
                     resolution-state
                     command
                     (or (world-source-opts db source-id params) {})))))))))

(defn- sword-piece-target? [db source-id params piece]
  (and piece
       (= :piece (:sword-target-kind params))
       (sword-command-resolves?
        db
        source-id
        params
        (assoc (sword-base-command db source-id params)
               :target {:kind :piece
                        :piece-id (:id piece)}
               :damage 1))))

(defn- sword-territory-target? [db source-id params cell]
  (and cell
       (= :territory (:sword-target-kind params))
       (sword-command-resolves?
        db
        source-id
        params
        (assoc (sword-base-command db source-id params)
               :target {:kind :territory
                        :board-index (:index cell)}
               :damage 1))))

(defn- sword-target-piece [db source-id params]
  (let [piece (piece-by-id db (:target-piece-id params))]
    (when (sword-piece-target? db source-id params piece)
      piece)))

(defn- sword-territory-destroyed? [db params]
  (let [target-value (some-> (target-board-cell db params)
                             :card
                             cards/card-point-value)]
    (and (some? target-value)
         (= target-value (:damage params)))))

(defn- sword-replacement-card-source-option-ids [db source-id params]
  (when (and (sword-move? db source-id params)
             (= :territory (:sword-target-kind params))
             (some? (:target-board-index params))
             (some? (:damage params))
             (not (sword-territory-destroyed? db params)))
    (if (= :sword-from-discard (selected-sword-variant db source-id params))
      disc-replacement-card-source-order
      [:hand])))

(defn- selected-sword-replacement-card-source [db source-id params]
  (let [options (set (sword-replacement-card-source-option-ids db source-id params))
        selected (:replacement-card-source params)]
    (cond
      (contains? options selected)
      selected

      (= 1 (count options))
      (first options)

      :else
      nil)))

(defn- sword-replacement-card-options-for [db source-id params]
  (let [target-cell (target-board-cell db params)
        original-card (:card target-cell)
        damage (:damage params)
        source-card-id (source-hand-card-id source-id params)
        replacement-source (selected-sword-replacement-card-source db source-id params)]
    (if-not (and original-card damage replacement-source)
      []
      (->> (case replacement-source
             :hand (current-player-hand db)
             :discard-pile (discard-replacement-options db source-id params)
             [])
           (filter #(and (card-worth-sword-damage-less?
                          %
                          original-card
                          damage)
                         (or (not= :hand replacement-source)
                             (not= source-card-id (:id %)))))
           vec))))

(defn- sword-replacement-card-by-id [db source-id params card-id]
  (some #(when (= card-id (:id %)) %)
        (sword-replacement-card-options-for db source-id params)))

(defn- sword-damage-options-for [db source-id params]
  (if-not (sword-move? db source-id params)
    []
    (let [piece (piece-by-id db (:piece-id params))
          max-damage (pieces/pips piece)
          target-pips (case (:sword-target-kind params)
                        :piece (some-> (sword-target-piece db source-id params)
                                       pieces/pips)
                        :territory (when (sword-territory-target?
                                          db
                                          source-id
                                          params
                                          (target-board-cell db params))
                                     (some-> (target-board-cell db params)
                                             :card
                                             cards/card-point-value))
                        nil)]
      (if (and (int? max-damage)
               (pos? max-damage)
               (int? target-pips)
               (pos? target-pips))
        (vec (range 1 (inc (min max-damage target-pips))))
        []))))

(defn- sword-orientation-available? [db source-id params]
  (let [target-piece (sword-target-piece db source-id params)
        target-pips (pieces/pips target-piece)
        damage (:damage params)]
    (and (sword-move? db source-id params)
         (= :piece (:sword-target-kind params))
         (current-player-piece? db target-piece)
         (int? target-pips)
         (int? damage)
         (< damage target-pips))))

(defn- apply-composite-preview [state power command]
  (case power
    :empress (game-state/apply-empress-move state command)
    :emperor (game-state/apply-emperor-move state command)
    :lovers (game-state/apply-lovers-move state command)
    :chariot (game-state/apply-chariot-move state command)
    :hanged-man (game-state/apply-hanged-man-move state command)
    :temperance (game-state/apply-temperance-move state command)
    nil))

(defn- cup-target-state [db source-id params]
  (let [state (game db)
        actions (completed-major-actions params)]
    (if (and state
             (seq actions)
             (composite-action-power? db source-id params :cup))
      (let [command (cond-> {:player-id (current-player-id db)
                             :source (source-command source-id params)
                             :actions actions}
                      (world-move? db source-id params)
                      (assoc :copied-board-index (:copied-board-index params)))
            result (if (world-move? db source-id params)
                     (game-state/apply-world-move state command)
                     (apply-composite-preview state
                                              (active-power db source-id params)
                                              command))]
        (if (:ok? result)
          (:state result)
          state))
      state)))

(defn- cup-target-db [db source-id params]
  (if-let [state (cup-target-state db source-id params)]
    (assoc db :game state)
    db))

(defn- cup-target-piece? [db source-id params piece]
  (let [target-db (cup-target-db db source-id params)
        target-piece (piece-by-id target-db (:id piece))
        minion (piece-by-id target-db (:piece-id params))]
    (and target-piece
         (not (current-player-piece? target-db target-piece))
         (valid-board-index? target-db (:space-index target-piece))
         (targetable-piece? target-db minion target-piece))))

(defn- cup-target-territory? [db source-id params cell]
  (let [target-db (cup-target-db db source-id params)
        target-cell (board-cell-by-index target-db (:index cell))
        minion (piece-by-id target-db (:piece-id params))]
    (and target-cell
         (targetable-territory? target-db minion target-cell))))

(defn- cup-target-piece [db params]
  (let [source (get-in db [:move-selection :source])
        piece (piece-by-id (cup-target-db db source params)
                           (:target-piece-id params))]
    (when (cup-target-piece? db source params piece)
      piece)))

(defn- empty-board-target? [db index]
  (when-let [{:keys [row col]} (board-cell-by-index db index)]
    (empty? (pieces-at-coordinate db row col))))

(defn- empty-wasteland-target? [db {:keys [row col]}]
  (empty? (pieces-at-coordinate db row col)))

(declare valid-wasteland-target? enemy-pieces-at-coordinate)

(defn- cup-target-wasteland? [db source-id params {:keys [row col] :as space}]
  (let [target-db (cup-target-db db source-id params)
        minion (piece-by-id target-db (:piece-id params))]
    (and space
         (= [row col] (minion-target-coordinate target-db minion))
         (empty? (enemy-pieces-at-coordinate target-db row col)))))

(defn- enemy-pieces-at-coordinate [db row col]
  (let [player-id (current-player-id db)]
    (filterv #(and (not= player-id (:player-id %))
                   (= [row col] (piece-coordinate db %)))
             (board-pieces db))))

(defn- current-player-stash-count [db size]
  (or (get-in (current-player db) [:stash size])
      (get-in db [:game :pieces :stashes (current-player-id db) size])
      0))

(defn- major-target-piece? [db params piece]
  (let [minion (piece-by-id db (:piece-id params))]
    (and piece
         (targetable-piece? db minion piece))))

(defn- devil-target-state [db source-id params]
  (let [state (game db)
        actions (completed-major-actions params)]
    (if (and state
             (seq actions)
             (devil-move? db source-id params))
      (let [command (cond-> {:player-id (current-player-id db)
                             :source (source-command source-id params)
                             :actions actions}
                      (world-move? db source-id params)
                      (assoc :copied-board-index (:copied-board-index params)))
            result (if (world-move? db source-id params)
                     (game-state/apply-world-move state command)
                     (game-state/apply-devil-move state command))]
        (if (:ok? result)
          (:state result)
          state))
      state)))

(defn- devil-target-db [db source-id params]
  (if-let [state (devil-target-state db source-id params)]
    (assoc db :game state)
    db))

(defn- devil-target-piece? [db source-id params piece]
  (let [target-db (devil-target-db db source-id params)
        target-piece (piece-by-id target-db (:id piece))
        minion (piece-by-id target-db (:piece-id params))]
    (and target-piece
         (targetable-piece? target-db minion target-piece))))

(defn- hierophant-target-piece? [db params piece]
  (and (major-target-piece? db params piece)
       (pos? (current-player-stash-count db (:size piece)))))

(defn- hermit-target-piece? [db params piece]
  (and (nil? (:target-board-index params))
       (major-target-piece? db params piece)))

(defn- hermit-target-territory? [db params cell]
  (let [minion (piece-by-id db (:piece-id params))]
    (and (nil? (:target-piece-id params))
         (targetable-territory? db minion cell)
         (empty? (enemy-pieces-at-coordinate db (:row cell) (:col cell))))))

(defn- hermit-target-selected? [params]
  (or (:target-piece-id params)
      (some? (:target-board-index params))))

(defn- hermit-piece-target-selected? [params]
  (some? (:target-piece-id params)))

(defn- hermit-territory-target-selected? [params]
  (some? (:target-board-index params)))

(defn- hermit-empty-board-destination? [db index]
  (and (some? index)
       (empty-board-target? db index)))

(defn- hermit-empty-wasteland-destination? [db target]
  (and (valid-wasteland-target? db target)
       (empty-wasteland-target? db target)))

(defn- hermit-territory-wasteland-destination? [db target]
  (and (valid-wasteland-target? db target)
       (empty? (enemy-pieces-at-coordinate db (:row target) (:col target)))))

(defn- hermit-destination-complete? [db params]
  (cond
    (hermit-piece-target-selected? params)
    (or (hermit-empty-board-destination? db (:hermit-destination-board-index params))
        (hermit-empty-wasteland-destination? db (:hermit-destination-wasteland params)))

    (hermit-territory-target-selected? params)
    (hermit-territory-wasteland-destination? db (:hermit-destination-wasteland params))

    :else
    false))

(defn- hermit-orientation-required? [db params]
  (when-let [piece (piece-by-id db (:target-piece-id params))]
    (current-player-piece? db piece)))

(defn move-target-wasteland-options [db]
  (let [source (get-in db [:move-selection :source])
        params (get-in db [:move-selection :params])
        target-db (if (or (cup-move? db source params)
                          (sun-move? db source params))
                    (cup-target-db db source params)
                    db)
        spaces (layout/wasteland-spaces (board target-db))]
    (cond
      (= :place-initial-small source)
      (filterv #(empty-wasteland-target? db %) spaces)

      (or (cup-move? db source params)
          (sun-move? db source params))
      (filterv #(cup-target-wasteland? db source params %) spaces)

      (hermit-move? db source params)
      (cond
        (hermit-piece-target-selected? params)
        (filterv #(empty-wasteland-target? db %) spaces)

        (hermit-territory-target-selected? params)
        (filterv #(empty? (enemy-pieces-at-coordinate db (:row %) (:col %)))
                 spaces)

        :else
        [])

      :else
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

    (or (some? (some #(when (= (:target-board-index params) (:index %)) %)
                     (filterv #(cup-target-territory? db source-id params %)
                              (board (cup-target-db db source-id params)))))
        (some? (cup-target-piece db params))
        (valid-wasteland-target? db (:target-wasteland params)))))

(defn- target-resolution-complete? [db source-id params]
  (cond
    (some? (cup-target-piece db params))
    true

    (some? (some #(when (= (:target-board-index params) (:index %)) %)
                 (filterv #(cup-target-territory? db source-id params %)
                          (board (cup-target-db db source-id params)))))
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

(defn- sun-cup-target-kind [params]
  (cond
    (:target-wasteland params) :wasteland
    (:target-piece-id params) :piece
    (some? (:target-board-index params)) :territory
    :else nil))

(defn- sun-cup-target-ready? [db source-id params]
  (and (sun-move? db source-id params)
       (target-space-complete? db source-id params)
       (case (sun-cup-target-kind params)
         :territory (contains? pieces/legal-orientations (:orientation params))
         (:piece :wasteland) true
         false)))

(defn- sun-disc-mode-option-ids [db source-id params]
  (when (sun-cup-target-ready? db source-id params)
    (vec
     (concat [:skip]
             (case (sun-cup-target-kind params)
               :territory [:created-piece]
               :wasteland [:created-territory]
               [])
             [:piece :territory]))))

(defn- selected-sun-disc-mode [db source-id params]
  (let [options (set (sun-disc-mode-option-ids db source-id params))
        selected (:sun-disc-mode params)]
    (when (contains? options selected)
      selected)))

(defn- sun-cup-needs-one-point-card? [db source-id params]
  (and (sun-move? db source-id params)
       (= :wasteland (sun-cup-target-kind params))
       (some? (selected-sun-disc-mode db source-id params))
       (not= :created-territory
             (selected-sun-disc-mode db source-id params))))

(defn- sun-disc-base-command [db source-id params]
  (when (and (sun-move? db source-id params)
             (current-player-id db)
             (:piece-id params))
    {:player-id (current-player-id db)
     :source (source-command source-id params)
     :disc-variant :disc}))

(defn- sun-disc-command-resolves? [db command]
  (let [state (game db)]
    (boolean
     (and state
          command
          (:ok? (game-state/resolve-disc-command state command))))))

(defn- sun-disc-piece-target? [db source-id params piece]
  (and piece
       (= :piece (selected-sun-disc-mode db source-id params))
       (sun-disc-command-resolves?
        db
        (assoc (sun-disc-base-command db source-id params)
               :target {:kind :piece
                        :piece-id (:id piece)}))))

(defn- sun-disc-territory-target? [db source-id params cell]
  (and cell
       (= :territory (selected-sun-disc-mode db source-id params))
       (sun-disc-command-resolves?
        db
        (assoc (sun-disc-base-command db source-id params)
               :target {:kind :territory
                        :board-index (:index cell)}))))

(defn- sun-disc-target-piece [db source-id params]
  (let [piece (piece-by-id db (:sun-disc-target-piece-id params))]
    (when (sun-disc-piece-target? db source-id params piece)
      piece)))

(defn- sun-disc-orientation-available? [db source-id params]
  (and (= :piece (selected-sun-disc-mode db source-id params))
       (current-player-piece? db (sun-disc-target-piece db source-id params))))

(defn- sun-disc-target-cell [db params]
  (board-cell-by-index db (:sun-disc-target-board-index params)))

(defn- sun-disc-replacement-source-cards [db source-id params]
  (let [source-card-id (source-hand-card-id source-id params)
        spent-card-ids (cond-> #{}
                         source-card-id (conj source-card-id)
                         (:one-point-card-id params) (conj (:one-point-card-id params)))]
    (remove #(contains? spent-card-ids (:id %))
            (current-player-hand db))))

(defn- sun-disc-replacement-card-options-for [db source-id params]
  (let [replacement-cards (sun-disc-replacement-source-cards db source-id params)]
    (case (selected-sun-disc-mode db source-id params)
      :created-territory
      (->> replacement-cards
           (filter #(= 2 (cards/card-point-value %)))
           vec)

      :territory
      (let [original-card (:card (sun-disc-target-cell db params))]
        (if original-card
          (->> replacement-cards
               (filter #(card-worth-disc-actions-more? % original-card 1))
               vec)
          []))

      [])))

(defn- sun-disc-replacement-card-by-id [db source-id params card-id]
  (some #(when (= card-id (:id %)) %)
        (sun-disc-replacement-card-options-for db source-id params)))

(defn- sun-disc-territory-target-stage? [db source-id params]
  (and (sun-move? db source-id params)
       (= :territory (selected-sun-disc-mode db source-id params))))

(defn- rod-distance-options-for-piece [piece]
  (vec (range 1 (inc (or (pieces/pips piece) 0)))))

(defn- valid-discard-card-ids [db discard-card-ids]
  (let [hand-card-ids (set (map :id (current-player-hand db)))]
    (vec (filter hand-card-ids (distinct (or discard-card-ids []))))))

(defn max-draw-count
  ([db]
   (max-draw-count db (get-in db [:move-selection :params :discard-card-ids])))
  ([db discard-card-ids]
   (let [discard-count (count (valid-discard-card-ids db discard-card-ids))
         post-discard-hand-count (- (count (current-player-hand db))
                                    discard-count)
         hand-slots (- game-state/starting-hand-size
                       post-discard-hand-count)
         available-cards (+ (count (get-in db [:game :draw-pile] []))
                            (count (get-in db [:game :discard-pile] []))
                            discard-count)]
     (max 0 (min hand-slots available-cards)))))

(defn draw-count-options
  ([db]
   (draw-count-options db (get-in db [:move-selection :params :discard-card-ids])))
  ([db discard-card-ids]
   (let [discard-count (count (valid-discard-card-ids db discard-card-ids))
         min-draw-count (if (pos? discard-count) 0 1)
         max-draw-count (max-draw-count db discard-card-ids)]
     (if (< max-draw-count min-draw-count)
       []
       (vec (range min-draw-count (inc max-draw-count)))))))

(defn- max-potential-draw-count [db]
  (max-draw-count db (mapv :id (current-player-hand db))))

(defn- default-draw-count [options]
  (or (some #{1} options)
      (first options)))

(defn- normalize-draw-selection-params [db params]
  (let [discard-card-ids (valid-discard-card-ids db (:discard-card-ids params))
        params (assoc params :discard-card-ids discard-card-ids)
        options (draw-count-options db discard-card-ids)
        draw-count (:draw-count params)]
    (cond
      (some #{draw-count} options)
      params

      (seq options)
      (assoc params :draw-count (default-draw-count options))

      :else
      (dissoc params :draw-count))))

(defn- high-priestess-source-card [db source-id params]
  (when (= :play-hand-card source-id)
    (source-card db source-id params)))

(defn- redraw-pass-offset [pass-index]
  (when (and (int? pass-index)
             (<= 1 pass-index 2))
    (dec pass-index)))

(defn- high-priestess-initial-redraw-state [db source-id params]
  (let [source-card (high-priestess-source-card db source-id params)
        source-card-id (:id source-card)]
    {:hand (if source-card-id
             (vec (remove #(= source-card-id (:id %)) (current-player-hand db)))
             (current-player-hand db))
     :unknown-hand-count 0
     :draw-pile (vec (get-in db [:game :draw-pile] []))
     :unknown-draw-count 0
     :discard-pile (cond-> (discard-pile db)
                     source-card
                     (conj source-card))}))

(defn- high-priestess-hand-count [redraw-state]
  (+ (count (:hand redraw-state))
     (:unknown-hand-count redraw-state 0)))

(defn- high-priestess-valid-discard-card-ids-in-state [redraw-state discard-card-ids]
  (let [hand-card-ids (set (map :id (:hand redraw-state)))]
    (vec (filter hand-card-ids (distinct (or discard-card-ids []))))))

(defn- high-priestess-draw-count-options-in-state [redraw-state discard-card-ids]
  (let [discard-count (count (high-priestess-valid-discard-card-ids-in-state
                              redraw-state
                              discard-card-ids))
        post-discard-hand-count (- (high-priestess-hand-count redraw-state)
                                   discard-count)
        hand-slots (- game-state/starting-hand-size post-discard-hand-count)
        available-cards (+ (count (:draw-pile redraw-state))
                           (:unknown-draw-count redraw-state 0)
                           (count (:discard-pile redraw-state))
                           discard-count)
        max-draw-count (max 0 (min hand-slots available-cards))]
    (vec (range 0 (inc max-draw-count)))))

(defn- high-priestess-discard-staged-cards [redraw-state discard-card-ids]
  (let [valid-discard-card-ids (high-priestess-valid-discard-card-ids-in-state
                                redraw-state
                                discard-card-ids)
        discard-card-id-set (set valid-discard-card-ids)
        cards-to-discard (filterv #(contains? discard-card-id-set (:id %))
                                  (:hand redraw-state))]
    (-> redraw-state
        (update :hand
                (fn [hand]
                  (vec (remove #(contains? discard-card-id-set (:id %)) hand))))
        (update :discard-pile into cards-to-discard))))

(defn- high-priestess-refresh-staged-draw-pile [redraw-state]
  (if (and (empty? (:draw-pile redraw-state))
           (zero? (:unknown-draw-count redraw-state 0))
           (seq (:discard-pile redraw-state)))
    (let [discard-count (count (:discard-pile redraw-state))]
      (-> redraw-state
          (assoc :discard-pile [])
          (assoc :unknown-draw-count discard-count)))
    redraw-state))

(defn- high-priestess-stage-draw-one [redraw-state]
  (let [redraw-state (high-priestess-refresh-staged-draw-pile redraw-state)]
    (cond
      (seq (:draw-pile redraw-state))
      (let [card (first (:draw-pile redraw-state))]
        (-> redraw-state
            (update :draw-pile #(vec (rest %)))
            (update :hand conj card)))

      (pos? (:unknown-draw-count redraw-state 0))
      (-> redraw-state
          (update :unknown-draw-count dec)
          (update :unknown-hand-count inc))

      :else
      redraw-state)))

(defn- high-priestess-stage-draw [redraw-state draw-count]
  (let [drawn-state (nth (iterate high-priestess-stage-draw-one redraw-state)
                         draw-count)]
    (if (pos? draw-count)
      (high-priestess-refresh-staged-draw-pile drawn-state)
      drawn-state)))

(defn- high-priestess-apply-staged-redraw-pass [redraw-state pass]
  (let [discard-card-ids (high-priestess-valid-discard-card-ids-in-state
                          redraw-state
                          (:discard-card-ids pass))
        options (set (high-priestess-draw-count-options-in-state
                      redraw-state
                      discard-card-ids))
        draw-count (if (contains? options (:draw-count pass))
                     (:draw-count pass)
                     0)]
    (-> redraw-state
        (high-priestess-discard-staged-cards discard-card-ids)
        (high-priestess-stage-draw draw-count))))

(defn- high-priestess-redraw-state-before-pass [db source-id params pass-index]
  (when-let [target-offset (redraw-pass-offset pass-index)]
    (loop [redraw-state (high-priestess-initial-redraw-state db source-id params)
           offset 0]
      (if (>= offset target-offset)
        redraw-state
        (recur (high-priestess-apply-staged-redraw-pass
                redraw-state
                (get (vec (:redraws params)) offset {}))
               (inc offset))))))

(defn- high-priestess-hand-card-options
  ([db source-id params]
   (:hand (high-priestess-initial-redraw-state db source-id params)))
  ([db source-id params pass-index]
   (if-let [redraw-state (high-priestess-redraw-state-before-pass
                          db
                          source-id
                          params
                          pass-index)]
     (:hand redraw-state)
     [])))

(defn- high-priestess-valid-discard-card-ids
  ([db source-id params discard-card-ids]
   (high-priestess-valid-discard-card-ids-in-state
    (high-priestess-initial-redraw-state db source-id params)
    discard-card-ids))
  ([db source-id params pass-index discard-card-ids]
   (if-let [redraw-state (high-priestess-redraw-state-before-pass
                          db
                          source-id
                          params
                          pass-index)]
     (high-priestess-valid-discard-card-ids-in-state
      redraw-state
      discard-card-ids)
     [])))

(defn- high-priestess-draw-count-options
  ([db source-id params discard-card-ids]
   (high-priestess-draw-count-options-in-state
    (high-priestess-initial-redraw-state db source-id params)
    discard-card-ids))
  ([db source-id params pass-index discard-card-ids]
   (if-let [redraw-state (high-priestess-redraw-state-before-pass
                          db
                          source-id
                          params
                          pass-index)]
     (high-priestess-draw-count-options-in-state
      redraw-state
      discard-card-ids)
     [])))

(defn- selected-high-priestess-redraw-count [params]
  (when (some #{(:high-priestess-redraw-count params)}
              high-priestess-redraw-count-order)
    (:high-priestess-redraw-count params)))

(defn- high-priestess-redraw-pass [params pass-index]
  (let [offset (redraw-pass-offset pass-index)]
    (when (some? offset)
      (get (vec (:redraws params)) offset {:discard-card-ids []}))))

(defn- normalize-high-priestess-redraws [db source-id params]
  (if-let [redraw-count (selected-high-priestess-redraw-count params)]
    (let [redraws (vec (:redraws params))]
      (loop [redraw-state (high-priestess-initial-redraw-state db source-id params)
             offset 0
             normalized-redraws []]
        (if (= offset redraw-count)
          (assoc params :redraws normalized-redraws)
          (let [pass (get redraws offset {})
                discard-card-ids (high-priestess-valid-discard-card-ids-in-state
                                  redraw-state
                                  (:discard-card-ids pass))
                options (set (high-priestess-draw-count-options-in-state
                              redraw-state
                              discard-card-ids))
                normalized-pass (cond-> (assoc pass :discard-card-ids discard-card-ids)
                                  (not (contains? options (:draw-count pass)))
                                  (dissoc :draw-count))]
            (recur (high-priestess-apply-staged-redraw-pass
                    redraw-state
                    normalized-pass)
                   (inc offset)
                   (conj normalized-redraws normalized-pass))))))
    (dissoc params :redraws)))

(defn- high-priestess-redraws-complete? [db source-id params]
  (if-let [redraw-count (selected-high-priestess-redraw-count params)]
    (let [redraws (vec (:redraws params))]
      (loop [redraw-state (high-priestess-initial-redraw-state db source-id params)
             offset 0]
        (if (= offset redraw-count)
          true
          (let [pass (get redraws offset)
                discard-card-ids (high-priestess-valid-discard-card-ids-in-state
                                  redraw-state
                                  (:discard-card-ids pass))
                options (set (high-priestess-draw-count-options-in-state
                              redraw-state
                              discard-card-ids))
                normalized-pass (cond-> (assoc pass :discard-card-ids discard-card-ids)
                                  (not (contains? options (:draw-count pass)))
                                  (dissoc :draw-count))]
            (if (and (= (vec (:discard-card-ids pass)) discard-card-ids)
                     (contains? options (:draw-count pass)))
              (recur (high-priestess-apply-staged-redraw-pass
                      redraw-state
                      normalized-pass)
                     (inc offset))
              false)))))
    false))

(defn- judgement-discard-card-options [db source-id params]
  (let [source-card (source-card db source-id params)]
    (cond-> (discard-pile db)
      (and (= :play-hand-card source-id)
           (judgement-move? db source-id params)
           source-card)
      (conj source-card))))

(defn- judgement-card-maximum [db source-id params]
  (let [source-cost-count (if (and (= :play-hand-card source-id)
                                   (judgement-move? db source-id params)
                                   (source-card db source-id params))
                            1
                            0)
        hand-count (- (count (current-player-hand db)) source-cost-count)
        hand-slots (max 0 (- game-state/starting-hand-size hand-count))
        minion-pips (or (some-> (piece-by-id db (:piece-id params)) pieces/pips) 0)]
    (min minion-pips hand-slots)))

(defn- valid-judgement-card-ids [db source-id params card-ids]
  (let [selected-card-ids (set (or card-ids []))]
    (->> (judgement-discard-card-options db source-id params)
         (map :id)
         (filter selected-card-ids)
         (take (judgement-card-maximum db source-id params))
         vec)))

(defn- judgement-card-selection-complete? [db source-id params]
  (= (vec (:judgement-card-ids params))
     (valid-judgement-card-ids db source-id params (:judgement-card-ids params))))

(defn- small-stash-count [db]
  (or (get-in (current-player db) [:stash :small])
      (get-in db [:game :pieces :stashes (current-player-id db) :small])
      0))

(defn- source-unavailable-reason [db source-id]
  (let [player (current-player db)
        owned-pieces (current-player-pieces db)
        hand (current-player-hand db)
        max-draw (max-potential-draw-count db)
        turn-action-result (when player
                             (game-state/turn-action-unavailable-result
                              (game db)
                              (:id player)
                              source-id))]
    (cond
      (nil? player)
      "No current player is available."

      (game-state/finished? (game db))
      "The game is finished."

      (turn-action-consumed? db)
      "The current player has already taken a turn action."

      turn-action-result
      (get-in turn-action-result [:error :message])

      (= :activate-territory source-id)
      (when-not (seq owned-pieces)
        "The current player has no pieces on the board.")

      (= :play-hand-card source-id)
      (cond
        (empty? hand) "The current player has no hand cards."
        (empty? owned-pieces) "The current player needs a piece on the board.")

      (= :draw-cards source-id)
      (cond
        (empty? owned-pieces) "The current player has no pieces on the board."
        (zero? max-draw) "The current player cannot draw more cards.")

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

(defn move-world-copy-options [db]
  (let [copy-indexes (world-copy-board-indexes db)]
    (filterv #(contains? copy-indexes (:index %))
             (board db))))

(defn move-world-copied-power-options [db]
  (let [{:keys [source params]} (move-selection db)
        options (world-copied-power-ids-for-card (world-copied-card db params))]
    (if (world-move? db source params)
      (mapv move-power-definitions
            (filter #(contains? (set options) %)
                    move-power-order))
      [])))

(defn move-world-copied-power [db]
  (let [{:keys [source params]} (move-selection db)]
    (selected-world-copied-power db source params)))

(defn move-rod-mode-options [_db]
  (mapv rod-mode-definitions rod-mode-order))

(defn move-disc-action-count-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (disc-action-count-option-values db source params)))

(defn move-sword-action-count-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (death-sword-action-count-option-values db source params)))

(defn move-devil-action-count-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (devil-action-count-option-values db source params)))

(defn move-sun-disc-mode-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (mapv sun-disc-mode-definitions
          (sun-disc-mode-option-ids db source params))))

(defn move-fool-reveal-count-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (if (fool-move? db source params)
      fool-reveal-count-order
      [])))

(defn move-high-priestess-redraw-count-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (if (high-priestess-move? db source params)
      high-priestess-redraw-count-order
      [])))

(defn move-high-priestess-redraw-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (if-let [redraw-count (and (high-priestess-move? db source params)
                               (selected-high-priestess-redraw-count params))]
      (mapv (fn [pass-index]
              (let [pass (high-priestess-redraw-pass params pass-index)
                    discard-card-ids (:discard-card-ids pass)
                    draw-count-options (high-priestess-draw-count-options
                                        db
                                        source
                                        params
                                        pass-index
                                        discard-card-ids)]
                {:pass-index pass-index
                 :discard-card-options (high-priestess-hand-card-options
                                        db
                                        source
                                        params
                                        pass-index)
                 :selected-discard-card-ids (high-priestess-valid-discard-card-ids
                                             db
                                             source
                                             params
                                             pass-index
                                             discard-card-ids)
                 :draw-count-options draw-count-options
                 :selected-draw-count (when (some #{(:draw-count pass)}
                                                  draw-count-options)
                                        (:draw-count pass))}))
            (range 1 (inc redraw-count)))
      [])))

(defn move-judgement-card-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (if (judgement-move? db source params)
      (judgement-discard-card-options db source params)
      [])))

(defn move-judgement-card-maximum [db]
  (let [{:keys [source params]} (move-selection db)]
    (if (judgement-move? db source params)
      (judgement-card-maximum db source params)
      0)))

(declare power-control-groups)

(defn- control-group
  ([type]
   {:type type})
  ([type attrs]
   (assoc attrs :type type)))

(defn- power-control-group [power type]
  (control-group type {:power power}))

(defn- major-action-control-group [power action-power type]
  (control-group type {:power power
                       :action-power action-power}))

(defn- composite-major-control-groups [db source-id params]
  (let [power (active-power db source-id params)
        action-power (active-composite-action-power db source-id params)]
    (case action-power
      :orient-minion [(major-action-control-group power action-power :minion-orientation)]
      :rod [(major-action-control-group power action-power :rod)]
      :cup [(major-action-control-group power action-power :cup)]
      :trade-hand [(major-action-control-group power action-power :target-piece)]
      [])))

(defn- sword-major-control-groups [db source-id params]
  (let [power (active-power db source-id params)
        action-power (active-sword-major-action-power db source-id params)]
    (vec
     (concat
      (when (= :death power)
        [(power-control-group power :sword-action-count)])
      (when (or (not= :death power)
                (:sword-action-count params))
        (case action-power
          :trade-hand [(major-action-control-group power action-power :target-piece)]
          :orient-minion [(major-action-control-group power action-power :minion-orientation)]
          :rod [(major-action-control-group power action-power :rod)]
          :sword [(major-action-control-group power action-power :sword)]
          []))))))

(defn- world-control-groups [db source-id params]
  (vec
   (concat
    [(power-control-group :world :world-copy)]
    (when (some? (:copied-board-index params))
      [(power-control-group :world :world-copied-power)])
    (when-let [copied-power (selected-world-copied-power db source-id params)]
      (power-control-groups db source-id params copied-power)))))

(defn- power-control-groups [db source-id params power]
  (case power
    :rod [(power-control-group power :rod)]
    :cup [(power-control-group power :cup)]
    :disc [(power-control-group power :disc)]
    :sun [(power-control-group power :sun)]
    :sword [(power-control-group power :sword)]
    :fool [(power-control-group power :fool-reveal-count)]
    :high-priestess [(power-control-group power :high-priestess-redraw-count)
                     (power-control-group power :high-priestess-redraws)]
    :judgement [(power-control-group power :judgement-card-selection)]
    :hierophant [(power-control-group power :piece-orientation-major)]
    :hermit [(power-control-group power :hermit)]
    :devil [(power-control-group power :devil)]
    :world (world-control-groups db source-id params)
    (:empress :emperor :lovers :chariot :hanged-man :temperance)
    (composite-major-control-groups db source-id params)
    (:justice :death :tower :moon)
    (sword-major-control-groups db source-id params)
    []))

(defn- gameplay-control-groups [db source-id params]
  (let [power (selected-power db source-id params)]
    (vec
     (concat
      [(control-group :power)]
      (power-control-groups db source-id params power)))))

(defn move-control-groups [db]
  (let [{:keys [source params]} (move-selection db)]
    (case source
      :activate-territory
      (vec
       (concat
        [(control-group :source-board)]
        (when (some? (:source-board-index params))
          [(control-group :piece)])
        (when (:piece-id params)
          (gameplay-control-groups db source params))))

      :play-hand-card
      (vec
       (concat
        [(control-group :hand-card)]
        (when (:hand-card-id params)
          [(control-group :piece)])
        (when (:piece-id params)
          (gameplay-control-groups db source params))))

      :draw-cards
      [(control-group :discard-cards)
       (control-group :draw-count)]

      :orient-piece
      (vec
       (concat
        [(control-group :piece)]
        (when (:piece-id params)
          [(control-group :orientation)])))

      :place-initial-small
      (vec
       (concat
        [(control-group :target-space)]
        (when (or (some? (:target-board-index params))
                  (:target-wasteland params))
          [(control-group :orientation)])))

      [])))

(defn move-disc-minion-orientation-required? [db]
  (let [{:keys [source params]} (move-selection db)]
    (star-disc-source? db source params)))

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

      (= :piece (selected-sun-disc-mode db source params))
      (filterv #(sun-disc-piece-target? db source params %)
               (board-pieces db))

      (sun-move? db source params)
      (let [target-db (cup-target-db db source params)]
        (filterv #(cup-target-piece? db source params %)
                 (board-pieces target-db)))

      (cup-move? db source params)
      (let [target-db (cup-target-db db source params)]
        (filterv #(cup-target-piece? db source params %)
                 (board-pieces target-db)))

      (disc-move? db source params)
      (filterv #(disc-piece-target? db source params %)
               (board-pieces db))

      (sword-move? db source params)
      (filterv #(sword-piece-target? db source params %)
               (board-pieces db))

      (hierophant-move? db source params)
      (filterv #(hierophant-target-piece? db params %)
               (board-pieces db))

      (hermit-move? db source params)
      (if (hermit-target-selected? params)
        []
        (filterv #(hermit-target-piece? db params %)
                 (board-pieces db)))

      (devil-move? db source params)
      (let [target-db (devil-target-db db source params)]
        (filterv #(devil-target-piece? db source params %)
                 (board-pieces target-db)))

      (hanged-man-trade-stage? db source params)
      (board-pieces db)

      (justice-trade-stage? db source params)
      (board-pieces db)

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

(defn move-sun-disc-orientation-available? [db]
  (let [{:keys [source params]} (move-selection db)]
    (sun-disc-orientation-available? db source params)))

(defn move-sword-orientation-available? [db]
  (let [{:keys [source params]} (move-selection db)]
    (sword-orientation-available? db source params)))

(defn move-hermit-orientation-required? [db]
  (let [{:keys [source params]} (move-selection db)]
    (and (hermit-move? db source params)
         (hermit-destination-complete? db params)
         (hermit-orientation-required? db params))))

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

    :copied-board-index
    (some? (world-copy-board-cell db (:copied-board-index params)))

    :copied-power
    (some? (selected-world-copied-power db source-id params))

    :rod-mode
    (and (rod-move? db source-id params)
         (contains? rod-modes (:rod-mode params)))

    :disc-action-count
    (some #{(:disc-action-count params)}
          (disc-action-count-option-values db source-id params))

    :sword-action-count
    (some #{(:sword-action-count params)}
          (death-sword-action-count-option-values db source-id params))

    :devil-action-count
    (some #{(:devil-action-count params)}
          (devil-action-count-option-values db source-id params))

    :minion-orientation
    (contains? pieces/legal-orientations (:minion-orientation params))

    :sun-disc-mode
    (some? (selected-sun-disc-mode db source-id params))

    :sun-disc-target-piece-id
    (some? (sun-disc-target-piece db source-id params))

    :sun-disc-target-board-index
    (some #(= (:sun-disc-target-board-index params) (:index %))
          (filterv #(sun-disc-territory-target? db source-id params %)
                   (board db)))

    :sun-disc-replacement-card-id
    (some? (sun-disc-replacement-card-by-id db
                                            source-id
                                            params
                                            (:sun-disc-replacement-card-id params)))

    :disc-target-kind
    (and (disc-move? db source-id params)
         (contains? disc-target-kinds (:disc-target-kind params)))

    :sword-target-kind
    (and (sword-move? db source-id params)
         (contains? sword-target-kinds (:sword-target-kind params)))

    :fool-reveal-count
    (and (fool-move? db source-id params)
         (some #{(:fool-reveal-count params)} fool-reveal-count-order))

    :high-priestess-redraw-count
    (and (high-priestess-move? db source-id params)
         (some #{(:high-priestess-redraw-count params)}
               high-priestess-redraw-count-order))

    :high-priestess-redraws
    (and (high-priestess-move? db source-id params)
         (high-priestess-redraws-complete? db source-id params))

    :judgement-card-selection
    (and (judgement-move? db source-id params)
         (judgement-card-selection-complete? db source-id params))

    :target-piece-id
    (cond
      (rod-move? db source-id params)
      (some? (rod-target-piece db params))

      (sun-move? db source-id params)
      (some? (sun-disc-target-piece db source-id params))

      (disc-move? db source-id params)
      (some? (disc-target-piece db params))

      (sword-move? db source-id params)
      (some? (sword-target-piece db source-id params))

      (manipulation-piece-power? db source-id params)
      (some? (some #(when (= (:target-piece-id params) (:id %)) %)
                   (if (devil-move? db source-id params)
                     (move-target-piece-options db)
                     (filterv #(hierophant-target-piece? db params %)
                              (board-pieces db)))))

      (hanged-man-trade-stage? db source-id params)
      (some? (piece-by-id db (:target-piece-id params)))

      (justice-trade-stage? db source-id params)
      (some? (piece-by-id db (:target-piece-id params)))

      :else
      false)

    :target-board-index
    (cond
      (sun-disc-territory-target-stage? db source-id params)
      (some #(= (:sun-disc-target-board-index params) (:index %))
            (filterv #(sun-disc-territory-target? db source-id params %)
                     (board db)))

      (and (disc-move? db source-id params)
           (= :territory (:disc-target-kind params)))
      (some #(= (:target-board-index params) (:index %))
            (filterv #(disc-territory-target? db source-id params %)
                     (board db)))

      (and (sword-move? db source-id params)
           (= :territory (:sword-target-kind params)))
      (some #(= (:target-board-index params) (:index %))
            (filterv #(sword-territory-target? db source-id params %)
                     (board db)))

      :else
      (valid-board-index? db (:target-board-index params)))

    :target-space
    (case source-id
      :place-initial-small (target-space-complete? db source-id params)
      (and (or (cup-move? db source-id params)
               (sun-move? db source-id params))
           (target-space-complete? db source-id params)))

    :target-resolution
    (and (cup-move? db source-id params)
         (target-resolution-complete? db source-id params))

    :hermit-target-space
    (or (some? (some #(when (= (:target-piece-id params) (:id %)) %)
                     (filterv #(hermit-target-piece? db params %)
                              (board-pieces db))))
        (some? (some #(when (= (:target-board-index params) (:index %)) %)
                     (filterv #(hermit-target-territory? db params %)
                              (board db)))))

    :hermit-destination-space
    (hermit-destination-complete? db params)

    :replacement-card-source
    (cond
      (disc-move? db source-id params)
      (some? (selected-disc-replacement-card-source db source-id params))

      (sword-move? db source-id params)
      (some? (selected-sword-replacement-card-source db source-id params))

      :else
      false)

    :replacement-card-id
    (cond
      (sun-move? db source-id params)
      (some? (sun-disc-replacement-card-by-id db
                                              source-id
                                              params
                                              (:sun-disc-replacement-card-id params)))

      (disc-move? db source-id params)
      (some? (disc-replacement-card-by-id db
                                          source-id
                                          params
                                          (:replacement-card-id params)))

      (sword-move? db source-id params)
      (some? (sword-replacement-card-by-id db
                                           source-id
                                           params
                                           (:replacement-card-id params)))

      :else
      false)

    :one-point-card-id
    (some? (one-point-card-by-id db source-id params (:one-point-card-id params)))

    :orientation
    (contains? pieces/legal-orientations (:orientation params))

    :draw-count
    (some #{(:draw-count params)}
          (draw-count-options db (:discard-card-ids params)))

    :distance
    (some #{(:distance params)} (move-distance-options db))

    :damage
    (some #{(:damage params)} (sword-damage-options-for db source-id params))

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
   (concat (when (strength-disc-source? db source-id params)
             [:disc-action-count])
           (when (star-disc-source? db source-id params)
             [:minion-orientation])
           [:disc-target-kind]
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

(defn- sun-requirements [db source-id params]
  (let [mode (selected-sun-disc-mode db source-id params)]
    (vec
     (concat [:target-space]
             (when (= :territory (sun-cup-target-kind params))
               [:orientation])
             [:sun-disc-mode]
             (when (sun-cup-needs-one-point-card? db source-id params)
               [:one-point-card-id])
             (case mode
               :piece [:sun-disc-target-piece-id]
               :territory [:sun-disc-target-board-index
                           :sun-disc-replacement-card-id]
               :created-territory [:sun-disc-replacement-card-id]
               [])))))

(defn- sword-requirements [db source-id params]
  (vec
   (concat [:sword-target-kind]
           (case (:sword-target-kind params)
             :piece
             (concat [:target-piece-id :damage]
                     (when (and (some? (:damage params))
                                (sword-orientation-available? db source-id params))
                       [:orientation]))

             :territory
             (concat [:target-board-index :damage]
                     (when (and (some? (:damage params))
                                (< 1 (count (sword-replacement-card-source-option-ids
                                             db
                                             source-id
                                             params))))
                       [:replacement-card-source])
                     (when (seq (sword-replacement-card-source-option-ids
                                 db
                                 source-id
                                 params))
                       [:replacement-card-id]))

             []))))

(defn- sword-major-requirements [db source-id params]
  (case (active-sword-major-action-power db source-id params)
    :trade-hand [:target-piece-id]
    :orient-minion [:minion-orientation]
    :rod (rod-requirements db params)
    :sword (vec
            (concat (when (death-sword-source? db source-id params)
                      [:sword-action-count])
                    (sword-requirements db source-id params)))
    []))

(defn- fool-requirements [_db _source-id _params]
  [:fool-reveal-count])

(defn- high-priestess-requirements [_db _source-id params]
  (vec
   (concat [:high-priestess-redraw-count]
           (when (some? (selected-high-priestess-redraw-count params))
             [:high-priestess-redraws]))))

(defn- judgement-requirements [_db _source-id _params]
  [:judgement-card-selection])

(defn- hierophant-requirements [_db _source-id _params]
  [:target-piece-id :orientation])

(defn- hermit-requirements [db _source-id params]
  (vec
   (concat [:hermit-target-space]
           (when (hermit-target-selected? params)
             [:hermit-destination-space])
           (when (and (hermit-destination-complete? db params)
                      (hermit-orientation-required? db params))
             [:orientation]))))

(defn- devil-requirements [_db _source-id _params]
  [:devil-action-count :target-piece-id :orientation])

(defn- cup-requirements [_db _source-id _params]
  [:target-space :target-resolution])

(defn- composite-major-requirements [db source-id params]
  (case (active-composite-action-power db source-id params)
    :orient-minion [:minion-orientation]
    :cup (cup-requirements db source-id params)
    :rod (rod-requirements db params)
    :trade-hand [:target-piece-id]
    []))

(defn- power-requirements [db source-id params power]
  (case power
    :cup [:target-space :target-resolution]
    :rod (rod-requirements db params)
    :disc (disc-requirements db source-id params)
    :sun (sun-requirements db source-id params)
    :sword (sword-requirements db source-id params)
    (:empress :emperor :lovers :chariot :hanged-man :temperance)
    (composite-major-requirements db source-id params)
    (:justice :death :tower :moon)
    (sword-major-requirements db source-id params)
    :fool (fool-requirements db source-id params)
    :high-priestess (high-priestess-requirements db source-id params)
    :judgement (judgement-requirements db source-id params)
    :hierophant (hierophant-requirements db source-id params)
    :hermit (hermit-requirements db source-id params)
    :devil (devil-requirements db source-id params)
    []))

(defn- world-requirements [db source-id params]
  (let [copy-selected? (some? (world-copy-board-cell db (:copied-board-index params)))
        copied-power (selected-world-copied-power db source-id params)]
    (vec
     (concat [:copied-board-index]
             (when (and copy-selected? (nil? copied-power))
               [:copied-power])
             (when copied-power
               (power-requirements db source-id params copied-power))))))

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
             (if (= :world power)
               (world-requirements db source-id params)
               (power-requirements db source-id params power))))))

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
    :copied-board-index :world-copy
    :copied-power :copied-power
    :piece-id :piece
    :rod-mode :rod-mode
    :disc-action-count :disc-action-count
    :sword-action-count :sword-action-count
    :devil-action-count :devil-action-count
    :minion-orientation :minion-orientation
    :sun-disc-mode :sun-disc-mode
    :disc-target-kind :disc-target-kind
    :sword-target-kind :sword-target-kind
    :fool-reveal-count :fool-reveal-count
    :high-priestess-redraw-count :high-priestess-redraw-count
    :high-priestess-redraws :high-priestess-redraw
    :judgement-card-selection :judgement-card-selection
    :target-piece-id :target-piece
    :sun-disc-target-piece-id :target-piece
    :target-board-index :target
    :sun-disc-target-board-index :target
    :target-space :target
    :hermit-target-space :target
    :hermit-destination-space :hermit-destination
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
    :sun-disc-replacement-card-id :replacement-card
    :orientation :orientation
    :distance :distance
    :damage :damage
    :draw-count :draw-count
    :confirm))

(declare cup-target-command rod-command sword-target-command select-move-world-copy)

(def ^:private current-major-action-param-keys
  [:target-board-index
   :target-wasteland
   :target-piece-id
   :territory-card-source
   :one-point-card-id
   :replacement-card-source
   :replacement-card-id
   :orientation
   :distance
   :damage
   :rod-mode
   :sword-target-kind
   :minion-orientation])

(defn- clear-current-major-action-params [params]
  (apply dissoc params current-major-action-param-keys))

(defn- composite-final-action? [_power params]
  (pos? (completed-major-action-count params)))

(defn- composite-current-action-complete? [db source-id params]
  (case (active-composite-action-power db source-id params)
    :orient-minion
    (requirement-complete? db source-id params :minion-orientation)

    :cup
    (and (requirement-complete? db source-id params :target-space)
         (requirement-complete? db source-id params :target-resolution))

    :rod
    (every? #(requirement-complete? db source-id params %)
            (rod-requirements db params))

    :trade-hand
    (requirement-complete? db source-id params :target-piece-id)

    false))

(defn- composite-current-action [db source-id params]
  (case (active-composite-action-power db source-id params)
    :orient-minion
    {:power :orient-minion
     :piece-id (:piece-id params)
     :orientation (:minion-orientation params)}

    :cup
    (assoc (cup-target-command params)
           :power :cup
           :piece-id (:piece-id params))

    :rod
    (assoc (dissoc (rod-command db source-id params) :rod-variant)
           :power :rod
           :piece-id (:piece-id params))

    :trade-hand
    {:power :trade-hand
     :piece-id (:piece-id params)
     :target {:kind :piece
              :piece-id (:target-piece-id params)}}

    nil))

(defn- sword-major-final-action? [db source-id params]
  (case (active-power db source-id params)
    :justice (pos? (completed-major-action-count params))
    :tower (pos? (completed-major-action-count params))
    :moon (pos? (completed-major-action-count params))
    :death (<= (dec (selected-death-sword-action-count params))
               (completed-major-action-count params))
    false))

(defn- sword-major-current-action-complete? [db source-id params]
  (case (active-sword-major-action-power db source-id params)
    :trade-hand
    (requirement-complete? db source-id params :target-piece-id)

    :orient-minion
    (requirement-complete? db source-id params :minion-orientation)

    :rod
    (every? #(requirement-complete? db source-id params %)
            (rod-requirements db params))

    :sword
    (every? #(requirement-complete? db source-id params %)
            (sword-requirements db source-id params))

    false))

(defn- sword-major-current-action [db source-id params]
  (case (active-sword-major-action-power db source-id params)
    :trade-hand
    {:power :trade-hand
     :piece-id (:piece-id params)
     :target {:kind :piece
              :piece-id (:target-piece-id params)}}

    :orient-minion
    {:power :orient-minion
     :piece-id (:piece-id params)
     :orientation (:minion-orientation params)}

    :rod
    (assoc (dissoc (rod-command db source-id params) :rod-variant)
           :power :rod
           :piece-id (:piece-id params))

    :sword
    (assoc (sword-target-command db source-id params)
           :power :sword
           :piece-id (:piece-id params))

    nil))

(defn- devil-final-action? [params]
  (<= (dec (selected-devil-action-count params))
      (completed-major-action-count params)))

(defn- devil-current-action-complete? [db source-id params]
  (and (requirement-complete? db source-id params :target-piece-id)
       (requirement-complete? db source-id params :orientation)))

(defn- devil-current-action [_db _source-id params]
  {:power :orient-target
   :piece-id (:piece-id params)
   :target {:kind :piece
            :piece-id (:target-piece-id params)}
   :orientation (:orientation params)})

(defn- advance-composite-steps [db]
  (loop [db db]
    (let [{:keys [source params]} (move-selection db)
          power (selected-power db source params)]
      (if (and (composite-major-move? db source params)
               (not (composite-final-action? power params))
               (composite-current-action-complete? db source params))
        (let [action (composite-current-action db source params)]
          (recur (update-in db
                            [:move-selection :params]
                            (fn [params]
                              (-> params
                                  clear-current-major-action-params
                                  (update :major-actions
                                          (fnil conj [])
                                          action))))))
        db))))

(defn- advance-sword-major-steps [db]
  (loop [db db]
    (let [{:keys [source params]} (move-selection db)]
      (if (and (sword-major-move? db source params)
               (not (sword-major-final-action? db source params))
               (sword-major-current-action-complete? db source params))
        (let [action (sword-major-current-action db source params)]
          (recur (update-in db
                            [:move-selection :params]
                            (fn [params]
                              (-> params
                                  clear-current-major-action-params
                                  (update :major-actions
                                          (fnil conj [])
                                          action))))))
        db))))

(defn- advance-devil-steps [db]
  (loop [db db]
    (let [{:keys [source params]} (move-selection db)]
      (if (and (devil-move? db source params)
               (requirement-complete? db source params :devil-action-count)
               (not (devil-final-action? params))
               (devil-current-action-complete? db source params))
        (let [action (devil-current-action db source params)]
          (recur (update-in db
                            [:move-selection :params]
                            (fn [params]
                              (-> params
                                  clear-current-major-action-params
                                  (update :major-actions
                                          (fnil conj [])
                                          action))))))
        db))))

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
   (advance-devil-steps
    (advance-sword-major-steps
     (advance-composite-steps
      (update db :move-selection
              (fnil (fn [selection]
                      (assoc (apply f selection args)
                             :error nil
                             :last-result nil))
                    (empty-move-selection))))))))

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

                          (sun-move? db source (move-params db))
                          (if (sun-disc-territory-target-stage?
                               db
                               source
                               (move-params db))
                            (:target-board-index requirement-prompts)
                            (:target-space requirement-prompts))

                          (disc-move? db source (move-params db))
                          (:target-board-index requirement-prompts)

                          (sword-move? db source (move-params db))
                          (:target-board-index requirement-prompts)

                          (hermit-move? db source (move-params db))
                          (:hermit-target-space requirement-prompts)

                          (= :place-initial-small source)
                          (:initial-target-space requirement-prompts)

                          :else
                          (:target-board-index requirement-prompts))
      (= :hermit-destination stage) (:hermit-destination-space requirement-prompts)
      (= :territory-card-source stage) (:territory-card-source requirement-prompts)
      (= :one-point-card stage) (:one-point-card-id requirement-prompts)
      (= :replacement-card-source stage) (:replacement-card-source requirement-prompts)
      (= :replacement-card stage) (:replacement-card-id requirement-prompts)
      :else (get {:source-territory (:source-board-index requirement-prompts)
                  :hand-card (:hand-card-id requirement-prompts)
                  :power (:power requirement-prompts)
                  :world-copy (:copied-board-index requirement-prompts)
                  :copied-power (:copied-power requirement-prompts)
                  :piece (:piece-id requirement-prompts)
                  :rod-mode (:rod-mode requirement-prompts)
                  :disc-action-count (:disc-action-count requirement-prompts)
                  :sword-action-count (:sword-action-count requirement-prompts)
                  :devil-action-count (:devil-action-count requirement-prompts)
                  :minion-orientation (:minion-orientation requirement-prompts)
                  :sun-disc-mode (:sun-disc-mode requirement-prompts)
                  :disc-target-kind (:disc-target-kind requirement-prompts)
                  :sword-target-kind (:sword-target-kind requirement-prompts)
                  :fool-reveal-count (:fool-reveal-count requirement-prompts)
                  :high-priestess-redraw-count (:high-priestess-redraw-count requirement-prompts)
                  :high-priestess-redraw (:high-priestess-redraws requirement-prompts)
                  :judgement-card-selection (:judgement-card-selection requirement-prompts)
                  :target-piece (:target-piece-id requirement-prompts)
                  :orientation (:orientation requirement-prompts)
                  :distance (:distance requirement-prompts)
                  :damage (:damage requirement-prompts)
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
            base-params (cond-> (if (= source-id :draw-cards)
                                  (normalize-draw-selection-params db
                                                                   {:discard-card-ids []})
                                  {})

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

(def ^:private sun-disc-param-keys
  [:sun-disc-mode
   :sun-disc-target-piece-id
   :sun-disc-target-board-index
   :sun-disc-replacement-card-id
   :sun-disc-orientation])

(def ^:private hermit-destination-param-keys
  [:hermit-destination-board-index
   :hermit-destination-wasteland])

(defn- clear-sun-disc-params [params]
  (apply dissoc params sun-disc-param-keys))

(defn- clear-hermit-destination-params [params]
  (apply dissoc params hermit-destination-param-keys))

(defn- clear-power-target-params [params]
  (clear-hermit-destination-params
   (clear-sun-disc-params
    (dissoc params
            :target-board-index
            :target-wasteland
            :target-piece-id
            :territory-card-source
            :one-point-card-id
            :replacement-card-source
            :replacement-card-id
            :orientation
            :distance
            :damage
            :sword-action-count
            :devil-action-count
            :fool-reveal-count
            :high-priestess-redraw-count
            :redraws
            :judgement-card-ids
            :major-actions))))

(defn- clear-child-power-params [params]
  (-> params
      clear-power-target-params
      (dissoc :rod-mode
              :disc-target-kind
              :sword-target-kind
              :disc-action-count
              :sword-action-count
              :devil-action-count
              :minion-orientation
              :fool-reveal-count
              :high-priestess-redraw-count
              :redraws
              :judgement-card-ids
              :major-actions)))

(defn- clear-piece-when-source-changes [params board-index]
  (let [next-params (assoc params :source-board-index board-index)]
    (if (= (:source-board-index params) board-index)
      next-params
      (-> next-params
          clear-power-target-params
          (dissoc :piece-id
                  :power
                  :copied-board-index
                  :copied-power
                  :rod-mode
                  :disc-target-kind
                  :sword-target-kind
                  :disc-action-count
                  :sword-action-count
                  :devil-action-count
                  :minion-orientation
                  :fool-reveal-count
                  :high-priestess-redraw-count
                  :redraws
                  :judgement-card-ids
                  :major-actions)))))

(defn- set-territory-target [params board-index]
  (-> params
      (assoc :target-board-index board-index)
      (dissoc :target-wasteland
              :target-piece-id
              :territory-card-source
              :one-point-card-id
              :replacement-card-source
              :replacement-card-id
              :damage)
      clear-sun-disc-params
      clear-hermit-destination-params))

(defn- set-wasteland-target [params space]
  (-> params
      (assoc :target-wasteland (select-keys space [:kind :row :col]))
      (dissoc :target-board-index
              :target-piece-id
              :orientation
              :territory-card-source
              :one-point-card-id
              :replacement-card-source
              :replacement-card-id
              :damage)
      clear-sun-disc-params
      clear-hermit-destination-params))

(defn- set-hand-card-source [params card-id]
  (-> params
      (assoc :hand-card-id card-id)
      clear-power-target-params
      (dissoc :piece-id
              :power
              :rod-mode
              :disc-target-kind
              :sword-target-kind
              :disc-action-count
              :sword-action-count
              :devil-action-count
              :minion-orientation
              :fool-reveal-count
              :high-priestess-redraw-count
              :redraws
              :judgement-card-ids
              :copied-board-index
              :copied-power)))

(defn- set-acting-piece [params piece-id]
  (let [next-params (assoc params :piece-id piece-id)]
    (if (= (:piece-id params) piece-id)
      next-params
      (-> next-params
          clear-power-target-params
          (dissoc :minion-orientation)))))

(defn- set-power-param [params power]
  (let [next-params (assoc params :power power)]
    (if (= (:power params) power)
      next-params
      (-> next-params
          clear-power-target-params
          (dissoc :rod-mode
                  :disc-target-kind
                  :sword-target-kind
                  :disc-action-count
                  :sword-action-count
                  :minion-orientation
                  :fool-reveal-count
                  :high-priestess-redraw-count
                  :redraws
                  :judgement-card-ids
                  :major-actions
                  :copied-board-index
                  :copied-power)))))

(defn- set-world-copy-param [params board-index]
  (let [next-params (assoc params :copied-board-index board-index)]
    (if (= (:copied-board-index params) board-index)
      next-params
      (-> next-params
          clear-child-power-params
          (dissoc :copied-power)))))

(defn- set-world-copied-power-param [params power]
  (let [next-params (assoc params :copied-power power)]
    (if (= (:copied-power params) power)
      next-params
      (clear-child-power-params next-params))))

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
              :orientation
              :damage))))

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
              :orientation
              :damage))))

(defn- set-sword-target-kind-param [params target-kind]
  (let [next-params (assoc params :sword-target-kind target-kind)]
    (if (= (:sword-target-kind params) target-kind)
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
              :damage))))

(defn- set-disc-action-count-param [params action-count]
  (let [next-params (assoc params :disc-action-count action-count)]
    (if (= (:disc-action-count params) action-count)
      next-params
      (dissoc next-params :replacement-card-id))))

(defn- set-sword-action-count-param [params action-count]
  (let [next-params (assoc params :sword-action-count action-count)]
    (if (= (:sword-action-count params) action-count)
      next-params
      (-> next-params
          (dissoc :target-board-index
                  :target-wasteland
                  :target-piece-id
                  :replacement-card-source
                  :replacement-card-id
                  :orientation
                  :damage
                  :sword-target-kind)
          (assoc :major-actions [])))))

(defn- set-devil-action-count-param [params action-count]
  (let [next-params (assoc params :devil-action-count action-count)]
    (if (= (:devil-action-count params) action-count)
      next-params
      (-> next-params
          (dissoc :target-board-index
                  :target-wasteland
                  :target-piece-id
                  :orientation)
          (assoc :major-actions [])))))

(defn- set-high-priestess-redraw-count-param [params db source redraw-count]
  (normalize-high-priestess-redraws
   db
   source
   (assoc params :high-priestess-redraw-count redraw-count)))

(defn- update-high-priestess-redraw-pass [params pass-index f & args]
  (if-let [offset (redraw-pass-offset pass-index)]
    (let [redraw-count (or (selected-high-priestess-redraw-count params) 0)
          redraws (vec (concat (:redraws params)
                               (repeat (max 0 (- redraw-count
                                                 (count (:redraws params))))
                                       {:discard-card-ids []})))]
      (assoc params
             :redraws
             (update redraws offset #(apply f (or % {:discard-card-ids []}) args))))
    params))

(defn- set-damage-param [params damage]
  (let [next-params (assoc params :damage damage)]
    (if (= (:damage params) damage)
      next-params
      (dissoc next-params
              :replacement-card-source
              :replacement-card-id
              :orientation))))

(defn- set-minion-orientation-param [params orientation]
  (let [next-params (assoc params :minion-orientation orientation)]
    (if (= (:minion-orientation params) orientation)
      next-params
      (dissoc next-params
              :target-board-index
              :target-wasteland
              :target-piece-id
              :replacement-card-id
              :orientation
              :damage))))

(defn- set-target-piece [params piece-id]
  (let [next-params (assoc params :target-piece-id piece-id)]
    (if (= (:target-piece-id params) piece-id)
      next-params
      (-> next-params
          (dissoc :target-board-index
                  :target-wasteland
                  :territory-card-source
                  :one-point-card-id
                  :replacement-card-source
                  :replacement-card-id
                  :orientation
                  :damage)
          clear-sun-disc-params
          clear-hermit-destination-params))))

(defn- set-hermit-destination-territory [params board-index]
  (-> params
      (assoc :hermit-destination-board-index board-index)
      (dissoc :hermit-destination-wasteland
              :orientation)))

(defn- set-hermit-destination-wasteland [params space]
  (-> params
      (assoc :hermit-destination-wasteland (select-keys space [:kind :row :col]))
      (dissoc :hermit-destination-board-index
              :orientation)))

(defn- set-sun-disc-mode-param [params mode]
  (let [next-params (assoc params :sun-disc-mode mode)]
    (if (= (:sun-disc-mode params) mode)
      next-params
      (dissoc next-params
              :sun-disc-target-piece-id
              :sun-disc-target-board-index
              :sun-disc-replacement-card-id
              :sun-disc-orientation))))

(defn- set-sun-disc-target-piece [params piece-id]
  (let [next-params (assoc params :sun-disc-target-piece-id piece-id)]
    (if (= (:sun-disc-target-piece-id params) piece-id)
      next-params
      (dissoc next-params
              :sun-disc-target-board-index
              :sun-disc-replacement-card-id
              :sun-disc-orientation))))

(defn- set-sun-disc-target-territory [params board-index]
  (let [next-params (assoc params :sun-disc-target-board-index board-index)]
    (if (= (:sun-disc-target-board-index params) board-index)
      next-params
      (dissoc next-params
              :sun-disc-target-piece-id
              :sun-disc-replacement-card-id
              :sun-disc-orientation))))

(defn- select-hermit-board-target [db params index]
  (let [cell (board-cell-by-index db index)]
    (cond
      (hermit-piece-target-selected? params)
      (if (empty-board-target? db index)
        (update-move-selection-success db
                                       update
                                       :params
                                       set-hermit-destination-territory
                                       index)
        (update-move-selection db assoc
                               :error
                               (move-error :hermit-destination-occupied
                                           "Choose an empty destination territory."
                                           {:board-index index})))

      (hermit-territory-target-selected? params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-hermit-destination
                                         "Hermit territory moves must choose a wasteland destination."
                                         {:board-index index}))

      (hermit-target-territory? db params cell)
      (update-move-selection-success db update :params set-territory-target index)

      :else
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-hermit-target
                                         "Choose a Hermit-targetable territory."
                                         {:board-index index})))))

(defn select-board-for-active-move [db index]
  (if-not (valid-board-index? db index)
    db
    (let [{:keys [source params]} (move-selection db)
          cell (board-cell-by-index db index)]
      (case source
        :activate-territory
        (let [has-source? (requirement-complete? db source params :source-board-index)
              has-piece? (requirement-complete? db source params :piece-id)
              current-pieces (current-player-pieces-on-space db index)]
          (cond
            (and has-source?
                 has-piece?
                 (= :world-copy (:stage (move-selection db))))
            (select-move-world-copy db index)

            (and has-source?
                 has-piece?
                 (sun-disc-territory-target-stage? db source params)
                 (sun-disc-territory-target? db source params cell))
            (update-move-selection-success db
                                           update
                                           :params
                                           set-sun-disc-target-territory
                                           index)

            (and has-source?
                 has-piece?
                 (sun-disc-territory-target-stage? db source params))
            (update-move-selection db assoc
                                   :error
                                   (move-error :invalid-sun-disc-target
                                               "Choose a Sun Disc-targetable territory."
                                               {:board-index index}))

            (and has-source?
                 has-piece?
                 (hermit-move? db source params))
            (select-hermit-board-target db params index)

            (and has-source?
                 has-piece?
                 (or (cup-move? db source params)
                     (sun-move? db source params))
                 (cup-target-territory? db source params cell))
            (update-move-selection-success db update :params set-territory-target index)

            (and has-source?
                 has-piece?
                 (or (cup-move? db source params)
                     (sun-move? db source params)))
            (update-move-selection db assoc
                                   :error
                                   (move-error :invalid-cup-target
                                               "Choose a Cup-targetable territory."
                                               {:board-index index}))

            (and has-source?
                 has-piece?
                 (or (and (disc-move? db source params)
                          (= :territory (:disc-target-kind params))
                          (disc-territory-target? db source params cell))
                     (and (sword-move? db source params)
                          (= :territory (:sword-target-kind params))
                          (sword-territory-target? db source params cell))))
            (update-move-selection-success db update :params set-territory-target index)

            (and has-source?
                 has-piece?
                 (or (and (disc-move? db source params)
                          (not= :territory (:disc-target-kind params)))
                     (and (sword-move? db source params)
                          (not= :territory (:sword-target-kind params)))))
            db

            (and has-source?
                 has-piece?
                 (not (or (disc-move? db source params)
                          (sword-move? db source params))))
            (update-move-selection-success db update :params set-territory-target index)

            (and has-source?
                 has-piece?
                 (disc-move? db source params))
            (update-move-selection db assoc
                                   :error
                                   (move-error :invalid-disc-target
                                               "Choose a Disc-targetable territory."
                                               {:board-index index}))

            (and has-source?
                 has-piece?
                 (sword-move? db source params))
            (update-move-selection db assoc
                                   :error
                                   (move-error :invalid-sword-target
                                               "Choose a Sword-targetable territory."
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
          (= :world-copy (:stage (move-selection db)))
          (select-move-world-copy db index)

          (and (sun-disc-territory-target-stage? db source params)
               (sun-disc-territory-target? db source params cell))
          (update-move-selection-success db
                                         update
                                         :params
                                         set-sun-disc-target-territory
                                         index)

          (sun-disc-territory-target-stage? db source params)
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-sun-disc-target
                                             "Choose a Sun Disc-targetable territory."
                                             {:board-index index}))

          (hermit-move? db source params)
          (select-hermit-board-target db params index)

          (and (disc-move? db source params)
               (not= :territory (:disc-target-kind params)))
          db

          (and (sword-move? db source params)
               (not= :territory (:sword-target-kind params)))
          db

          (and (disc-move? db source params)
               (not (disc-territory-target? db source params cell)))
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-disc-target
                                             "Choose a Disc-targetable territory."
                                             {:board-index index}))

          (and (sword-move? db source params)
               (not (sword-territory-target? db source params cell)))
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-sword-target
                                             "Choose a Sword-targetable territory."
                                             {:board-index index}))

          (and (or (cup-move? db source params)
                   (sun-move? db source params))
               (cup-target-territory? db source params cell))
          (update-move-selection-success db update :params set-territory-target index)

          (or (cup-move? db source params)
              (sun-move? db source params))
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-cup-target
                                             "Choose a Cup-targetable territory."
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
               (sun-move? db source params)
               (hermit-move? db source params)
               (= :place-initial-small source)))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-wasteland-target
                                         "Wasteland targets are only available for Cup, Sun, Hermit, or initial placement moves."
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

      (hermit-move? db source params)
      (if (hermit-target-selected? params)
        (update-move-selection-success db
                                       update
                                       :params
                                       set-hermit-destination-wasteland
                                       space)
        (update-move-selection db assoc
                               :error
                               (move-error :invalid-hermit-destination
                                           "Choose a Hermit target before choosing a destination."
                                           {:row row
                                            :col col})))

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

(defn select-move-world-copy [db board-index]
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (not (world-move? db source params))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-world-copy
                                         "World copy choices are only available for World moves."
                                         {:board-index board-index
                                          :source source}))

      (world-copy-board-cell db board-index)
      (update-move-selection-success db update :params set-world-copy-param board-index)

      :else
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-world-copy
                                         "Choose a non-World major territory for World to copy."
                                         {:board-index board-index})))))

(defn select-move-power [db power]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (world-move? db source params)
             (world-copy-board-cell db (:copied-board-index params)))
      (let [power-ids (set (map :id (move-world-copied-power-options db)))]
        (if (contains? power-ids power)
          (update-move-selection-success db
                                         update
                                         :params
                                         set-world-copied-power-param
                                         power)
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-world-copied-power
                                             "Choose a power provided by the copied major territory."
                                             {:power power
                                              :options (vec power-ids)}))))
      (let [power-ids (set (map :id (move-power-options db)))]
        (if (contains? power-ids power)
          (update-move-selection-success db update :params set-power-param power)
          (update-move-selection db assoc
                                 :error
                                 (move-error :invalid-move-power
                                             "Choose a power provided by the selected card."
                                             {:power power
                                              :options (vec power-ids)})))))))

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

(defn select-move-sword-target-kind [db target-kind]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (sword-move? db source params)
             (contains? sword-target-kinds target-kind))
      (update-move-selection-success db update :params set-sword-target-kind-param target-kind)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sword-target-kind
                                         "Choose a supported Sword attack target."
                                         {:target-kind target-kind
                                          :options sword-target-kind-order})))))

(defn set-move-disc-action-count [db action-count]
  (let [{:keys [source params]} (move-selection db)
        options (disc-action-count-option-values db source params)]
    (if (some #{action-count} options)
      (update-move-selection-success db update :params set-disc-action-count-param action-count)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-disc-action-count
                                     "Choose a supported Disc action count."
                                     {:action-count action-count
                                      :options options})))))

(defn set-move-sword-action-count [db action-count]
  (let [{:keys [source params]} (move-selection db)
        options (death-sword-action-count-option-values db source params)]
    (if (some #{action-count} options)
      (update-move-selection-success db update :params set-sword-action-count-param action-count)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sword-action-count
                                         "Choose a supported Sword action count."
                                         {:action-count action-count
                                          :options options})))))

(defn set-move-devil-action-count [db action-count]
  (let [{:keys [source params]} (move-selection db)
        options (devil-action-count-option-values db source params)]
    (if (some #{action-count} options)
      (update-move-selection-success db update :params set-devil-action-count-param action-count)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-devil-action-count
                                         "Choose a supported Devil orientation count."
                                         {:action-count action-count
                                          :options options})))))

(defn set-move-fool-reveal-count [db reveal-count]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (fool-move? db source params)
             (some #{reveal-count} fool-reveal-count-order))
      (update-move-selection-success db assoc-in [:params :fool-reveal-count] reveal-count)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-fool-reveal-count
                                         "Choose a supported Fool reveal count."
                                         {:reveal-count reveal-count
                                          :options fool-reveal-count-order})))))

(defn set-move-high-priestess-redraw-count [db redraw-count]
  (let [{:keys [source params]} (move-selection db)]
    (if (and (high-priestess-move? db source params)
             (some #{redraw-count} high-priestess-redraw-count-order))
      (update-move-selection-success db
                                     update
                                     :params
                                     set-high-priestess-redraw-count-param
                                     db
                                     source
                                     redraw-count)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-high-priestess-redraw-count
                                         "Choose a supported High Priestess redraw count."
                                         {:redraw-count redraw-count
                                          :options high-priestess-redraw-count-order})))))

(defn toggle-move-high-priestess-discard-card [db pass-index card-id]
  (let [{:keys [source params]} (move-selection db)
        pass (high-priestess-redraw-pass params pass-index)
        card-options (high-priestess-hand-card-options db source params pass-index)
        card-option-ids (set (map :id card-options))]
    (cond
      (not (high-priestess-move? db source params))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-high-priestess-discard-card
                                         "High Priestess discard choices are only available for High Priestess moves."
                                         {:card-id card-id
                                          :source source}))

      (or (nil? pass)
          (not (<= 1 pass-index (or (selected-high-priestess-redraw-count params) 0))))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-high-priestess-pass
                                         "Choose an available High Priestess redraw pass."
                                         {:pass-index pass-index}))

      (not (contains? card-option-ids card-id))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-high-priestess-discard-card
                                         "Choose a card from the current player's redraw hand."
                                         {:card-id card-id}))

      :else
      (let [selected-card-ids (set (:discard-card-ids pass))
            next-selected-card-ids (if (contains? selected-card-ids card-id)
                                     (disj selected-card-ids card-id)
                                     (conj selected-card-ids card-id))
            next-discard-card-ids (->> card-options
                                       (map :id)
                                       (filter next-selected-card-ids)
                                       vec)]
        (update-move-selection-success
         db
         update
         :params
         (fn [params]
           (normalize-high-priestess-redraws
            db
            source
            (update-high-priestess-redraw-pass
             params
             pass-index
             assoc
             :discard-card-ids
             next-discard-card-ids))))))))

(defn set-move-high-priestess-draw-count [db pass-index draw-count]
  (let [{:keys [source params]} (move-selection db)
        pass (high-priestess-redraw-pass params pass-index)
        options (high-priestess-draw-count-options db
                                                   source
                                                   params
                                                   pass-index
                                                   (:discard-card-ids pass))]
    (if (and (high-priestess-move? db source params)
             (int? pass-index)
             (<= 1 pass-index (or (selected-high-priestess-redraw-count params) 0))
             (some #{draw-count} options))
      (update-move-selection-success
       db
       update
       :params
       (fn [params]
         (normalize-high-priestess-redraws
          db
          source
          (update-high-priestess-redraw-pass params
                                             pass-index
                                             assoc
                                             :draw-count
                                             draw-count))))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-high-priestess-draw-count
                                         "Choose a draw count for this High Priestess redraw pass."
                                         {:pass-index pass-index
                                          :draw-count draw-count
                                          :options options})))))

(defn toggle-move-judgement-card [db card-id]
  (let [{:keys [source params]} (move-selection db)
        options (judgement-discard-card-options db source params)
        option-ids (set (map :id options))
        selected-card-ids (set (:judgement-card-ids params))
        selected? (contains? selected-card-ids card-id)
        maximum (judgement-card-maximum db source params)]
    (cond
      (not (judgement-move? db source params))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-judgement-card
                                         "Judgement card choices are only available for Judgement moves."
                                         {:card-id card-id
                                          :source source}))

      (not (contains? option-ids card-id))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-judgement-card
                                         "Choose a card from the discard pile."
                                         {:card-id card-id}))

      (and (not selected?)
           (<= maximum (count selected-card-ids)))
      (update-move-selection db assoc
                             :error
                             (move-error :judgement-card-limit
                                         "Judgement cannot draw more cards than the minion's pips or hand limit allow."
                                         {:card-id card-id
                                          :maximum maximum}))

      :else
      (let [next-selected-card-ids (if selected?
                                     (disj selected-card-ids card-id)
                                     (conj selected-card-ids card-id))]
        (update-move-selection-success
         db
         assoc-in
         [:params :judgement-card-ids]
         (->> options
              (map :id)
              (filter next-selected-card-ids)
              vec))))))

(defn set-move-minion-orientation [db orientation]
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (not (or (star-disc-source? db source params)
               (major-orient-step? db source params)))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-minion-orientation
                                         "Minion orientation is only available for Star Disc or ordered major moves."
                                         {:orientation orientation
                                          :source source}))

      (not (contains? pieces/legal-orientations orientation))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-orientation
                                         "Choose a legal piece orientation."
                                         {:orientation orientation}))

      :else
      (update-move-selection-success db update :params set-minion-orientation-param orientation))))

(defn select-move-sun-disc-mode [db mode]
  (let [{:keys [source params]} (move-selection db)
        options (set (sun-disc-mode-option-ids db source params))]
    (if (contains? options mode)
      (update-move-selection-success db update :params set-sun-disc-mode-param mode)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sun-disc-mode
                                         "Choose a supported Sun Disc option."
                                         {:mode mode
                                          :options (vec options)})))))

(defn set-move-sun-disc-orientation [db orientation]
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (not (sun-disc-orientation-available? db source params))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sun-disc-orientation
                                         "Sun Disc orientation is only available for current-player piece targets."
                                         {:orientation orientation
                                          :source source}))

      (not (contains? pieces/legal-orientations orientation))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-orientation
                                         "Choose a legal piece orientation."
                                         {:orientation orientation}))

      :else
      (update-move-selection-success db assoc-in
                                     [:params :sun-disc-orientation]
                                     orientation))))

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

      (and (= :piece (selected-sun-disc-mode db source params))
           selectable-piece?)
      (update-move-selection-success db
                                     update
                                     :params
                                     set-sun-disc-target-piece
                                     (:id piece))

      (= :piece (selected-sun-disc-mode db source params))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Sun Disc-targetable piece."
                                         {:piece-id piece-id}))

      (and (sun-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params set-target-piece (:id piece))

      (sun-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose an enemy piece targeted by the minion."
                                         {:piece-id piece-id}))

      (and (cup-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params set-target-piece (:id piece))

      (cup-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose an enemy piece targeted by the minion."
                                         {:piece-id piece-id}))

      (and (hierophant-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params set-target-piece (:id piece))

      (hierophant-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Hierophant-targetable piece with a same-size stash piece available."
                                         {:piece-id piece-id}))

      (and (hermit-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params set-target-piece (:id piece))

      (hermit-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Hermit-targetable piece."
                                         {:piece-id piece-id}))

      (and (devil-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params set-target-piece (:id piece))

      (devil-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Devil-targetable piece."
                                         {:piece-id piece-id}))

      (and (hanged-man-trade-stage? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params set-target-piece (:id piece))

      (hanged-man-trade-stage? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Hanged Man hand-trade target piece."
                                         {:piece-id piece-id}))

      (and (justice-trade-stage? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params set-target-piece (:id piece))

      (justice-trade-stage? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Justice hand-trade target piece."
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

      (and (sword-move? db source params)
           selectable-piece?)
      (update-move-selection-success db update :params set-target-piece (:id piece))

      (sword-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Choose a Sword-targetable piece."
                                         {:piece-id piece-id}))

      :else
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-target-piece
                                         "Target pieces are only available for Cup, Rod, Disc, Sword, Justice, Hierophant, Hermit, or Devil moves."
                                         {:piece-id piece-id})))))

(defn- set-territory-card-source [params territory-card-source]
  (cond-> (assoc params :territory-card-source territory-card-source)
    (= :draw-pile-top territory-card-source)
    (dissoc :one-point-card-id)))

(defn select-move-territory-card-source [db territory-card-source]
  (let [{:keys [source params]} (move-selection db)
        replacement-source? (or (disc-move? db source params)
                                (sword-move? db source params))
        option-ids (set (cond
                          (disc-move? db source params)
                          (disc-replacement-card-source-option-ids db source params)

                          (sword-move? db source params)
                          (sword-replacement-card-source-option-ids db source params)

                          :else
                          (territory-card-source-option-ids db source params)))]
    (if (contains? option-ids territory-card-source)
      (update-move-selection-success db
                                     update
                                     :params
                                     (if replacement-source?
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
    (if (and (or (cup-move? db source params)
                 (sun-cup-needs-one-point-card? db source params))
             (one-point-card-by-id db source params card-id))
      (update-move-selection-success db assoc-in [:params :one-point-card-id] card-id)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-one-point-card
                                         "Choose a one-point card from the current player's hand."
                                         {:card-id card-id})))))

(defn select-move-replacement-card [db card-id]
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (and (sun-move? db source params)
           (sun-disc-replacement-card-by-id db source params card-id))
      (update-move-selection-success db assoc-in
                                     [:params :sun-disc-replacement-card-id]
                                     card-id)

      (sun-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sun-disc-replacement-card
                                         "Choose a replacement card for Sun's Disc action."
                                         {:card-id card-id}))

      (and (disc-move? db source params)
           (disc-replacement-card-by-id db source params card-id))
      (update-move-selection-success db assoc-in [:params :replacement-card-id] card-id)

      (and (sword-move? db source params)
           (sword-replacement-card-by-id db source params card-id))
      (update-move-selection-success db assoc-in [:params :replacement-card-id] card-id)

      (disc-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-disc-replacement-card
                                         "Choose a replacement card worth exactly one more point."
                                         {:card-id card-id}))

      (sword-move? db source params)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sword-replacement-card
                                         "Choose a replacement card worth the selected damage less."
                                         {:card-id card-id}))

      :else
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-replacement-card
                                         "Replacement cards are not available for this move."
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

(defn toggle-move-discard-card [db card-id]
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (not= :draw-cards source)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-discard-card
                                         "Discard choices are only available for draw moves."
                                         {:card-id card-id
                                          :source source}))

      (nil? (hand-card-by-id db card-id))
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-discard-card
                                         "Choose a card from the current player's hand."
                                         {:card-id card-id}))

      :else
      (let [selected-card-ids (set (:discard-card-ids params))
            next-selected-card-ids (if (contains? selected-card-ids card-id)
                                     (disj selected-card-ids card-id)
                                     (conj selected-card-ids card-id))
            next-discard-card-ids (->> (current-player-hand db)
                                       (map :id)
                                       (filter next-selected-card-ids)
                                       vec)]
        (update-move-selection-success
         db
         assoc
         :params
         (normalize-draw-selection-params
          db
          (assoc params :discard-card-ids next-discard-card-ids)))))))

(defn set-move-distance [db distance]
  (if (some #{distance} (move-distance-options db))
    (update-move-selection-success db assoc-in [:params :distance] distance)
    (update-move-selection db assoc
                           :error
                           (move-error :invalid-distance
                                       "Choose a distance the acting minion can move."
                                       {:distance distance
                                        :options (move-distance-options db)}))))

(defn move-damage-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (sword-damage-options-for db source params)))

(defn set-move-damage [db damage]
  (let [options (move-damage-options db)]
    (if (some #{damage} options)
      (update-move-selection-success db update :params set-damage-param damage)
      (update-move-selection db assoc
                             :error
                             (move-error :invalid-sword-damage
                                         "Choose damage the acting minion can deal."
                                         {:damage damage
                                          :options options})))))

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

(defn move-discard-card-options [db]
  (if (= :draw-cards (move-source db))
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

      (and (world-move? db source params)
           (nil? (world-copy-board-cell db (:copied-board-index params))))
      (move-world-copy-options db)

      (sun-disc-territory-target-stage? db source params)
      (filterv #(sun-disc-territory-target? db source params %)
               (board db))

      (or (cup-move? db source params)
          (sun-move? db source params))
      (let [target-db (cup-target-db db source params)]
        (filterv #(cup-target-territory? db source params %)
                 (board target-db)))

      (and (disc-move? db source params)
           (= :territory (:disc-target-kind params)))
      (filterv #(disc-territory-target? db source params %)
               (board db))

      (and (sword-move? db source params)
           (= :territory (:sword-target-kind params)))
      (filterv #(sword-territory-target? db source params %)
               (board db))

      (hermit-move? db source params)
      (cond
        (hermit-piece-target-selected? params)
        (filterv #(empty-board-target? db (:index %))
                 (board db))

        (hermit-territory-target-selected? params)
        []

        :else
        (filterv #(hermit-target-territory? db params %)
                 (board db)))

      :else
      (board db))))

(defn move-one-point-card-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (if (or (and (cup-move? db source params)
                 (not= :draw-pile-top (:territory-card-source params)))
            (sun-cup-needs-one-point-card? db source params))
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

      (sword-move? db source params)
      (mapv disc-replacement-card-source-definitions
            (sword-replacement-card-source-option-ids db source params))

      :else
      [])))

(defn move-disc-target-kind-options [db]
  (if (disc-move? db (move-source db) (move-params db))
    (mapv disc-target-kind-definitions disc-target-kind-order)
    []))

(defn move-sword-target-kind-options [db]
  (if (sword-move? db (move-source db) (move-params db))
    (mapv sword-target-kind-definitions sword-target-kind-order)
    []))

(defn move-replacement-card-options [db]
  (let [{:keys [source params]} (move-selection db)]
    (cond
      (sun-move? db source params)
      (sun-disc-replacement-card-options-for db source params)

      (disc-move? db source params)
      (disc-replacement-card-options-for db source params)

      (sword-move? db source params)
      (sword-replacement-card-options-for db source params)

      :else
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

(defn- sun-cup-command [params]
  (let [cup-target (cup-target-command params)]
    (if (= :created-territory (:sun-disc-mode params))
      (select-keys cup-target [:target])
      cup-target)))

(defn- sun-disc-command [params]
  (case (:sun-disc-mode params)
    :skip nil

    :created-piece
    (cond-> {:target {:kind :created-piece}}
      (:orientation params)
      (assoc :orientation (:orientation params)))

    :created-territory
    {:target {:kind :created-territory}
     :replacement-card-source :hand
     :replacement-card-id (:sun-disc-replacement-card-id params)}

    :piece
    (cond-> {:target {:kind :piece
                      :piece-id (:sun-disc-target-piece-id params)}}
      (:sun-disc-orientation params)
      (assoc :orientation (:sun-disc-orientation params)))

    :territory
    {:target {:kind :territory
              :board-index (:sun-disc-target-board-index params)}
     :replacement-card-source :hand
     :replacement-card-id (:sun-disc-replacement-card-id params)}

    nil))

(defn- sun-command [_db _source params]
  (let [disc-command (sun-disc-command params)]
    (cond-> {:cup (sun-cup-command params)}
      disc-command
      (assoc :disc disc-command))))

(defn- piece-orientation-command [params]
  {:target {:kind :piece
            :piece-id (:target-piece-id params)}
   :orientation (:orientation params)})

(defn- hermit-destination-command [params]
  (if-let [destination-wasteland (:hermit-destination-wasteland params)]
    (select-keys destination-wasteland [:kind :row :col])
    {:kind :territory
     :board-index (:hermit-destination-board-index params)}))

(defn- hermit-command [_db _source params]
  (cond-> {:target (if (:target-piece-id params)
                     {:kind :piece
                      :piece-id (:target-piece-id params)}
                     {:kind :territory
                      :board-index (:target-board-index params)})
           :destination (hermit-destination-command params)}
    (:orientation params)
    (assoc :orientation (:orientation params))))

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

(defn- strength-disc-command [db source params]
  (let [action (disc-target-command db source params)
        action-count (selected-disc-action-count db source params)]
    {:disc-variant (selected-disc-variant db source params)
     :disc-actions (vec (repeat action-count action))}))

(defn- disc-command [db source params]
  (if (strength-disc-source? db source params)
    (strength-disc-command db source params)
    (cond-> (assoc (disc-target-command db source params)
                   :disc-variant (selected-disc-variant db source params))
      (and (star-disc-source? db source params)
           (:minion-orientation params))
      (assoc :minion-orientation (:minion-orientation params)))))

(defn- sword-target-command [db source params]
  (case (:sword-target-kind params)
    :piece
    (cond-> {:target {:kind :piece
                      :piece-id (:target-piece-id params)}
             :damage (:damage params)}
      (and (sword-orientation-available? db source params)
           (:orientation params))
      (assoc :orientation (:orientation params)))

    :territory
    (cond-> {:target {:kind :territory
                      :board-index (:target-board-index params)}
             :damage (:damage params)}
      (seq (sword-replacement-card-source-option-ids db source params))
      (assoc :replacement-card-source
             (selected-sword-replacement-card-source db source params))

      (:replacement-card-id params)
      (assoc :replacement-card-id (:replacement-card-id params)))

    {}))

(defn- sword-command [db source params]
  (let [sword-variant (selected-sword-variant db source params)]
    (cond-> (sword-target-command db source params)
      sword-variant
      (assoc :sword-variant sword-variant))))

(defn- sword-major-command [db source params]
  (let [actions (vec (remove nil?
                             (conj (completed-major-actions params)
                                   (sword-major-current-action db source params))))
        action-by-power (fn [power]
                          (some #(when (= power (:power %)) %) actions))]
    (case (active-power db source params)
      :justice
      (merge {:hand-trade-target (:target (action-by-power :trade-hand))}
             (sword-command db source params))

      :death
      {:sword-actions (mapv #(dissoc % :power) actions)}

      :tower
      (merge {:minion-orientation (:orientation (action-by-power :orient-minion))}
             (sword-command db source params))

      :moon
      {:rod (dissoc (action-by-power :rod) :power)
       :sword (dissoc (sword-major-current-action db source params) :power)}

      {})))

(defn- fool-command [_db _source params]
  {:reveals (vec (repeat (or (:fool-reveal-count params) 0) {}))})

(defn- high-priestess-command [db source params]
  {:redraws (vec (:redraws (normalize-high-priestess-redraws db source params)))})

(defn- judgement-command [db source params]
  {:piece-id (:piece-id params)
   :card-ids (valid-judgement-card-ids db source params (:judgement-card-ids params))})

(defn- devil-command [db source params]
  (let [actions (vec (remove nil?
                             (conj (completed-major-actions params)
                                   (devil-current-action db source params))))]
    (if (< 1 (count actions))
      {:actions actions}
      (piece-orientation-command params))))

(defn- unavailable-power-command [db source params power]
  (let [card (active-card db source params)]
    (cond-> {:power power}
      card
      (assoc :card-id (:id card)))))

(defn- composite-major-command [db source params]
  {:actions (vec (remove nil?
                         (conj (completed-major-actions params)
                               (composite-current-action db source params))))})

(defn- gameplay-power-command-for-power [db source params power]
  (case power
    :rod (rod-command db source params)
    :disc (disc-command db source params)
    :cup (cup-command db source params)
    :sun (sun-command db source params)
    :sword (sword-command db source params)
    (:justice :death :tower :moon)
    (sword-major-command db source params)
    :fool (fool-command db source params)
    :high-priestess (high-priestess-command db source params)
    :judgement (judgement-command db source params)
    :hierophant (piece-orientation-command params)
    :hermit (hermit-command db source params)
    :devil (devil-command db source params)
    (:empress :emperor :lovers :chariot :hanged-man :temperance)
    (composite-major-command db source params)
    (unavailable-power-command db source params power)))

(defn- world-command [db source params]
  (let [power (selected-world-copied-power db source params)
        command (gameplay-power-command-for-power db source params power)]
    (cond-> (assoc command :copied-board-index (:copied-board-index params))
      (contains? copied-suit-powers power)
      (assoc :copied-power power))))

(defn- gameplay-power-command [db source params]
  (if (world-move? db source params)
    (world-command db source params)
    (gameplay-power-command-for-power db source params (selected-power db source params))))

(defn move-command [db]
  (let [{:keys [source params]} (move-selection db)]
    (when source
      (case source
        :activate-territory
        (merge {:player-id (current-player-id db)
                :source (source-command source params)}
               (gameplay-power-command db source params))

        :play-hand-card
        (merge {:player-id (current-player-id db)
                :source (source-command source params)}
               (gameplay-power-command db source params))

        :draw-cards
        {:source :draw-cards
         :player-id (current-player-id db)
         :discard-card-ids (valid-discard-card-ids db (:discard-card-ids params))
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

(defn- command-with-transition-options [command transition-options power]
  (cond-> command
    (and (or (= :draw-cards (:source command))
             (contains? #{:fool :high-priestess} power))
         (:shuffle-fn transition-options)
         (not (contains? command :shuffle-fn)))
    (assoc :shuffle-fn (:shuffle-fn transition-options))))

(defn- transition-power [db]
  (let [{:keys [source params]} (move-selection db)]
    (if (world-move? db source params)
      (selected-world-copied-power db source params)
      (move-power db))))

(defn- confirmed-move-result [db command transition-options]
  (let [power (transition-power db)
        command (command-with-transition-options command transition-options power)]
    (cond
      (= :draw-cards (:source command))
      (game-state/apply-draw-move (game db) command)

      (= :orient-piece (:source command))
      (game-state/apply-orient-move (game db) command)

      (= :place-initial-small (:source command))
      (game-state/apply-initial-placement (game db) command)

      (= :world (move-power db))
      (game-state/apply-world-move (game db) command)

      (= :cup (move-power db))
      (game-state/apply-cup-move (game db) command)

      (= :rod (move-power db))
      (game-state/apply-rod-move (game db) command)

      (= :disc (move-power db))
      (game-state/apply-disc-move (game db) command)

      (= :sword (move-power db))
      (game-state/apply-sword-move (game db) command)

      (contains? #{:justice :death :tower :moon} (move-power db))
      (game-state/apply-sword-move (game db) command)

      (= :sun (move-power db))
      (game-state/apply-sun-move (game db) command)

      (= :fool (move-power db))
      (game-state/apply-fool-move (game db) command)

      (= :high-priestess (move-power db))
      (game-state/apply-high-priestess-move (game db) command)

      (= :judgement (move-power db))
      (game-state/apply-judgement-move (game db) command)

      (= :hierophant (move-power db))
      (game-state/apply-hierophant-move (game db) command)

      (= :hermit (move-power db))
      (game-state/apply-hermit-move (game db) command)

      (= :devil (move-power db))
      (game-state/apply-devil-move (game db) command)

      (= :empress (move-power db))
      (game-state/apply-empress-move (game db) command)

      (= :emperor (move-power db))
      (game-state/apply-emperor-move (game db) command)

      (= :lovers (move-power db))
      (game-state/apply-lovers-move (game db) command)

      (= :chariot (move-power db))
      (game-state/apply-chariot-move (game db) command)

      (= :hanged-man (move-power db))
      (game-state/apply-hanged-man-move (game db) command)

      (= :temperance (move-power db))
      (game-state/apply-temperance-move (game db) command)

      :else
      (game-state/failure :move-transition-unavailable
                          "Move selection is complete, but this gameplay rule transition is not implemented yet."
                          {:command command}))))

(defn- consumed-turn-action [db state]
  {:consumed? true
   :turn-key (game-turn-key state)
   :source (move-source db)})

(defn- apply-confirmed-move-result [db result]
  (if (:ok? result)
    (assoc db
           :game (:state result)
           :turn-action (consumed-turn-action db (:state result))
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
     (if-let [reason (source-unavailable-reason db (move-source db))]
       (apply-confirmed-move-result
        db
        (game-state/failure :move-source-unavailable
                            reason
                            {:source (move-source db)}))
       (let [command (move-command db)
             result (confirmed-move-result db command transition-options)]
         (apply-confirmed-move-result db result))))))
