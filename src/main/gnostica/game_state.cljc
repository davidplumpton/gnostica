(ns gnostica.game-state
  (:require [clojure.string :as str]
            [gnostica.board :as board]
            [gnostica.board-layout :as board-layout]
            [gnostica.cards :as cards]
            [gnostica.pieces :as pieces]))

(def min-players 2)
(def max-players 6)
(def starting-hand-size 6)
(def pieces-per-size-in-stash 5)
(def initial-phase :setup)
(def default-target-score 9)
(def required-player-fields [:id :name :color :css-color])
(def required-card-fields [:id :title :image])

(defn success
  ([state]
   (success state []))
  ([state events]
   {:ok? true
    :state state
    :events (vec events)}))

(defn failure [code message data]
  {:ok? false
   :error {:code code
           :message message
           :data data}})

(defn valid-player-count? [player-specs]
  (<= min-players (count player-specs) max-players))

(defn- player-id [player-spec]
  (:id player-spec))

(defn- duplicate-player-ids [player-specs]
  (->> player-specs
       (map player-id)
       frequencies
       (filter (fn [[_ occurrences]]
                 (> occurrences 1)))
       (map first)
       vec))

(defn- missing-player-id-indexes [player-specs]
  (->> player-specs
       (map-indexed vector)
       (keep (fn [[index player-spec]]
               (when (nil? (player-id player-spec))
                 index)))
       vec))

(defn- unknown-player-ids [player-specs]
  (->> player-specs
       (map player-id)
       (remove #(contains? pieces/players-by-id %))
       distinct
       vec))

(defn- valid-css-color? [value]
  (and (string? value)
       (some? (re-matches #"#[0-9a-fA-F]{6}" value))))

(defn- valid-three-color? [value]
  (and (integer? value)
       (<= 0 value 0xffffff)))

(defn- missing-or-invalid-player-fields [player]
  (cond-> []
    (nil? (:id player))
    (conj :id)

    (not (and (string? (:name player))
              (not (str/blank? (:name player)))))
    (conj :name)

    (not (valid-three-color? (:color player)))
    (conj :color)

    (not (valid-css-color? (:css-color player)))
    (conj :css-color)))

(defn- invalid-player-metadata [player-specs]
  (->> player-specs
       (map-indexed
        (fn [index player-spec]
          (let [player (when (map? player-spec)
                         (merge (get pieces/players-by-id (player-id player-spec))
                                player-spec))
                invalid-fields (missing-or-invalid-player-fields player)]
            (when (seq invalid-fields)
              {:index index
               :id (player-id player-spec)
               :invalid-fields invalid-fields}))))
       (remove nil?)
       vec))

(defn- validate-player-specs [player-specs]
  (if-not (sequential? player-specs)
    (failure :invalid-player-specs
             "Player specs must be a sequential collection."
             {:player-specs player-specs})
    (let [missing-id-indexes (missing-player-id-indexes player-specs)
          duplicate-ids (duplicate-player-ids player-specs)
          unknown-ids (unknown-player-ids player-specs)
          invalid-players (invalid-player-metadata player-specs)]
      (cond
        (not (valid-player-count? player-specs))
        (failure :invalid-player-count
                 "Gnostica requires between two and six players."
                 {:count (count player-specs)
                  :minimum min-players
                  :maximum max-players})

        (seq missing-id-indexes)
        (failure :invalid-player-specs
                 "Each player spec must include an :id."
                 {:missing-id-indexes missing-id-indexes})

        (seq duplicate-ids)
        (failure :duplicate-player-ids
                 "Player ids must be unique."
                 {:duplicate-ids duplicate-ids})

        (seq unknown-ids)
        (failure :unknown-player-ids
                 "Player ids must reference known player metadata."
                 {:unknown-ids unknown-ids
                  :known-ids (mapv :id pieces/players)})

        (seq invalid-players)
        (failure :invalid-player-metadata
                 "Player metadata must include a name, Three.js color, and CSS color."
                 {:players invalid-players
                  :required-fields required-player-fields})))))

(defn- initial-stash []
  (into {}
        (map (fn [size]
               [size pieces-per-size-in-stash]))
        (keys pieces/piece-sizes)))

(defn- normalize-player [index player-spec]
  (let [id (player-id player-spec)
        piece-player (get pieces/players-by-id id)]
    (merge
     (select-keys piece-player [:id :name :color :css-color])
     player-spec
     {:order-index index
      :hand []
      :score 0
      :challenge nil
      :eliminated? false
      :stash (initial-stash)
      :bid nil})))

(defn- deck-source [{:keys [deck deck-order]
                     :or {deck cards/deck}}]
  (if deck-order
    deck-order
    deck))

(defn- ordered-deck [{:keys [deck deck-order shuffle-fn]
                      :or {deck cards/deck
                           shuffle-fn shuffle}}]
  (let [ordered (if deck-order
                  deck-order
                  (shuffle-fn deck))]
    (if (sequential? ordered)
      (vec ordered)
      ordered)))

(defn- invalid-card-fields [card]
  (if-not (map? card)
    required-card-fields
    (cond-> []
      (not (and (string? (:id card))
                (not (str/blank? (:id card)))))
      (conj :id)

      (not (and (string? (:title card))
                (not (str/blank? (:title card)))))
      (conj :title)

      (not (and (string? (:image card))
                (not (str/blank? (:image card)))))
      (conj :image))))

(defn- invalid-cards [deck]
  (->> deck
       (map-indexed
        (fn [index card]
          (let [invalid-fields (invalid-card-fields card)]
            (when (seq invalid-fields)
              {:index index
               :card-id (:id card)
               :invalid-fields invalid-fields}))))
       (remove nil?)
       vec))

(defn- duplicate-card-ids [deck]
  (->> deck
       (map :id)
       frequencies
       (filter (fn [[_ occurrences]]
                 (> occurrences 1)))
       (map first)
       vec))

(defn- required-starting-card-count [player-count]
  (+ board/board-card-count
     (* starting-hand-size player-count)))

(defn- validate-deck
  ([deck]
   (validate-deck deck board/board-card-count))
  ([deck minimum-card-count]
   (if-not (sequential? deck)
     (failure :invalid-deck
              "The deck must be an ordered sequential collection of card maps."
              {:deck deck})
     (let [invalid-deck-cards (invalid-cards deck)
           duplicate-ids (duplicate-card-ids deck)]
       (cond
         (< (count deck) minimum-card-count)
         (failure :insufficient-deck
                  "The deck must contain enough cards to deal player hands and build the territory board."
                  {:count (count deck)
                   :minimum minimum-card-count
                   :starting-hand-size starting-hand-size
                   :board-card-count board/board-card-count})

         (seq invalid-deck-cards)
         (failure :invalid-deck-cards
                  "Every deck card must include an id, title, and image path."
                  {:invalid-cards invalid-deck-cards
                   :required-fields required-card-fields})

         (seq duplicate-ids)
         (failure :duplicate-card-ids
                  "Deck card ids must be unique."
                  {:duplicate-ids duplicate-ids}))))))

(defn- initial-stashes [players]
  (into {}
        (map (fn [player]
               [(:id player) (:stash player)]))
        players))

(defn- initial-turn [players]
  (let [order (mapv :id players)]
    {:order order
     :current-player-index 0
     :current-player-id (first order)
     :round 1}))

(defn- deal-starting-hands [players deck]
  (let [hand-card-count (* starting-hand-size (count players))
        hand-cards (take hand-card-count deck)
        hands (partition starting-hand-size hand-cards)
        board-deck (vec (drop hand-card-count deck))]
    {:players (mapv (fn [player hand]
                      (assoc player :hand (vec hand)))
                    players
                    hands)
     :board-deck board-deck}))

(defn current-player [state]
  (get-in state [:players-by-id (get-in state [:turn :current-player-id])]))

(defn append-history [state event]
  (update state :history conj event))

(defn- rebuild-players-by-id [players]
  (into {} (map (juxt :id identity)) players))

(defn- update-player [state player-id f & args]
  (let [players (mapv (fn [player]
                        (if (= player-id (:id player))
                          (apply f player args)
                          player))
                      (:players state))]
    (assoc state
           :players players
           :players-by-id (rebuild-players-by-id players))))

(defn- board-piece-counts [board-pieces]
  (reduce (fn [counts {:keys [player-id size]}]
            (if (and player-id size)
              (update-in counts [player-id size] (fnil inc 0))
              counts))
          {}
          board-pieces))

(defn- stash-after-board-pieces [piece-counts player-id]
  (into {}
        (map (fn [size]
               [size (- pieces-per-size-in-stash
                        (get-in piece-counts [player-id size] 0))]))
        (keys pieces/piece-sizes)))

(defn with-board-pieces
  "Replace active pieces and rebuild both stash mirrors from the starting stash size."
  [state board-pieces]
  (let [board-pieces (vec board-pieces)
        piece-counts (board-piece-counts board-pieces)
        players (mapv (fn [player]
                        (assoc player
                               :stash (stash-after-board-pieces piece-counts
                                                                 (:id player))))
                      (:players state))]
    (assoc state
           :players players
           :players-by-id (rebuild-players-by-id players)
           :pieces (assoc (:pieces state)
                          :on-board board-pieces
                          :stashes (initial-stashes players)))))

(defn- board-cell-by-index [state board-index]
  (some (fn [cell]
          (when (= board-index (:index cell))
            cell))
        (:board state)))

(defn- board-cell-at [state row col]
  (some (fn [cell]
          (when (and (= row (:row cell))
                     (= col (:col cell)))
            cell))
        (:board state)))

(defn- piece-by-id [state piece-id]
  (some (fn [piece]
          (when (= piece-id (:id piece))
            piece))
        (get-in state [:pieces :on-board])))

(defn- pieces-at-board-index [state board-index]
  (filterv #(= board-index (:space-index %))
           (get-in state [:pieces :on-board])))

(defn- piece-coordinate [state piece]
  (if-let [{:keys [row col]} (:space piece)]
    [row col]
    (when-let [{:keys [row col]} (board-cell-by-index state (:space-index piece))]
      [row col])))

(defn- player-hand-card [state player-id card-id]
  (some (fn [card]
          (when (= card-id (:id card))
            card))
        (get-in state [:players-by-id player-id :hand])))

(defn- remove-card-from-hand [state player-id card-id]
  (update-player state player-id update :hand
                 (fn [hand]
                   (vec (remove #(= card-id (:id %)) hand)))))

(defn- remove-cards-from-hand [state player-id card-ids]
  (let [card-id-set (set card-ids)]
    (update-player state player-id update :hand
                   (fn [hand]
                     (vec (remove #(contains? card-id-set (:id %)) hand))))))

(defn- discard-card [state card]
  (update state :discard-pile conj card))

(defn- discard-cards [state cards]
  (update state :discard-pile into (vec cards)))

(defn- append-cards-to-hand [state player-id cards]
  (update-player state player-id update :hand
                 (fn [hand]
                   (vec (concat hand cards)))))

(defn- duplicate-values [values]
  (->> values
       frequencies
       (filter (fn [[_ occurrences]]
                 (> occurrences 1)))
       (map first)
       vec))

(defn- small-stash-count [state player-id]
  (or (get-in state [:players-by-id player-id :stash :small])
      (get-in state [:pieces :stashes player-id :small])
      0))

(defn- decrement-small-stash [state player-id]
  (-> state
      (update-player player-id update-in [:stash :small] dec)
      (update-in [:pieces :stashes player-id :small] dec)))

(defn- next-piece-id [state player-id size]
  (let [prefix (str (name player-id) "-" (name size) "-")
        used-ids (set (map :id (get-in state [:pieces :on-board])))]
    (->> (iterate inc 1)
         (map #(keyword (str prefix %)))
         (remove used-ids)
         first)))

(defn- source-summary [source]
  (select-keys source [:kind :board-index :card-id :piece-id]))

(defn- current-player-id? [state player-id]
  (= player-id (get-in state [:turn :current-player-id])))

(def rod-modes #{:move-minion
                 :push-piece
                 :push-territory})

(def rod-direction-offsets
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
      (when (and (int? row) (int? col))
        {:row row
         :col col}))))

(defn rod-destination-coordinate [coordinate direction distance]
  (when-let [[row-offset col-offset] (get rod-direction-offsets direction)]
    (when-let [{:keys [row col]} (coordinate-map coordinate)]
      (when (int? distance)
        {:row (+ row (* row-offset distance))
         :col (+ col (* col-offset distance))}))))

(defn- same-coordinate? [left right]
  (= (coordinate-map left)
     (coordinate-map right)))

(defn- rod-targetable-coordinate? [actor-coordinate target-coordinate direction target-self?]
  (or target-self?
      (same-coordinate? target-coordinate
                        (rod-destination-coordinate actor-coordinate direction 1))))

(defn- target-summary [target]
  (select-keys target [:kind :piece-id :board-index :row :col]))

(defn- territory-target-cell [state target]
  (cond
    (not (map? target))
    (failure :invalid-rod-target
             "Rod territory targets require a target map."
             {:target target})

    (not= :territory (:kind target))
    (failure :invalid-rod-target
             "Rod territory targets must use :kind :territory."
             {:target target})

    (some? (:board-index target))
    (if-let [cell (board-cell-by-index state (:board-index target))]
      {:ok? true
       :cell cell}
      (failure :invalid-target-territory
               "Rod territory targets must reference an existing board cell."
               {:target target}))

    (and (int? (:row target))
         (int? (:col target)))
    (if-let [cell (board-cell-at state (:row target) (:col target))]
      {:ok? true
       :cell cell}
      (failure :invalid-target-territory
               "Rod territory targets must reference an existing board cell."
               {:target target}))

    :else
    (failure :invalid-rod-target
             "Rod territory targets require a board index or row and column."
             {:target target})))

(defn- resolve-rod-source [state player-id source]
  (let [piece (piece-by-id state (:piece-id source))
        piece-coordinate (when piece
                           (piece-coordinate state piece))]
    (cond
      (not (map? source))
      (failure :invalid-rod-command
               "Rod moves require a source map."
               {:source source})

      (nil? piece)
      (failure :invalid-piece
               "Rod moves require one of the player's pieces as the acting minion."
               {:piece-id (:piece-id source)})

      (not= player-id (:player-id piece))
      (failure :invalid-piece
               "The acting minion must belong to the move's player."
               {:piece-id (:piece-id source)
                :player-id player-id
                :piece-player-id (:player-id piece)})

      (nil? piece-coordinate)
      (failure :invalid-piece-space
               "Rod moves require an acting minion with a board coordinate."
               {:piece-id (:piece-id source)
                :space-index (:space-index piece)
                :space (:space piece)})

      (= :up (:orientation piece))
      (failure :rod-minion-upright
               "A piece standing upright may not use a Rod."
               {:piece-id (:id piece)
                :orientation (:orientation piece)})

      (not (contains? pieces/cardinal-orientations (:orientation piece)))
      (failure :invalid-rod-direction
               "Rod moves require the acting minion to point in a cardinal direction."
               {:piece-id (:id piece)
                :orientation (:orientation piece)
                :legal-directions pieces/cardinal-orientations})

      (= :territory (:kind source))
      (let [cell (board-cell-by-index state (:board-index source))]
        (cond
          (nil? cell)
          (failure :invalid-source-territory
                   "Rod territory sources must reference an existing board cell."
                   {:board-index (:board-index source)})

          (not= (:board-index source) (:space-index piece))
          (failure :source-piece-mismatch
                   "The acting minion must occupy the source territory."
                   {:piece-id (:piece-id source)
                    :piece-space-index (:space-index piece)
                    :source-board-index (:board-index source)})

          (not (cards/rod-card? (:card cell)))
          (failure :source-card-not-rod
                   "The source card does not provide a Rod power."
                   {:card-id (get-in cell [:card :id])
                    :source source})

          :else
          {:ok? true
           :source source
           :source-card (:card cell)
           :piece piece
           :piece-coordinate (coordinate-map piece-coordinate)
           :direction (:orientation piece)}))

      (= :hand-card (:kind source))
      (let [card (player-hand-card state player-id (:card-id source))]
        (cond
          (nil? card)
          (failure :invalid-hand-card
                   "Rod hand-card sources must reference a card in the player's hand."
                   {:card-id (:card-id source)
                    :player-id player-id})

          (not (cards/rod-card? card))
          (failure :source-card-not-rod
                   "The source card does not provide a Rod power."
                   {:card-id (:id card)
                    :source source})

          :else
          {:ok? true
           :source source
           :source-card card
           :discard-source-card? true
           :piece piece
           :piece-coordinate (coordinate-map piece-coordinate)
           :direction (:orientation piece)}))

      :else
      (failure :invalid-rod-command
               "Rod move sources must be either :territory or :hand-card."
               {:source source}))))

(defn- resolve-rod-distance [piece distance]
  (let [maximum (or (pieces/pips piece) 0)]
    (cond
      (not (int? distance))
      (failure :invalid-rod-distance
               "Rod moves require an integer distance."
               {:distance distance
                :maximum maximum})

      (not (pos? distance))
      (failure :invalid-rod-distance
               "Rod moves cannot move zero spaces."
               {:distance distance
                :maximum maximum})

      (< maximum distance)
      (failure :invalid-rod-distance
               "Rod moves cannot exceed the acting minion's pip count."
               {:distance distance
                :maximum maximum})

      :else
      {:ok? true
       :distance distance
       :maximum maximum})))

(defn- resolve-rod-orientation [player-id moved-piece orientation]
  (cond
    (nil? orientation)
    {:ok? true}

    (not= player-id (:player-id moved-piece))
    (failure :invalid-orientation
             "Enemy pieces retain their original orientation when moved by a Rod."
             {:piece-id (:id moved-piece)
              :piece-player-id (:player-id moved-piece)
              :orientation orientation})

    (not (contains? pieces/legal-orientations orientation))
    (failure :invalid-orientation
             "Rod moves can only reorient current-player pieces to a legal orientation."
             {:piece-id (:id moved-piece)
              :orientation orientation
              :legal-orientations pieces/legal-orientations})

    :else
    {:ok? true
     :orientation orientation}))

(defn- normalize-rod-piece-target [state player-id source-result target-piece distance orientation]
  (let [{:keys [piece direction]
         source-coordinate :piece-coordinate} source-result
        target-coordinate (coordinate-map (piece-coordinate state target-piece))
        target-self? (= (:id piece) (:id target-piece))]
    (cond
      (nil? target-coordinate)
      (failure :invalid-piece-space
               "Rod piece targets must have a board coordinate."
               {:piece-id (:id target-piece)
                :space-index (:space-index target-piece)
                :space (:space target-piece)})

      (not (rod-targetable-coordinate? source-coordinate target-coordinate direction target-self?))
      (failure :invalid-rod-target
               "Rod piece targets must be the minion itself or occupy the adjacent space in the minion direction."
               {:piece-id (:id target-piece)
                :direction direction
                :source-coordinate source-coordinate
                :target-coordinate target-coordinate
                :expected-coordinate (rod-destination-coordinate source-coordinate direction 1)})

      :else
      (let [orientation-result (resolve-rod-orientation player-id target-piece orientation)]
        (if (:ok? orientation-result)
          {:ok? true
           :target (cond-> {:kind :piece
                            :piece-id (:id target-piece)
                            :player-id (:player-id target-piece)
                            :row (:row target-coordinate)
                            :col (:col target-coordinate)
                            :destination (rod-destination-coordinate target-coordinate
                                                                     direction
                                                                     distance)}
                     (:orientation orientation-result)
                     (assoc :orientation (:orientation orientation-result)))
           :target-piece target-piece}
          orientation-result)))))

(defn- resolve-rod-target [state player-id source-result mode target distance orientation]
  (case mode
    :move-minion
    (let [piece (:piece source-result)]
      (if (and (some? target)
               (not= {:kind :piece
                      :piece-id (:id piece)}
                     (target-summary target)))
        (failure :invalid-rod-target
                 "Move-minion Rod commands move the acting minion and do not need another target."
                 {:target target
                  :piece-id (:id piece)})
        (normalize-rod-piece-target state
                                    player-id
                                    source-result
                                    piece
                                    distance
                                    orientation)))

    :push-piece
    (cond
      (not (map? target))
      (failure :invalid-rod-target
               "Push-piece Rod commands require a target piece map."
               {:target target})

      (not= :piece (:kind target))
      (failure :invalid-rod-target
               "Push-piece Rod commands target :kind :piece."
               {:target target})

      (nil? (:piece-id target))
      (failure :invalid-rod-target
               "Push-piece Rod commands require a target piece id."
               {:target target})

      :else
      (if-let [target-piece (piece-by-id state (:piece-id target))]
        (normalize-rod-piece-target state
                                    player-id
                                    source-result
                                    target-piece
                                    distance
                                    orientation)
        (failure :invalid-target-piece
                 "Push-piece Rod commands must reference a piece on the board."
                 {:target target})))

    :push-territory
    (if (some? orientation)
      (failure :invalid-orientation
               "Rod territory pushes do not take a piece orientation."
               {:orientation orientation
                :target target})
      (let [cell-result (territory-target-cell state target)]
        (if (:ok? cell-result)
          (let [cell (:cell cell-result)
                cell-coordinate (select-keys cell [:row :col])
                {:keys [piece-coordinate direction]} source-result]
            (if (rod-targetable-coordinate? piece-coordinate
                                            cell-coordinate
                                            direction
                                            false)
              {:ok? true
               :target {:kind :territory
                        :board-index (:index cell)
                        :row (:row cell)
                        :col (:col cell)
                        :destination (rod-destination-coordinate cell-coordinate
                                                                 direction
                                                                 distance)}
               :target-cell cell}
              (failure :invalid-rod-target
                       "Rod territory targets must occupy the adjacent space in the minion direction."
                       {:target target
                        :direction direction
                        :source-coordinate piece-coordinate
                        :target-coordinate cell-coordinate
                        :expected-coordinate (rod-destination-coordinate piece-coordinate direction 1)})))
          cell-result)))))

(defn resolve-rod-command [state command]
  (let [{:keys [player-id source mode target distance orientation direction]} command]
    (cond
      (not (map? command))
      (failure :invalid-rod-command
               "Rod moves require a command map."
               {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (failure :unknown-player
               "Rod moves require a participating player."
               {:player-id player-id})

      (not (current-player-id? state player-id))
      (failure :not-current-player
               "Only the current player can resolve a Rod move."
               {:player-id player-id
                :current-player-id (get-in state [:turn :current-player-id])})

      (not (contains? rod-modes mode))
      (failure :invalid-rod-mode
               "Rod moves require a supported mode."
               {:mode mode
                :supported-modes rod-modes})

      :else
      (let [source-result (resolve-rod-source state player-id source)]
        (if-not (:ok? source-result)
          source-result
          (let [minion-direction (:direction source-result)]
            (cond
              (and (some? direction)
                   (not= direction minion-direction))
              (failure :invalid-rod-direction
                       "Rod command direction must match the acting minion orientation."
                       {:direction direction
                        :minion-direction minion-direction
                        :piece-id (get-in source-result [:piece :id])})

              :else
              (let [distance-result (resolve-rod-distance (:piece source-result)
                                                          distance)]
                (if-not (:ok? distance-result)
                  distance-result
                  (let [target-result (resolve-rod-target state
                                                          player-id
                                                          source-result
                                                          mode
                                                          target
                                                          (:distance distance-result)
                                                          orientation)]
                    (if-not (:ok? target-result)
                      target-result
                      (let [normalized-command (cond-> {:player-id player-id
                                                        :source (source-summary source)
                                                        :mode mode
                                                        :target (:target target-result)
                                                        :distance (:distance distance-result)
                                                        :direction minion-direction}
                                                 (:orientation (:target target-result))
                                                 (assoc :orientation (:orientation (:target target-result))))]
                        (merge {:ok? true
                                :command normalized-command
                                :source-card (:source-card source-result)
                                :piece (:piece source-result)}
                               (select-keys target-result
                                            [:target-piece :target-cell]))))))))))))))

(defn- resolve-cup-source [state player-id source]
  (let [piece (piece-by-id state (:piece-id source))]
    (cond
      (not (map? source))
      (failure :invalid-cup-command
               "Cup moves require a source map."
               {:source source})

      (nil? piece)
      (failure :invalid-piece
               "Cup moves require one of the player's pieces as the acting minion."
               {:piece-id (:piece-id source)})

      (not= player-id (:player-id piece))
      (failure :invalid-piece
               "The acting minion must belong to the move's player."
               {:piece-id (:piece-id source)
                :player-id player-id
                :piece-player-id (:player-id piece)})

      (= :territory (:kind source))
      (let [cell (board-cell-by-index state (:board-index source))]
        (cond
          (nil? cell)
          (failure :invalid-source-territory
                   "Cup territory sources must reference an existing board cell."
                   {:board-index (:board-index source)})

          (not= (:board-index source) (:space-index piece))
          (failure :source-piece-mismatch
                   "The acting minion must occupy the source territory."
                   {:piece-id (:piece-id source)
                    :piece-space-index (:space-index piece)
                    :source-board-index (:board-index source)})

          (not (cards/cup-card? (:card cell)))
          (failure :source-card-not-cup
                   "The source card does not provide a Cup power."
                   {:card-id (get-in cell [:card :id])
                    :source source})

          :else
          {:ok? true
           :source source
           :source-card (:card cell)
           :piece piece}))

      (= :hand-card (:kind source))
      (let [card (player-hand-card state player-id (:card-id source))]
        (cond
          (nil? card)
          (failure :invalid-hand-card
                   "Cup hand-card sources must reference a card in the player's hand."
                   {:card-id (:card-id source)
                    :player-id player-id})

          (not (cards/cup-card? card))
          (failure :source-card-not-cup
                   "The source card does not provide a Cup power."
                   {:card-id (:id card)
                    :source source})

          :else
          {:ok? true
           :source source
           :source-card card
           :discard-source-card? true
           :piece piece}))

      :else
      (failure :invalid-cup-command
               "Cup move sources must be either :territory or :hand-card."
               {:source source}))))

(defn- apply-source-cost [state player-id {:keys [source-card discard-source-card?]}]
  (if discard-source-card?
    (-> state
        (remove-card-from-hand player-id (:id source-card))
        (discard-card source-card))
    state))

(defn- place-small-piece [state player-id source target orientation]
  (cond
    (not (map? target))
    (failure :invalid-cup-target
             "Cup small-piece targets must be territory target maps."
             {:target target})

    (not= :territory (:kind target))
    (failure :invalid-cup-target
             "Cup small-piece placement targets an existing territory."
             {:target target})

    (nil? (board-cell-by-index state (:board-index target)))
    (failure :invalid-target-territory
             "Cup small-piece targets must reference an existing board cell."
             {:board-index (:board-index target)})

    (not (contains? pieces/legal-orientations orientation))
    (failure :invalid-orientation
             "Cup small-piece placement requires a legal orientation."
             {:orientation orientation
              :legal-orientations pieces/legal-orientations})

    (<= pieces/max-pieces-per-space
        (count (pieces-at-board-index state (:board-index target))))
    (failure :target-territory-full
             "Cup small-piece placement requires fewer than three pieces on the target territory."
             {:board-index (:board-index target)
              :maximum pieces/max-pieces-per-space})

    (not (pos? (small-stash-count state player-id)))
    (failure :no-small-piece-available
             "The player has no small pieces available in stash."
             {:player-id player-id})

    :else
    (let [piece {:id (next-piece-id state player-id :small)
                 :player-id player-id
                 :space-index (:board-index target)
                 :size :small
                 :orientation orientation}
          event {:type :cup/small-piece-created
                 :player-id player-id
                 :source (source-summary (:source source))
                 :target {:kind :territory
                          :board-index (:board-index target)}
                 :piece piece}
          next-state (-> state
                         (apply-source-cost player-id source)
                         (decrement-small-stash player-id)
                         (update-in [:pieces :on-board] conj piece)
                         (append-history event))]
      (success next-state [event]))))

(defn- wasteland-target [target]
  (when (and (map? target)
             (= :wasteland (:kind target))
             (int? (:row target))
             (int? (:col target)))
    (select-keys target [:kind :row :col])))

(defn- wasteland-target? [state target]
  (boolean
   (some (fn [space]
           (and (= (:row target) (:row space))
                (= (:col target) (:col space))))
         (board-layout/wasteland-spaces (:board state)))))

(defn- enemy-pieces-at-coordinate [state player-id row col]
  (->> (get-in state [:pieces :on-board])
       (filter (fn [piece]
                 (and (not= player-id (:player-id piece))
                      (= [row col] (piece-coordinate state piece)))))
       vec))

(defn- move-wasteland-pieces-to-board-index [state row col board-index]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (mapv (fn [piece]
                       (if (= [row col] (piece-coordinate state piece))
                         (-> piece
                             (dissoc :space)
                             (assoc :space-index board-index))
                         piece))
                     board-pieces))))

(defn- move-board-index-pieces-to-wasteland [state board-index row col]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (mapv (fn [piece]
                       (if (= board-index (:space-index piece))
                         (-> piece
                             (dissoc :space-index)
                             (assoc :space {:kind :wasteland
                                            :row row
                                            :col col}))
                         piece))
                     board-pieces))))

(defn- pieces-at-coordinate [state row col]
  (filterv #(= [row col] (piece-coordinate state %))
           (get-in state [:pieces :on-board])))

(defn- next-board-index [state]
  (inc (apply max -1 (map :index (:board state)))))

(defn- create-wasteland-territory [state player-id source target one-point-card-id]
  (let [{:keys [row col] :as normalized-target} (wasteland-target target)
        source-card-id (get-in source [:source-card :id])
        one-point-card (player-hand-card state player-id one-point-card-id)]
    (cond
      (nil? normalized-target)
      (failure :invalid-cup-target
               "Cup territory creation targets an explicit wasteland coordinate."
               {:target target})

      (some? (board-cell-at state row col))
      (failure :target-not-wasteland
               "Cup territory creation must target an empty wasteland space."
               {:target normalized-target})

      (not (wasteland-target? state normalized-target))
      (failure :target-not-wasteland
               "Cup territory creation cannot target the void."
               {:target normalized-target})

      (seq (enemy-pieces-at-coordinate state player-id row col))
      (failure :wasteland-occupied-by-enemy
               "Cup territory creation cannot target a wasteland occupied by enemy pieces."
               {:target normalized-target
                :enemy-piece-ids (mapv :id (enemy-pieces-at-coordinate state player-id row col))})

      (nil? one-point-card)
      (failure :invalid-one-point-card
               "Cup territory creation requires a selected card from the player's hand."
               {:card-id one-point-card-id
                :player-id player-id})

      (= source-card-id one-point-card-id)
      (failure :card-already-used
               "A played source card cannot also become the new territory."
               {:card-id one-point-card-id})

      (not (cards/one-point-card? one-point-card))
      (failure :invalid-one-point-card
               "Cup territory creation requires a one-point spot card."
               {:card-id one-point-card-id})

      :else
      (let [board-index (next-board-index state)
            cell {:index board-index
                  :row row
                  :col col
                  :orientation (board/orientation-for row col)
                  :face :up
                  :card one-point-card}
            event {:type :cup/territory-created
                   :player-id player-id
                   :source (source-summary (:source source))
                   :target normalized-target
                   :board-index board-index
                   :card-id one-point-card-id}
            next-state (-> state
                           (apply-source-cost player-id source)
                           (remove-card-from-hand player-id one-point-card-id)
                           (update :board conj cell)
                           (move-wasteland-pieces-to-board-index row col board-index)
                           (append-history event))]
        (success next-state [event])))))

(defn- rod-destination-space [state moved-piece {:keys [row col] :as destination}]
  (cond
    (not (and (int? row) (int? col)))
    (failure :invalid-rod-destination
             "Rod piece movement requires a destination coordinate."
             {:piece-id (:id moved-piece)
              :destination destination})

    :else
    (if-let [cell (board-cell-at state row col)]
      (let [space-pieces (remove #(= (:id moved-piece) (:id %))
                                 (pieces-at-coordinate state row col))]
        (if (<= pieces/max-pieces-per-space (count space-pieces))
          (failure :target-territory-full
                   "Rod piece movement requires fewer than three pieces on the destination territory."
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
        (if (wasteland-target? state target)
          {:ok? true
           :piece-space {:space target}
           :destination target}
          (failure :rod-destination-void
                   "Rod piece movement cannot end in the void."
                   {:piece-id (:id moved-piece)
                    :destination destination}))))))

(defn- rod-territory-destination-space [state player-id {:keys [row col] :as destination}]
  (cond
    (not (and (int? row) (int? col)))
    (failure :invalid-rod-destination
             "Rod territory pushing requires a destination coordinate."
             {:destination destination})

    (some? (board-cell-at state row col))
    (failure :target-not-wasteland
             "Rod territory pushing must land in an empty wasteland space."
             {:destination destination})

    (not (wasteland-target? state {:kind :wasteland
                                   :row row
                                   :col col}))
    (failure :rod-destination-void
             "Rod territory pushing cannot land in the void."
             {:destination destination})

    (seq (enemy-pieces-at-coordinate state player-id row col))
    (failure :wasteland-occupied-by-enemy
             "Rod territory pushing cannot land on a wasteland occupied by enemy pieces."
             {:destination destination
              :enemy-piece-ids (mapv :id (enemy-pieces-at-coordinate state player-id row col))})

    :else
    {:ok? true
     :destination {:kind :wasteland
                   :row row
                   :col col}}))

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

(defn- replace-piece [state piece]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (mapv (fn [board-piece]
                       (if (= (:id piece) (:id board-piece))
                         piece
                         board-piece))
                     board-pieces))))

(defn- player-pieces [state player-id]
  (filterv #(= player-id (:player-id %))
           (get-in state [:pieces :on-board])))

(defn apply-orient-move [state command]
  (let [{:keys [player-id piece-id orientation]} command
        piece (piece-by-id state piece-id)]
    (cond
      (not (map? command))
      (failure :invalid-orient-command
               "Orient moves require a command map."
               {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (failure :unknown-player
               "Orient moves require a participating player."
               {:player-id player-id})

      (not (current-player-id? state player-id))
      (failure :not-current-player
               "Only the current player can orient a piece."
               {:player-id player-id
                :current-player-id (get-in state [:turn :current-player-id])})

      (nil? piece)
      (failure :invalid-piece
               "Orient moves require a piece on the board."
               {:piece-id piece-id})

      (not= player-id (:player-id piece))
      (failure :invalid-piece
               "Orient moves can only target one of the current player's pieces."
               {:piece-id piece-id
                :player-id player-id
                :piece-player-id (:player-id piece)})

      (not (contains? pieces/legal-orientations orientation))
      (failure :invalid-orientation
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
                           (replace-piece oriented-piece)
                           (append-history event))]
        (success next-state [event])))))

(defn- initial-placement-territory-target [state target]
  (let [board-index (:board-index target)
        cell (board-cell-by-index state board-index)
        pieces (when cell
                 (pieces-at-coordinate state (:row cell) (:col cell)))]
    (cond
      (nil? cell)
      (failure :invalid-target-territory
               "Initial small placement territory targets must reference an existing board cell."
               {:target target})

      (seq pieces)
      (failure :target-space-occupied
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
  (let [{:keys [row col] :as normalized-target} (wasteland-target target)
        pieces (when normalized-target
                 (pieces-at-coordinate state row col))]
    (cond
      (nil? normalized-target)
      (failure :invalid-initial-placement-target
               "Initial small placement wasteland targets require an explicit coordinate."
               {:target target})

      (some? (board-cell-at state row col))
      (failure :target-not-wasteland
               "Initial small placement wasteland targets must be empty spaces next to a territory."
               {:target normalized-target})

      (not (wasteland-target? state normalized-target))
      (failure :target-not-wasteland
               "Initial small placement cannot target the void."
               {:target normalized-target})

      (seq pieces)
      (failure :target-space-occupied
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
    (failure :invalid-initial-placement-target
             "Initial small placement requires a target map."
             {:target target})

    (= :territory (:kind target))
    (initial-placement-territory-target state target)

    (= :wasteland (:kind target))
    (initial-placement-wasteland-target state target)

    :else
    (failure :invalid-initial-placement-target
             "Initial small placement targets must be either :territory or :wasteland."
             {:target target})))

(defn apply-initial-placement [state command]
  (let [{:keys [player-id target orientation]} command]
    (cond
      (not (map? command))
      (failure :invalid-initial-placement-command
               "Initial small placement requires a command map."
               {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (failure :unknown-player
               "Initial small placement requires a participating player."
               {:player-id player-id})

      (not (current-player-id? state player-id))
      (failure :not-current-player
               "Only the current player can place an initial small piece."
               {:player-id player-id
                :current-player-id (get-in state [:turn :current-player-id])})

      (seq (player-pieces state player-id))
      (failure :initial-placement-has-pieces
               "Initial small placement is only available when the player has no pieces on the board."
               {:player-id player-id
                :piece-ids (mapv :id (player-pieces state player-id))})

      (not (contains? pieces/legal-orientations orientation))
      (failure :invalid-orientation
               "Initial small placement requires a legal orientation."
               {:orientation orientation
                :legal-orientations pieces/legal-orientations})

      (not (pos? (small-stash-count state player-id)))
      (failure :no-small-piece-available
               "The player has no small pieces available in stash."
               {:player-id player-id})

      :else
      (let [target-result (initial-placement-target state target)]
        (if-not (:ok? target-result)
          target-result
          (let [piece (merge {:id (next-piece-id state player-id :small)
                              :player-id player-id
                              :size :small
                              :orientation orientation}
                             (:piece-space target-result))
                event {:type :initial-placement/small-piece-placed
                       :player-id player-id
                       :target (:target target-result)
                       :piece piece}
                next-state (-> state
                               (decrement-small-stash player-id)
                               (update-in [:pieces :on-board] conj piece)
                               (append-history event))]
            (success next-state [event])))))))

(defn- apply-rod-piece-move [state player-id {:keys [command source-card target-piece]}]
  (let [{:keys [mode source target distance direction]} command
        destination-result (rod-destination-space state
                                                  target-piece
                                                  (:destination target))]
    (if-not (:ok? destination-result)
      destination-result
      (let [moved-piece (move-piece-to-space target-piece
                                             (:piece-space destination-result)
                                             (:orientation target))
            event {:type (case mode
                           :move-minion :rod/minion-moved
                           :push-piece :rod/piece-pushed)
                   :player-id player-id
                   :source source
                   :target (select-keys target [:kind :piece-id :player-id :row :col])
                   :destination (:destination destination-result)
                   :distance distance
                   :direction direction
                   :piece moved-piece}
            source-cost {:source-card source-card
                         :discard-source-card? (= :hand-card (:kind source))}
            next-state (-> state
                           (apply-source-cost player-id source-cost)
                           (replace-piece moved-piece)
                           (append-history event))]
        (success next-state [event])))))

(defn- move-territory-cell [state board-index row col]
  (update state :board
          (fn [cells]
            (mapv (fn [cell]
                    (if (= board-index (:index cell))
                      (assoc cell
                             :row row
                             :col col
                             :orientation (board/orientation-for row col))
                      cell))
                  cells))))

(defn- apply-rod-territory-push [state player-id {:keys [command source-card target-cell]}]
  (let [{:keys [source target distance direction]} command
        {:keys [row col]} target-cell
        destination-result (rod-territory-destination-space state
                                                           player-id
                                                           (:destination target))
        enemy-pieces (enemy-pieces-at-coordinate state player-id row col)]
    (cond
      (seq enemy-pieces)
      (failure :target-territory-occupied-by-enemy
               "Rod territory pushing cannot target a territory occupied by enemy pieces."
               {:target (select-keys target [:kind :board-index :row :col])
                :enemy-piece-ids (mapv :id enemy-pieces)})

      (not (:ok? destination-result))
      destination-result

      :else
      (let [destination (:destination destination-result)
            moved-territory (assoc target-cell
                                   :row (:row destination)
                                   :col (:col destination)
                                   :orientation (board/orientation-for (:row destination)
                                                                       (:col destination)))
            event {:type :rod/territory-pushed
                   :player-id player-id
                   :source source
                   :target (select-keys target [:kind :board-index :row :col])
                   :destination destination
                   :distance distance
                   :direction direction
                   :territory moved-territory}
            source-cost {:source-card source-card
                         :discard-source-card? (= :hand-card (:kind source))}
            next-state (-> state
                           (apply-source-cost player-id source-cost)
                           (move-board-index-pieces-to-wasteland (:index target-cell) row col)
                           (move-territory-cell (:index target-cell)
                                                (:row destination)
                                                (:col destination))
                           (move-wasteland-pieces-to-board-index (:row destination)
                                                                 (:col destination)
                                                                 (:index target-cell))
                           (append-history event))]
        (success next-state [event])))))

(defn apply-rod-move [state command]
  (let [result (resolve-rod-command state command)]
    (if-not (:ok? result)
      result
      (let [normalized-command (:command result)
            player-id (:player-id normalized-command)]
        (case (:mode normalized-command)
          (:move-minion :push-piece)
          (apply-rod-piece-move state player-id result)

          :push-territory
          (apply-rod-territory-push state player-id result))))))

(defn apply-cup-move [state command]
  (let [{:keys [player-id source target orientation one-point-card-id]} command]
    (cond
      (not (map? command))
      (failure :invalid-cup-command
               "Cup moves require a command map."
               {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (failure :unknown-player
               "Cup moves require a participating player."
               {:player-id player-id})

      (not (current-player-id? state player-id))
      (failure :not-current-player
               "Only the current player can apply a Cup move."
               {:player-id player-id
                :current-player-id (get-in state [:turn :current-player-id])})

      :else
      (let [source-result (resolve-cup-source state player-id source)]
        (if (:ok? source-result)
          (case (:kind target)
            :territory
            (place-small-piece state player-id source-result target orientation)

            :wasteland
            (create-wasteland-territory state player-id source-result target one-point-card-id)

            (failure :invalid-cup-target
                     "Cup move targets must be either :territory or :wasteland."
                     {:target target}))
          source-result)))))

(defn- refresh-draw-pile [state shuffle-fn]
  (if (and (empty? (:draw-pile state))
           (seq (:discard-pile state)))
    (let [shuffled-cards (shuffle-fn (:discard-pile state))]
      (if (sequential? shuffled-cards)
        {:ok? true
         :state (-> state
                    (assoc :draw-pile (vec shuffled-cards))
                    (assoc :discard-pile []))
         :reshuffled? true}
        (failure :invalid-shuffle-result
                 "The draw-pile shuffle function must return a sequential collection of cards."
                 {:result shuffled-cards})))
    {:ok? true
     :state state
     :reshuffled? false}))

(defn- draw-from-piles [state draw-count shuffle-fn]
  (loop [state state
         remaining draw-count
         drawn-cards []
         reshuffled? false]
    (if (zero? remaining)
      (let [refresh-result (if (pos? draw-count)
                             (refresh-draw-pile state shuffle-fn)
                             {:ok? true
                              :state state
                              :reshuffled? false})]
        (if (:ok? refresh-result)
          {:ok? true
           :state (:state refresh-result)
           :drawn-cards drawn-cards
           :reshuffled? (or reshuffled? (:reshuffled? refresh-result))}
          refresh-result))
      (let [refresh-result (refresh-draw-pile state shuffle-fn)]
        (if-not (:ok? refresh-result)
          refresh-result
          (let [state (:state refresh-result)
                draw-pile (:draw-pile state)]
            (if-let [card (first draw-pile)]
              (recur (assoc state :draw-pile (vec (rest draw-pile)))
                     (dec remaining)
                     (conj drawn-cards card)
                     (or reshuffled? (:reshuffled? refresh-result)))
              (failure :insufficient-draw-cards
                       "There are not enough cards available to draw."
                       {:draw-count draw-count
                        :drawn-count (count drawn-cards)}))))))))

(defn apply-draw-move [state command]
  (let [{:keys [player-id draw-count shuffle-fn]} command
        discard-card-ids (or (:discard-card-ids command) [])
        shuffle-fn (or shuffle-fn shuffle)]
    (cond
      (not (map? command))
      (failure :invalid-draw-command
               "Draw moves require a command map."
               {:command command})

      (nil? (get-in state [:players-by-id player-id]))
      (failure :unknown-player
               "Draw moves require a participating player."
               {:player-id player-id})

      (not (current-player-id? state player-id))
      (failure :not-current-player
               "Only the current player can apply a draw move."
               {:player-id player-id
                :current-player-id (get-in state [:turn :current-player-id])})

      (not (sequential? discard-card-ids))
      (failure :invalid-discard-cards
               "Draw moves require :discard-card-ids to be a sequential collection."
               {:discard-card-ids discard-card-ids})

      (seq (duplicate-values discard-card-ids))
      (failure :duplicate-discard-cards
               "A card can only be discarded once by a draw move."
               {:duplicate-card-ids (duplicate-values discard-card-ids)})

      (not (int? draw-count))
      (failure :invalid-draw-count
               "Draw moves require an integer draw count."
               {:draw-count draw-count})

      (neg? draw-count)
      (failure :invalid-draw-count
               "Draw moves cannot draw a negative number of cards."
               {:draw-count draw-count})

      (not (ifn? shuffle-fn))
      (failure :invalid-shuffle-fn
               "Draw moves require a callable shuffle function."
               {:shuffle-fn shuffle-fn})

      :else
      (let [hand (get-in state [:players-by-id player-id :hand])
            hand-by-id (into {} (map (juxt :id identity)) hand)
            missing-card-ids (vec (remove #(contains? hand-by-id %) discard-card-ids))
            cards-to-discard (mapv hand-by-id discard-card-ids)
            post-discard-hand-count (- (count hand) (count discard-card-ids))
            hand-slots (max 0 (- starting-hand-size post-discard-hand-count))
            available-cards (+ (count (:draw-pile state))
                               (count (:discard-pile state))
                               (count cards-to-discard))
            maximum-draw-count (min hand-slots available-cards)]
        (cond
          (seq missing-card-ids)
          (failure :invalid-discard-cards
                   "Discarded cards must be in the current player's hand."
                   {:player-id player-id
                    :missing-card-ids missing-card-ids})

          (< maximum-draw-count draw-count)
          (failure :invalid-draw-count
                   "Draw moves cannot exceed the six-card hand limit or available deck cards."
                   {:draw-count draw-count
                    :maximum maximum-draw-count
                    :hand-size (count hand)
                    :discard-count (count cards-to-discard)
                    :available-cards available-cards})

          :else
          (let [discarded-state (-> state
                                    (remove-cards-from-hand player-id discard-card-ids)
                                    (discard-cards cards-to-discard))
                draw-result (draw-from-piles discarded-state draw-count shuffle-fn)]
            (if-not (:ok? draw-result)
              draw-result
              (let [drawn-cards (:drawn-cards draw-result)
                    event {:type :draw/cards-drawn
                           :player-id player-id
                           :discarded-card-ids (vec discard-card-ids)
                           :draw-count draw-count
                           :drawn-card-ids (mapv :id drawn-cards)
                           :reshuffled-discard? (true? (:reshuffled? draw-result))}
                    next-state (-> (:state draw-result)
                                   (append-cards-to-hand player-id drawn-cards)
                                   (append-history event))]
                (success next-state [event])))))))))

(defn create-game
  ([player-specs]
   (create-game player-specs {}))
  ([player-specs opts]
   (if-let [error (validate-player-specs player-specs)]
     error
     (let [minimum-card-count (required-starting-card-count (count player-specs))
           source-deck (deck-source opts)]
       (if-let [error (validate-deck source-deck minimum-card-count)]
         error
         (let [deck (ordered-deck opts)]
           (if-let [error (validate-deck deck minimum-card-count)]
             error
             (let [base-players (mapv normalize-player (range) player-specs)
                   {:keys [players board-deck]} (deal-starting-hands base-players deck)
                   board-cells (board/initial-board board-deck identity)
                   event {:type :game/created
                          :phase initial-phase
                          :player-ids (mapv :id players)
                          :starting-hand-size starting-hand-size
                          :board-card-count (count board-cells)}
                   state {:phase initial-phase
                          :players players
                          :players-by-id (into {} (map (juxt :id identity) players))
                          :turn (initial-turn players)
                          :board board-cells
                          :pieces {:on-board []
                                   :stashes (initial-stashes players)}
                          :draw-pile (vec (drop board/board-card-count board-deck))
                          :discard-pile []
                          :setup {:bids {}
                                  :bid-history []
                                  :starting-player-id nil
                                  :target-score default-target-score}
                          :history [event]}]
               (success state [event])))))))))

(defn advance-turn [state]
  (let [{:keys [order current-player-index round]} (:turn state)]
    (if (seq order)
      (let [next-index (mod (inc current-player-index) (count order))
            next-round (if (zero? next-index) (inc round) round)
            next-player-id (get order next-index)
            event {:type :turn/advanced
                   :player-id next-player-id
                   :round next-round}
            next-state (-> state
                           (assoc :turn {:order order
                                         :current-player-index next-index
                                         :current-player-id next-player-id
                                         :round next-round})
                           (append-history event))]
        (success next-state [event]))
      (failure :missing-turn-order
               "Cannot advance turn without a player order."
               {:turn (:turn state)}))))
