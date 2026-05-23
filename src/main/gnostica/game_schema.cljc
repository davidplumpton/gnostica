(ns gnostica.game-schema
  (:require [clojure.string :as str]
            [gnostica.board :as board]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as pieces]
            [malli.core :as m]
            [malli.error :as me]))

(defn- enum-schema [values]
  (into [:enum] values))

(defn- nonblank-string? [value]
  (and (string? value)
       (not (str/blank? value))))

(defn- css-color? [value]
  (and (string? value)
       (some? (re-matches #"#[0-9a-fA-F]{6}" value))))

(defn- three-color? [value]
  (and (integer? value)
       (<= 0 value 0xffffff)))

(defn- non-negative-int? [value]
  (and (int? value)
       (not (neg? value))))

(defn- positive-int? [value]
  (and (int? value)
       (pos? value)))

(defn- distinct-by? [f values]
  (let [ks (map f values)]
    (= (count ks) (count (set ks)))))

(defn- player-ids [state]
  (mapv :id (:players state)))

(defn- player-id-set [state]
  (set (player-ids state)))

(defn- expected-players-by-id [state]
  (into {} (map (juxt :id identity)) (:players state)))

(defn- card-zones [state]
  (concat
   (mapcat :hand (:players state))
   (map :card (:board state))
   (:draw-pile state)
   (:discard-pile state)))

(defn- card-ids [state]
  (keep :id (card-zones state)))

(defn- duplicate-values [values]
  (->> values
       frequencies
       (filter (fn [[_ occurrences]]
                 (> occurrences 1)))
       (map first)
       vec))

(defn- all-card-ids-unique? [state]
  (let [ids (card-ids state)]
    (= (count ids) (count (set ids)))))

(defn- board-indexes-unique? [cells]
  (and (sequential? cells)
       (distinct-by? :index cells)))

(defn- board-cell-positions-match? [cells]
  (and (sequential? cells)
       (every?
        (fn [{:keys [index row col orientation]}]
          (and (int? index)
               (<= 0 index (dec board/board-card-count))
               (int? row)
               (<= 0 row (dec board/board-size))
               (int? col)
               (<= 0 col (dec board/board-size))
               (let [position (board/position-for-index index)]
                 (and (= (:row position) row)
                      (= (:col position) col)
                      (= (board/orientation-for row col) orientation)))))
        cells)))

(defn- participating-player-count? [state]
  (<= game-state/min-players
      (count (:players state))
      game-state/max-players))

(defn- players-have-unique-ids? [state]
  (distinct-by? :id (:players state)))

(defn- players-by-id-matches? [state]
  (= (expected-players-by-id state)
     (:players-by-id state)))

(defn- turn-order-matches-players? [state]
  (= (player-ids state)
     (get-in state [:turn :order])))

(defn- current-player-matches-turn-index? [state]
  (let [{:keys [order current-player-index current-player-id]} (:turn state)]
    (and (vector? order)
         (int? current-player-index)
         (<= 0 current-player-index)
         (< current-player-index (count order))
         (= (get order current-player-index) current-player-id))))

(defn- pieces-owned-by-players? [state]
  (let [ids (player-id-set state)]
    (every? #(contains? ids (:player-id %))
            (get-in state [:pieces :on-board]))))

(defn- stashes-match-players? [state]
  (= (player-id-set state)
     (set (keys (get-in state [:pieces :stashes])))))

(defn- hand-limit-errors [state]
  (->> (:players state)
       (keep (fn [{:keys [id hand]}]
               (when (< game-state/starting-hand-size (count hand))
                 {:code :hand-too-large
                  :message "Player hands must contain no more than six cards."
                  :data {:player-id id
                         :count (count hand)
                         :maximum game-state/starting-hand-size}})))
       vec))

(defn- player-count-errors [state]
  (when-not (participating-player-count? state)
    [{:code :invalid-player-count
      :message "Game state must contain between two and six players."
      :data {:count (count (:players state))
             :minimum game-state/min-players
             :maximum game-state/max-players}}]))

(defn- duplicate-card-errors [state]
  (let [duplicates (duplicate-values (card-ids state))]
    (when (seq duplicates)
      [{:code :duplicate-card-ids
        :message "Cards must appear only once across hands, board, draw pile, and discard pile."
        :data {:card-ids duplicates}}])))

(defn- piece-owner-errors [state]
  (let [ids (player-id-set state)]
    (->> (get-in state [:pieces :on-board])
         (keep (fn [{:keys [id player-id]}]
                 (when-not (contains? ids player-id)
                   {:code :unknown-piece-player
                    :message "Pieces on the board must belong to a player in the game."
                    :data {:piece-id id
                           :player-id player-id
                           :player-ids (player-ids state)}})))
         vec)))

(defn- turn-errors [state]
  (cond-> []
    (not (turn-order-matches-players? state))
    (conj {:code :turn-order-mismatch
           :message "Turn order must match the game players."
           :data {:player-ids (player-ids state)
                  :turn-order (get-in state [:turn :order])}})

    (not (current-player-matches-turn-index? state))
    (conj {:code :current-player-mismatch
           :message "The current player id must match the current turn index."
           :data {:turn (:turn state)}})))

(defn invariant-errors [state]
  (vec
   (concat
    (player-count-errors state)
    (hand-limit-errors state)
    (duplicate-card-errors state)
    (piece-owner-errors state)
    (turn-errors state))))

(def PlayerId
  (enum-schema (map :id pieces/players)))

(def NonBlankString
  [:fn {:error/message "must be a nonblank string"} nonblank-string?])

(def CssColor
  [:fn {:error/message "must be a six-digit CSS hex color"} css-color?])

(def ThreeColor
  [:fn {:error/message "must be an integer between 0x000000 and 0xffffff"} three-color?])

(def NonNegativeInt
  [:fn {:error/message "must be a non-negative integer"} non-negative-int?])

(def PositiveInt
  [:fn {:error/message "must be a positive integer"} positive-int?])

(def CardReference
  [:map
   [:id NonBlankString]
   [:title NonBlankString]
   [:image NonBlankString]])

(def BoardIndex
  [:int {:min 0 :max (dec board/board-card-count)}])

(def BoardCoordinate
  [:int {:min 0 :max (dec board/board-size)}])

(def BoardCell
  [:map
   [:index BoardIndex]
   [:row BoardCoordinate]
   [:col BoardCoordinate]
   [:orientation [:enum :portrait :landscape]]
   [:face [:enum :up :down]]
   [:card CardReference]])

(def Board
  [:and
   [:vector {:min board/board-card-count
             :max board/board-card-count}
    BoardCell]
   [:fn {:error/message "board cell indexes must be unique"} board-indexes-unique?]
   [:fn {:error/message "board cells must match their index row, column, and orientation"} board-cell-positions-match?]])

(def Hand
  [:vector {:max game-state/starting-hand-size}
   CardReference])

(def CardPile
  [:vector CardReference])

(def Stash
  [:map
   [:small NonNegativeInt]
   [:medium NonNegativeInt]
   [:large NonNegativeInt]])

(def PlayerState
  [:map
   [:id PlayerId]
   [:name NonBlankString]
   [:color ThreeColor]
   [:css-color CssColor]
   [:order-index NonNegativeInt]
   [:hand Hand]
   [:score NonNegativeInt]
   [:challenge [:maybe :any]]
   [:eliminated? :boolean]
   [:stash Stash]
   [:bid [:maybe :any]]])

(def Piece
  [:map
   [:id :keyword]
   [:player-id PlayerId]
   [:space-index BoardIndex]
   [:size (enum-schema (keys pieces/piece-sizes))]
   [:orientation (enum-schema pieces/legal-orientations)]])

(def Turn
  [:map
   [:order [:vector {:min game-state/min-players
                     :max game-state/max-players}
            PlayerId]]
   [:current-player-index NonNegativeInt]
   [:current-player-id PlayerId]
   [:round PositiveInt]])

(def Pieces
  [:map
   [:on-board [:vector Piece]]
   [:stashes [:map-of PlayerId Stash]]])

(def Setup
  [:map
   [:bids [:map-of PlayerId :any]]
   [:bid-history [:vector :any]]
   [:starting-player-id [:maybe PlayerId]]
   [:target-score PositiveInt]])

(def HistoryEvent
  [:map
   [:type :keyword]])

(def GameState
  [:and
   [:map
    [:phase :keyword]
    [:players [:vector {:min game-state/min-players
                        :max game-state/max-players}
               PlayerState]]
    [:players-by-id [:map-of PlayerId PlayerState]]
    [:turn Turn]
    [:board Board]
    [:pieces Pieces]
    [:draw-pile CardPile]
    [:discard-pile CardPile]
    [:setup Setup]
    [:history [:vector HistoryEvent]]]
   [:fn {:error/message "game state must contain two to six players"} participating-player-count?]
   [:fn {:error/message "player ids must be unique"} players-have-unique-ids?]
   [:fn {:error/message "players-by-id must match players"} players-by-id-matches?]
   [:fn {:error/message "turn order must match players"} turn-order-matches-players?]
   [:fn {:error/message "current player must match the turn index"} current-player-matches-turn-index?]
   [:fn {:error/message "piece owners must be participating players"} pieces-owned-by-players?]
   [:fn {:error/message "piece stashes must match participating players"} stashes-match-players?]
   [:fn {:error/message "card ids must be unique across all game zones"} all-card-ids-unique?]])

(defn valid-game? [state]
  (m/validate GameState state))

(defn explain-game [state]
  (when-let [explanation (m/explain GameState state)]
    {:message "Game state failed schema validation."
     :errors (me/humanize explanation)
     :invariants (invariant-errors state)
     :explanation explanation}))

(defn assert-valid-game [state]
  (if-let [explanation (explain-game state)]
    (throw (ex-info (:message explanation) explanation))
    state))
