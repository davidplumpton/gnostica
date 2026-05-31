(ns gnostica.game-schema
  (:require [clojure.string :as str]
            [gnostica.board :as board]
            [gnostica.board-layout :as board-layout]
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

(defn- active-piece-ids [state]
  (keep :id (get-in state [:pieces :on-board])))

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

(defn- active-piece-ids-unique? [state]
  (let [ids (active-piece-ids state)]
    (= (count ids) (count (set ids)))))

(defn- board-indexes-unique? [cells]
  (and (sequential? cells)
       (distinct-by? :index cells)))

(defn- board-cell-coordinates-unique? [cells]
  (and (sequential? cells)
       (distinct-by? (juxt :row :col) cells)))

(defn- board-cell-orientations-match? [cells]
  (and (sequential? cells)
       (every?
        (fn [{:keys [row col orientation]}]
          (and (int? row)
               (int? col)
               (= (board/orientation-for row col) orientation)))
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

(defn- player-stashes-match-piece-stashes? [state]
  (= (into {} (map (juxt :id :stash)) (:players state))
     (get-in state [:pieces :stashes])))

(defn- piece-counts-by-player-size [state]
  (reduce (fn [counts {:keys [player-id size]}]
            (if (and player-id size)
              (update-in counts [player-id size] (fnil inc 0))
              counts))
          {}
          (get-in state [:pieces :on-board])))

(defn- stash-mirror-errors [state]
  (->> (:players state)
       (keep (fn [{:keys [id stash]}]
               (let [piece-stash (get-in state [:pieces :stashes id])]
                 (when (not= stash piece-stash)
                   {:code :stash-mirror-mismatch
                    :message "Player stashes must match the pieces stash mirror."
                    :data {:player-id id
                           :player-stash stash
                           :piece-stash piece-stash}}))))
       vec))

(defn- stash-piece-count-errors [state]
  (let [piece-counts (piece-counts-by-player-size state)]
    (vec
     (for [{:keys [id stash]} (:players state)
           size (keys pieces/piece-sizes)
           :let [stash-count (get stash size)
                 board-piece-count (get-in piece-counts [id size] 0)
                 actual-total (+ (or stash-count 0) board-piece-count)]
           :when (not= game-state/pieces-per-size-in-stash actual-total)]
       {:code :piece-count-mismatch
        :message "A player's stash plus active pieces must equal the starting stash size."
        :data {:player-id id
               :size size
               :stash-count stash-count
               :active-piece-count board-piece-count
               :expected-total game-state/pieces-per-size-in-stash
               :actual-total actual-total}}))))

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

(defn- duplicate-active-piece-errors [state]
  (let [duplicates (duplicate-values (active-piece-ids state))]
    (when (seq duplicates)
      [{:code :duplicate-active-piece-ids
        :message "Active pieces must have unique ids."
        :data {:piece-ids duplicates}}])))

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

(defn- piece-space-errors [state]
  (let [board-indexes (set (map :index (:board state)))]
    (->> (get-in state [:pieces :on-board])
         (keep (fn [{:keys [id space-index] :as piece}]
                 (when (and (contains? piece :space-index)
                            (not (contains? board-indexes space-index)))
                   {:code :piece-space-missing
                    :message "Pieces with a space index must reference an existing board cell."
                    :data {:piece-id id
                           :space-index space-index
                           :board-indexes (vec (sort board-indexes))}})))
         vec)))

(defn- exactly-one-piece-location? [piece]
  (not= (contains? piece :space-index)
        (contains? piece :space)))

(defn- piece-location-errors [state]
  (->> (get-in state [:pieces :on-board])
       (keep (fn [{:keys [id] :as piece}]
               (let [has-space-index? (contains? piece :space-index)
                     has-space? (contains? piece :space)]
                 (cond
                   (and has-space-index? has-space?)
                   {:code :ambiguous-piece-location
                    :message "Pieces must use either :space-index or :space, not both."
                    :data {:piece-id id
                           :space-index (:space-index piece)
                           :space (:space piece)}}

                   (not (or has-space-index? has-space?))
                   {:code :missing-piece-location
                    :message "Pieces must include exactly one location field: :space-index or :space."
                    :data {:piece-id id}}))))
       vec))

(defn- wasteland-space-key [space]
  (select-keys space [:kind :row :col]))

(defn- wasteland-spaces-by-key [state]
  (->> (board-layout/wasteland-spaces (:board state))
       (map wasteland-space-key)
       set))

(defn- sorted-wasteland-spaces [state]
  (->> (board-layout/wasteland-spaces (:board state))
       (map wasteland-space-key)
       (sort-by (juxt :row :col))
       vec))

(defn- piece-wasteland-errors [state]
  (let [valid-spaces (wasteland-spaces-by-key state)
        sorted-spaces (delay (sorted-wasteland-spaces state))]
    (->> (get-in state [:pieces :on-board])
         (keep (fn [{:keys [id space] :as piece}]
                 (when (and (contains? piece :space)
                            (= :wasteland (:kind space))
                            (not (contains? valid-spaces (wasteland-space-key space))))
                   {:code :piece-wasteland-missing
                    :message "Pieces in wasteland space must reference a current wasteland coordinate."
                    :data {:piece-id id
                           :space (wasteland-space-key space)
                           :wasteland-spaces @sorted-spaces}})))
         vec)))

(defn- target-score-errors [state]
  (let [target-score (get-in state [:setup :target-score])]
    (when-not (contains? game-state/allowed-target-scores target-score)
      [{:code :invalid-target-score
        :message "Target score must be one of the official Gnostica target scores."
        :data {:path [:setup :target-score]
               :target-score target-score
               :allowed-target-scores (vec (sort game-state/allowed-target-scores))}}])))

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
    (duplicate-active-piece-errors state)
    (piece-owner-errors state)
    (piece-location-errors state)
    (piece-space-errors state)
    (piece-wasteland-errors state)
    (target-score-errors state)
    (stash-mirror-errors state)
    (stash-piece-count-errors state)
    (turn-errors state))))

(defn- piece-space-indexes-match-board? [state]
  (empty? (piece-space-errors state)))

(defn- pieces-have-exactly-one-location? [state]
  (empty? (piece-location-errors state)))

(defn- piece-wasteland-spaces-current? [state]
  (empty? (piece-wasteland-errors state)))

(defn- setup-target-score-allowed? [state]
  (empty? (target-score-errors state)))

(defn- piece-stashes-consistent? [state]
  (and (player-stashes-match-piece-stashes? state)
       (empty? (stash-piece-count-errors state))))

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

(def TargetScore
  (enum-schema (sort game-state/allowed-target-scores)))

(def CardReference
  [:map
   [:id NonBlankString]
   [:title NonBlankString]
   [:image NonBlankString]])

(def BoardIndex
  NonNegativeInt)

(def BoardCoordinate
  :int)

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
   [:vector {:max 78}
    BoardCell]
   [:fn {:error/message "board cell indexes must be unique"} board-indexes-unique?]
   [:fn {:error/message "board cell coordinates must be unique"} board-cell-coordinates-unique?]
   [:fn {:error/message "board cell orientations must match their coordinates"} board-cell-orientations-match?]])

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

(def BoardPiece
  [:map
   [:id :keyword]
   [:player-id PlayerId]
   [:space-index BoardIndex]
   [:size (enum-schema (keys pieces/piece-sizes))]
   [:orientation (enum-schema pieces/legal-orientations)]])

(def WastelandSpace
  [:map
   [:kind [:enum :wasteland]]
   [:row BoardCoordinate]
   [:col BoardCoordinate]])

(def WastelandPiece
  [:map
   [:id :keyword]
   [:player-id PlayerId]
   [:space WastelandSpace]
   [:size (enum-schema (keys pieces/piece-sizes))]
   [:orientation (enum-schema pieces/legal-orientations)]])

(def Piece
  [:and
   [:or BoardPiece WastelandPiece]
   [:fn {:error/message "piece must include exactly one location field"} exactly-one-piece-location?]])

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
   [:bid-redraw-order {:optional true} [:vector PlayerId]]
   [:bid-redraws {:optional true} [:vector :any]]
   [:starting-player-id [:maybe PlayerId]]
   [:target-score TargetScore]])

(def Winner
  [:map
   [:player-id PlayerId]
   [:reason [:enum :challenge :last-active-player]]
   [:score NonNegativeInt]
   [:target-score TargetScore]])

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
    [:winner [:maybe Winner]]
    [:history [:vector HistoryEvent]]]
   [:fn {:error/message "game state must contain two to six players"} participating-player-count?]
   [:fn {:error/message "player ids must be unique"} players-have-unique-ids?]
   [:fn {:error/message "players-by-id must match players"} players-by-id-matches?]
   [:fn {:error/message "turn order must match players"} turn-order-matches-players?]
   [:fn {:error/message "current player must match the turn index"} current-player-matches-turn-index?]
   [:fn {:error/message "piece owners must be participating players"} pieces-owned-by-players?]
   [:fn {:error/message "active piece ids must be unique"} active-piece-ids-unique?]
   [:fn {:error/message "pieces must include exactly one location field"} pieces-have-exactly-one-location?]
   [:fn {:error/message "piece space indexes must reference board cells"} piece-space-indexes-match-board?]
   [:fn {:error/message "wasteland pieces must reference current wasteland coordinates"} piece-wasteland-spaces-current?]
   [:fn {:error/message "target score must be one of the official scores"} setup-target-score-allowed?]
   [:fn {:error/message "piece stashes must match participating players"} stashes-match-players?]
   [:fn {:error/message "player stashes must match piece stash mirrors and active piece counts"} piece-stashes-consistent?]
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
