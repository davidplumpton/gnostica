(ns gnostica.game-schema-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.board :as board]
            [gnostica.board-layout :as board-layout]
            [gnostica.cards :as cards]
            [gnostica.game-schema :as game-schema]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as pieces]))

(defn- game-for [player-count]
  (let [player-specs (mapv #(select-keys % [:id])
                           (take player-count pieces/players))
        {:keys [state]} (game-state/create-game player-specs {:shuffle-fn identity})]
    state))

(defn- replace-player [state player-id f]
  (let [players (mapv (fn [player]
                        (if (= player-id (:id player))
                          (f player)
                          player))
                      (:players state))]
    (assoc state
           :players players
           :players-by-id (into {} (map (juxt :id identity)) players))))

(defn- first-player-id [state]
  (get-in state [:players 0 :id]))

(defn- synthetic-deck [card-count]
  (mapv (fn [index]
          {:id (str "custom-card-" index)
           :title (str "Custom Card " index)
           :image (str "/images/custom-card-" index ".png")})
        (range card-count)))

(defn- invariant-with-code [explanation code]
  (some #(when (= code (:code %)) %)
        (:invariants explanation)))

(deftest validates-generated-setup-state-for-two-through-six-players
  (doseq [player-count (range game-state/min-players (inc game-state/max-players))
          :let [state (game-for player-count)]]
    (is (game-schema/valid-game? state))
    (is (= state (game-schema/assert-valid-game state)))
    (is (nil? (game-schema/explain-game state)))))

(deftest rejects-too-few-and-too-many-players
  (let [base-state (game-for 2)
        too-few (assoc base-state
                       :players [(first (:players base-state))]
                       :players-by-id {(first-player-id base-state)
                                       (first (:players base-state))}
                       :turn {:order [(first-player-id base-state)]
                              :current-player-index 0
                              :current-player-id (first-player-id base-state)
                              :round 1}
                       :pieces {:on-board []
                                :stashes {(first-player-id base-state)
                                          (get-in base-state [:pieces :stashes (first-player-id base-state)])}})
        extra-player (assoc (first (:players base-state))
                            :id :extra
                            :name "Extra"
                            :order-index 6
                            :hand [])
        too-many (update (game-for 6) :players conj extra-player)
        too-many (assoc too-many
                        :players-by-id (into {} (map (juxt :id identity)) (:players too-many))
                        :turn (assoc (:turn too-many)
                                     :order (mapv :id (:players too-many))))]
    (is (false? (game-schema/valid-game? too-few)))
    (is (= :invalid-player-count
           (-> too-few game-schema/explain-game :invariants first :code)))
    (is (false? (game-schema/valid-game? too-many)))
    (is (= :invalid-player-count
           (-> too-many game-schema/explain-game :invariants first :code)))))

(deftest rejects-hands-over-six-cards
  (let [state (game-for 2)
        player-id (first-player-id state)
        overflow-card (assoc (first (:draw-pile state))
                             :id "synthetic-overflow-card")
        overfull-hand (replace-player state player-id
                                      #(update % :hand conj overflow-card))
        explanation (game-schema/explain-game overfull-hand)]
    (is (false? (game-schema/valid-game? overfull-hand)))
    (is (= {:code :hand-too-large
            :message "Player hands must contain no more than six cards."
            :data {:player-id player-id
                   :count 7
                   :maximum game-state/starting-hand-size}}
           (invariant-with-code explanation :hand-too-large)))
    (is (re-find #"six cards" (pr-str explanation)))))

(deftest rejects-duplicate-cards-across-game-zones
  (let [state (game-for 2)
        duplicate-card (get-in state [:players 0 :hand 0])
        replaced-card (get-in state [:draw-pile 0])
        duplicated-state (assoc-in state [:draw-pile 0] duplicate-card)
        explanation (game-schema/explain-game duplicated-state)]
    (is (false? (game-schema/valid-game? duplicated-state)))
    (is (= [{:code :duplicate-card-ids
             :message "Cards must appear only once across hands, board, draw pile, and discard pile."
             :data {:card-ids [(:id duplicate-card)]}}
            {:code :missing-card-ids
             :message "Every expected deck card must appear in a hand, on the board, in the draw pile, or in the discard pile."
             :data {:card-ids [(:id replaced-card)]}}]
           (:invariants explanation)))
    (is (re-find (re-pattern (:id duplicate-card))
                 (pr-str explanation)))))

(deftest rejects-missing-cards-from-official-deck-accounting
  (let [state (game-for 2)
        missing-card (first (:draw-pile state))
        invalid-state (update state :draw-pile subvec 1)
        explanation (game-schema/explain-game invalid-state)]
    (is (false? (game-schema/valid-game? invalid-state)))
    (is (= {:code :card-count-mismatch
            :message "Hands, board, draw pile, and discard pile must contain exactly the expected deck card count."
            :data {:expected-count (count cards/deck)
                   :actual-count (dec (count cards/deck))}}
           (invariant-with-code explanation :card-count-mismatch)))
    (is (= {:code :missing-card-ids
            :message "Every expected deck card must appear in a hand, on the board, in the draw pile, or in the discard pile."
            :data {:card-ids [(:id missing-card)]}}
           (invariant-with-code explanation :missing-card-ids)))))

(deftest rejects-unknown-cards-in-official-deck-accounting
  (let [state (game-for 2)
        replaced-card (get-in state [:draw-pile 0])
        unknown-card-id "synthetic-card"
        invalid-state (assoc-in state [:draw-pile 0 :id] unknown-card-id)
        explanation (game-schema/explain-game invalid-state)]
    (is (false? (game-schema/valid-game? invalid-state)))
    (is (= {:code :missing-card-ids
            :message "Every expected deck card must appear in a hand, on the board, in the draw pile, or in the discard pile."
            :data {:card-ids [(:id replaced-card)]}}
           (invariant-with-code explanation :missing-card-ids)))
    (is (= {:code :unknown-card-ids
            :message "Game zones must not contain cards outside the expected deck."
            :data {:card-ids [unknown-card-id]}}
           (invariant-with-code explanation :unknown-card-ids)))))

(deftest rejects-extra-cards-from-official-deck-accounting
  (let [state (game-for 2)
        extra-card {:id "synthetic-extra-card"
                    :title "Synthetic Extra Card"
                    :image "/images/synthetic-extra-card.png"}
        invalid-state (update state :discard-pile conj extra-card)
        explanation (game-schema/explain-game invalid-state)]
    (is (false? (game-schema/valid-game? invalid-state)))
    (is (= {:code :card-count-mismatch
            :message "Hands, board, draw pile, and discard pile must contain exactly the expected deck card count."
            :data {:expected-count (count cards/deck)
                   :actual-count (inc (count cards/deck))}}
           (invariant-with-code explanation :card-count-mismatch)))
    (is (= {:code :unknown-card-ids
            :message "Game zones must not contain cards outside the expected deck."
            :data {:card-ids [(:id extra-card)]}}
           (invariant-with-code explanation :unknown-card-ids)))))

(deftest injected-decks-carry-their-own-accounting-contract
  (let [custom-deck (synthetic-deck 24)
        player-specs (mapv #(select-keys % [:id])
                           (take 2 pieces/players))
        {:keys [state]} (game-state/create-game player-specs
                                                {:deck custom-deck
                                                 :shuffle-fn identity})
        replaced-card (get-in state [:draw-pile 0])
        unknown-card-id "custom-card-outside-contract"
        invalid-state (assoc-in state [:draw-pile 0 :id] unknown-card-id)
        explanation (game-schema/explain-game invalid-state)]
    (is (= (mapv :id custom-deck)
           (get-in state [:setup :deck-card-ids])))
    (is (game-schema/valid-game? state))
    (is (false? (game-schema/valid-game? invalid-state)))
    (is (= {:code :missing-card-ids
            :message "Every expected deck card must appear in a hand, on the board, in the draw pile, or in the discard pile."
            :data {:card-ids [(:id replaced-card)]}}
           (invariant-with-code explanation :missing-card-ids)))
    (is (= {:code :unknown-card-ids
            :message "Game zones must not contain cards outside the expected deck."
            :data {:card-ids [unknown-card-id]}}
           (invariant-with-code explanation :unknown-card-ids)))))

(deftest rejects-duplicate-active-piece-ids
  (let [state (game-for 2)
        duplicate-id :duplicate-scout
        same-player-state (game-state/with-board-pieces
                           state
                           [{:id duplicate-id
                             :player-id :rose
                             :space-index 0
                             :size :small
                             :orientation :up}
                            {:id duplicate-id
                             :player-id :rose
                             :space-index 1
                             :size :small
                             :orientation :north}])
        cross-player-state (game-state/with-board-pieces
                            state
                            [{:id duplicate-id
                              :player-id :rose
                              :space-index 0
                              :size :small
                              :orientation :up}
                             {:id duplicate-id
                              :player-id :indigo
                              :space-index 1
                              :size :small
                              :orientation :north}])]
    (doseq [invalid-state [same-player-state cross-player-state]
            :let [explanation (game-schema/explain-game invalid-state)]]
      (is (false? (game-schema/valid-game? invalid-state)))
      (is (= [{:code :duplicate-active-piece-ids
               :message "Active pieces must have unique ids."
               :data {:piece-ids [duplicate-id]}}]
             (:invariants explanation)))
      (is (re-find #":duplicate-scout" (pr-str explanation))))))

(deftest rejects-pieces-owned-by-unknown-players
  (let [state (game-for 2)
        unknown-piece {:id :obsidian-scout
                       :player-id :obsidian
                       :space-index 0
                       :size :small
                       :orientation :north}
        invalid-state (assoc-in state [:pieces :on-board] [unknown-piece])
        explanation (game-schema/explain-game invalid-state)]
    (is (false? (game-schema/valid-game? invalid-state)))
    (is (= [{:code :unknown-piece-player
             :message "Pieces on the board must belong to a player in the game."
             :data {:piece-id :obsidian-scout
                    :player-id :obsidian
                    :player-ids [:rose :indigo]}}]
           (:invariants explanation)))
    (is (re-find #":obsidian" (pr-str explanation)))))

(deftest rejects-piece-space-indexes-without-board-cells
  (let [state (game-for 2)
        missing-space-piece {:id :rose-missing-space
                             :player-id :rose
                             :space-index 99
                             :size :small
                             :orientation :up}
        invalid-state (game-state/with-board-pieces state [missing-space-piece])
        explanation (game-schema/explain-game invalid-state)]
    (is (false? (game-schema/valid-game? invalid-state)))
    (is (= [{:code :piece-space-missing
             :message "Pieces with a space index must reference an existing board cell."
             :data {:piece-id :rose-missing-space
                    :space-index 99
                    :board-indexes (vec (range board/board-card-count))}}]
           (:invariants explanation)))))

(deftest rejects-pieces-with-ambiguous-or-missing-locations
  (let [state (game-for 2)
        ambiguous-piece {:id :rose-ambiguous-location
                         :player-id :rose
                         :space-index 0
                         :space {:kind :wasteland
                                 :row 0
                                 :col 3}
                         :size :small
                         :orientation :up}
        missing-location-piece {:id :rose-missing-location
                                :player-id :rose
                                :size :small
                                :orientation :up}
        ambiguous-state (game-state/with-board-pieces state [ambiguous-piece])
        missing-state (game-state/with-board-pieces state [missing-location-piece])]
    (is (false? (game-schema/valid-game? ambiguous-state)))
    (is (= [{:code :ambiguous-piece-location
             :message "Pieces must use either :space-index or :space, not both."
             :data {:piece-id :rose-ambiguous-location
                    :space-index 0
                    :space {:kind :wasteland
                            :row 0
                            :col 3}}}]
           (:invariants (game-schema/explain-game ambiguous-state))))
    (is (false? (game-schema/valid-game? missing-state)))
    (is (= [{:code :missing-piece-location
             :message "Pieces must include exactly one location field: :space-index or :space."
             :data {:piece-id :rose-missing-location}}]
           (:invariants (game-schema/explain-game missing-state))))))

(deftest rejects-wasteland-pieces-outside-current-wasteland-spaces
  (let [state (game-for 2)
        invalid-piece {:id :rose-lost-wasteland
                       :player-id :rose
                       :space {:kind :wasteland
                               :row 99
                               :col 99}
                       :size :small
                       :orientation :up}
        invalid-state (game-state/with-board-pieces state [invalid-piece])
        expected-wastelands (->> (board-layout/wasteland-spaces (:board state))
                                 (map #(select-keys % [:kind :row :col]))
                                 (sort-by (juxt :row :col))
                                 vec)
        explanation (game-schema/explain-game invalid-state)]
    (is (false? (game-schema/valid-game? invalid-state)))
    (is (= [{:code :piece-wasteland-missing
             :message "Pieces in wasteland space must reference a current wasteland coordinate."
             :data {:piece-id :rose-lost-wasteland
                    :space {:kind :wasteland
                            :row 99
                            :col 99}
                    :wasteland-spaces expected-wastelands}}]
           (:invariants explanation)))))

(deftest rejects-unofficial-setup-target-scores
  (let [state (assoc-in (game-for 2) [:setup :target-score] 99)
        explanation (game-schema/explain-game state)]
    (is (false? (game-schema/valid-game? state)))
    (is (= [{:code :invalid-target-score
             :message "Target score must be one of the official Gnostica target scores."
             :data {:path [:setup :target-score]
                    :target-score 99
                    :allowed-target-scores [8 9 10]}}]
           (:invariants explanation)))))

(deftest rejects-stashes-that-do-not-account-for-active-pieces
  (let [state (game-for 2)
        piece {:id :rose-extra-small
               :player-id :rose
               :space-index 0
               :size :small
               :orientation :north}
        invalid-state (assoc-in state [:pieces :on-board] [piece])
        explanation (game-schema/explain-game invalid-state)]
    (is (false? (game-schema/valid-game? invalid-state)))
    (is (some #(= {:code :piece-count-mismatch
                   :message "A player's stash plus active pieces must equal the starting stash size."
                   :data {:player-id :rose
                          :size :small
                          :stash-count 5
                          :active-piece-count 1
                          :expected-total game-state/pieces-per-size-in-stash
                          :actual-total 6}}
                 %)
              (:invariants explanation)))))

(deftest rejects-diverged-player-and-piece-stash-mirrors
  (let [state (game-for 2)
        invalid-state (assoc-in state [:pieces :stashes :rose :small] 4)
        explanation (game-schema/explain-game invalid-state)]
    (is (false? (game-schema/valid-game? invalid-state)))
    (is (some #(= {:code :stash-mirror-mismatch
                   :message "Player stashes must match the pieces stash mirror."
                   :data {:player-id :rose
                          :player-stash {:small 5
                                         :medium 5
                                         :large 5}
                          :piece-stash {:small 4
                                        :medium 5
                                        :large 5}}}
                 %)
              (:invariants explanation)))))

(deftest assert-valid-game-throws-readable-ex-data
  (let [state (assoc-in (game-for 2) [:players 0 :hand] (vec (take 7 cards/deck)))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Game state failed schema validation"
                          (game-schema/assert-valid-game state)))))
