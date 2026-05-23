(ns gnostica.game-schema-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.board :as board]
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
    (is (= [{:code :hand-too-large
             :message "Player hands must contain no more than six cards."
             :data {:player-id player-id
                    :count 7
                    :maximum game-state/starting-hand-size}}]
           (:invariants explanation)))
    (is (re-find #"six cards" (pr-str explanation)))))

(deftest rejects-duplicate-cards-across-game-zones
  (let [state (game-for 2)
        duplicate-card (get-in state [:players 0 :hand 0])
        duplicated-state (assoc-in state [:draw-pile 0] duplicate-card)
        explanation (game-schema/explain-game duplicated-state)]
    (is (false? (game-schema/valid-game? duplicated-state)))
    (is (= [{:code :duplicate-card-ids
             :message "Cards must appear only once across hands, board, draw pile, and discard pile."
             :data {:card-ids [(:id duplicate-card)]}}]
           (:invariants explanation)))
    (is (re-find (re-pattern (:id duplicate-card))
                 (pr-str explanation)))))

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
        invalid-state (assoc-in state [:pieces :on-board] [missing-space-piece])
        explanation (game-schema/explain-game invalid-state)]
    (is (false? (game-schema/valid-game? invalid-state)))
    (is (= [{:code :piece-space-missing
             :message "Pieces with a space index must reference an existing board cell."
             :data {:piece-id :rose-missing-space
                    :space-index 99
                    :board-indexes (vec (range board/board-card-count))}}]
           (:invariants explanation)))))

(deftest assert-valid-game-throws-readable-ex-data
  (let [state (assoc-in (game-for 2) [:players 0 :hand] (vec (take 7 cards/deck)))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Game state failed schema validation"
                          (game-schema/assert-valid-game state)))))
