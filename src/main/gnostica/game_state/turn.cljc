(ns gnostica.game-state.turn
  (:require [gnostica.game-state.constants :as constants]
            [gnostica.game-state.hands :as hands]
            [gnostica.game-state.pieces :as piece-state]
            [gnostica.game-state.players :as players]
            [gnostica.game-state.result :as result]
            [gnostica.game-state.score :as score]))

(defn target-score [state]
  (get-in state [:setup :target-score] constants/default-target-score))

(defn finished? [state]
  (= constants/finished-phase (:phase state)))

(defn player-eliminated? [state player-id]
  (true? (get-in state [:players-by-id player-id :eliminated?])))

(defn- initial-placement-required-result [player-id action]
  (result/failure
   :initial-placement-required
   (case action
     :draw-cards
     "A player with no pieces must place their initial small piece instead of drawing cards."

     :end-turn
     "A player with no pieces must place their initial small piece before ending the turn."

     :announce-challenge
     "A player with no pieces must place their initial small piece before announcing a challenge."

     "A player with no pieces must place their initial small piece before taking another turn action.")
   {:player-id player-id
    :action action}))

(defn- initial-placement-required? [state player-id action]
  (and (not= :place-initial-small action)
       (empty? (piece-state/player-pieces state player-id))))

(defn turn-action-unavailable-result [state player-id action]
  (cond
    (nil? (get-in state [:players-by-id player-id]))
    (result/failure :unknown-player
                    "Turn actions require a participating player."
                    {:player-id player-id})

    (not (players/current-player-id? state player-id))
    (result/failure :not-current-player
                    "Only the current player can take a turn action."
                    {:player-id player-id
                     :current-player-id (get-in state [:turn :current-player-id])
                     :action action})

    (finished? state)
    (result/failure :game-finished
                    "The game is already finished."
                    {:winner (:winner state)
                     :action action})

    (player-eliminated? state player-id)
    (result/failure :player-eliminated
                    "Eliminated players cannot take turn actions."
                    {:player-id player-id
                     :action action})

    (initial-placement-required? state player-id action)
    (initial-placement-required-result player-id action)))

(defn- unresolved-challenge? [challenge]
  (= :announced (:status challenge)))

(defn active-challenge-player-id [state]
  (some (fn [{:keys [id challenge eliminated?]}]
          (when (and (not eliminated?)
                     (unresolved-challenge? challenge))
            id))
        (:players state)))

(defn can-announce-challenge? [state player-id]
  (boolean
   (and (some? (get-in state [:players-by-id player-id]))
        (players/current-player-id? state player-id)
        (not (finished? state))
        (not (player-eliminated? state player-id))
        (seq (piece-state/player-pieces state player-id))
        (nil? (active-challenge-player-id state)))))

(defn challenge-unavailable-reason [state player-id]
  (let [active-challenger-id (active-challenge-player-id state)]
    (cond
      (nil? (get-in state [:players-by-id player-id]))
      "Unknown player."

      (not (players/current-player-id? state player-id))
      "Only the current player can announce a challenge."

      (finished? state)
      "The game is already finished."

      (player-eliminated? state player-id)
      "Eliminated players cannot announce a challenge."

      (initial-placement-required? state player-id :announce-challenge)
      "A player with no pieces must place their initial small piece before announcing a challenge."

      (= active-challenger-id player-id)
      "This player already has an unresolved challenge."

      active-challenger-id
      "Another player has an unresolved challenge.")))

(defn can-end-turn? [state player-id]
  (nil? (turn-action-unavailable-result state player-id :end-turn)))

(declare end-turn)

(defn- record-challenge [state {:keys [player-id]}]
  (let [state (score/with-current-scores state)]
    (if-let [reason (challenge-unavailable-reason state player-id)]
      (result/failure :challenge-unavailable
                      reason
                      {:player-id player-id
                       :active-challenge-player-id (active-challenge-player-id state)})
      (let [challenge {:status :announced
                       :target-score (target-score state)
                       :score-at-announcement (get-in state [:players-by-id player-id :score])
                       :announced-round (get-in state [:turn :round])
                       :announced-turn-index (get-in state [:turn :current-player-index])}
            event {:type :challenge/announced
                   :player-id player-id
                   :score (:score-at-announcement challenge)
                   :target-score (:target-score challenge)
                   :round (:announced-round challenge)}
            next-state (-> state
                           (players/update-player player-id assoc :challenge challenge)
                           (players/append-history event))]
        (result/success next-state [event])))))

(defn announce-challenge [state command]
  (end-turn state (assoc command :announce-challenge? true)))

(defn- active-players [state]
  (filterv (comp not :eliminated?) (:players state)))

(defn- single-active-player-id [state]
  (let [active (active-players state)]
    (when (= 1 (count active))
      (:id (first active)))))

(defn- current-player-index-for [state player-id]
  (let [order (get-in state [:turn :order])]
    (first
     (keep-indexed (fn [index id]
                     (when (= player-id id)
                       index))
                   order))))

(defn- set-current-player [state player-id]
  (if-let [index (current-player-index-for state player-id)]
    (assoc-in (assoc-in state [:turn :current-player-index] index)
              [:turn :current-player-id]
              player-id)
    state))

(defn- finish-game [state winner-id reason score target-score]
  (let [event {:type :game/won
               :player-id winner-id
               :reason reason
               :score score
               :target-score target-score}
        next-state (-> state
                       (set-current-player winner-id)
                       (assoc :phase constants/finished-phase
                              :winner {:player-id winner-id
                                       :reason reason
                                       :score score
                                       :target-score target-score})
                       (players/append-history event))]
    {:state next-state
     :event event}))

(defn return-pieces-to-stash [state pieces]
  (reduce (fn [next-state {:keys [id player-id size]}]
            (-> next-state
                (piece-state/increment-stash player-id size)
                (piece-state/remove-piece-by-id id)))
          state
          pieces))

(defn return-void-pieces-to-stash [state]
  (return-pieces-to-stash state (piece-state/void-pieces state)))

(defn- remove-player-pieces [state player-id]
  (update-in state [:pieces :on-board]
             (fn [board-pieces]
               (vec (remove #(= player-id (:player-id %)) board-pieces)))))

(defn- eliminate-player [state player-id score target-score]
  (let [removed-pieces (piece-state/player-pieces state player-id)
        discarded-hand (vec (get-in state [:players-by-id player-id :hand]))
        event {:type :challenge/failed
               :player-id player-id
               :score score
               :target-score target-score
               :discarded-card-ids (mapv :id discarded-hand)
               :removed-piece-ids (mapv :id removed-pieces)}
        next-state (-> state
                       (return-pieces-to-stash removed-pieces)
                       (remove-player-pieces player-id)
                       (players/update-player player-id assoc
                                              :hand []
                                              :score 0
                                              :challenge nil
                                              :eliminated? true)
                       (hands/discard-cards discarded-hand)
                       (score/with-current-scores)
                       (players/append-history event))]
    {:state next-state
     :event event}))

(defn- next-active-index [state order current-player-index]
  (let [player-active? (fn [player-id]
                         (not (player-eliminated? state player-id)))
        player-count (count order)]
    (some (fn [offset]
            (let [index (mod (+ current-player-index offset) player-count)]
              (when (player-active? (get order index))
                index)))
          (range 1 (inc player-count)))))

(defn advance-turn [state]
  (let [state (score/with-current-scores state)
        {:keys [order current-player-index round]} (:turn state)]
    (cond
      (finished? state)
      (result/failure :game-finished
                      "Cannot advance turn after the game is finished."
                      {:winner (:winner state)})

      (not (seq order))
      (result/failure :missing-turn-order
                      "Cannot advance turn without a player order."
                      {:turn (:turn state)})

      :else
      (if-let [next-index (next-active-index state order current-player-index)]
        (let [next-round (if (<= next-index current-player-index)
                           (inc round)
                           round)
              next-player-id (get order next-index)
              event {:type :turn/advanced
                     :player-id next-player-id
                     :round next-round}
              next-state (-> state
                             (assoc :turn {:order order
                                           :current-player-index next-index
                                           :current-player-id next-player-id
                                           :round next-round})
                             (players/append-history event))]
          (result/success next-state [event]))
        (result/failure :no-active-players
                        "Cannot advance turn without an active player."
                        {:turn (:turn state)})))))

(defn- resolve-challenge [state player-id]
  (let [state (score/with-current-scores state)
        score (get-in state [:players-by-id player-id :score] 0)
        target-score (target-score state)]
    (if (<= target-score score)
      (let [challenge-event {:type :challenge/won
                             :player-id player-id
                             :score score
                             :target-score target-score}
            winner-result (finish-game
                           (-> state
                               (players/update-player player-id assoc-in
                                                      [:challenge :status]
                                                      :won)
                               (players/append-history challenge-event))
                           player-id
                           :challenge
                           score
                           target-score)]
        (result/success (:state winner-result)
                        [challenge-event (:event winner-result)]))
      (let [{eliminated-state :state
             eliminated-event :event} (eliminate-player state
                                                        player-id
                                                        score
                                                        target-score)]
        (if-let [winner-id (single-active-player-id eliminated-state)]
          (let [winner-score (get-in eliminated-state [:players-by-id winner-id :score] 0)
                winner-result (finish-game eliminated-state
                                           winner-id
                                           :last-active-player
                                           winner-score
                                           target-score)]
            (result/success (:state winner-result)
                            [eliminated-event (:event winner-result)]))
          (let [{:keys [ok? state events error]} (advance-turn eliminated-state)]
            (if ok?
              (result/success state (into [eliminated-event] events))
              (result/failure (:code error)
                              (:message error)
                              (:data error)))))))))

(defn end-turn [state {:keys [player-id announce-challenge? challenge?]}]
  (let [announce-challenge? (or announce-challenge? challenge?)
        state (score/with-current-scores state)
        player (get-in state [:players-by-id player-id])]
    (cond
      (nil? player)
      (result/failure :unknown-player
                      "Cannot end a turn for an unknown player."
                      {:player-id player-id})

      (not (players/current-player-id? state player-id))
      (result/failure :not-current-player
                      "Only the current player can end their turn."
                      {:player-id player-id
                       :current-player-id (get-in state [:turn :current-player-id])})

      (finished? state)
      (result/failure :game-finished
                      "The game is already finished."
                      {:winner (:winner state)})

      (player-eliminated? state player-id)
      (result/failure :player-eliminated
                      "Eliminated players cannot take turns."
                      {:player-id player-id})

      (initial-placement-required? state player-id :end-turn)
      (initial-placement-required-result player-id :end-turn)

      (unresolved-challenge? (:challenge player))
      (resolve-challenge state player-id)

      announce-challenge?
      (let [{:keys [ok? state events error]} (record-challenge state
                                                               {:player-id player-id})]
        (if ok?
          (let [{advanced-ok? :ok?
                 advanced-state :state
                 advanced-events :events
                 advanced-error :error} (advance-turn state)]
            (if advanced-ok?
              (result/success advanced-state (into events advanced-events))
              (result/failure (:code advanced-error)
                              (:message advanced-error)
                              (:data advanced-error))))
          (result/failure (:code error)
                          (:message error)
                          (:data error))))

      :else
      (advance-turn state))))
