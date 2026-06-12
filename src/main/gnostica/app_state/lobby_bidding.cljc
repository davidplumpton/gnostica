(ns gnostica.app-state.lobby-bidding
  (:require [clojure.string :as str]
            [gnostica.app-state.lobby-setup :as setup]
            [gnostica.game-state :as game-state]))

(def starting-bid-choice-error-codes
  #{:invalid-starting-bid-card
    :incomplete-starting-bid-round
    :starting-bid-unresolved})

(def starting-bid-redraw-error-codes
  #{:inactive-starting-bid-redraw-player
    :invalid-bid-redraw-card
    :starting-bid-redraw-incomplete})

(defn initial-starting-bid [state]
  {:stage :choosing
   :initial-game state
   :current-game state
   :rounds []
   :current-bids {}
   :bid-history []
   :bid-cards []})

(defn starting-bid-player-ids [starting-bid]
  (mapv :id (get-in starting-bid [:initial-game :players])))

(defn starting-bid-card [state player-id card-id]
  (some #(when (= card-id (:id %)) %)
        (get-in state [:players-by-id player-id :hand])))

(defn starting-bid-redraw-needed-count [state player-id]
  (- game-state/starting-hand-size
     (count (get-in state [:players-by-id player-id :hand]))))

(defn starting-bid-redraw-card-ids [starting-bid]
  (vec (mapcat #(get-in starting-bid [:redraws %] [])
               (:redraw-order starting-bid))))

(defn starting-bid-remaining-redraw-cards [starting-bid]
  (let [selected-card-ids (set (starting-bid-redraw-card-ids starting-bid))]
    (vec (remove #(contains? selected-card-ids (:id %))
                 (:bid-cards starting-bid)))))

(defn starting-bid-active-redraw-player-id [starting-bid]
  (let [state (:current-game starting-bid)]
    (some (fn [player-id]
            (let [needed-count (starting-bid-redraw-needed-count state player-id)
                  selected-count (count (get-in starting-bid
                                                [:redraws player-id]
                                                []))]
              (when (< selected-count needed-count)
                player-id)))
          (:redraw-order starting-bid))))

(defn starting-bid-redraw-complete? [starting-bid]
  (let [state (:current-game starting-bid)
        selected-card-ids (starting-bid-redraw-card-ids starting-bid)]
    (and (seq (:redraw-order starting-bid))
         (= (count selected-card-ids)
            (count (set selected-card-ids)))
         (empty? (starting-bid-remaining-redraw-cards starting-bid))
         (every? (fn [player-id]
                   (= (starting-bid-redraw-needed-count state player-id)
                      (count (get-in starting-bid
                                     [:redraws player-id]
                                     []))))
                 (:redraw-order starting-bid)))))

(defn with-starting-bid-redraw-stage [starting-bid]
  (assoc starting-bid
         :stage (if (starting-bid-redraw-complete? starting-bid)
                  :resolved
                  :redrawing)))

(defn select-lobby-bid-card [app-db player-id card-id]
  (let [player-id (setup/normalize-player-id player-id)
        card-id (str card-id)
        starting-bid (get-in app-db [:lobby :starting-bid])
        current-game (:current-game starting-bid)]
    (cond
      (nil? starting-bid)
      app-db

      (not= :choosing (:stage starting-bid))
      app-db

      (str/blank? card-id)
      (-> app-db
          (update-in [:lobby :starting-bid :current-bids] dissoc player-id)
          (setup/clear-lobby-error-codes starting-bid-choice-error-codes))

      (nil? (starting-bid-card current-game player-id card-id))
      (assoc-in app-db [:lobby :error]
                (setup/lobby-error :invalid-starting-bid-card
                                   "Choose a card from that player's current bid hand."
                                   {:player-id player-id
                                    :card-id card-id
                                    :hand-card-ids (mapv :id
                                                         (get-in current-game
                                                                 [:players-by-id
                                                                  player-id
                                                                  :hand]))}))

      :else
      (-> app-db
          (assoc-in [:lobby :starting-bid :current-bids player-id] card-id)
          (setup/clear-lobby-error-codes starting-bid-choice-error-codes)))))

(defn reveal-lobby-bids [app-db]
  (let [starting-bid (get-in app-db [:lobby :starting-bid])
        player-ids (starting-bid-player-ids starting-bid)
        current-bids (select-keys (:current-bids starting-bid) player-ids)
        missing-player-ids (vec (remove #(contains? current-bids %) player-ids))]
    (cond
      (nil? starting-bid)
      app-db

      (not= :choosing (:stage starting-bid))
      app-db

      (seq missing-player-ids)
      (assoc-in app-db [:lobby :error]
                (setup/lobby-error :incomplete-starting-bid-round
                                   "Every player must choose a bid card."
                                   {:missing-player-ids missing-player-ids}))

      :else
      (let [rounds (conj (:rounds starting-bid) current-bids)
            {:keys [ok? state resolved? winner-id bid-history bid-cards
                    redraw-order error]}
            (game-state/resolve-starting-bid-rounds
             (:initial-game starting-bid)
             {:rounds rounds})]
        (if-not ok?
          (assoc-in app-db [:lobby :error] error)
          (let [next-starting-bid
                (cond-> (assoc starting-bid
                               :current-game state
                               :rounds rounds
                               :current-bids {}
                               :bid-history bid-history
                               :bid-cards bid-cards)
                  resolved?
                  (assoc :stage :redrawing
                         :winner-id winner-id
                         :redraw-order redraw-order
                         :redraws {})

                  (not resolved?)
                  (assoc :stage :choosing
                         :winner-id nil
                         :redraw-order nil
                         :redraws nil))]
            (-> app-db
                (assoc-in [:lobby :starting-bid] next-starting-bid)
                (update :lobby dissoc :error))))))))

(defn select-lobby-redraw-card [app-db player-id card-id]
  (let [player-id (setup/normalize-player-id player-id)
        card-id (str card-id)
        starting-bid (get-in app-db [:lobby :starting-bid])
        active-player-id (starting-bid-active-redraw-player-id starting-bid)
        remaining-cards (starting-bid-remaining-redraw-cards starting-bid)
        remaining-card-ids (set (map :id remaining-cards))]
    (cond
      (nil? starting-bid)
      app-db

      (not (contains? #{:redrawing :resolved} (:stage starting-bid)))
      app-db

      (str/blank? card-id)
      (-> app-db
          (update-in [:lobby :starting-bid :redraws] dissoc player-id)
          (update-in [:lobby :starting-bid]
                     with-starting-bid-redraw-stage)
          (setup/clear-lobby-error-codes starting-bid-redraw-error-codes))

      (not= :redrawing (:stage starting-bid))
      app-db

      (not= player-id active-player-id)
      (assoc-in app-db [:lobby :error]
                (setup/lobby-error :inactive-starting-bid-redraw-player
                                   "Choose bid redraw cards for the active redraw player."
                                   {:player-id player-id
                                    :active-player-id active-player-id}))

      (not (contains? remaining-card-ids card-id))
      (assoc-in app-db [:lobby :error]
                (setup/lobby-error :invalid-bid-redraw-card
                                   "Players can only redraw from bid cards that are still available."
                                   {:player-id player-id
                                    :card-id card-id
                                    :available-card-ids (mapv :id remaining-cards)}))

      :else
      (-> app-db
          (update-in [:lobby :starting-bid :redraws player-id]
                     (fnil conj [])
                     card-id)
          (update-in [:lobby :starting-bid]
                     with-starting-bid-redraw-stage)
          (update :lobby dissoc :error)))))

(defn cancel-lobby-bidding [app-db]
  (-> app-db
      setup/clear-lobby-starting-bid
      setup/refresh-lobby-error))
