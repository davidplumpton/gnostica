(ns gnostica.game-state.setup
  (:require [gnostica.game-state.result :as result]
            [gnostica.game-state.setup.creation :as creation]
            [gnostica.game-state.setup.starting-bid :as starting-bid]))

(defn resolve-starting-bid-rounds [state command]
  (starting-bid/resolve-starting-bid-rounds state command))

(defn apply-starting-bids [state command]
  (starting-bid/apply-starting-bids state command))

(defn create-game
  ([player-specs]
   (create-game player-specs {}))
  ([player-specs opts]
   (let [{base-ok? :ok?
          base-state :state
          base-events :events
          :as base-result}
         (creation/create-base-game player-specs opts)]
     (if-not base-ok?
       base-result
       (if-let [starting-bids (:starting-bids opts)]
         (let [{bid-ok? :ok?
                bid-state :state
                bid-events :events
                :as bid-result}
               (apply-starting-bids base-state starting-bids)]
           (if bid-ok?
             (result/success bid-state (into base-events bid-events))
             bid-result))
         base-result)))))
