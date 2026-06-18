(ns gnostica.game-state.command-contracts.suits
  (:require [gnostica.game-state.command-contracts.primitives :as primitives]
            [gnostica.game-state.command-contracts.targets :as targets]
            [malli.core :as m]))

(def CupTarget
  [:or targets/TerritoryTarget targets/PieceTarget targets/WastelandTarget])

(def cup-action-entries
  [[:target CupTarget]
   [:orientation {:optional true} primitives/Orientation]
   [:cup-variant {:optional true} :keyword]
   [:territory-card-source {:optional true} [:enum :hand :draw-pile-top]]
   [:one-point-card-id {:optional true} primitives/CardId]
   [:shuffle-fn {:optional true} primitives/ShuffleFn]])

(def CupAction
  (apply primitives/closed-map cup-action-entries))

(def MajorCupAction
  (apply primitives/closed-map
         (conj cup-action-entries
               [:piece-id {:optional true} primitives/PieceId])))

(def CupCommand
  (apply primitives/closed-map
         (concat targets/acting-source-command-entries
                 cup-action-entries)))

(def RodTarget
  [:or targets/PieceTarget targets/TerritoryTarget])

(def rod-action-entries
  [[:mode [:enum :move-minion :push-piece :push-territory]]
   [:distance primitives/PositiveInt]
   [:target {:optional true} RodTarget]
   [:direction {:optional true} [:enum :north :east :south :west]]
   [:orientation {:optional true} primitives/Orientation]
   [:rod-variant {:optional true} :keyword]])

(def RodAction
  (apply primitives/closed-map rod-action-entries))

(def MajorRodAction
  (apply primitives/closed-map
         (conj rod-action-entries
               [:piece-id {:optional true} primitives/PieceId])))

(def RodCommand
  (apply primitives/closed-map
         (concat targets/acting-source-command-entries
                 rod-action-entries)))

(def DiscTarget
  [:or
   targets/PieceTarget
   targets/TerritoryTarget
   targets/CreatedPieceTarget
   targets/CreatedTerritoryTarget])

(def disc-action-entries
  [[:target DiscTarget]
   [:orientation {:optional true} primitives/Orientation]
   [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
   [:replacement-card-id {:optional true} primitives/CardId]])

(def DiscAction
  (apply primitives/closed-map disc-action-entries))

(def MajorDiscAction
  (apply primitives/closed-map
         (conj disc-action-entries
               [:piece-id {:optional true} primitives/PieceId])))

(defn- star-disc-command? [command]
  (if (contains? command :disc-variant)
    (= :disc-from-discard (:disc-variant command))
    (= "star" (get-in command [:source :card-id]))))

(def SingleDiscCommand
  (apply primitives/closed-map
         (concat targets/acting-source-command-entries
                 disc-action-entries
                 [[:disc-variant {:optional true} :keyword]])))

(def StarDiscCommand
  [:and
   (apply primitives/closed-map
          (concat targets/acting-source-command-entries
                  disc-action-entries
                  [[:disc-variant {:optional true} :keyword]
                   [:minion-orientation primitives/Orientation]]))
   [:fn {:error/message "minion orientation requires a Star Disc command"}
    star-disc-command?]])

(def StrengthDiscCommand
  (apply primitives/closed-map
         (concat targets/acting-source-command-entries
                 [[:disc-variant {:optional true} :keyword]
                  [:disc-actions [:vector {:min 1 :max 2} MajorDiscAction]]])))

(def DiscCommand
  [:or SingleDiscCommand StarDiscCommand StrengthDiscCommand])

(def SwordTarget
  [:or targets/PieceTarget targets/TerritoryTarget])

(def sword-action-entries
  [[:target SwordTarget]
   [:damage primitives/PositiveInt]
   [:orientation {:optional true} primitives/Orientation]
   [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
   [:replacement-card-id {:optional true} primitives/CardId]])

(def SwordAction
  (apply primitives/closed-map sword-action-entries))

(def MajorSwordAction
  (apply primitives/closed-map
         (conj sword-action-entries
               [:piece-id {:optional true} primitives/PieceId])))

(def ^:private sword-action-keys
  #{:target :damage :orientation :replacement-card-source :replacement-card-id})

(defn- justice-trade-target? [command]
  (or (contains? command :hand-trade-target)
      (contains? command :hand-trade-target-piece-id)))

(defn- justice-command? [command]
  (and (justice-trade-target? command)
       (or (not (primitives/contains-any? command sword-action-keys))
           (m/validate SwordAction (select-keys command sword-action-keys)))))

(defn- moon-command-action? [command]
  (or (contains? command :rod)
      (contains? command :sword)))

(defn- tower-sword-command? [command]
  (if (contains? command :sword-variant)
    (= :sword-from-discard (:sword-variant command))
    (= "tower" (get-in command [:source :card-id]))))

(def SingleSwordCommand
  (apply primitives/closed-map
         (concat targets/acting-source-command-entries
                 sword-action-entries
                 [[:sword-variant {:optional true} :keyword]])))

(def TowerSwordCommand
  [:and
   (apply primitives/closed-map
          (concat targets/acting-source-command-entries
                  sword-action-entries
                  [[:sword-variant {:optional true} :keyword]
                   [:minion-orientation primitives/Orientation]]))
   [:fn {:error/message "minion orientation requires a Tower Sword command"}
    tower-sword-command?]])

(def DeathSwordCommand
  (apply primitives/closed-map
         (concat targets/acting-source-command-entries
                 [[:sword-variant {:optional true} :keyword]
                  [:sword-actions [:vector {:min 1 :max 2} MajorSwordAction]]])))

(def HandTradeAction
  (primitives/closed-map
   [:target targets/PieceTarget]))

(def justice-sword-command-entries
  (concat targets/acting-source-command-entries
          [[:hand-trade-target {:optional true} targets/PieceTarget]
           [:hand-trade-target-piece-id {:optional true} primitives/PieceId]
           [:sword-variant {:optional true} :keyword]
           [:target {:optional true} SwordTarget]
           [:damage {:optional true} primitives/PositiveInt]
           [:orientation {:optional true} primitives/Orientation]
           [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
           [:replacement-card-id {:optional true} primitives/CardId]]))

(def JusticeSwordCommand
  [:and
   (apply primitives/closed-map justice-sword-command-entries)
   [:fn {:error/message "must include a Justice hand-trade target and any Sword fields must form a Sword action"}
    justice-command?]])

(def MoonCommand
  [:and
   (apply primitives/closed-map
          (concat targets/acting-source-command-entries
                  [[:rod {:optional true} MajorRodAction]
                   [:sword {:optional true} MajorSwordAction]]))
   [:fn {:error/message "must include :rod or :sword"} moon-command-action?]])

(def SwordCommand
  [:or
   SingleSwordCommand
   TowerSwordCommand
   DeathSwordCommand
   JusticeSwordCommand
   MoonCommand])
