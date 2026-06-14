(ns gnostica.game-state.command-contracts.delegated
  (:require [gnostica.game-state.command-contracts.full-cards :as full-cards]
            [gnostica.game-state.command-contracts.primitives :as primitives]
            [gnostica.game-state.command-contracts.suits :as suits]
            [gnostica.game-state.command-contracts.targets :as targets]
            [gnostica.power-taxonomy :as taxonomy]
            [malli.core :as m]))

(def ^:private schema-player-id ::schema-player)
(def ^:private schema-card-id "schema-card")

(declare valid-fool-reveal? valid-world-command?)

(def FoolReveal
  [:and
   (primitives/closed-map
    [:power {:optional true} :keyword]
    [:piece-id {:optional true} primitives/PieceId]
    [:command {:optional true} :map]
    [:play-command {:optional true} :map])
   [:fn {:error/message "Fool play commands must match the selected child command shape"}
    (fn [reveal] (valid-fool-reveal? reveal))]])

(def FoolCommand
  (apply primitives/closed-map
         (concat targets/source-command-entries
                 [[:reveals {:optional true} [:vector {:max 2} FoolReveal]]
                  [:shuffle-fn {:optional true} primitives/ShuffleFn]])))

(def ^:private delegated-payload-keys
  #{:actions
    :card-ids
    :cup
    :cup-actions
    :cup-variant
    :damage
    :destination
    :direction
    :disc
    :disc-actions
    :disc-variant
    :distance
    :hand-trade-target
    :hand-trade-target-piece-id
    :minion-orientation
    :mode
    :one-point-card-id
    :orientation
    :orientations
    :piece-id
    :redraws
    :replacement-card-id
    :replacement-card-source
    :reveals
    :rod
    :rod-actions
    :rod-variant
    :shuffle-fn
    :sword
    :sword-actions
    :sword-variant
    :target
    :territory-card-source})

(def ^:private world-command-entries
  [[:copied-board-index primitives/BoardIndex]
   [:copied-power {:optional true} targets/WorldCopiedPower]
   [:power {:optional true} targets/WorldCopiedPower]
   [:actions {:optional true} [:vector full-cards/MajorAction]]
   [:card-ids {:optional true} [:vector primitives/CardId]]
   [:cup {:optional true} [:or full-cards/SunCupAction suits/MajorCupAction]]
   [:cup-actions {:optional true} [:vector suits/MajorCupAction]]
   [:cup-variant {:optional true} :keyword]
   [:damage {:optional true} primitives/PositiveInt]
   [:destination {:optional true} [:or targets/TerritoryDestination
                                   targets/WastelandDestination]]
   [:direction {:optional true} [:enum :north :east :south :west]]
   [:disc {:optional true} full-cards/SunDiscAction]
   [:disc-actions {:optional true} [:vector {:min 1 :max 2} suits/DiscAction]]
   [:disc-variant {:optional true} :keyword]
   [:distance {:optional true} primitives/PositiveInt]
   [:hand-trade-target {:optional true} targets/PieceTarget]
   [:hand-trade-target-piece-id {:optional true} primitives/PieceId]
   [:minion-orientation {:optional true} primitives/Orientation]
   [:mode {:optional true} [:enum :move-minion :push-piece :push-territory]]
   [:one-point-card-id {:optional true} primitives/CardId]
   [:orientation {:optional true} primitives/Orientation]
   [:orientations {:optional true} [:vector {:min 1 :max 3} full-cards/MajorAction]]
   [:piece-id {:optional true} primitives/PieceId]
   [:redraws {:optional true} [:vector {:max 2} full-cards/RedrawPass]]
   [:replacement-card-id {:optional true} primitives/CardId]
   [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
   [:reveals {:optional true} [:vector {:max 2} FoolReveal]]
   [:rod {:optional true} suits/MajorRodAction]
   [:rod-actions {:optional true} [:vector suits/MajorRodAction]]
   [:rod-variant {:optional true} :keyword]
   [:shuffle-fn {:optional true} primitives/ShuffleFn]
   [:sword {:optional true} suits/MajorSwordAction]
   [:sword-actions {:optional true} [:vector {:min 1 :max 2} suits/MajorSwordAction]]
   [:sword-variant {:optional true} :keyword]
   [:target {:optional true} [:or targets/PieceTarget
                              targets/TerritoryTarget
                              targets/WastelandTarget
                              targets/CreatedPieceTarget
                              targets/CreatedTerritoryTarget]]
   [:territory-card-source {:optional true} [:enum :hand :draw-pile-top]]])

(def WorldCommand
  [:and
   (apply primitives/closed-map
          (concat targets/source-command-entries world-command-entries))
   [:fn {:error/message "World delegated command fields must match a copied child command shape"}
    (fn [command] (valid-world-command? command))]])

(def ^:private composite-major-payload-keys
  #{:actions
    :cup
    :cup-actions
    :hand-trade-target
    :hand-trade-target-piece-id
    :minion-orientation
    :rod
    :rod-actions})

(def ^:private delegated-selector-keys
  #{:copied-board-index :copied-power :power})

(defn- delegated-command-payload [command]
  (apply dissoc command delegated-selector-keys))

(defn- selected-world-power [command]
  (or (:copied-power command)
      (:power command)))

(defn- matching-selected-world-powers? [command]
  (or (not (and (contains? command :copied-power)
                (contains? command :power)))
      (= (:copied-power command) (:power command))))

(defn- valid-suit-command? [power command]
  (let [payload (delegated-command-payload command)]
    (case power
      :cup (m/validate suits/CupCommand payload)
      :rod (m/validate suits/RodCommand payload)
      :disc (m/validate suits/DiscCommand payload)
      :sword (m/validate suits/SwordCommand payload)
      false)))

(defn- valid-optional-keyed-command? [schema required-key command]
  (and (contains? command required-key)
       (m/validate schema command)))

(defn- valid-composite-major-command? [command]
  (and (primitives/contains-any? command composite-major-payload-keys)
       (m/validate full-cards/CompositeMajorCommand command)))

(defn- valid-full-major-command? [power command]
  (let [payload (delegated-command-payload command)]
    (case power
      :fool (m/validate FoolCommand payload)
      :high-priestess (m/validate full-cards/HighPriestessCommand payload)
      :empress (m/validate full-cards/CompositeMajorCommand payload)
      :emperor (m/validate full-cards/CompositeMajorCommand payload)
      :hierophant (m/validate full-cards/HierophantCommand payload)
      :lovers (m/validate full-cards/CompositeMajorCommand payload)
      :chariot (m/validate full-cards/CompositeMajorCommand payload)
      :hermit (m/validate full-cards/HermitCommand payload)
      :hanged-man (m/validate full-cards/CompositeMajorCommand payload)
      :temperance (m/validate full-cards/CompositeMajorCommand payload)
      :devil (m/validate full-cards/DevilCommand payload)
      :moon (m/validate suits/MoonCommand payload)
      :sun (m/validate full-cards/SunCommand payload)
      :judgement (m/validate full-cards/JudgementCommand payload)
      :justice (m/validate suits/JusticeSwordCommand payload)
      :death (m/validate suits/DeathSwordCommand payload)
      :tower (m/validate suits/TowerSwordCommand payload)
      false)))

(defn- valid-delegated-command? [command]
  (let [payload (delegated-command-payload command)]
    (or (some #(valid-suit-command? % command) taxonomy/suit-power-values)
        (m/validate full-cards/SunCommand payload)
        (m/validate suits/SwordCommand payload)
        (valid-optional-keyed-command? FoolCommand :reveals payload)
        (valid-optional-keyed-command? full-cards/HighPriestessCommand
                                       :redraws
                                       payload)
        (m/validate full-cards/JudgementCommand payload)
        (valid-composite-major-command? payload)
        (m/validate full-cards/HierophantCommand payload)
        (m/validate full-cards/HermitCommand payload)
        (m/validate full-cards/DevilCommand payload))))

(defn- valid-world-command? [command]
  (let [power (selected-world-power command)]
    (and (matching-selected-world-powers? command)
         (cond
           (contains? taxonomy/suit-power-set power)
           (valid-suit-command? power command)

           (contains? taxonomy/world-copied-full-card-power-set power)
           (valid-full-major-command? power command)

           (not (primitives/contains-any? command delegated-payload-keys))
           true

           :else
           (valid-delegated-command? command)))))

(defn- fool-play-command-shapes [reveal]
  (cond-> []
    (contains? reveal :command) (conj (:command reveal))
    (contains? reveal :play-command) (conj (:play-command reveal))))

(defn- fool-play-power [reveal command]
  (or (:power reveal)
      (:power command)))

(defn- fool-play-piece-id [reveal command]
  (or (:piece-id reveal)
      (get-in command [:source :piece-id])))

(defn- fool-play-command-with-source [reveal command acting-source?]
  (assoc command
         :player-id schema-player-id
         :source (cond-> {:kind :hand-card
                          :card-id schema-card-id}
                   acting-source?
                   (assoc :piece-id (fool-play-piece-id reveal command)))))

(def FoolPlaySource
  (primitives/closed-map
   [:piece-id {:optional true} primitives/PieceId]))

(defn- valid-fool-play-source? [command]
  (or (not (contains? command :source))
      (m/validate FoolPlaySource (:source command))))

(defn- valid-fool-suit-play-command? [reveal command power]
  (valid-suit-command?
   power
   (fool-play-command-with-source reveal command true)))

(defn- valid-fool-delegated-play-command? [reveal command]
  (valid-delegated-command?
   (fool-play-command-with-source reveal command false)))

(defn- valid-fool-play-command? [reveal command]
  (and (valid-fool-play-source? command)
       (let [power (fool-play-power reveal command)]
         (if (contains? taxonomy/suit-power-set power)
           (valid-fool-suit-play-command? reveal command power)
           (valid-fool-delegated-play-command? reveal command)))))

(defn- valid-fool-reveal? [reveal]
  (every? #(valid-fool-play-command? reveal %)
          (fool-play-command-shapes reveal)))
