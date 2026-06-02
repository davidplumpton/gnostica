(ns gnostica.game-state.command-contracts
  (:require [clojure.string :as str]
            [gnostica.pieces :as pieces]
            [malli.core :as m]
            [malli.error :as me]))

(defn- enum-schema [values]
  (into [:enum] values))

(defn- closed-map [& entries]
  (into [:map {:closed true}] entries))

(defn- nonblank-string? [value]
  (and (string? value)
       (not (str/blank? value))))

(defn- non-negative-int? [value]
  (and (int? value)
       (not (neg? value))))

(defn- positive-int? [value]
  (and (int? value)
       (pos? value)))

(def NonBlankString
  [:fn {:error/message "must be a nonblank string"} nonblank-string?])

(def NonNegativeInt
  [:fn {:error/message "must be a non-negative integer"} non-negative-int?])

(def PositiveInt
  [:fn {:error/message "must be a positive integer"} positive-int?])

(def PlayerId :keyword)

(def PieceId :keyword)

(def CardId NonBlankString)

(def BoardIndex NonNegativeInt)

(def Orientation
  (enum-schema pieces/legal-orientations))

(def ShuffleFn
  [:fn {:error/message "must be callable"} ifn?])

(def ^:private suit-power-values
  [:cup :rod :disc :sword])

(def ^:private suit-power-set
  (set suit-power-values))

(def SuitPower
  (enum-schema suit-power-values))

(def ^:private full-major-power-values
  [:fool
   :high-priestess
   :empress
   :emperor
   :hierophant
   :lovers
   :chariot
   :hermit
   :hanged-man
   :temperance
   :devil
   :moon
   :sun
   :judgement
   :justice
   :death
   :tower])

(def ^:private full-major-power-set
  (set full-major-power-values))

(def ^:private world-copied-power-values
  (vec (concat suit-power-values full-major-power-values)))

(def WorldCopiedPower
  (enum-schema world-copied-power-values))

(def TerritorySource
  (closed-map
   [:kind [:enum :territory]]
   [:board-index BoardIndex]
   [:piece-id {:optional true} PieceId]))

(def HandCardSource
  (closed-map
   [:kind [:enum :hand-card]]
   [:card-id CardId]
   [:piece-id {:optional true} PieceId]))

(def MoveSource
  [:or TerritorySource HandCardSource])

(def ^:private source-command-entries
  [[:player-id PlayerId]
   [:source MoveSource]])

(def SourceCommand
  (apply closed-map source-command-entries))

(def ActingTerritorySource
  (closed-map
   [:kind [:enum :territory]]
   [:board-index BoardIndex]
   [:piece-id PieceId]))

(def ActingHandCardSource
  (closed-map
   [:kind [:enum :hand-card]]
   [:card-id CardId]
   [:piece-id PieceId]))

(def ActingMoveSource
  [:or ActingTerritorySource ActingHandCardSource])

(def ^:private acting-source-command-entries
  [[:player-id PlayerId]
   [:source ActingMoveSource]])

(def ActingSourceCommand
  (apply closed-map acting-source-command-entries))

(def TerritoryTarget
  (closed-map
   [:kind [:enum :territory]]
   [:board-index BoardIndex]))

(def PieceTarget
  (closed-map
   [:kind [:enum :piece]]
   [:piece-id PieceId]))

(def WastelandTarget
  (closed-map
   [:kind [:enum :wasteland]]
   [:row :int]
   [:col :int]))

(def CreatedPieceTarget
  (closed-map
   [:kind [:enum :created-piece]]))

(def CreatedTerritoryTarget
  (closed-map
   [:kind [:enum :created-territory]]))

(def TerritoryDestination
  TerritoryTarget)

(def WastelandDestination
  WastelandTarget)

(def CupTarget
  [:or TerritoryTarget PieceTarget WastelandTarget])

(def ^:private cup-action-entries
  [[:target CupTarget]
   [:orientation {:optional true} Orientation]
   [:cup-variant {:optional true} :keyword]
   [:territory-card-source {:optional true} [:enum :hand :draw-pile-top]]
   [:one-point-card-id {:optional true} CardId]
   [:shuffle-fn {:optional true} ShuffleFn]])

(def CupAction
  (apply closed-map cup-action-entries))

(def MajorCupAction
  (apply closed-map (conj cup-action-entries
                          [:piece-id {:optional true} PieceId])))

(def CupCommand
  (apply closed-map (concat acting-source-command-entries
                            cup-action-entries)))

(def RodTarget
  [:or PieceTarget TerritoryTarget])

(def ^:private rod-action-entries
  [[:mode [:enum :move-minion :push-piece :push-territory]]
   [:distance PositiveInt]
   [:target {:optional true} RodTarget]
   [:direction {:optional true} [:enum :north :east :south :west]]
   [:orientation {:optional true} Orientation]
   [:rod-variant {:optional true} :keyword]])

(def RodAction
  (apply closed-map rod-action-entries))

(def MajorRodAction
  (apply closed-map (conj rod-action-entries
                          [:piece-id {:optional true} PieceId])))

(def RodCommand
  (apply closed-map (concat acting-source-command-entries
                            rod-action-entries)))

(def DiscTarget
  [:or PieceTarget TerritoryTarget CreatedPieceTarget CreatedTerritoryTarget])

(def ^:private disc-action-entries
  [[:target DiscTarget]
   [:orientation {:optional true} Orientation]
   [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
   [:replacement-card-id {:optional true} CardId]])

(def DiscAction
  (apply closed-map disc-action-entries))

(defn- star-disc-command? [command]
  (if (contains? command :disc-variant)
    (= :disc-from-discard (:disc-variant command))
    (= "star" (get-in command [:source :card-id]))))

(def SingleDiscCommand
  (apply closed-map (concat acting-source-command-entries
                            disc-action-entries
                            [[:disc-variant {:optional true} :keyword]])))

(def StarDiscCommand
  [:and
   (apply closed-map (concat acting-source-command-entries
                             disc-action-entries
                             [[:disc-variant {:optional true} :keyword]
                              [:minion-orientation Orientation]]))
   [:fn {:error/message "minion orientation requires a Star Disc command"}
    star-disc-command?]])

(def StrengthDiscCommand
  (apply closed-map (concat acting-source-command-entries
                            [[:disc-variant {:optional true} :keyword]
                             [:disc-actions [:vector {:min 1 :max 2} DiscAction]]])))

(def DiscCommand
  [:or SingleDiscCommand StarDiscCommand StrengthDiscCommand])

(def ^:private sun-cup-action-entries
  [[:target CupTarget]
   [:orientation {:optional true} Orientation]
   [:territory-card-source {:optional true} [:enum :hand :draw-pile-top]]
   [:one-point-card-id {:optional true} CardId]])

(def SunCupAction
  (apply closed-map sun-cup-action-entries))

(def SunDiscTarget
  [:or PieceTarget TerritoryTarget CreatedPieceTarget CreatedTerritoryTarget])

(def ^:private sun-disc-action-entries
  [[:target SunDiscTarget]
   [:orientation {:optional true} Orientation]
   [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
   [:replacement-card-id {:optional true} CardId]])

(def SunDiscAction
  (apply closed-map sun-disc-action-entries))

(def SunCommand
  (apply closed-map (concat acting-source-command-entries
                            [[:cup SunCupAction]
                             [:disc {:optional true} SunDiscAction]])))

(def SwordTarget
  [:or PieceTarget TerritoryTarget])

(def ^:private sword-action-entries
  [[:target SwordTarget]
   [:damage PositiveInt]
   [:orientation {:optional true} Orientation]
   [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
   [:replacement-card-id {:optional true} CardId]])

(def SwordAction
  (apply closed-map sword-action-entries))

(def MajorSwordAction
  (apply closed-map (conj sword-action-entries
                          [:piece-id {:optional true} PieceId])))

(def ^:private sword-action-keys
  #{:target :damage :orientation :replacement-card-source :replacement-card-id})

(defn- contains-any? [command ks]
  (boolean (some #(contains? command %) ks)))

(defn- justice-trade-target? [command]
  (or (contains? command :hand-trade-target)
      (contains? command :hand-trade-target-piece-id)))

(defn- justice-command? [command]
  (and (justice-trade-target? command)
       (or (not (contains-any? command sword-action-keys))
           (m/validate SwordAction command))))

(defn- moon-command-action? [command]
  (or (contains? command :rod)
      (contains? command :sword)))

(defn- tower-sword-command? [command]
  (if (contains? command :sword-variant)
    (= :sword-from-discard (:sword-variant command))
    (= "tower" (get-in command [:source :card-id]))))

(def SingleSwordCommand
  (apply closed-map (concat acting-source-command-entries
                            sword-action-entries
                            [[:sword-variant {:optional true} :keyword]])))

(def TowerSwordCommand
  [:and
   (apply closed-map (concat acting-source-command-entries
                             sword-action-entries
                             [[:sword-variant {:optional true} :keyword]
                              [:minion-orientation Orientation]]))
   [:fn {:error/message "minion orientation requires a Tower Sword command"}
    tower-sword-command?]])

(def DeathSwordCommand
  (apply closed-map (concat acting-source-command-entries
                            [[:sword-variant {:optional true} :keyword]
                             [:sword-actions [:vector {:min 1 :max 2} MajorSwordAction]]])))

(def HandTradeAction
  (closed-map
   [:target PieceTarget]))

(def ^:private justice-sword-command-entries
  (concat acting-source-command-entries
          [[:hand-trade-target {:optional true} PieceTarget]
           [:hand-trade-target-piece-id {:optional true} PieceId]
           [:sword-variant {:optional true} :keyword]]
          sword-action-entries))

(def JusticeSwordCommand
  [:and
   (apply closed-map justice-sword-command-entries)
   [:fn {:error/message "must include a Justice hand-trade target and any Sword fields must form a Sword action"}
    justice-command?]])

(def MoonCommand
  [:and
   (apply closed-map (concat acting-source-command-entries
                             [[:rod {:optional true} MajorRodAction]
                              [:sword {:optional true} MajorSwordAction]]))
   [:fn {:error/message "must include :rod or :sword"} moon-command-action?]])

(def SwordCommand
  [:or SingleSwordCommand
   TowerSwordCommand
   DeathSwordCommand
   JusticeSwordCommand
   MoonCommand])

(def RedrawPass
  (closed-map
   [:discard-card-ids {:optional true} [:vector CardId]]
   [:draw-count NonNegativeInt]))

(def HighPriestessCommand
  (apply closed-map (concat source-command-entries
                            [[:redraws {:optional true} [:vector {:max 2} RedrawPass]]
                             [:shuffle-fn {:optional true} ShuffleFn]])))

(def JudgementCommand
  (apply closed-map (concat source-command-entries
                            [[:piece-id PieceId]
                             [:card-ids [:vector CardId]]])))

(declare valid-fool-reveal? valid-world-command?)

(def FoolReveal
  [:and
   (closed-map
    [:power {:optional true} :keyword]
    [:piece-id {:optional true} PieceId]
    [:command {:optional true} :map]
    [:play-command {:optional true} :map])
   [:fn {:error/message "Fool play commands must match the selected child command shape"}
    (fn [reveal] (valid-fool-reveal? reveal))]])

(def FoolCommand
  (apply closed-map (concat source-command-entries
                            [[:reveals {:optional true} [:vector {:max 2} FoolReveal]]
                             [:shuffle-fn {:optional true} ShuffleFn]])))

(def MajorAction
  (closed-map
   [:power :keyword]
   [:piece-id {:optional true} PieceId]
   [:target {:optional true} [:or PieceTarget TerritoryTarget WastelandTarget]]
   [:destination {:optional true} [:or TerritoryDestination WastelandDestination]]
   [:orientation {:optional true} Orientation]
   [:distance {:optional true} PositiveInt]
   [:damage {:optional true} PositiveInt]
   [:mode {:optional true} [:enum :move-minion :push-piece :push-territory]]
   [:direction {:optional true} [:enum :north :east :south :west]]
   [:one-point-card-id {:optional true} CardId]
   [:territory-card-source {:optional true} [:enum :hand :draw-pile-top]]
   [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
   [:replacement-card-id {:optional true} CardId]))

(def CompositeMajorCommand
  (apply closed-map (concat source-command-entries
                            [[:actions {:optional true} [:vector MajorAction]]
                             [:cup {:optional true} MajorCupAction]
                             [:cup-actions {:optional true} [:vector MajorCupAction]]
                             [:rod {:optional true} MajorRodAction]
                             [:rod-actions {:optional true} [:vector MajorRodAction]]
                             [:minion-orientation {:optional true} Orientation]
                             [:hand-trade-target {:optional true} PieceTarget]
                             [:hand-trade-target-piece-id {:optional true} PieceId]])))

(def HierophantCommand
  [:or
   (apply closed-map (concat source-command-entries
                             [[:target PieceTarget]
                              [:orientation Orientation]]))
   (apply closed-map (concat source-command-entries
                             [[:actions [:vector {:min 1} MajorAction]]]))])

(def HermitCommand
  [:or
   (apply closed-map (concat source-command-entries
                             [[:target [:or PieceTarget TerritoryTarget]]
                              [:destination [:or TerritoryDestination WastelandDestination]]
                              [:orientation {:optional true} Orientation]]))
   (apply closed-map (concat source-command-entries
                             [[:actions [:vector {:min 1} MajorAction]]]))])

(def DevilCommand
  [:or
   (apply closed-map (concat source-command-entries
                             [[:target PieceTarget]
                              [:orientation Orientation]]))
   (apply closed-map (concat source-command-entries
                             [[:orientations [:vector {:min 1 :max 3} MajorAction]]]))
   (apply closed-map (concat source-command-entries
                             [[:actions [:vector {:min 1 :max 3} MajorAction]]]))])

(def ^:private schema-player-id ::schema-player)
(def ^:private schema-card-id "schema-card")

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
  [[:copied-board-index BoardIndex]
   [:copied-power {:optional true} WorldCopiedPower]
   [:power {:optional true} WorldCopiedPower]
   [:actions {:optional true} [:vector MajorAction]]
   [:card-ids {:optional true} [:vector CardId]]
   [:cup {:optional true} [:or SunCupAction MajorCupAction]]
   [:cup-actions {:optional true} [:vector MajorCupAction]]
   [:cup-variant {:optional true} :keyword]
   [:damage {:optional true} PositiveInt]
   [:destination {:optional true} [:or TerritoryDestination WastelandDestination]]
   [:direction {:optional true} [:enum :north :east :south :west]]
   [:disc {:optional true} SunDiscAction]
   [:disc-actions {:optional true} [:vector {:min 1 :max 2} DiscAction]]
   [:disc-variant {:optional true} :keyword]
   [:distance {:optional true} PositiveInt]
   [:hand-trade-target {:optional true} PieceTarget]
   [:hand-trade-target-piece-id {:optional true} PieceId]
   [:minion-orientation {:optional true} Orientation]
   [:mode {:optional true} [:enum :move-minion :push-piece :push-territory]]
   [:one-point-card-id {:optional true} CardId]
   [:orientation {:optional true} Orientation]
   [:orientations {:optional true} [:vector {:min 1 :max 3} MajorAction]]
   [:piece-id {:optional true} PieceId]
   [:redraws {:optional true} [:vector {:max 2} RedrawPass]]
   [:replacement-card-id {:optional true} CardId]
   [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
   [:reveals {:optional true} [:vector {:max 2} FoolReveal]]
   [:rod {:optional true} MajorRodAction]
   [:rod-actions {:optional true} [:vector MajorRodAction]]
   [:rod-variant {:optional true} :keyword]
   [:shuffle-fn {:optional true} ShuffleFn]
   [:sword {:optional true} MajorSwordAction]
   [:sword-actions {:optional true} [:vector {:min 1 :max 2} MajorSwordAction]]
   [:sword-variant {:optional true} :keyword]
   [:target {:optional true} [:or PieceTarget
                              TerritoryTarget
                              WastelandTarget
                              CreatedPieceTarget
                              CreatedTerritoryTarget]]
   [:territory-card-source {:optional true} [:enum :hand :draw-pile-top]]])

(def WorldCommand
  [:and
   (apply closed-map (concat source-command-entries world-command-entries))
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
      :cup (m/validate CupCommand payload)
      :rod (m/validate RodCommand payload)
      :disc (m/validate DiscCommand payload)
      :sword (m/validate SwordCommand payload)
      false)))

(defn- valid-optional-keyed-command? [schema required-key command]
  (and (contains? command required-key)
       (m/validate schema command)))

(defn- valid-composite-major-command? [command]
  (and (contains-any? command composite-major-payload-keys)
       (m/validate CompositeMajorCommand command)))

(defn- valid-full-major-command? [power command]
  (let [payload (delegated-command-payload command)]
    (case power
      :fool (m/validate FoolCommand payload)
      :high-priestess (m/validate HighPriestessCommand payload)
      :empress (m/validate CompositeMajorCommand payload)
      :emperor (m/validate CompositeMajorCommand payload)
      :hierophant (m/validate HierophantCommand payload)
      :lovers (m/validate CompositeMajorCommand payload)
      :chariot (m/validate CompositeMajorCommand payload)
      :hermit (m/validate HermitCommand payload)
      :hanged-man (m/validate CompositeMajorCommand payload)
      :temperance (m/validate CompositeMajorCommand payload)
      :devil (m/validate DevilCommand payload)
      :moon (m/validate MoonCommand payload)
      :sun (m/validate SunCommand payload)
      :judgement (m/validate JudgementCommand payload)
      :justice (m/validate JusticeSwordCommand payload)
      :death (m/validate DeathSwordCommand payload)
      :tower (m/validate TowerSwordCommand payload)
      false)))

(defn- valid-delegated-command? [command]
  (let [payload (delegated-command-payload command)]
    (or (some #(valid-suit-command? % command) suit-power-values)
        (m/validate SunCommand payload)
        (m/validate SwordCommand payload)
        (valid-optional-keyed-command? FoolCommand :reveals payload)
        (valid-optional-keyed-command? HighPriestessCommand :redraws payload)
        (m/validate JudgementCommand payload)
        (valid-composite-major-command? payload)
        (m/validate HierophantCommand payload)
        (m/validate HermitCommand payload)
        (m/validate DevilCommand payload))))

(defn- valid-world-command? [command]
  (let [power (selected-world-power command)]
    (and (matching-selected-world-powers? command)
         (cond
           (contains? suit-power-set power)
           (valid-suit-command? power command)

           (contains? full-major-power-set power)
           (valid-full-major-command? power command)

           (not (contains-any? command delegated-payload-keys))
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
  (closed-map
   [:piece-id {:optional true} PieceId]))

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
         (if (contains? suit-power-set power)
           (valid-fool-suit-play-command? reveal command power)
           (valid-fool-delegated-play-command? reveal command)))))

(defn- valid-fool-reveal? [reveal]
  (every? #(valid-fool-play-command? reveal %)
          (fool-play-command-shapes reveal)))

(def DrawCommand
  (closed-map
   [:player-id PlayerId]
   [:discard-card-ids {:optional true} [:vector CardId]]
   [:draw-count NonNegativeInt]
   [:shuffle-fn {:optional true} ShuffleFn]))

(def OrientCommand
  (closed-map
   [:player-id PlayerId]
   [:piece-id PieceId]
   [:orientation Orientation]))

(def InitialPlacementCommand
  (closed-map
   [:player-id PlayerId]
   [:target [:or TerritoryTarget WastelandTarget]]
   [:orientation Orientation]))

(def ErrorShape
  [:map
   [:code :keyword]
   [:message NonBlankString]
   [:data :any]])

(def StateSuccessResult
  (closed-map
   [:ok? [:= true]]
   [:state :map]
   [:events [:vector :map]]))

(def CommandValidationSuccessResult
  (closed-map
   [:ok? [:= true]]
   [:command :map]))

(def SuccessResult
  [:or StateSuccessResult CommandValidationSuccessResult])

(def FailureResult
  (closed-map
   [:ok? [:= false]]
   [:error ErrorShape]))

(def Result
  [:or SuccessResult FailureResult])

(def command-schemas
  {:cup CupCommand
   :rod RodCommand
   :disc DiscCommand
   :sun SunCommand
   :sword SwordCommand
   :moon MoonCommand
   :fool FoolCommand
   :high-priestess HighPriestessCommand
   :judgement JudgementCommand
   :world WorldCommand
   :draw DrawCommand
   :orient OrientCommand
   :initial-placement InitialPlacementCommand
   :hierophant HierophantCommand
   :hermit HermitCommand
   :devil DevilCommand
   :composite-major CompositeMajorCommand
   :empress CompositeMajorCommand
   :emperor CompositeMajorCommand
   :lovers CompositeMajorCommand
   :chariot CompositeMajorCommand
   :hanged-man CompositeMajorCommand
   :temperance CompositeMajorCommand})

(defn command-schema [command-kind]
  (get command-schemas command-kind))

(defn known-command-kinds []
  (vec (sort (keys command-schemas))))

(defn valid-command? [command-kind command]
  (boolean
   (when-let [schema (command-schema command-kind)]
     (m/validate schema command))))

(defn explain-command [command-kind command]
  (if-let [schema (command-schema command-kind)]
    (when-let [explanation (m/explain schema command)]
      {:message "Command failed the game-state command contract."
       :errors (me/humanize explanation)
       :explanation explanation})
    {:message "Unknown game-state command contract."
     :errors {:command-kind ["is not a known command contract"]}
     :known-command-kinds (known-command-kinds)}))

(defn- failure [code message data]
  {:ok? false
   :error {:code code
           :message message
           :data data}})

(defn validate-command [command-kind command]
  (if-let [explanation (explain-command command-kind command)]
    (failure :invalid-command-contract
             (:message explanation)
             (assoc explanation
                    :command-kind command-kind
                    :command command))
    {:ok? true
     :command command}))

(defn valid-result? [result]
  (m/validate Result result))

(defn explain-result [result]
  (when-let [explanation (m/explain Result result)]
    {:message "Result failed the game-state structured result contract."
     :errors (me/humanize explanation)
     :explanation explanation}))
