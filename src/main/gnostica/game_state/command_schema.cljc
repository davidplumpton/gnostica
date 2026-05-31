(ns gnostica.game-state.command-schema
  (:require [clojure.string :as str]
            [gnostica.pieces :as pieces]
            [malli.core :as m]
            [malli.error :as me]))

(defn- enum-schema [values]
  (into [:enum] values))

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

(def TerritorySource
  [:map
   [:kind [:enum :territory]]
   [:board-index BoardIndex]
   [:piece-id {:optional true} PieceId]])

(def HandCardSource
  [:map
   [:kind [:enum :hand-card]]
   [:card-id CardId]
   [:piece-id {:optional true} PieceId]])

(def MoveSource
  [:or TerritorySource HandCardSource])

(def SourceCommand
  [:map
   [:player-id PlayerId]
   [:source MoveSource]])

(def TerritoryTarget
  [:map
   [:kind [:enum :territory]]
   [:board-index BoardIndex]])

(def PieceTarget
  [:map
   [:kind [:enum :piece]]
   [:piece-id PieceId]])

(def WastelandTarget
  [:map
   [:kind [:enum :wasteland]]
   [:row :int]
   [:col :int]])

(def CreatedPieceTarget
  [:map
   [:kind [:enum :created-piece]]])

(def CreatedTerritoryTarget
  [:map
   [:kind [:enum :created-territory]]])

(def TerritoryDestination
  TerritoryTarget)

(def WastelandDestination
  WastelandTarget)

(def CupTarget
  [:or TerritoryTarget PieceTarget WastelandTarget])

(def CupAction
  [:map
   [:target CupTarget]
   [:orientation {:optional true} Orientation]
   [:cup-variant {:optional true} :keyword]
   [:territory-card-source {:optional true} [:enum :hand :draw-pile-top]]
   [:one-point-card-id {:optional true} CardId]
   [:shuffle-fn {:optional true} ShuffleFn]])

(def CupCommand
  [:and SourceCommand CupAction])

(def RodTarget
  [:or PieceTarget TerritoryTarget])

(def RodAction
  [:map
   [:mode [:enum :move-minion :push-piece :push-territory]]
   [:distance PositiveInt]
   [:target {:optional true} RodTarget]
   [:direction {:optional true} [:enum :north :east :south :west]]
   [:orientation {:optional true} Orientation]
   [:rod-variant {:optional true} :keyword]])

(def RodCommand
  [:and SourceCommand RodAction])

(def DiscTarget
  [:or PieceTarget TerritoryTarget CreatedPieceTarget CreatedTerritoryTarget])

(def DiscAction
  [:map
   [:target DiscTarget]
   [:orientation {:optional true} Orientation]
   [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
   [:replacement-card-id {:optional true} CardId]])

(def SingleDiscCommand
  [:and
   SourceCommand
   DiscAction
   [:map
    [:disc-variant {:optional true} :keyword]
    [:minion-orientation {:optional true} Orientation]]])

(def StrengthDiscCommand
  [:and
   SourceCommand
   [:map
    [:disc-variant {:optional true} :keyword]
    [:disc-actions [:vector {:min 1 :max 2} DiscAction]]]])

(def DiscCommand
  [:or SingleDiscCommand StrengthDiscCommand])

(def SunCupAction
  [:map
   [:target CupTarget]
   [:orientation {:optional true} Orientation]
   [:territory-card-source {:optional true} [:enum :hand :draw-pile-top]]
   [:one-point-card-id {:optional true} CardId]])

(def SunDiscTarget
  [:or PieceTarget TerritoryTarget CreatedPieceTarget CreatedTerritoryTarget])

(def SunDiscAction
  [:map
   [:target SunDiscTarget]
   [:orientation {:optional true} Orientation]
   [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
   [:replacement-card-id {:optional true} CardId]])

(def SunCommand
  [:and
   SourceCommand
   [:map
    [:cup SunCupAction]
    [:disc {:optional true} SunDiscAction]]])

(def SwordTarget
  [:or PieceTarget TerritoryTarget])

(def SwordAction
  [:map
   [:target SwordTarget]
   [:damage PositiveInt]
   [:orientation {:optional true} Orientation]
   [:replacement-card-source {:optional true} [:enum :hand :discard-pile]]
   [:replacement-card-id {:optional true} CardId]])

(def ^:private sword-action-keys
  #{:target :damage :orientation :replacement-card-source :replacement-card-id})

(defn- contains-any? [command ks]
  (boolean (some #(contains? command %) ks)))

(defn- justice-trade-target? [command]
  (or (m/validate [:map [:hand-trade-target PieceTarget]] command)
      (m/validate [:map [:hand-trade-target-piece-id PieceId]] command)))

(defn- justice-command? [command]
  (and (justice-trade-target? command)
       (or (not (contains-any? command sword-action-keys))
           (m/validate SwordAction command))))

(defn- moon-command-action? [command]
  (or (contains? command :rod)
      (contains? command :sword)))

(def SingleSwordCommand
  [:and
   SourceCommand
   SwordAction
   [:map
    [:sword-variant {:optional true} :keyword]
    [:minion-orientation {:optional true} Orientation]]])

(def DeathSwordCommand
  [:and
   SourceCommand
   [:map
    [:sword-variant {:optional true} :keyword]
    [:sword-actions [:vector {:min 1 :max 2} SwordAction]]]])

(def HandTradeAction
  [:map
   [:target PieceTarget]])

(def JusticeSwordCommand
  [:and
   SourceCommand
   [:fn {:error/message "must include a Justice hand-trade target and any Sword fields must form a Sword action"}
    justice-command?]])

(def MoonCommand
  [:and
   SourceCommand
   [:map
    [:rod {:optional true} RodAction]
    [:sword {:optional true} SwordAction]]
   [:fn {:error/message "must include :rod or :sword"} moon-command-action?]])

(def SwordCommand
  [:or SingleSwordCommand DeathSwordCommand JusticeSwordCommand MoonCommand])

(def RedrawPass
  [:map
   [:discard-card-ids {:optional true} [:vector CardId]]
   [:draw-count NonNegativeInt]])

(def HighPriestessCommand
  [:and
   SourceCommand
   [:map
    [:redraws {:optional true} [:vector {:max 2} RedrawPass]]
    [:shuffle-fn {:optional true} ShuffleFn]]])

(def JudgementCommand
  [:and
   SourceCommand
   [:map
    [:piece-id PieceId]
    [:card-ids [:vector CardId]]]])

(def FoolReveal
  [:map
   [:power {:optional true} :keyword]
   [:piece-id {:optional true} PieceId]
   [:command {:optional true} :map]
   [:play-command {:optional true} :map]])

(def FoolCommand
  [:and
   SourceCommand
   [:map
    [:reveals {:optional true} [:vector {:max 2} FoolReveal]]
    [:shuffle-fn {:optional true} ShuffleFn]]])

(def MajorAction
  [:map
   [:power :keyword]
   [:piece-id {:optional true} PieceId]
   [:target {:optional true} [:or PieceTarget TerritoryTarget WastelandTarget]]
   [:destination {:optional true} [:or TerritoryDestination WastelandDestination]]
   [:orientation {:optional true} Orientation]
   [:distance {:optional true} PositiveInt]
   [:damage {:optional true} PositiveInt]])

(def CompositeMajorCommand
  [:and
   SourceCommand
   [:map
    [:actions {:optional true} [:vector MajorAction]]
    [:cup {:optional true} CupAction]
    [:cup-actions {:optional true} [:vector CupAction]]
    [:rod {:optional true} RodAction]
    [:rod-actions {:optional true} [:vector RodAction]]
    [:minion-orientation {:optional true} Orientation]
    [:hand-trade-target {:optional true} PieceTarget]
    [:hand-trade-target-piece-id {:optional true} PieceId]]])

(def HierophantCommand
  [:and
   SourceCommand
   [:or
    [:map
     [:target PieceTarget]
     [:orientation Orientation]]
    [:map
     [:actions [:vector {:min 1} MajorAction]]]]])

(def HermitCommand
  [:and
   SourceCommand
   [:or
    [:map
     [:target [:or PieceTarget TerritoryTarget]]
     [:destination [:or TerritoryDestination WastelandDestination]]
     [:orientation {:optional true} Orientation]]
    [:map
     [:actions [:vector {:min 1} MajorAction]]]]])

(def DevilCommand
  [:and
   SourceCommand
   [:or
    [:map
     [:target PieceTarget]
     [:orientation Orientation]]
    [:map
     [:orientations [:vector {:min 1 :max 3} MajorAction]]]
    [:map
     [:actions [:vector {:min 1 :max 3} MajorAction]]]]])

(def WorldCommand
  [:and
   SourceCommand
   [:map
    [:copied-board-index BoardIndex]
    [:copied-power {:optional true} :keyword]
    [:power {:optional true} :keyword]]])

(def DrawCommand
  [:map
   [:player-id PlayerId]
   [:discard-card-ids {:optional true} [:vector CardId]]
   [:draw-count NonNegativeInt]
   [:shuffle-fn {:optional true} ShuffleFn]])

(def OrientCommand
  [:map
   [:player-id PlayerId]
   [:piece-id PieceId]
   [:orientation Orientation]])

(def InitialPlacementCommand
  [:map
   [:player-id PlayerId]
   [:target [:or TerritoryTarget WastelandTarget]]
   [:orientation Orientation]])

(def ErrorShape
  [:map
   [:code :keyword]
   [:message NonBlankString]
   [:data :any]])

(def FailureResult
  [:map
   [:ok? [:= false]]
   [:error ErrorShape]])

(def SuccessResult
  [:map
   [:ok? [:= true]]])

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
