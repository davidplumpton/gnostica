(ns gnostica.game-state.command-contracts
  (:require [gnostica.game-state.command-contracts.basic :as basic]
            [gnostica.game-state.command-contracts.delegated :as delegated]
            [gnostica.game-state.command-contracts.full-cards :as full-cards]
            [gnostica.game-state.command-contracts.primitives :as primitives]
            [gnostica.game-state.command-contracts.result :as result]
            [gnostica.game-state.command-contracts.suits :as suits]
            [gnostica.game-state.command-contracts.targets :as targets]
            [malli.core :as m]
            [malli.error :as me]))

(def NonBlankString primitives/NonBlankString)
(def NonNegativeInt primitives/NonNegativeInt)
(def PositiveInt primitives/PositiveInt)
(def PlayerId primitives/PlayerId)
(def PieceId primitives/PieceId)
(def CardId primitives/CardId)
(def BoardIndex primitives/BoardIndex)
(def Orientation primitives/Orientation)
(def ShuffleFn primitives/ShuffleFn)

(def SuitPower targets/SuitPower)
(def WorldCopiedPower targets/WorldCopiedPower)
(def TerritorySource targets/TerritorySource)
(def HandCardSource targets/HandCardSource)
(def MoveSource targets/MoveSource)
(def SourceCommand targets/SourceCommand)
(def ActingTerritorySource targets/ActingTerritorySource)
(def ActingHandCardSource targets/ActingHandCardSource)
(def ActingMoveSource targets/ActingMoveSource)
(def ActingSourceCommand targets/ActingSourceCommand)
(def TerritoryTarget targets/TerritoryTarget)
(def PieceTarget targets/PieceTarget)
(def WastelandTarget targets/WastelandTarget)
(def CreatedPieceTarget targets/CreatedPieceTarget)
(def CreatedTerritoryTarget targets/CreatedTerritoryTarget)
(def TerritoryDestination targets/TerritoryDestination)
(def WastelandDestination targets/WastelandDestination)

(def CupTarget suits/CupTarget)
(def CupAction suits/CupAction)
(def MajorCupAction suits/MajorCupAction)
(def CupCommand suits/CupCommand)
(def RodTarget suits/RodTarget)
(def RodAction suits/RodAction)
(def MajorRodAction suits/MajorRodAction)
(def RodCommand suits/RodCommand)
(def DiscTarget suits/DiscTarget)
(def DiscAction suits/DiscAction)
(def MajorDiscAction suits/MajorDiscAction)
(def SingleDiscCommand suits/SingleDiscCommand)
(def StarDiscCommand suits/StarDiscCommand)
(def StrengthDiscCommand suits/StrengthDiscCommand)
(def DiscCommand suits/DiscCommand)
(def SwordTarget suits/SwordTarget)
(def SwordAction suits/SwordAction)
(def MajorSwordAction suits/MajorSwordAction)
(def SingleSwordCommand suits/SingleSwordCommand)
(def TowerSwordCommand suits/TowerSwordCommand)
(def DeathSwordCommand suits/DeathSwordCommand)
(def HandTradeAction suits/HandTradeAction)
(def JusticeSwordCommand suits/JusticeSwordCommand)
(def MoonCommand suits/MoonCommand)
(def SwordCommand suits/SwordCommand)

(def RedrawPass full-cards/RedrawPass)
(def HighPriestessCommand full-cards/HighPriestessCommand)
(def JudgementCommand full-cards/JudgementCommand)
(def SunCupAction full-cards/SunCupAction)
(def SunDiscTarget full-cards/SunDiscTarget)
(def SunDiscAction full-cards/SunDiscAction)
(def SunCommand full-cards/SunCommand)
(def MajorAction full-cards/MajorAction)
(def CompositeMajorCommand full-cards/CompositeMajorCommand)
(def HierophantCommand full-cards/HierophantCommand)
(def HermitCommand full-cards/HermitCommand)
(def DevilCommand full-cards/DevilCommand)

(def FoolReveal delegated/FoolReveal)
(def FoolCommand delegated/FoolCommand)
(def WorldCommand delegated/WorldCommand)

(def DrawCommand basic/DrawCommand)
(def OrientCommand basic/OrientCommand)
(def InitialPlacementCommand basic/InitialPlacementCommand)

(def ErrorShape result/ErrorShape)
(def StateSuccessResult result/StateSuccessResult)
(def CommandValidationSuccessResult result/CommandValidationSuccessResult)
(def SuccessResult result/SuccessResult)
(def FailureResult result/FailureResult)
(def Result result/Result)

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

(defn valid-result? [value]
  (result/valid-result? value))

(defn explain-result [value]
  (result/explain-result value))
