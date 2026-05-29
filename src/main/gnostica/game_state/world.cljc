(ns gnostica.game-state.world
  (:require [gnostica.cards :as cards]
            [gnostica.game-state.composite :as composite]
            [gnostica.game-state.core :as core]
            [gnostica.game-state.cup :as cup]
            [gnostica.game-state.disc :as disc]
            [gnostica.game-state.draw :as draw]
            [gnostica.game-state.major :as major]
            [gnostica.game-state.major-power :as major-power]
            [gnostica.game-state.manipulation :as manipulation]
            [gnostica.game-state.rod :as rod]
            [gnostica.game-state.sword :as sword]))

(def world-card-id "world")

(def suit-powers #{:cup :rod :disc :sword})

(def default-suit-copy-card-ids
  #{"wheeloffortune"
    "strength"
    "justice"
    "death"
    "tower"
    "star"})

(defn- major-card? [card]
  (= :major (:arcana card)))

(defn- world-card? [card]
  (= world-card-id (:id card)))

(defn- eligible-copied-cell? [cell]
  (let [card (:card cell)]
    (and (major-card? card)
         (not (world-card? card)))))

(defn world-major-territories [state]
  (->> (:board state)
       (filter eligible-copied-cell?)
       (mapv (fn [cell]
               {:board-index (:index cell)
                :row (:row cell)
                :col (:col cell)
                :card-id (get-in cell [:card :id])
                :title (get-in cell [:card :title])
                :powers (get-in cell [:card :gnostica-icons] [])}))))

(defn- copied-board-index [command]
  (or (:copied-board-index command)
      (:copied-territory-board-index command)
      (get-in command [:copy :board-index])
      (get-in command [:copied-territory :board-index])))

(defn- source-summary [source-result]
  (core/source-summary (:source source-result)))

(defn- resolve-world-source
  ([state command]
   (resolve-world-source state command {}))
  ([state command source-opts]
   (let [source-result (major/resolve-major-source state command source-opts)]
    (cond
      (not (:ok? source-result))
      source-result

      (not (world-card? (:source-card source-result)))
      (core/failure :world-source-required
                    "World delegation requires World as the paid source card."
                    {:card-id (get-in source-result [:source-card :id])
                     :source (source-summary source-result)})

      :else
      source-result))))

(defn- resolve-copied-cell [state command]
  (let [board-index (copied-board-index command)
        cell (core/board-cell-by-index state board-index)
        card (:card cell)]
    (cond
      (nil? board-index)
      (core/failure :missing-world-copy
                    "World requires a copied major territory board index."
                    {:command (select-keys command [:copied-board-index
                                                    :copied-territory-board-index
                                                    :copy
                                                    :copied-territory])})

      (nil? cell)
      (core/failure :invalid-world-copy
                    "World can only copy an existing board territory."
                    {:board-index board-index})

      (not (major-card? card))
      (core/failure :invalid-world-copy
                    "World can only copy a major arcana territory."
                    {:board-index board-index
                     :card-id (:id card)})

      (world-card? card)
      (core/failure :invalid-world-copy
                    "World cannot copy World."
                    {:board-index board-index
                     :card-id (:id card)})

      :else
      {:ok? true
       :cell cell
       :card card})))

(defn- clean-command [command]
  (dissoc command
          :copied-board-index
          :copied-territory-board-index
          :copied-territory
          :copy
          :copied-power))

(defn- clean-suit-command [command]
  (dissoc (clean-command command) :power))

(defn- copied-power [command]
  (or (:copied-power command)
      (:power command)))

(defn- suit-powers-for-card [card]
  (cond-> []
    (cards/cup-card? card) (conj :cup)
    (cards/rod-card? card) (conj :rod)
    (cards/disc-card? card) (conj :disc)
    (cards/sword-card? card) (conj :sword)))

(defn- resolve-suit-power [command copied-card]
  (let [requested (copied-power command)
        powers (suit-powers-for-card copied-card)
        power-set (set powers)]
    (cond
      (and requested (not (contains? suit-powers requested)))
      (core/failure :invalid-world-copied-power
                    "World copied powers must name a supported power keyword."
                    {:power requested
                     :supported-powers suit-powers})

      (and requested (not (contains? power-set requested)))
      (core/failure :world-copied-power-unavailable
                    "The copied major territory does not provide the selected suit power."
                    {:card-id (:id copied-card)
                     :power requested
                     :available-powers powers})

      requested
      {:ok? true
       :power requested}

      (and (= 1 (count powers))
           (contains? default-suit-copy-card-ids (:id copied-card)))
      {:ok? true
       :power (first powers)}

      :else
      nil)))

(defn- source-opts [source-result copied-card]
  {:source-card (:source-card source-result)
   :power-card copied-card
   :source-card-already-discarded? (:source-card-already-discarded? source-result)
   :allow-major-minion? true})

(defn- delegated-source-opts [source-result]
  {:source-card (:source-card source-result)
   :source-card-already-discarded? (:source-card-already-discarded? source-result)})

(defn- paid-source-card-id [source-result]
  (get-in source-result [:source-card :id]))

(defn- validate-source-piece-minion [source-result command]
  (let [piece-id (get-in command [:source :piece-id])
        minion-ids (set (:minion-ids source-result))]
    (cond
      (nil? piece-id)
      (core/failure :missing-major-minion
                    "World copied suit powers require an acting minion in the source map."
                    {:source (core/source-summary (:source source-result))})

      (not (contains? minion-ids piece-id))
      (core/failure :invalid-major-minion
                    "The acting piece is not a minion for the World source."
                    {:piece-id piece-id
                     :available-minion-ids (vec (sort-by str minion-ids))
                     :source (core/source-summary (:source source-result))})

      :else
      {:ok? true
       :piece-id piece-id})))

(defn- apply-copied-suit-power [state command source-result copied-card power]
  (let [minion-result (validate-source-piece-minion source-result command)
        opts {:source-opts (source-opts source-result copied-card)
              :required-card-id (paid-source-card-id source-result)}]
    (if-not (:ok? minion-result)
      minion-result
      (case power
        :cup
        (cup/apply-cup-move-with-opts
         state
         (assoc (clean-suit-command command)
                :cup-variant (cards/cup-variant copied-card))
         {:source-opts (:source-opts opts)})

        :rod
        (rod/apply-rod-move-with-opts
         state
         (assoc (clean-suit-command command)
                :rod-variant (cards/rod-variant copied-card))
         {:source-opts (:source-opts opts)})

        :disc
        (disc/apply-disc-move-with-opts
         state
         (assoc (clean-suit-command command)
                :disc-variant (cards/disc-variant copied-card))
         {:source-opts (:source-opts opts)})

        :sword
        (sword/apply-sword-move-with-opts
         state
         (assoc (clean-suit-command command)
                :sword-variant (cards/sword-variant copied-card))
         opts)))))

(defn- apply-copied-card-power [state command source-result copied-card]
  (let [command (clean-command command)
        source-card-id (paid-source-card-id source-result)
        opts {:source-opts (delegated-source-opts source-result)}]
    (case (:id copied-card)
      "fool"
      (draw/apply-fool-move-with-source-card-id state command source-card-id opts)

      "high-priestess"
      (draw/apply-high-priestess-move-with-source-card-id state
                                                          command
                                                          source-card-id
                                                          opts)

      "empress"
      (composite/apply-empress-move-with-source-card-id state
                                                        command
                                                        source-card-id
                                                        copied-card
                                                        opts)

      "emperor"
      (composite/apply-emperor-move-with-source-card-id state
                                                        command
                                                        source-card-id
                                                        copied-card
                                                        opts)

      "hierophant"
      (manipulation/apply-hierophant-move-with-source-card-id state
                                                              command
                                                              source-card-id
                                                              opts)

      "lovers"
      (composite/apply-lovers-move-with-source-card-id state
                                                       command
                                                       source-card-id
                                                       copied-card
                                                       opts)

      "chariot"
      (composite/apply-chariot-move-with-source-card-id state
                                                        command
                                                        source-card-id
                                                        copied-card
                                                        opts)

      "hermit"
      (manipulation/apply-hermit-move-with-source-card-id state
                                                          command
                                                          source-card-id
                                                          opts)

      "hangedman"
      (composite/apply-hanged-man-move-with-source-card-id state
                                                           command
                                                           source-card-id
                                                           copied-card
                                                           opts)

      "temperance"
      (composite/apply-temperance-move-with-source-card-id state
                                                           command
                                                           source-card-id
                                                           copied-card
                                                           opts)

      "devil"
      (manipulation/apply-devil-move-with-source-card-id state
                                                         command
                                                         source-card-id
                                                         opts)

      "moon"
      (sword/apply-moon-move-with-opts
       state
       command
       {:required-card-id source-card-id
        :power-card copied-card
        :source-opts (:source-opts opts)})

      "sun"
      (let [minion-result (validate-source-piece-minion source-result command)]
        (if-not (:ok? minion-result)
          minion-result
          (disc/apply-sun-move-with-opts
           state
           command
           {:source-opts (source-opts source-result copied-card)})))

      "judgement"
      (draw/apply-judgement-move-with-source-card-id state
                                                     command
                                                     source-card-id
                                                     opts)

      (core/failure :world-copied-power-unavailable
                    "The copied major territory does not have an implemented World delegation."
                    {:card-id (:id copied-card)
                     :source (source-summary source-result)}))))

(defn apply-world-move
  ([state command]
   (apply-world-move state command {}))
  ([state command {:keys [source-opts]}]
  (let [source-result (resolve-world-source state command source-opts)
        copied-result (resolve-copied-cell state command)]
    (cond
      (not (:ok? source-result))
      source-result

      (not (:ok? copied-result))
      copied-result

      :else
      (let [copied-card (:card copied-result)
            suit-result (resolve-suit-power command copied-card)]
        (cond
          (and (map? suit-result) (not (:ok? suit-result)))
          suit-result

          (and (:ok? suit-result) (:power suit-result))
          (apply-copied-suit-power state
                                   command
                                   source-result
                                   copied-card
                                   (:power suit-result))

          :else
          (apply-copied-card-power state
                                   command
                                   source-result
                                   copied-card)))))))

(defmethod major-power/apply-card-power "world"
  [state command _card {:keys [source-opts]}]
  (apply-world-move state command {:source-opts source-opts}))
