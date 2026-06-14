(ns gnostica.move-selection.registry
  (:require [gnostica.game-state :as game-state]
            [gnostica.power-taxonomy :as taxonomy]))

(def move-source-order
  [:activate-territory
   :play-hand-card
   :draw-cards
   :orient-piece
   :place-initial-small])

(def move-source-definitions
  {:activate-territory
   {:id :activate-territory
    :label "Activate territory"
    :summary "Use a board card through one of your pieces."
    :requirements [:source-board-index :piece-id :target-space :target-resolution]}
   :play-hand-card
   {:id :play-hand-card
    :label "Play hand card"
    :summary "Discard a hand card and use its power through a piece."
    :requirements [:hand-card-id :piece-id :target-space :target-resolution]}
   :draw-cards
   {:id :draw-cards
    :label "Draw cards"
    :summary "Discard hand cards, then draw toward the six-card hand limit."
    :requirements [:draw-count]}
   :orient-piece
   {:id :orient-piece
    :label "Orient piece"
    :summary "Turn one of your pieces to a legal direction."
    :requirements [:piece-id :orientation]}
   :place-initial-small
   {:id :place-initial-small
    :label "Place first piece"
    :summary "Special rule: with no pieces, put your first small piece on an empty territory or wasteland."
    :requirements [:target-space :orientation]}})

(def copied-suit-powers
  taxonomy/suit-power-set)

(def composite-major-card-powers
  taxonomy/composite-major-card-powers)

(def composite-major-powers
  taxonomy/composite-major-power-set)

(def sword-major-card-powers
  taxonomy/sword-major-card-powers)

(def sword-major-powers
  taxonomy/sword-major-power-set)

(def move-power-order
  [:empress :emperor :lovers :chariot :hanged-man :temperance
   :justice :death :tower :moon
   :cup :rod :disc :sun :sword
   :fool :high-priestess :judgement :hierophant :hermit :devil :world])

(def move-power-registry
  {:empress {:id :empress
             :label "Empress"
             :control-kind :composite-major
             :command-builder :composite-major
             :transition-fn game-state/apply-empress-move}
   :emperor {:id :emperor
             :label "Emperor"
             :control-kind :composite-major
             :command-builder :composite-major
             :transition-fn game-state/apply-emperor-move
             :preview? true}
   :lovers {:id :lovers
            :label "Lovers"
            :control-kind :composite-major
            :command-builder :composite-major
            :transition-fn game-state/apply-lovers-move
            :preview? true}
   :chariot {:id :chariot
             :label "Chariot"
             :control-kind :composite-major
             :command-builder :composite-major
             :transition-fn game-state/apply-chariot-move
             :preview? true}
   :hanged-man {:id :hanged-man
                :label "Hanged Man"
                :control-kind :composite-major
                :command-builder :composite-major
                :transition-fn game-state/apply-hanged-man-move
                :preview? true}
   :temperance {:id :temperance
                :label "Temperance"
                :control-kind :composite-major
                :command-builder :composite-major
                :transition-fn game-state/apply-temperance-move}
   :justice {:id :justice
             :label "Justice"
             :control-kind :sword-major
             :command-builder :sword-major
             :transition-fn game-state/apply-sword-move}
   :death {:id :death
           :label "Death"
           :control-kind :sword-major
           :command-builder :sword-major
           :transition-fn game-state/apply-sword-move}
   :tower {:id :tower
           :label "Tower"
           :control-kind :sword-major
           :command-builder :sword-major
           :transition-fn game-state/apply-sword-move}
   :moon {:id :moon
          :label "Moon"
          :control-kind :sword-major
          :command-builder :sword-major
          :transition-fn game-state/apply-sword-move
          :preview? true}
   :cup {:id :cup
         :label "Cup"
         :control-kind :static
         :control-groups [:cup]
         :renderer-control-keys [:cup]
         :command-builder :cup
         :transition-fn game-state/apply-cup-move}
   :rod {:id :rod
         :label "Rod"
         :control-kind :static
         :control-groups [:rod]
         :renderer-control-keys [:rod]
         :command-builder :rod
         :transition-fn game-state/apply-rod-move
         :preview? true}
   :disc {:id :disc
          :label "Disc"
          :control-kind :static
          :control-groups [:disc]
          :renderer-control-keys [:disc]
          :command-builder :disc
          :transition-fn game-state/apply-disc-move}
   :sun {:id :sun
         :label "Sun"
         :control-kind :static
         :control-groups [:sun]
         :renderer-control-keys [:sun]
         :command-builder :sun
         :transition-fn game-state/apply-sun-move}
   :sword {:id :sword
           :label "Sword"
           :control-kind :static
           :control-groups [:sword]
           :renderer-control-keys [:sword]
           :command-builder :sword
           :transition-fn game-state/apply-sword-move}
   :fool {:id :fool
          :label "Fool"
          :control-kind :fool
          :renderer-control-keys [:fool-reveal-count
                                  :fool-reveal-card
                                  :fool-reveal-decision
                                  :fool-play-power]
          :command-builder :fool
          :transition-fn game-state/apply-fool-move}
   :high-priestess {:id :high-priestess
                    :label "High Priestess"
                    :control-kind :static
                    :control-groups [:high-priestess-redraw-count
                                     :high-priestess-redraws]
                    :renderer-control-keys [:high-priestess-redraw-count
                                            :high-priestess-redraws]
                    :command-builder :high-priestess
                    :transition-fn game-state/apply-high-priestess-move}
   :judgement {:id :judgement
               :label "Judgement"
               :control-kind :static
               :control-groups [:judgement-card-selection]
               :renderer-control-keys [:judgement-card-selection]
               :command-builder :judgement
               :transition-fn game-state/apply-judgement-move}
   :hierophant {:id :hierophant
                :label "Hierophant"
                :control-kind :static
                :control-groups [:piece-orientation-major]
                :renderer-control-keys [:piece-orientation-major]
                :command-builder :piece-orientation
                :transition-fn game-state/apply-hierophant-move}
   :hermit {:id :hermit
            :label "Hermit"
            :control-kind :static
            :control-groups [:hermit]
            :renderer-control-keys [:hermit]
            :command-builder :hermit
            :transition-fn game-state/apply-hermit-move
            :preview? true}
   :devil {:id :devil
           :label "Devil"
           :control-kind :static
           :control-groups [:devil]
           :renderer-control-keys [:devil]
           :command-builder :devil
           :transition-fn game-state/apply-devil-move}
   :world {:id :world
           :label "World"
           :control-kind :world
           :renderer-control-keys [:world-copy :world-copied-power]
           :command-builder :world
           :transition-fn game-state/apply-world-move}})

(def move-power-definitions
  (into {}
        (map (fn [[power definition]]
               [power (select-keys definition [:id :label])]))
        move-power-registry))

(defn power-definition [power]
  (get move-power-registry power))

(defn power-label [power]
  (get-in move-power-registry [power :label] (name power)))

(defn power-command-builder [power]
  (:command-builder (power-definition power)))

(defn power-transition-fn [power]
  (:transition-fn (power-definition power)))

(defn power-control-kind [power]
  (:control-kind (power-definition power)))

(defn power-control-groups [power]
  (vec (:control-groups (power-definition power))))

(defn previewable-power? [power]
  (true? (:preview? (power-definition power))))

(defn power-ids-for-card [card]
  (taxonomy/powers-for-card card))

(defn copied-power-ids-for-card [card]
  (taxonomy/world-copied-power-ids-for-card card))

(defn fool-play-power-ids-for-card [card]
  (taxonomy/fool-play-power-ids-for-card card))

(def control-renderer-order
  [:source-board
   :hand-card
   :piece
   :power
   :major-action-count
   :sword-action-count
   :world-copy
   :world-copied-power
   :rod
   :cup
   :disc
   :sun
   :sword
   :fool-reveal-count
   :fool-reveal-card
   :fool-reveal-decision
   :fool-play-power
   :high-priestess-redraw-count
   :high-priestess-redraws
   :judgement-card-selection
   :piece-orientation-major
   :hermit
   :devil
   :target-piece
   :minion-orientation
   :discard-cards
   :draw-count
   :orientation
   :target-space])

(def control-renderer-definitions
  (into {}
        (map (fn [control-type]
               [control-type {:id control-type
                              :renderer-key control-type}]))
        control-renderer-order))

(defn control-renderer [control-type]
  (get control-renderer-definitions control-type))

(defn control-renderer-key [control-type]
  (:renderer-key (control-renderer control-type)))

(defn control-renderer-types []
  (set control-renderer-order))
