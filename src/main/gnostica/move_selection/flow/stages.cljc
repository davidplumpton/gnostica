(ns gnostica.move-selection.flow.stages
  (:require [gnostica.move-selection.context :as context]
            [gnostica.move-selection.power-context :as power]))

(def required-context-keys
  #{:valid-wasteland-target?})

(defn make-context [deps]
  (context/make "gnostica.move-selection.flow.stages"
                required-context-keys
                deps))

(defn- call [ctx key & args]
  (apply context/call ctx key args))

(defn stage-for-requirement [ctx db source-id params requirement]
  (case requirement
    :source-board-index :source-territory
    :hand-card-id :hand-card
    :power :power
    :copied-board-index :world-copy
    :copied-power :copied-power
    :piece-id :piece
    :rod-mode :rod-mode
    :disc-action-count :disc-action-count
    :sword-action-count :sword-action-count
    :devil-action-count :devil-action-count
    :minion-orientation :minion-orientation
    :sun-disc-mode :sun-disc-mode
    :disc-target-kind :disc-target-kind
    :sword-target-kind :sword-target-kind
    :fool-reveal-count :fool-reveal-count
    :fool-reveal-card :fool-reveal-card
    :fool-reveal-choice :fool-reveal-choice
    :fool-play-power :fool-play-power
    :high-priestess-redraw-count :high-priestess-redraw-count
    :high-priestess-redraws :high-priestess-redraw
    :judgement-card-selection :judgement-card-selection
    :target-piece-id :target-piece
    :sun-disc-target-piece-id :target-piece
    :target-board-index :target
    :sun-disc-target-board-index :target
    :target-space :target
    :hermit-target-space :target
    :hermit-destination-space :hermit-destination
    :target-resolution (cond
                         (and (call ctx
                                    :valid-wasteland-target?
                                    db
                                    (:target-wasteland params))
                              (= :wheel-cup
                                 (power/selected-cup-variant
                                  db
                                  source-id
                                  params))
                              (nil? (:territory-card-source params)))
                         :territory-card-source

                         (call ctx :valid-wasteland-target?
                               db
                               (:target-wasteland params))
                         :one-point-card

                         :else
                         :orientation)
    :one-point-card-id :one-point-card
    :territory-card-source :territory-card-source
    :replacement-card-source :replacement-card-source
    :replacement-card-id :replacement-card
    :sun-disc-replacement-card-id :replacement-card
    :orientation :orientation
    :distance :distance
    :damage :damage
    :draw-count :draw-count
    :confirm))
