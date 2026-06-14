(ns gnostica.move-selection.prompt
  (:require [gnostica.move-selection.context :as context]
            [gnostica.move-selection.options :as options]))

(def required-context-keys
  #{:cup-move?
    :disc-move?
    :hermit-move?
    :move-params
    :move-selection
    :sun-disc-territory-target-stage?
    :sun-move?
    :sword-move?})

(defn make-context [deps]
  (context/make "gnostica.move-selection.prompt" required-context-keys deps))

(defn- call [ctx key & args]
  (apply context/call ctx key args))

(defn move-prompt [ctx db]
  (let [{:keys [source stage]} (call ctx :move-selection db)
        params (call ctx :move-params db)
        prompts options/requirement-prompts]
    (cond
      (nil? source) "Choose a move source."
      (= :confirm stage) "Confirm the selected move."
      (= :rejected stage) "Review or cancel the rejected move."
      (= :target stage) (cond
                          (call ctx :cup-move? db source params)
                          (:target-space prompts)

                          (call ctx :sun-move? db source params)
                          (if (call ctx
                                    :sun-disc-territory-target-stage?
                                    db
                                    source
                                    params)
                            (:target-board-index prompts)
                            (:target-space prompts))

                          (call ctx :disc-move? db source params)
                          (:target-board-index prompts)

                          (call ctx :sword-move? db source params)
                          (:target-board-index prompts)

                          (call ctx :hermit-move? db source params)
                          (:hermit-target-space prompts)

                          (= :place-initial-small source)
                          (:initial-target-space prompts)

                          :else
                          (:target-board-index prompts))
      (= :hermit-destination stage) (:hermit-destination-space prompts)
      (= :territory-card-source stage) (:territory-card-source prompts)
      (= :one-point-card stage) (:one-point-card-id prompts)
      (= :replacement-card-source stage) (:replacement-card-source prompts)
      (= :replacement-card stage) (:replacement-card-id prompts)
      :else (get {:source-territory (:source-board-index prompts)
                  :hand-card (:hand-card-id prompts)
                  :power (:power prompts)
                  :world-copy (:copied-board-index prompts)
                  :copied-power (:copied-power prompts)
                  :piece (:piece-id prompts)
                  :rod-mode (:rod-mode prompts)
                  :disc-action-count (:disc-action-count prompts)
                  :sword-action-count (:sword-action-count prompts)
                  :devil-action-count (:devil-action-count prompts)
                  :minion-orientation (:minion-orientation prompts)
                  :sun-disc-mode (:sun-disc-mode prompts)
                  :disc-target-kind (:disc-target-kind prompts)
                  :sword-target-kind (:sword-target-kind prompts)
                  :fool-reveal-count (:fool-reveal-count prompts)
                  :fool-reveal-card (:fool-reveal-card prompts)
                  :fool-reveal-choice (:fool-reveal-choice prompts)
                  :fool-play-power (:fool-play-power prompts)
                  :high-priestess-redraw-count
                  (:high-priestess-redraw-count prompts)
                  :high-priestess-redraw (:high-priestess-redraws prompts)
                  :judgement-card-selection (:judgement-card-selection prompts)
                  :target-piece (:target-piece-id prompts)
                  :orientation (:orientation prompts)
                  :distance (:distance prompts)
                  :damage (:damage prompts)
                  :draw-count (:draw-count prompts)}
                 stage
                 "Complete the move selection."))))
