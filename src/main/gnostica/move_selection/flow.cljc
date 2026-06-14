(ns gnostica.move-selection.flow
  (:require [clojure.set :as set]
            [gnostica.move-selection.context :as context]
            [gnostica.move-selection.flow.advancement :as advancement]
            [gnostica.move-selection.flow.requirements :as requirements]
            [gnostica.move-selection.flow.stages :as stages]
            [gnostica.move-selection.power-context :as power]
            [gnostica.move-selection.state :as selection]))

(def required-context-keys
  (set/union requirements/required-context-keys
             stages/required-context-keys
             advancement/required-context-keys))

(defn make-context [deps]
  (context/make "gnostica.move-selection.flow" required-context-keys deps))

(def requirement-complete? requirements/requirement-complete?)
(def move-requirements requirements/move-requirements)
(def first-missing-requirement requirements/first-missing-requirement)
(def move-missing-fields requirements/move-missing-fields)
(def stage-for-requirement stages/stage-for-requirement)
(def composite-current-action-complete?
  advancement/composite-current-action-complete?)
(def composite-current-action advancement/composite-current-action)
(def sword-major-current-action-complete?
  advancement/sword-major-current-action-complete?)
(def sword-major-current-action advancement/sword-major-current-action)
(def devil-current-action-complete? advancement/devil-current-action-complete?)
(def devil-current-action advancement/devil-current-action)

(def stored-fool-reveal-actions power/stored-fool-reveal-actions)
(def advance-move-selection-steps advancement/advance-move-selection-steps)

(defn refresh-move-selection [ctx db]
  (let [{:keys [source params] :as move-selection} (selection/move-selection db)]
    (assoc db :move-selection
           (if source
             (let [missing (first-missing-requirement ctx db source params)]
               (assoc move-selection
                      :stage (if missing
                               (stage-for-requirement ctx db source params missing)
                               :confirm)))
             (assoc move-selection :stage :source)))))
