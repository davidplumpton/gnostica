(ns gnostica.ui.move-panel.renderer-registry
  (:require [clojure.set :as set]))

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

(def control-renderer-keys
  (set control-renderer-order))

(defn control-renderer-types []
  control-renderer-keys)

(defn renderer-key-diff [renderer-map]
  (let [renderer-keys (set (keys renderer-map))]
    {:missing (set/difference control-renderer-keys renderer-keys)
     :unexpected (set/difference renderer-keys control-renderer-keys)}))

(defn assert-control-renderers [renderer-map]
  (let [{:keys [missing unexpected]} (renderer-key-diff renderer-map)]
    (when (or (seq missing) (seq unexpected))
      (throw (ex-info "Move panel renderer map does not match renderer registry"
                      {:missing missing
                       :unexpected unexpected})))
    renderer-map))
