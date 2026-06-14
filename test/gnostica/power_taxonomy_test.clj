(ns gnostica.power-taxonomy-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]
            [gnostica.move-selection.registry :as move-registry]
            [gnostica.power-taxonomy :as taxonomy]
            [malli.core :as m]))

(defn- map-entries [map-form]
  (if (map? (second map-form))
    (nnext map-form)
    (next map-form)))

(defn- top-level-map-form [schema]
  (some #(when (and (vector? %)
                    (= :map (first %)))
           %)
        (tree-seq coll? seq (m/form schema))))

(defn- top-level-field-entry [schema field]
  (some #(when (= field (first %)) %)
        (map-entries (top-level-map-form schema))))

(defn- field-enum-values [schema field]
  (let [entry (top-level-field-entry schema field)
        field-schema (if (map? (second entry))
                       (nth entry 2)
                       (second entry))]
    (vec (rest field-schema))))

(deftest world-command-contract-copied-powers-follow-taxonomy
  (doseq [field [:copied-power :power]]
    (testing field
      (is (= taxonomy/world-copied-power-values
             (field-enum-values (game-state/command-schema :world) field))))))

(deftest move-selection-card-powers-follow-taxonomy
  (doseq [card cards/deck
          :let [card-id (:id card)
                powers (taxonomy/powers-for-card card)]
          :when (seq powers)]
    (testing card-id
      (is (= powers
             (move-registry/power-ids-for-card card)))
      (is (= (taxonomy/world-copied-power-ids-for-card card)
             (move-registry/copied-power-ids-for-card card)))
      (is (= (taxonomy/fool-play-power-ids-for-card card)
             (move-registry/fool-play-power-ids-for-card card))))))
