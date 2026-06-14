(ns gnostica.move-selection.contexts
  (:require [gnostica.move-selection.commands :as commands]
            [gnostica.move-selection.confirmation :as confirmation]
            [gnostica.move-selection.controls :as controls]
            [gnostica.move-selection.flow :as flow]
            [gnostica.move-selection.prompt :as prompt]
            [gnostica.move-selection.preview :as preview]
            [gnostica.move-selection.ribbon :as ribbon]
            [gnostica.move-selection.target-options :as target-options]
            [gnostica.move-selection.targets :as targets]
            [gnostica.move-selection.targeting :as targeting]
            [gnostica.move-selection.updates :as updates]))

(defn control-context [deps]
  (controls/make-context deps))

(defn ribbon-context [deps]
  (ribbon/make-context deps))

(defn target-context [deps]
  (targets/make-context deps))

(defn targeting-context [deps]
  (targeting/make-context deps))

(defn command-context [deps]
  (commands/make-context deps))

(defn flow-context [deps]
  (flow/make-context deps))

(defn prompt-context [deps]
  (prompt/make-context deps))

(defn confirmation-context [deps]
  (confirmation/make-context deps))

(defn preview-context [deps]
  (preview/make-context deps))

(defn target-options-context [deps]
  (target-options/make-context deps))

(defn update-context [deps]
  (updates/make-context deps))
