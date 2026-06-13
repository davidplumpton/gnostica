(ns gnostica.move-selection.contexts
  (:require [gnostica.move-selection.commands :as commands]
            [gnostica.move-selection.confirmation :as confirmation]
            [gnostica.move-selection.controls :as controls]
            [gnostica.move-selection.flow :as flow]
            [gnostica.move-selection.preview :as preview]
            [gnostica.move-selection.ribbon :as ribbon]
            [gnostica.move-selection.targets :as targets]))

(defn control-context [deps]
  (controls/make-context deps))

(defn ribbon-context [deps]
  (ribbon/make-context deps))

(defn target-context [deps]
  (targets/make-context deps))

(defn command-context [deps]
  (commands/make-context deps))

(defn flow-context [deps]
  (flow/make-context deps))

(defn confirmation-context [deps]
  (confirmation/make-context deps))

(defn preview-context [deps]
  (preview/make-context deps))
