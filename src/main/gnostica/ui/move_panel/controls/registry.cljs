(ns gnostica.ui.move-panel.controls.registry
  (:require [gnostica.ui.move-panel.controls.basic :as basic]
            [gnostica.ui.move-panel.controls.majors :as majors]
            [gnostica.ui.move-panel.controls.special :as special]
            [gnostica.ui.move-panel.controls.suits :as suits]
            [gnostica.ui.move-panel.renderer-registry :as renderer-registry]))

(def control-renderers
  (renderer-registry/assert-control-renderers
   (merge basic/control-renderers
          suits/control-renderers
          special/control-renderers
          majors/control-renderers)))
