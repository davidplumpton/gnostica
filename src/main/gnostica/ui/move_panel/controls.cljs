(ns gnostica.ui.move-panel.controls
  (:require [gnostica.move-selection.registry :as move-registry]
            [gnostica.ui.move-panel.controls.registry :as controls-registry]))

(def control-renderers
  controls-registry/control-renderers)

(defn- render-control-group [{:keys [type]} selection controls]
  (when-let [renderer (get control-renderers
                           (move-registry/control-renderer-key type))]
    [renderer selection controls]))

(defn active-controls [selection controls control-groups]
  [:<>
   (for [[index group] (map-indexed vector control-groups)]
     ^{:key (str index "-" (:type group) "-" (:power group) "-" (:action-power group))}
     [render-control-group group selection controls])])
