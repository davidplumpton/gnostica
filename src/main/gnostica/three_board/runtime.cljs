(ns gnostica.three-board.runtime)

(def expected-three-revision "128")

(defn three-runtime []
  (when (exists? js/THREE)
    js/THREE))

(defn three-revision []
  (some-> (three-runtime) (.-REVISION)))

(defn orbit-controls-runtime []
  (when-let [three (three-runtime)]
    (.-OrbitControls three)))

(defn runtime-status []
  (cond
    (not (three-runtime))
    {:ok? false
     :code :three-missing
     :message "Three.js is unavailable; check the pinned CDN script before /js/main.js."}

    (not= expected-three-revision (three-revision))
    {:ok? false
     :code :three-revision-mismatch
     :message (str "Three.js revision "
                   (or (three-revision) "unknown")
                   " is incompatible; expected r"
                   expected-three-revision
                   " from the pinned CDN script before /js/main.js.")}

    (not (orbit-controls-runtime))
    {:ok? false
     :code :orbit-controls-missing
     :message "Three.js OrbitControls are unavailable; check the pinned CDN control script before /js/main.js."}

    :else
    {:ok? true
     :code :ready
     :message "Three.js r128 runtime is ready."}))

(defn available? []
  (true? (:ok? (runtime-status))))
