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
  (let [revision (three-revision)]
    (cond
      (not (three-runtime))
      {:ok? false
       :code :three-missing
       :revision nil
       :expected-revision expected-three-revision
       :message "Three.js is unavailable; check the pinned CDN script before /js/main.js."}

      (not= expected-three-revision revision)
      {:ok? false
       :code :three-revision-mismatch
       :revision revision
       :expected-revision expected-three-revision
       :message (str "Three.js revision "
                     (or revision "unknown")
                     " is incompatible; expected r"
                     expected-three-revision
                     " from the pinned CDN script before /js/main.js.")}

      (not (orbit-controls-runtime))
      {:ok? false
       :code :orbit-controls-missing
       :revision revision
       :expected-revision expected-three-revision
       :message "Three.js OrbitControls are unavailable; check the pinned CDN control script before /js/main.js."}

      :else
      {:ok? true
       :code :ready
       :revision revision
       :expected-revision expected-three-revision
       :message "Three.js r128 runtime is ready."})))

(defn available? []
  (true? (:ok? (runtime-status))))
