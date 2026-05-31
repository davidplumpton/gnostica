(ns gnostica.move-selection.context
  (:require [clojure.string :as str]))

(defn- sorted-keys [keys]
  (sort-by pr-str keys))

(defn- format-keys [keys]
  (str/join ", " (map pr-str (sorted-keys keys))))

(defn- context-label [ctx]
  (or (::name ctx) "move-selection helper"))

(defn- context-error [code message data]
  (let [required-keys (:required-keys data)]
    (ex-info message
             (assoc data
                    :code code
                    :required-keys (vec (sorted-keys required-keys))))))

(defn make [context-name required-keys deps]
  (let [required-keys (set required-keys)
        missing-keys (remove #(contains? deps %) required-keys)
        nil-keys (filter #(and (contains? deps %) (nil? (get deps %)))
                         required-keys)]
    (when (or (seq missing-keys) (seq nil-keys))
      (let [message (str context-name
                         " context missing required dependencies"
                         (when (seq missing-keys)
                           (str ": " (format-keys missing-keys)))
                         (when (seq nil-keys)
                           (str (when (seq missing-keys) ";")
                                " nil dependencies: "
                                (format-keys nil-keys))))]
        (throw
         (context-error
          :move-selection-context/missing-dependencies
          message
          {:context context-name
           :missing-keys (vec (sorted-keys missing-keys))
           :nil-keys (vec (sorted-keys nil-keys))
           :required-keys required-keys}))))
    (assoc deps
           ::name context-name
           ::required-keys required-keys)))

(defn value [ctx key]
  (if (and (contains? ctx key) (some? (get ctx key)))
    (get ctx key)
    (throw
     (context-error
      :move-selection-context/missing-dependency
      (str (context-label ctx) " context missing dependency: " (pr-str key))
      {:context (context-label ctx)
       :missing-keys [key]
       :nil-keys (if (contains? ctx key) [key] [])
       :required-keys (::required-keys ctx #{})}))))

(defn call [ctx key & args]
  (let [f (value ctx key)]
    (when-not (ifn? f)
      (throw
       (context-error
        :move-selection-context/non-callable-dependency
        (str (context-label ctx)
             " context dependency is not callable: "
             (pr-str key))
        {:context (context-label ctx)
         :dependency key
         :dependency-value f
         :required-keys (::required-keys ctx #{})})))
    (apply f args)))
