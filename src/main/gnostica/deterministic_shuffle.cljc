(ns gnostica.deterministic-shuffle)

(def ^:private modulus 4294967296)
(def ^:private multiplier 1664525)
(def ^:private increment 1013904223)

(defn- numeric-seed [seed]
  (cond
    (number? seed)
    #?(:clj (long seed)
       :cljs (js/Math.floor seed))

    (keyword? seed)
    (numeric-seed (name seed))

    (string? seed)
    (reduce (fn [acc ch]
              (mod (+ (* 31 acc) (int ch)) modulus))
            0
            seed)

    :else
    0))

(defn normalize-seed [seed]
  (mod (numeric-seed seed) modulus))

(defn- next-state [state]
  (mod (+ (* multiplier state) increment) modulus))

(defn- next-index [state upper-bound]
  (let [state (next-state state)]
    [state (int (mod state upper-bound))]))

(defn shuffle-with-seed [seed coll]
  (loop [state (normalize-seed seed)
         index (dec (count coll))
         cards (vec coll)]
    (if (<= index 0)
      cards
      (let [[state swap-index] (next-index state (inc index))
            index-card (get cards index)
            swap-card (get cards swap-index)]
        (recur state
               (dec index)
               (assoc cards
                      index swap-card
                      swap-index index-card))))))

(defn shuffle-fn [seed]
  (fn seeded-shuffle [coll]
    (shuffle-with-seed seed coll)))
