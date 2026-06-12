(ns gnostica.app-events-test
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [gnostica.app.ids :as app-ids]))

(defn- source-file [& path]
  (apply io/file path))

(defn- cljs-files-under [path]
  (->> (file-seq (source-file path))
       (filter #(.isFile %))
       (filter #(str/ends-with? (.getName %) ".cljs"))))

(defn- cljs-registration-files [root-file child-dir]
  (cons (source-file root-file)
        (cljs-files-under child-dir)))

(defn- id-names [ids]
  (set (map name ids)))

(defn- public-event-defs []
  (->> (re-seq #"(?m)^\(def\s+([^\s]+)\s+"
               (slurp (source-file "src/main/gnostica/app/events.cljs")))
       (map second)
       set))

(defn- subscribed-event-symbols [source]
  (->> (re-seq #"rf/subscribe\s+\[events/([^\]\s]+)" source)
       (map second)
       set))

(defn- ids-referenced-by [files]
  (->> files
       (mapcat #(re-seq #"ids/([A-Za-z0-9*+!_\-?]+)" (slurp %)))
       (map second)
       set))

(deftest events-namespace-exports-only-public-ids
  (let [expected (set/union (id-names app-ids/event-ids)
                            (id-names app-ids/public-subscription-ids))]
    (is (= expected (public-event-defs)))))

(deftest registration-load-points-reference-all-event-ids
  (let [expected (id-names app-ids/event-ids)
        referenced (ids-referenced-by
                    (cljs-registration-files
                     "src/main/gnostica/app/registrations/events.cljs"
                     "src/main/gnostica/app/registrations/events"))]
    (is (empty? (set/difference expected referenced)))))

(deftest registration-load-points-reference-all-subscription-ids
  (let [expected (id-names app-ids/subscription-ids)
        referenced (ids-referenced-by
                    (cljs-registration-files
                     "src/main/gnostica/app/registrations/subscriptions.cljs"
                     "src/main/gnostica/app/registrations/subscriptions"))]
    (is (empty? (set/difference expected referenced)))))

(deftest ui-subscriptions-use-composed-view-boundary
  (let [public-subscriptions (id-names app-ids/public-subscription-ids)
        ui-files (cons (source-file "src/main/gnostica/app.cljs")
                       (cljs-files-under "src/main/gnostica/ui"))
        subscriptions (->> ui-files
                           (mapcat #(subscribed-event-symbols (slurp %)))
                           set)]
    (is (empty? (set/difference subscriptions public-subscriptions)))))
