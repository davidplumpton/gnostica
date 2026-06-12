(ns gnostica.move-selection.staging-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.move-selection.staging :as staging]))

(def stale-params
  {:source-board-index 0
   :hand-card-id "cups2"
   :piece-id :rose-scout
   :power :cup
   :copied-board-index 3
   :copied-power :rod
   :rod-mode :push-piece
   :disc-target-kind :piece
   :sword-target-kind :piece
   :disc-action-count 2
   :sword-action-count 2
   :devil-action-count 3
   :major-action-count 2
   :minion-orientation :north
   :fool-reveal-count 2
   :fool-reveals [{:card-id "cups2" :choice :skip :action {}}]
   :fool-active-reveal {:index 2 :card-id "wands2" :choice :play}
   :fool-shuffle-fn :shuffle-token
   :fool-play-power :cup
   :high-priestess-redraw-count 2
   :redraws [{:discard-card-ids ["cups2"] :draw-count 1}]
   :judgement-card-ids ["cups2"]
   :major-actions [{:power :rod}]
   :target-board-index 4
   :target-wasteland {:kind :wasteland :row 1 :col 0}
   :target-piece-id :indigo-target
   :territory-card-source :hand
   :one-point-card-id "cupsace"
   :replacement-card-source :hand
   :replacement-card-id "cupsking"
   :orientation :east
   :distance 1
   :damage 1
   :sun-disc-mode :piece
   :sun-disc-target-piece-id :rose-target
   :sun-disc-target-board-index 5
   :sun-disc-replacement-card-id "coins2"
   :sun-disc-orientation :west
   :hermit-destination-board-index 8
   :hermit-destination-wasteland {:kind :wasteland :row 2 :col 3}})

(def stale-target-keys
  [:target-board-index
   :target-wasteland
   :target-piece-id
   :territory-card-source
   :one-point-card-id
   :replacement-card-source
   :replacement-card-id
   :orientation
   :damage])

(defn absent? [m k]
  (not (contains? m k)))

(deftest changing-source-clears-dependent-staged-params
  (testing "reselecting the same source territory preserves dependent choices"
    (is (= stale-params
           (staging/set-source-board-index stale-params 0))))
  (testing "changing the source territory clears source-scoped choices"
    (let [next-params (staging/set-source-board-index stale-params 2)]
      (is (= 2 (:source-board-index next-params)))
      (is (= "cups2" (:hand-card-id next-params)))
      (is (every? #(absent? next-params %)
                  [:piece-id
                   :power
                   :copied-board-index
                   :copied-power
                   :rod-mode
                   :disc-target-kind
                   :sword-target-kind
                   :major-actions]))
      (is (every? #(absent? next-params %)
                  (concat stale-target-keys
                          [:distance
                           :sun-disc-mode
                           :hermit-destination-board-index]))))))

(deftest changing-power-clears-power-scoped-staged-params
  (testing "reselecting the same power is a no-op"
    (is (= stale-params
           (staging/set-power stale-params :cup))))
  (testing "changing power keeps the chosen source and minion but clears child choices"
    (let [next-params (staging/set-power stale-params :rod)]
      (is (= :rod (:power next-params)))
      (is (= 0 (:source-board-index next-params)))
      (is (= :rose-scout (:piece-id next-params)))
      (is (every? #(absent? next-params %)
                  [:copied-board-index
                   :copied-power
                   :rod-mode
                   :disc-target-kind
                   :sword-target-kind
                   :fool-reveal-count
                   :major-actions]))
      (is (every? #(absent? next-params %)
                  (concat stale-target-keys
                          [:distance
                           :sun-disc-mode
                           :hermit-destination-board-index]))))))

(deftest changing-target-kind-clears-target-and-replacement-params
  (doseq [[label f new-kind] [[:disc staging/set-disc-target-kind :territory]
                              [:sword staging/set-sword-target-kind :territory]]]
    (testing label
      (let [next-params (f stale-params new-kind)]
        (is (= new-kind (get next-params (keyword (str (name label)
                                                       "-target-kind")))))
        (is (every? #(absent? next-params %) stale-target-keys))
        (is (= :cup (:power next-params)))
        (is (= 2 (:disc-action-count next-params)))))))

(deftest changing-world-copy-and-copied-power-clears-child-params
  (testing "changing the copied board clears copied power and child state"
    (let [next-params (staging/set-world-copy stale-params 5)]
      (is (= 5 (:copied-board-index next-params)))
      (is (every? #(absent? next-params %)
                  [:copied-power
                   :rod-mode
                   :disc-target-kind
                   :sword-target-kind
                   :fool-reveal-count
                   :major-actions]))
      (is (every? #(absent? next-params %) stale-target-keys))))
  (testing "changing the copied power keeps the copied board but clears child state"
    (let [next-params (staging/set-world-copied-power stale-params :disc)]
      (is (= 3 (:copied-board-index next-params)))
      (is (= :disc (:copied-power next-params)))
      (is (every? #(absent? next-params %)
                  [:rod-mode
                   :disc-target-kind
                   :sword-target-kind
                   :fool-reveal-count
                   :major-actions]))
      (is (every? #(absent? next-params %) stale-target-keys)))))

(deftest changing-fool-reveal-and-play-power-clears-child-params
  (testing "changing reveal count clears reveal state and any child play choices"
    (let [next-params (staging/set-fool-reveal-count stale-params 1)]
      (is (= 1 (:fool-reveal-count next-params)))
      (is (every? #(absent? next-params %)
                  [:fool-reveals
                   :fool-active-reveal
                   :fool-shuffle-fn
                   :fool-play-power]))
      (is (every? #(absent? next-params %) stale-target-keys))))
  (testing "changing the revealed-card play power keeps the active reveal"
    (let [next-params (staging/set-fool-play-power stale-params :rod)]
      (is (= :rod (:fool-play-power next-params)))
      (is (= (:fool-active-reveal stale-params)
             (:fool-active-reveal next-params)))
      (is (every? #(absent? next-params %)
                  (concat stale-target-keys
                          [:rod-mode
                           :disc-target-kind
                           :sword-target-kind
                           :major-actions]))))))
