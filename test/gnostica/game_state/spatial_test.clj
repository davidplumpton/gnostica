(ns gnostica.game-state.spatial-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.game-state.spatial :as spatial]))

(deftest coordinate-map-normalizes-supported-shapes
  (testing "maps and sequential coordinates normalize to row/col maps"
    (is (= {:row 1 :col 2}
           (spatial/coordinate-map {:row 1 :col 2 :extra true})))
    (is (= {:row 1 :col 2}
           (spatial/coordinate-map [1 2]))))
  (testing "invalid coordinates do not normalize"
    (is (nil? (spatial/coordinate-map {:row 1})))
    (is (nil? (spatial/coordinate-map ["1" 2])))
    (is (nil? (spatial/coordinate-map nil)))))

(deftest coordinate-comparison-accepts-map-and-vector-shapes
  (is (true? (spatial/same-coordinate? [1 2] {:row 1 :col 2})))
  (is (false? (spatial/same-coordinate? [1 2] {:row 2 :col 1})))
  (is (false? (spatial/same-coordinate? [1 2] nil))))

(deftest target-and-offset-coordinate-share-cardinal-semantics
  (is (= {:row 1 :col 2}
         (spatial/target-coordinate [1 2] :up)))
  (is (= {:row 0 :col 2}
         (spatial/target-coordinate [1 2] :north)))
  (is (= {:row 1 :col 5}
         (spatial/offset-coordinate [1 2] :east 3)))
  (is (nil? (spatial/target-coordinate [1 2] :diagonal)))
  (is (nil? (spatial/offset-coordinate [1 2] :east "3"))))

(deftest target-summary-keeps-command-target-identity-fields
  (is (= {:kind :piece
          :piece-id "red-small-1"
          :board-index 4
          :row 1
          :col 1}
         (spatial/target-summary {:kind :piece
                                  :piece-id "red-small-1"
                                  :board-index 4
                                  :row 1
                                  :col 1
                                  :orientation :north
                                  :ignored true}))))

(deftest move-piece-to-space-preserves-one-location-shape
  (let [piece {:id "red-small-1"
               :player-id :red
               :size :small
               :orientation :north
               :space {:kind :wasteland :row 0 :col 1}}]
    (is (= {:id "red-small-1"
            :player-id :red
            :size :small
            :orientation :east
            :space-index 0}
           (spatial/move-piece-to-space piece {:space-index 0} :east)))
    (is (= {:id "red-small-1"
            :player-id :red
            :size :small
            :orientation :north
            :space {:kind :wasteland :row 2 :col 1}}
           (spatial/move-piece-to-space (assoc piece :space-index 4)
                                        {:space {:kind :wasteland :row 2 :col 1}}
                                        nil)))))
