(ns gnostica.legal-targets-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.legal-targets :as legal-targets]))

(deftest status-presentation-preserves-target-css-classes
  (testing "classes are emitted only for active descriptors"
    (is (= " is-legal-target"
           (legal-targets/status-class {:active? true
                                        :status :legal})))
    (is (= " is-disabled-target"
           (legal-targets/status-class {:active? true
                                        :status :disabled})))
    (is (= "" (legal-targets/status-class {:active? true
                                           :status :idle})))
    (is (nil? (legal-targets/status-class {:active? false
                                           :status :legal})))
    (is (nil? (legal-targets/status-class false
                                          {:active? true
                                           :status :legal})))))

(deftest descriptor-reason-prefers-explicit-reason-over-error-message
  (is (= "Already selected"
         (legal-targets/reason {:reason "Already selected"
                                :error {:message "Fallback"}})))
  (is (= "Fallback"
         (legal-targets/reason {:error {:message "Fallback"}})))
  (is (nil? (legal-targets/reason nil))))

(deftest descriptor-lookups-share-card-board-piece-and-wasteland-keys
  (let [active-hand {:card-id "cups2"
                     :active? true
                     :status :legal}
        idle-hand {:card-id "coins3"
                   :active? false
                   :status :idle}
        territory {:board-index 7
                   :active? true
                   :status :disabled}
        wasteland {:row -1
                   :col 2
                   :active? true
                   :status :legal}
        piece {:piece-id :rose-small
               :active? true
               :status :legal}
        legal-targets {:territories [territory]
                       :wastelands [wasteland]
                       :pieces [piece]}]
    (is (= active-hand
           (legal-targets/descriptor-for-card [active-hand idle-hand]
                                              {:id "cups2"})))
    (is (nil? (legal-targets/descriptor-for-card [active-hand idle-hand]
                                                 {:id "coins3"})))
    (is (= territory
           (legal-targets/descriptor-for-target
            legal-targets
            {:kind :territory
             :board-index 7})))
    (is (= wasteland
           (legal-targets/descriptor-for-target
            legal-targets
            {:kind :wasteland
             :row -1
             :col 2})))
    (is (= piece
           (legal-targets/descriptor-for-target
            legal-targets
            {:kind :piece
             :piece-id :rose-small})))))

(deftest drag-preview-target-status-is-added-and-cleared-consistently
  (let [preview {:active? true
                 :target {:kind :territory
                          :board-index 1}}
        descriptor {:status :disabled
                    :enabled? false
                    :error {:message "Blocked"}}]
    (is (= (assoc preview
                  :target-status :disabled
                  :target-enabled? false
                  :target-reason "Blocked")
           (legal-targets/with-target-status preview descriptor)))
    (is (= preview
           (legal-targets/with-target-status
             (assoc preview
                    :target-status :legal
                    :target-enabled? true
                    :target-reason "Old")
             nil)))))
