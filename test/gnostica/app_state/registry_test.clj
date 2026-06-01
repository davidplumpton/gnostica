(ns gnostica.app-state.registry-test
  (:require [clojure.test :refer [deftest is testing]]
            [gnostica.app.handlers :as app-handlers]
            [gnostica.app.subscriptions :as app-subscriptions]
            [gnostica.app-state :as app-state]
            [gnostica.board :as board]
            [gnostica.board-layout :as layout]
            [gnostica.cards :as cards]
            [gnostica.deterministic-shuffle :as deterministic-shuffle]
            [gnostica.fixtures :as fixtures]
            [gnostica.game-schema :as game-schema]
            [gnostica.game-state :as game-state]
            [gnostica.gesture-input :as gesture-input]
            [gnostica.move-selection.registry :as move-registry]
            [gnostica.pieces :as pieces]
            [gnostica.test-support.app-db :refer [board-cell-by-index
                                                  discard-card-target
                                                  hand-card-target
                                                  mark-game-player-eliminated
                                                  piece-by-id
                                                  piece-target
                                                  remove-board-cell
                                                  replace-game-player-hand
                                                  territory-target]]
            [gnostica.test-support.app-state :refer :all]
            [gnostica.test-support.deck :refer [board-card-position
                                                deck-starting-with
                                                deck-with-card-at
                                                deck-with-cards-at]]))

(deftest move-power-registry-covers-implemented-browser-powers
  (is (= (set move-registry/move-power-order)
         (set (keys move-registry/move-power-registry))))
  (doseq [power move-registry/move-power-order
          :let [definition (move-registry/power-definition power)]]
    (is (= power (:id definition)) power)
    (is (seq (:label definition)) power)
    (is (keyword? (move-registry/power-command-builder power)) power)
    (is (fn? (move-registry/power-transition-fn power)) power)
    (is (contains? #{:static :fool :world :composite-major :sword-major}
                   (move-registry/power-control-kind power))
        power))
  (doseq [[card-id expected-options] expected-major-power-options]
    (is (= expected-options
           (move-registry/power-ids-for-card (cards/card-by-id card-id)))
        card-id)))
(deftest major-card-power-options-cover-implemented-browser-powers
  (doseq [[card-id expected-options] expected-major-power-options]
    (let [db (app-state/initialize {:player-specs test-player-specs
                                    :game-options {:deck-order
                                                   (deck-starting-with
                                                    [card-id])}
                                    :demo-board-pieces [rose-hand-piece]})
          piece-db (-> db
                       (app-state/select-move-source :play-hand-card)
                       (app-state/select-move-hand-card card-id)
                       (app-state/select-move-piece :rose-striker))
          expected-power (when (= 1 (count expected-options))
                           (first expected-options))]
      (is (= expected-options
             (mapv :id (app-state/move-power-options piece-db)))
          card-id)
      (is (= expected-power
             (app-state/move-power piece-db))
          card-id))))
(deftest unsupported-card-powers-fail-explicitly-without-mutating-game
  (let [blank-card {:id "blank-major"
                    :title "Blank Major"
                    :image "/images/blank-major.png"
                    :arcana :major
                    :group "Major Arcana"}
        db (-> (app-state/initialize {:player-specs test-player-specs
                                      :game-options {:shuffle-fn identity}
                                      :demo-board-pieces [rose-hand-piece]})
               (replace-game-player-hand :rose [blank-card]))
        ready-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "blank-major")
                     (app-state/select-move-piece :rose-striker))
        confirmed-db (app-state/confirm-move ready-db)]
    (is (empty? (app-state/move-power-options ready-db)))
    (is (= :unavailable (app-state/move-power ready-db)))
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "blank-major"
                     :piece-id :rose-striker}
            :power :unavailable
            :card-id "blank-major"}
           (app-state/move-command ready-db)))
    (is (= :rejected (:stage (app-state/move-selection confirmed-db))))
    (is (= :move-transition-unavailable
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= (app-state/game ready-db)
           (app-state/game confirmed-db)))))
(deftest move-panel-registry-covers-emitted-control-groups
  (let [emitted-types (move-registry/control-renderer-types)
        registry-types (set (keys move-registry/control-renderer-definitions))
        power-renderer-types (set (mapcat #(or (:renderer-control-keys
                                                (move-registry/power-definition %))
                                               (move-registry/power-control-groups %))
                                           move-registry/move-power-order))
        missing-registry-types (sort (remove registry-types emitted-types))
        missing-power-renderers (sort (remove emitted-types power-renderer-types))
        nil-renderer-keys (sort (keep (fn [type]
                                        (when (nil? (move-registry/control-renderer-key type))
                                          type))
                                      emitted-types))]
    (is (seq emitted-types))
    (is (= [] missing-registry-types))
    (is (= [] missing-power-renderers))
    (is (= [] nil-renderer-keys))))
