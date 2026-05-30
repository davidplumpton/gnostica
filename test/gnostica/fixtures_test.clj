(ns gnostica.fixtures-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.board :as board]
            [gnostica.cards :as cards]
            [gnostica.fixtures :as fixtures]
            [gnostica.game-state :as game-state]
            [gnostica.pieces :as pieces]))

(deftest demo-board-pieces-reference-known-rendering-data
  (doseq [piece fixtures/demo-board-pieces]
    (is (contains? pieces/players-by-id (:player-id piece)))
    (is (contains? pieces/piece-sizes (:size piece)))
    (is (contains? pieces/legal-orientations (:orientation piece)))
    (is (<= 0 (:space-index piece) (dec board/board-card-count)))))

(deftest demo-board-pieces-fit-space-capacity
  (doseq [[space-key space-pieces] (pieces/pieces-by-space fixtures/demo-board-pieces)]
    (is (<= (count space-pieces) pieces/max-pieces-per-space)
        (str "Expected no more than "
             pieces/max-pieces-per-space
             " pieces at space "
             space-key))))

(deftest demo-board-pieces-exercise-three-piece-layout
  (is (some #(= pieces/max-pieces-per-space (count %))
            (vals (pieces/pieces-by-space fixtures/demo-board-pieces)))))

(deftest smoke-board-pieces-exercise-wasteland-rendering
  (is (= [fixtures/smoke-wasteland-piece]
         (pieces/pieces-for-wasteland fixtures/smoke-board-pieces 0 3))))

(deftest demo-board-pieces-can-be-filtered-by-player-specs
  (is (= #{:rose :indigo}
         (set (map :player-id
                   (fixtures/demo-board-pieces-for [{:id :rose}
                                                    {:id :indigo}]))))))

(deftest major-icon-smoke-deck-order-stages-hand-and-board-icons
  (let [deck-order (fixtures/major-icon-smoke-deck-order)
        board-start (* game-state/starting-hand-size 6)
        current-hand (take game-state/starting-hand-size deck-order)
        board-cards (take board/board-card-count (drop board-start deck-order))]
    (is (= (count cards/deck) (count deck-order)))
    (is (= (set (map :id cards/deck))
           (set (map :id deck-order))))
    (is (= ["magician"] (mapv :id (filter :gnostica-icons current-hand))))
    (is (= ["chariot" "devil"]
           (mapv :id (filter :gnostica-icons board-cards))))
    (is (= {:start-in-lobby? false
            :bypass-lobby? true
            :player-specs (mapv #(select-keys % [:id :name]) pieces/players)
            :demo-board-pieces fixtures/smoke-board-pieces
            :game-options {:deck-order deck-order}}
           (fixtures/smoke-init-options fixtures/smoke-major-icons-mode)))))

(deftest direct-drop-smoke-init-options-start-no-piece-confirmation-page
  (is (= {:start-in-lobby? false
          :bypass-lobby? true
          :player-specs fixtures/default-browser-lobby-player-specs
          :open-panels #{:cards :move}
          :demo-board-pieces []
          :game-options {:shuffle-fn identity}}
         (fixtures/smoke-init-options fixtures/smoke-direct-drop-mode))))

(deftest browser-lobby-init-options-prefill-two-local-players
  (is (= {:start-in-lobby? true
          :player-specs [{:id :rose
                          :name "Rose"}
                         {:id :indigo
                          :name "Indigo"}]}
         (fixtures/lobby-init-options))))

(deftest shared-local-control-init-options-are-dev-scoped
  (is (nil? (fixtures/shared-local-control-init-options false)))
  (is (= {:start-in-lobby? true
          :player-specs [{:id :rose
                          :name "Rose"}
                         {:id :indigo
                          :name "Indigo"}]
          :local-controller fixtures/shared-local-controller}
         (fixtures/shared-local-control-init-options true))))
