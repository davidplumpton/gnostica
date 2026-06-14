(ns gnostica.app-state.move-selection-draw-test
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

(deftest no-piece-player-with-hand-room-must-place-initial-small
  (let [initial-db (app-state/initialize {:player-specs test-player-specs
                                          :game-options {:shuffle-fn identity}})
        original-hand (app-state/current-player-hand initial-db)
        discarded-card (last original-hand)
        db (-> initial-db
               (replace-game-player-hand :rose (vec (butlast original-hand)))
               (update-in [:game :discard-pile] conj discarded-card))
        draw-option (source-option db :draw-cards)
        initial-option (source-option db :place-initial-small)
        header-view (app-state/header-view db)
        ended-db (app-state/end-turn db)
        source-db (app-state/select-move-source db :draw-cards)]
    (is (not (:enabled? draw-option)))
    (is (= "A player with no pieces must place their initial small piece instead of drawing cards."
           (:reason draw-option)))
    (is (:enabled? initial-option))
    (is (false? (:can-end-turn? header-view)))
    (is (false? (:can-announce-challenge? header-view)))
    (is (= :initial-placement-required
           (get-in ended-db [:turn-result :error :code])))
    (is (= :rose
           (get-in ended-db [:game :turn :current-player-id])))
    (is (= "Place first piece" (:label initial-option)))
    (is (re-find #"Special rule" (:summary initial-option)))
    (is (= :move-source-unavailable
           (get-in source-db [:move-selection :error :code])))
    (is (= :draw-cards
           (get-in source-db [:move-selection :error :data :source])))
    (is (game-schema/valid-game? (app-state/game db)))))
(deftest drawing-cards-confirms-through-game-state
  (let [initial-db (app-state/initialize {:player-specs test-player-specs
                                          :game-options {:shuffle-fn identity}
                                          :demo-board-pieces [rose-source-piece]})
        original-hand (app-state/current-player-hand initial-db)
        discarded-card (last original-hand)
        shortened-hand (vec (butlast original-hand))
        draw-card (first (get-in initial-db [:game :draw-pile]))
        db (-> initial-db
               (replace-game-player-hand :rose shortened-hand)
               (update-in [:game :discard-pile] conj discarded-card))
        empty-draw-db (assoc-in db [:game :draw-pile] [])
        source-db (app-state/select-move-source db :draw-cards)
        confirmed-db (app-state/confirm-move source-db)
        blocked-confirm-db (app-state/confirm-move
                            (assoc confirmed-db
                                   :move-selection
                                   (app-state/move-selection source-db)))
        ended-db (app-state/end-turn confirmed-db)
        zones (app-state/card-zones confirmed-db)]
    (is (:enabled? (source-option db :draw-cards)))
    (is (= 1 (app-state/max-draw-count empty-draw-db)))
    (is (= :confirm (:stage (app-state/move-selection source-db))))
    (is (= {:discard-card-ids []
            :draw-count 1}
           (app-state/move-params source-db)))
    (is (= {:source :draw-cards
            :player-id :rose
            :discard-card-ids []
            :draw-count 1}
           (app-state/move-command source-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (app-state/turn-action-consumed? confirmed-db))
    (is (every? (comp not :enabled?)
                (app-state/move-source-options confirmed-db)))
    (is (= #{"The current player has already taken a turn action."}
           (set (map :reason (app-state/move-source-options confirmed-db)))))
    (is (= :rejected (:stage (app-state/move-selection blocked-confirm-db))))
    (is (= :move-source-unavailable
           (get-in blocked-confirm-db [:move-selection :error :code])))
    (is (= "The current player has already taken a turn action."
           (get-in blocked-confirm-db [:move-selection :error :message])))
    (is (false? (app-state/turn-action-consumed? ended-db)))
    (is (= (mapv :id (conj shortened-hand draw-card))
           (mapv :id (:hand zones))))
    (is (= [(:id discarded-card)]
           (mapv :id (:discard-pile zones))))
    (is (= (dec (count (get-in db [:game :draw-pile])))
           (:draw-count zones)))))
(deftest no-piece-draw-confirmation-is-rejected
  (let [initial-db (app-state/initialize {:player-specs test-player-specs
                                          :game-options {:shuffle-fn identity}
                                          :demo-board-pieces [rose-source-piece]})
        original-hand (app-state/current-player-hand initial-db)
        discarded-card (last original-hand)
        db (-> initial-db
               (replace-game-player-hand :rose (vec (butlast original-hand)))
               (update-in [:game :discard-pile] conj discarded-card))
        staged-db (app-state/select-move-source db :draw-cards)
        no-piece-db (update staged-db :game game-state/with-board-pieces [])
        confirmed-db (app-state/confirm-move no-piece-db)]
    (is (= :confirm (:stage (app-state/move-selection staged-db))))
    (is (= :rejected (:stage (app-state/move-selection confirmed-db))))
    (is (= :move-source-unavailable
           (get-in confirmed-db [:move-selection :error :code])))
    (is (= (app-state/game no-piece-db)
           (app-state/game confirmed-db)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest full-hand-draw-can-select-discards-before-drawing
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:shuffle-fn identity}
                                  :demo-board-pieces [rose-source-piece]})
        original-hand (app-state/current-player-hand db)
        discarded-card (last original-hand)
        draw-card (first (get-in db [:game :draw-pile]))
        source-db (app-state/select-move-source db :draw-cards)
        discard-db (app-state/toggle-move-discard-card source-db (:id discarded-card))
        controls (:controls (app-state/move-panel-view source-db))
        confirmed-db (app-state/confirm-move discard-db)
        zones (app-state/card-zones confirmed-db)]
    (is (:enabled? (source-option db :draw-cards)))
    (is (= :draw-count (:stage (app-state/move-selection source-db))))
    (is (= {:discard-card-ids []}
           (app-state/move-params source-db)))
    (is (= (mapv :id original-hand)
           (mapv :id (:discard-card-options controls))))
    (is (empty? (:draw-options controls)))
    (is (= :confirm (:stage (app-state/move-selection discard-db))))
    (is (= [0 1] (app-state/draw-count-options discard-db)))
    (is (= {:discard-card-ids [(:id discarded-card)]
            :draw-count 1}
           (app-state/move-params discard-db)))
    (is (= {:source :draw-cards
            :player-id :rose
            :discard-card-ids [(:id discarded-card)]
            :draw-count 1}
           (app-state/move-command discard-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= (mapv :id (conj (vec (butlast original-hand)) draw-card))
           (mapv :id (:hand zones))))
    (is (= [(:id discarded-card)]
           (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest confirm-move-event-handler-is-deterministic-with-injected-draw-shuffle-seed
  (let [initial-db (app-state/initialize {:player-specs test-player-specs
                                          :game-options {:shuffle-fn identity}
                                          :demo-board-pieces [rose-source-piece]})
        original-hand (app-state/current-player-hand initial-db)
        shortened-hand (vec (drop 2 original-hand))
        first-draw-card (first (get-in initial-db [:game :draw-pile]))
        prepared-discard (vec (concat (take 2 original-hand)
                                      (rest (get-in initial-db [:game :draw-pile]))))
        db (-> initial-db
               (replace-game-player-hand :rose shortened-hand)
               (assoc-in [:game :draw-pile] [first-draw-card])
               (assoc-in [:game :discard-pile] prepared-discard))
        ready-db (-> db
                     (app-state/select-move-source :draw-cards)
                     (app-state/set-move-draw-count 2))
        first-confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260524})
        second-confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260524})
        shuffled-discard (deterministic-shuffle/shuffle-with-seed 20260524 prepared-discard)
        expected-drawn [first-draw-card (first shuffled-discard)]
        zones (app-state/card-zones first-confirmed-db)]
    (is (= (app-state/game first-confirmed-db)
           (app-state/game second-confirmed-db)))
    (is (= (mapv :id (concat shortened-hand expected-drawn))
           (mapv :id (:hand zones))))
    (is (empty? (:discard-pile zones)))
    (is (= (mapv :id (rest shuffled-discard))
           (mapv :id (:draw-pile zones))))
    (is (true? (get-in first-confirmed-db
                       [:move-selection :last-result :events 0 :reshuffled-discard?])))
    (is (game-schema/valid-game? (app-state/game first-confirmed-db)))))
(deftest fool-hand-card-can_skip_two_reveals_with_injected_shuffle
  (let [seed 20260528
        initial-db (app-state/initialize {:player-specs test-player-specs
                                          :game-options {:deck-order (deck-starting-with ["fool"])}
                                          :demo-board-pieces [rose-hand-piece]})
        prepared-discard (vec (get-in initial-db [:game :draw-pile]))
        db (-> initial-db
               (assoc-in [:game :draw-pile] [])
               (assoc-in [:game :discard-pile] prepared-discard))
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "fool")
                     (app-state/select-move-piece :rose-striker))
        count-db (app-state/set-move-fool-reveal-count piece-db 2)
        first-reveal-db (app-handlers/reveal-move-fool-card-db count-db {:shuffle-seed seed})
        first-skip-db (app-state/skip-move-fool-reveal first-reveal-db)
        second-reveal-db (app-handlers/reveal-move-fool-card-db first-skip-db
                                                                {:shuffle-seed (inc seed)})
        reveal-db (app-state/skip-move-fool-reveal second-reveal-db)
        confirmed-db (app-handlers/confirm-move-db reveal-db {:shuffle-seed (+ seed 2)})
        repeated-confirmed-db (app-handlers/confirm-move-db reveal-db {:shuffle-seed (+ seed 3)})
        expected-shuffled (deterministic-shuffle/shuffle-with-seed
                           seed
                           (conj prepared-discard (cards/card-by-id "fool")))
        events (get-in confirmed-db [:move-selection :last-result :events])
        command (app-state/move-command reveal-db)
        zones (app-state/card-zones confirmed-db)]
    (is (= :fool-reveal-count (:stage (app-state/move-selection piece-db))))
    (is (= [0 1 2] (app-state/move-fool-reveal-count-options piece-db)))
    (is (= :fool-reveal-card (:stage (app-state/move-selection count-db))))
    (is (= (-> expected-shuffled first :id)
           (get-in first-reveal-db [:move-selection :params :fool-active-reveal :card-id])))
    (is (= :fool-reveal-card (:stage (app-state/move-selection first-skip-db))))
    (is (= (-> expected-shuffled second :id)
           (get-in second-reveal-db [:move-selection :params :fool-active-reveal :card-id])))
    (is (= :confirm (:stage (app-state/move-selection reveal-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "fool"
                     :piece-id :rose-striker}
            :reveals [{} {}]}
           (dissoc command :shuffle-fn)))
    (is (= (app-state/game confirmed-db)
           (app-state/game repeated-confirmed-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= [:fool/card-revealed :fool/card-revealed]
           (mapv :type events)))
    (is (= (mapv :id (take 2 expected-shuffled))
           (mapv :card-id events)))
    (is (not (some #{"fool"} (map :id (:hand zones)))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest fool-hand-card-can_play_first_reveal_and_skip_second
  (let [draw-start (+ (* game-state/starting-hand-size (count test-player-specs))
                      board/board-card-count)
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at {0 "fool"
                                                             draw-start "cups2"
                                                             (inc draw-start) "wands2"})}
             :demo-board-pieces [rose-hand-cup-territory-piece]})
        ready-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "fool")
                     (app-state/select-move-piece :rose-striker)
                     (app-state/set-move-fool-reveal-count 2)
                     app-state/reveal-move-fool-card
                     app-state/play-move-fool-reveal
                     (app-state/select-board-card 3)
                     (app-state/set-move-orientation :north)
                     app-state/reveal-move-fool-card
                     app-state/skip-move-fool-reveal)
        command (app-state/move-command ready-db)
        confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260528})
        events (get-in confirmed-db [:move-selection :last-result :events])
        created-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "fool"
                     :piece-id :rose-striker}
            :reveals [{:power :cup
                       :piece-id :rose-striker
                       :play-command {:target {:kind :territory
                                               :board-index 3}
                                      :orientation :north
                                      :cup-variant :cup}}
                      {}]}
           command))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= [:fool/card-revealed
            :cup/small-piece-created
            :fool/card-revealed]
           (mapv :type events)))
    (is (= 3 (:space-index created-piece)))
    (is (= :north (:orientation created-piece)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest fool-hand-card-can_skip_first_reveal_and_play_second
  (let [draw-start (+ (* game-state/starting-hand-size (count test-player-specs))
                      board/board-card-count)
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at {0 "fool"
                                                             draw-start "wands2"
                                                             (inc draw-start) "cups2"})}
             :demo-board-pieces [rose-hand-cup-territory-piece]})
        ready-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "fool")
                     (app-state/select-move-piece :rose-striker)
                     (app-state/set-move-fool-reveal-count 2)
                     app-state/reveal-move-fool-card
                     app-state/skip-move-fool-reveal
                     app-state/reveal-move-fool-card
                     app-state/play-move-fool-reveal
                     (app-state/select-board-card 3)
                     (app-state/set-move-orientation :east))
        command (app-state/move-command ready-db)
        confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260528})
        events (get-in confirmed-db [:move-selection :last-result :events])]
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= [{} {:power :cup
                :piece-id :rose-striker
                :play-command {:target {:kind :territory
                                        :board-index 3}
                               :orientation :east
                               :cup-variant :cup}}]
           (:reveals command)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= [false true]
           (mapv :played? (filter #(= :fool/card-revealed (:type %)) events))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest fool-hand-card-can_play_two_reveals
  (let [draw-start (+ (* game-state/starting-hand-size (count test-player-specs))
                      board/board-card-count)
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at {0 "fool"
                                                             draw-start "cups2"
                                                             (inc draw-start) "wands2"})}
             :demo-board-pieces [rose-hand-cup-territory-piece]})
        ready-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "fool")
                     (app-state/select-move-piece :rose-striker)
                     (app-state/set-move-fool-reveal-count 2)
                     app-state/reveal-move-fool-card
                     app-state/play-move-fool-reveal
                     (app-state/select-board-card 3)
                     (app-state/set-move-orientation :north)
                     app-state/reveal-move-fool-card
                     app-state/play-move-fool-reveal
                     (app-state/select-move-rod-mode :move-minion)
                     (app-state/set-move-distance 1)
                     (app-state/set-move-orientation :east))
        confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260528})
        events (get-in confirmed-db [:move-selection :last-result :events])
        moved-piece (piece-by-id confirmed-db :rose-striker)]
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= [:fool/card-revealed
            :cup/small-piece-created
            :fool/card-revealed
            :rod/minion-moved]
           (mapv :type events)))
    (is (= 3 (:space-index moved-piece)))
    (is (= :east (:orientation moved-piece)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest fool-hand-card-can_play_revealed_major
  (let [draw-start (+ (* game-state/starting-hand-size (count test-player-specs))
                      board/board-card-count)
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at {0 "fool"
                                                             draw-start "hangedman"})}
             :demo-board-pieces [rose-rod-minion indigo-rod-target]})
        rose-hand-before (mapv :id (get-in db [:game :players-by-id :rose :hand]))
        indigo-hand-before (mapv :id (get-in db [:game :players-by-id :indigo :hand]))
        ready-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "fool")
                     (app-state/select-move-piece :rose-rod-minion)
                     (app-state/set-move-fool-reveal-count 1)
                     app-state/reveal-move-fool-card
                     app-state/play-move-fool-reveal
                     (app-state/select-move-fool-play-power :hanged-man)
                     (app-state/set-move-major-action-count 1)
                     (app-state/select-move-target-piece :indigo-rod-target))
        command (app-state/move-command ready-db)
        confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260528})
        events (get-in confirmed-db [:move-selection :last-result :events])]
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= :hanged-man (get-in command [:reveals 0 :power])))
    (is (= [:fool/card-revealed :hanged-man/hands-traded]
           (mapv :type events)))
    (is (= indigo-hand-before
           (mapv :id (get-in confirmed-db [:game :players-by-id :rose :hand]))))
    (is (= (vec (remove #{"fool"} rose-hand-before))
           (mapv :id (get-in confirmed-db [:game :players-by-id :indigo :hand]))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest world-copy-of-fool_uses_reveal_play_flow
  (let [draw-start (+ (* game-state/starting-hand-size (count test-player-specs))
                      board/board-card-count)
        world-index 0
        fool-index 1
        db (app-state/initialize
            {:player-specs test-player-specs
             :game-options {:deck-order (deck-with-cards-at
                                         {(board-card-position test-player-specs world-index) "world"
                                          (board-card-position test-player-specs fool-index) "fool"
                                          draw-start "cups2"})}
             :demo-board-pieces [rose-source-piece]})
        ready-db (-> db
                     (app-state/select-move-source :activate-territory)
                     (app-state/select-board-card world-index)
                     (app-state/select-move-piece :rose-scout)
                     (app-state/select-move-world-copy fool-index)
                     (app-state/set-move-fool-reveal-count 1)
                     app-state/reveal-move-fool-card
                     app-state/play-move-fool-reveal
                     (app-state/select-board-card fool-index)
                     (app-state/set-move-orientation :east))
        command (app-state/move-command ready-db)
        confirmed-db (app-handlers/confirm-move-db ready-db {:shuffle-seed 20260528})
        created-piece (piece-by-id confirmed-db :rose-small-1)]
    (is (= :confirm (:stage (app-state/move-selection ready-db))))
    (is (= fool-index (:copied-board-index command)))
    (is (= :cup (get-in command [:reveals 0 :power])))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= fool-index (:space-index created-piece)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest high-priestess-hand-card-can_stage_two_redraw_passes
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order
                                                 (deck-starting-with
                                                  ["high-priestess" "cups2" "wands2"
                                                   "coins2" "swords2" "cups3"])}
                                  :demo-board-pieces [rose-hand-piece]})
        first-drawn-card (first (get-in db [:game :draw-pile]))
        second-drawn-card (second (get-in db [:game :draw-pile]))
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "high-priestess")
                     (app-state/select-move-piece :rose-striker))
        count-db (app-state/set-move-high-priestess-redraw-count piece-db 2)
        first-pass-db (-> count-db
                          (app-state/toggle-move-high-priestess-discard-card 1 "cups2")
                          (app-state/set-move-high-priestess-draw-count 1 1))
        second-pass-options (second (app-state/move-high-priestess-redraw-options
                                     first-pass-db))
        second-pass-db (-> first-pass-db
                           (app-state/toggle-move-high-priestess-discard-card 2 "wands2")
                           (app-state/set-move-high-priestess-draw-count 2 1))
        confirmed-db (app-state/confirm-move second-pass-db)
        zones (app-state/card-zones confirmed-db)]
    (is (= :high-priestess-redraw-count
           (:stage (app-state/move-selection piece-db))))
    (is (= [0 1 2] (app-state/move-high-priestess-redraw-count-options piece-db)))
    (is (= :high-priestess-redraw
           (:stage (app-state/move-selection count-db))))
    (is (= [1 2]
           (mapv :pass-index (app-state/move-high-priestess-redraw-options count-db))))
    (is (= ["wands2" "coins2" "swords2" "cups3" (:id first-drawn-card)]
           (mapv :id (:discard-card-options second-pass-options))))
    (is (= :confirm (:stage (app-state/move-selection second-pass-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "high-priestess"
                     :piece-id :rose-striker}
            :redraws [{:discard-card-ids ["cups2"]
                       :draw-count 1}
                      {:discard-card-ids ["wands2"]
                       :draw-count 1}]}
           (app-state/move-command second-pass-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= ["coins2" "swords2" "cups3" (:id first-drawn-card) (:id second-drawn-card)]
           (mapv :id (:hand zones))))
    (is (= ["high-priestess" "cups2" "wands2"]
           (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest high-priestess-redraw-staging-prevents_duplicate_discards_across_passes
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order
                                                 (deck-starting-with
                                                  ["high-priestess" "cups2" "wands2"
                                                   "coins2" "swords2" "cups3"])}
                                  :demo-board-pieces [rose-hand-piece]})
        count-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "high-priestess")
                     (app-state/select-move-piece :rose-striker)
                     (app-state/set-move-high-priestess-redraw-count 2))
        first-pass-db (-> count-db
                          (app-state/toggle-move-high-priestess-discard-card 1 "cups2")
                          (app-state/set-move-high-priestess-draw-count 1 0))
        second-pass-options (second (app-state/move-high-priestess-redraw-options
                                     first-pass-db))
        duplicate-db (app-state/toggle-move-high-priestess-discard-card
                      first-pass-db
                      2
                      "cups2")]
    (is (not (some #{"cups2"} (map :id (:discard-card-options second-pass-options)))))
    (is (= [] (:selected-discard-card-ids second-pass-options)))
    (is (= :invalid-high-priestess-discard-card
           (get-in duplicate-db [:move-selection :error :code])))
    (is (= [] (:selected-discard-card-ids
               (second (app-state/move-high-priestess-redraw-options duplicate-db)))))))
(deftest high-priestess-second_redraw_can_discard_card_drawn_in_first_pass
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order
                                                 (deck-starting-with
                                                  ["high-priestess" "cups2" "wands2"
                                                   "coins2" "swords2" "cups3"])}
                                  :demo-board-pieces [rose-hand-piece]})
        first-drawn-card (first (get-in db [:game :draw-pile]))
        drawn-card-id (:id first-drawn-card)
        first-pass-db (-> db
                          (app-state/select-move-source :play-hand-card)
                          (app-state/select-move-hand-card "high-priestess")
                          (app-state/select-move-piece :rose-striker)
                          (app-state/set-move-high-priestess-redraw-count 2)
                          (app-state/toggle-move-high-priestess-discard-card 1 "cups2")
                          (app-state/set-move-high-priestess-draw-count 1 1))
        second-pass-options (second (app-state/move-high-priestess-redraw-options
                                     first-pass-db))
        second-pass-db (-> first-pass-db
                           (app-state/toggle-move-high-priestess-discard-card
                            2
                            drawn-card-id)
                           (app-state/set-move-high-priestess-draw-count 2 0))
        confirmed-db (app-state/confirm-move second-pass-db)
        zones (app-state/card-zones confirmed-db)]
    (is (some #{drawn-card-id} (map :id (:discard-card-options second-pass-options))))
    (is (= :confirm (:stage (app-state/move-selection second-pass-db))))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "high-priestess"
                     :piece-id :rose-striker}
            :redraws [{:discard-card-ids ["cups2"]
                       :draw-count 1}
                      {:discard-card-ids [drawn-card-id]
                       :draw-count 0}]}
           (app-state/move-command second-pass-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= ["wands2" "coins2" "swords2" "cups3"]
           (mapv :id (:hand zones))))
    (is (= ["high-priestess" "cups2" drawn-card-id]
           (mapv :id (:discard-pile zones))))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
(deftest judgement-hand-card-can_stage_source_card_draw_after_cost
  (let [db (app-state/initialize {:player-specs test-player-specs
                                  :game-options {:deck-order (deck-starting-with ["judgement"])}
                                  :demo-board-pieces [rose-hand-piece]})
        piece-db (-> db
                     (app-state/select-move-source :play-hand-card)
                     (app-state/select-move-hand-card "judgement")
                     (app-state/select-move-piece :rose-striker))
        card-db (app-state/toggle-move-judgement-card piece-db "judgement")
        confirmed-db (app-state/confirm-move card-db)
        zones (app-state/card-zones confirmed-db)]
    (is (= :confirm (:stage (app-state/move-selection piece-db))))
    (is (= ["judgement"]
           (mapv :id (app-state/move-judgement-card-options piece-db))))
    (is (= 1 (app-state/move-judgement-card-maximum piece-db)))
    (is (= {:player-id :rose
            :source {:kind :hand-card
                     :card-id "judgement"
                     :piece-id :rose-striker}
            :piece-id :rose-striker
            :card-ids ["judgement"]}
           (app-state/move-command card-db)))
    (is (:ok? (get-in confirmed-db [:move-selection :last-result])))
    (is (= "judgement" (:id (last (:hand zones)))))
    (is (empty? (:discard-pile zones)))
    (is (game-schema/valid-game? (app-state/game confirmed-db)))))
