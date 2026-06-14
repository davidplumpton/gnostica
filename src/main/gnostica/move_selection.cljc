(ns gnostica.move-selection
  (:require [gnostica.board-layout :as layout]
            [gnostica.cards :as cards]
            [gnostica.game-state :as game-state]
            [gnostica.move-selection.commands :as commands]
            [gnostica.move-selection.confirmation :as confirmation]
            [gnostica.move-selection.contexts :as contexts]
            [gnostica.move-selection.controls :as controls]
            [gnostica.move-selection.flow :as flow]
            [gnostica.move-selection.high-priestess :as high-priestess]
            [gnostica.move-selection.options :as options]
            [gnostica.move-selection.prompt :as prompt]
            [gnostica.move-selection.preview :as preview]
            [gnostica.move-selection.registry :as registry]
            [gnostica.move-selection.ribbon :as ribbon]
            [gnostica.move-selection.source-availability :as source-availability]
            [gnostica.move-selection.staging :as staging]
            [gnostica.move-selection.power-context :as power]
            [gnostica.move-selection.state :as selection]
            [gnostica.move-selection.target-options :as target-options]
            [gnostica.move-selection.targets :as targets]
            [gnostica.move-selection.targeting :as targeting]
            [gnostica.move-selection.updates :as updates]
            [gnostica.pieces :as pieces]))

(def move-source-order registry/move-source-order)
(def move-source-definitions registry/move-source-definitions)
(def move-power-order registry/move-power-order)
(def move-power-definitions registry/move-power-definitions)
(def copied-suit-powers registry/copied-suit-powers)
(def composite-major-powers registry/composite-major-powers)
(def sword-major-powers registry/sword-major-powers)

(def rod-mode-order options/rod-mode-order)
(def rod-modes options/rod-modes)
(def rod-mode-definitions options/rod-mode-definitions)
(def disc-target-kind-order options/disc-target-kind-order)
(def disc-target-kinds options/disc-target-kinds)
(def disc-target-kind-definitions options/disc-target-kind-definitions)
(def sun-disc-mode-order options/sun-disc-mode-order)
(def sun-disc-modes options/sun-disc-modes)
(def sun-disc-mode-definitions options/sun-disc-mode-definitions)
(def sword-target-kind-order options/sword-target-kind-order)
(def sword-target-kinds options/sword-target-kinds)
(def sword-target-kind-definitions options/sword-target-kind-definitions)
(def fool-reveal-count-order options/fool-reveal-count-order)
(def high-priestess-redraw-count-order options/high-priestess-redraw-count-order)
(def strength-disc-action-count-order options/strength-disc-action-count-order)
(def death-sword-action-count-order options/death-sword-action-count-order)
(def hand-trade-major-action-count-order options/hand-trade-major-action-count-order)
(def hand-trade-major-action-count-definitions
  options/hand-trade-major-action-count-definitions)
(def ordered-major-action-count-order
  options/ordered-major-action-count-order)
(def ordered-major-action-count-definitions
  options/ordered-major-action-count-definitions)
(def moon-action-choice-order options/moon-action-choice-order)
(def moon-action-choice-definitions options/moon-action-choice-definitions)
(def devil-action-count-order options/devil-action-count-order)
(def territory-card-source-order options/territory-card-source-order)
(def territory-card-source-definitions options/territory-card-source-definitions)
(def disc-replacement-card-source-order options/disc-replacement-card-source-order)
(def disc-replacement-card-source-definitions
  options/disc-replacement-card-source-definitions)
(def requirement-prompts options/requirement-prompts)

(defn empty-move-selection []
  (staging/empty-selection))

(def game selection/game)
(def board selection/board)
(def ^:private board-cell-by-index selection/board-cell-by-index)
(def board-pieces selection/board-pieces)
(def selected-board-index selection/selected-board-index)
(def current-player selection/current-player)
(def current-player-id selection/current-player-id)
(def current-player-hand selection/current-player-hand)
(def discard-pile selection/discard-pile)
(def current-player-pieces selection/current-player-pieces)
(def ^:private game-turn-key selection/game-turn-key)
(def turn-action-consumed? selection/turn-action-consumed?)
(def ^:private piece-coordinate selection/piece-coordinate)
(def current-player-piece? selection/current-player-piece?)
(def piece-by-id selection/piece-by-id)
(def current-player-piece-by-id selection/current-player-piece-by-id)
(def current-player-pieces-on-space selection/current-player-pieces-on-space)
(def hand-card-by-id selection/hand-card-by-id)
(def ^:private source-card selection/source-card)
(def ^:private move-power-ids-for-card power/move-power-ids-for-card)
(def ^:private selected-power power/selected-power)
(def ^:private world-move? power/world-move?)
(def ^:private world-copy-board-indexes power/world-copy-board-indexes)
(def ^:private world-copy-board-cell power/world-copy-board-cell)
(def ^:private world-copied-card power/world-copied-card)
(def ^:private world-copied-power-ids-for-card
  power/world-copied-power-ids-for-card)
(def ^:private selected-world-copied-power
  power/selected-world-copied-power)
(def ^:private fool-completed-reveals power/fool-completed-reveals)
(def ^:private fool-completed-reveal-count
  power/fool-completed-reveal-count)
(def ^:private selected-fool-reveal-count
  power/selected-fool-reveal-count)
(def ^:private fool-active-reveal power/fool-active-reveal)
(def ^:private fool-active-reveal-card power/fool-active-reveal-card)
(def ^:private fool-play-power-options power/fool-play-power-options)
(def ^:private fool-active-play? power/fool-active-play?)
(def ^:private selected-fool-play-power power/selected-fool-play-power)
(def ^:private active-power power/active-power)
(def ^:private completed-major-actions power/completed-major-actions)
(def ^:private composite-major-move? power/composite-major-move?)
(def ^:private active-composite-action-power
  power/active-composite-action-power)
(def ^:private sword-major-move? power/sword-major-move?)
(def ^:private active-sword-major-action-power
  power/active-sword-major-action-power)
(def ^:private hanged-man-trade-stage? power/hanged-man-trade-stage?)
(def ^:private justice-trade-stage? power/justice-trade-stage?)
(def ^:private major-orient-step? power/major-orient-step?)
(def ^:private cup-move? power/cup-move?)
(def ^:private sun-move? power/sun-move?)
(def ^:private territory-card-source-option-ids
  power/territory-card-source-option-ids)
(def ^:private rod-move? power/rod-move?)
(def ^:private disc-move? power/disc-move?)
(def ^:private disc-action-count-option-values
  power/disc-action-count-option-values)
(def ^:private selected-disc-action-count
  power/selected-disc-action-count)
(def ^:private sword-move? power/sword-move?)
(def ^:private death-sword-action-count-option-values
  power/death-sword-action-count-option-values)
(def ^:private selected-death-sword-action-count
  power/selected-death-sword-action-count)
(def ^:private hand-trade-major-action-count-source?
  power/hand-trade-major-action-count-source?)
(def ^:private hand-trade-major-action-count-option-values
  power/hand-trade-major-action-count-option-values)
(def ^:private major-action-count-option-values
  power/major-action-count-option-values)
(def ^:private major-action-count-option-definitions
  power/major-action-count-option-definitions)
(def ^:private selected-major-action-count
  power/selected-major-action-count)
(def ^:private selected-hand-trade-major-action-count
  power/selected-hand-trade-major-action-count)
(def ^:private fool-move? power/fool-move?)
(def ^:private high-priestess-move? power/high-priestess-move?)
(def ^:private judgement-move? power/judgement-move?)
(def ^:private hierophant-move? power/hierophant-move?)
(def ^:private hermit-move? power/hermit-move?)
(def ^:private devil-move? power/devil-move?)
(def ^:private devil-action-count-option-values
  power/devil-action-count-option-values)
(def ^:private selected-devil-action-count power/selected-devil-action-count)
(def ^:private manipulation-piece-power?
  power/manipulation-piece-power?)
(def valid-board-index? selection/valid-board-index?)
(def ^:private target-board-cell selection/target-board-cell)

(declare facade-context-deps command-context targeting-context update-context)

(defn- targeting-context []
  (contexts/targeting-context (facade-context-deps)))

(defn- with-targeting-context [f & args]
  (apply targeting/with-context (targeting-context) f args))

(defn- rod-territory-target? [db source-id params cell]
  (with-targeting-context targeting/rod-territory-target? db source-id params cell))

(defn- disc-target-piece [db params]
  (with-targeting-context targeting/disc-target-piece db params))

(defn- disc-replacement-card-source-option-ids [db source-id params]
  (with-targeting-context targeting/disc-replacement-card-source-option-ids db source-id params))

(defn- selected-disc-replacement-card-source [db source-id params]
  (with-targeting-context targeting/selected-disc-replacement-card-source db source-id params))

(defn- disc-replacement-card-options-for [db source-id params]
  (with-targeting-context targeting/disc-replacement-card-options-for db source-id params))

(defn- disc-replacement-card-by-id [db source-id params card-id]
  (with-targeting-context targeting/disc-replacement-card-by-id db source-id params card-id))

(defn- disc-territory-target? [db source-id params cell]
  (with-targeting-context targeting/disc-territory-target? db source-id params cell))

(defn- sword-territory-target? [db source-id params cell]
  (with-targeting-context targeting/sword-territory-target? db source-id params cell))

(defn- sword-target-piece [db source-id params]
  (with-targeting-context targeting/sword-target-piece db source-id params))

(defn- sword-replacement-card-source-option-ids [db source-id params]
  (with-targeting-context targeting/sword-replacement-card-source-option-ids db source-id params))

(defn- selected-sword-replacement-card-source [db source-id params]
  (with-targeting-context targeting/selected-sword-replacement-card-source db source-id params))

(defn- sword-replacement-card-options-for [db source-id params]
  (with-targeting-context targeting/sword-replacement-card-options-for db source-id params))

(defn- sword-replacement-card-by-id [db source-id params card-id]
  (with-targeting-context targeting/sword-replacement-card-by-id db source-id params card-id))

(defn- sword-damage-options-for [db source-id params]
  (with-targeting-context targeting/sword-damage-options-for db source-id params))

(defn- sword-orientation-available? [db source-id params]
  (with-targeting-context targeting/sword-orientation-available? db source-id params))

(defn- cup-target-db [db source-id params]
  (with-targeting-context targeting/cup-target-db db source-id params))

(defn- cup-target-territory? [db source-id params cell]
  (with-targeting-context targeting/cup-target-territory? db source-id params cell))

(defn- empty-board-target? [db index]
  (with-targeting-context targeting/empty-board-target? db index))

(defn- default-initial-placement-target-index [db selected-index]
  (with-targeting-context targeting/default-initial-placement-target-index db selected-index))

(defn- hierophant-target-piece? [db params piece]
  (with-targeting-context targeting/hierophant-target-piece? db params piece))

(defn- hermit-target-piece? [db params piece]
  (with-targeting-context targeting/hermit-target-piece? db params piece))

(defn- hermit-target-territory? [db params cell]
  (with-targeting-context targeting/hermit-target-territory? db params cell))

(defn- hermit-target-selected? [params]
  (with-targeting-context targeting/hermit-target-selected? params))

(defn- hermit-piece-target-selected? [params]
  (with-targeting-context targeting/hermit-piece-target-selected? params))

(defn- hermit-territory-target-selected? [params]
  (with-targeting-context targeting/hermit-territory-target-selected? params))

(defn- hermit-destination-complete? [db params]
  (with-targeting-context targeting/hermit-destination-complete? db params))

(defn- hermit-orientation-required? [db params]
  (with-targeting-context targeting/hermit-orientation-required? db params))

(defn move-target-wasteland-options [db]
  (with-targeting-context targeting/move-target-wasteland-options db))

(defn- valid-wasteland-target? [db target]
  (with-targeting-context targeting/valid-wasteland-target? db target))

(defn- target-space-complete? [db source-id params]
  (with-targeting-context targeting/target-space-complete? db source-id params))

(defn- target-resolution-complete? [db source-id params]
  (with-targeting-context targeting/target-resolution-complete? db source-id params))

(defn- sun-cup-target-kind [params]
  (with-targeting-context targeting/sun-cup-target-kind params))

(defn- sun-cup-target-ready? [db source-id params]
  (with-targeting-context targeting/sun-cup-target-ready? db source-id params))

(defn- sun-disc-mode-option-ids [db source-id params]
  (with-targeting-context targeting/sun-disc-mode-option-ids db source-id params))

(defn- selected-sun-disc-mode [db source-id params]
  (with-targeting-context targeting/selected-sun-disc-mode db source-id params))

(defn- sun-cup-needs-one-point-card? [db source-id params]
  (with-targeting-context targeting/sun-cup-needs-one-point-card? db source-id params))

(defn- sun-disc-territory-target? [db source-id params cell]
  (with-targeting-context targeting/sun-disc-territory-target? db source-id params cell))

(defn- sun-disc-target-piece [db source-id params]
  (with-targeting-context targeting/sun-disc-target-piece db source-id params))

(defn- sun-disc-orientation-available? [db source-id params]
  (with-targeting-context targeting/sun-disc-orientation-available? db source-id params))

(defn- sun-disc-target-cell [db params]
  (with-targeting-context targeting/sun-disc-target-cell db params))

(defn- sun-disc-replacement-card-options-for [db source-id params]
  (with-targeting-context targeting/sun-disc-replacement-card-options-for db source-id params))

(defn- sun-disc-replacement-card-by-id [db source-id params card-id]
  (with-targeting-context targeting/sun-disc-replacement-card-by-id db source-id params card-id))

(defn- replacement-card-source-option-ids [db source-id params]
  (with-targeting-context targeting/replacement-card-source-option-ids db source-id params))

(defn- selected-replacement-card-source [db source-id params]
  (with-targeting-context targeting/selected-replacement-card-source db source-id params))

(defn- replacement-card-options-for-source [db source-id params card-source]
  (with-targeting-context targeting/replacement-card-options-for-source db source-id params card-source))

(defn- replacement-card-source-for-card [db source-id params card-id]
  (with-targeting-context targeting/replacement-card-source-for-card db source-id params card-id))

(defn- sun-disc-territory-target-stage? [db source-id params]
  (with-targeting-context targeting/sun-disc-territory-target-stage? db source-id params))

(defn- with-update-context [f & args]
  (apply updates/with-context (update-context) f args))

(defn- valid-discard-card-ids [db discard-card-ids]
  (with-update-context updates/valid-discard-card-ids db discard-card-ids))

(defn max-draw-count
  ([db]
   (with-update-context updates/max-draw-count db))
  ([db discard-card-ids]
   (with-update-context updates/max-draw-count db discard-card-ids)))

(defn draw-count-options
  ([db]
   (with-update-context updates/draw-count-options db))
  ([db discard-card-ids]
   (with-update-context updates/draw-count-options db discard-card-ids)))

(def ^:private high-priestess-valid-discard-card-ids-in-state
  high-priestess/valid-discard-card-ids-in-state)
(def ^:private high-priestess-draw-count-options-in-state
  high-priestess/draw-count-options-in-state)
(def ^:private high-priestess-redraw-state-before-pass
  high-priestess/redraw-state-before-pass)
(def ^:private high-priestess-hand-card-options
  high-priestess/hand-card-options)
(def ^:private high-priestess-valid-discard-card-ids
  high-priestess/valid-discard-card-ids)
(def ^:private high-priestess-draw-count-options
  high-priestess/draw-count-options)
(def ^:private selected-high-priestess-redraw-count
  high-priestess/selected-redraw-count)
(def ^:private high-priestess-redraw-pass high-priestess/redraw-pass)
(def ^:private normalize-high-priestess-redraws
  high-priestess/normalize-redraws)
(def ^:private high-priestess-redraws-complete?
  high-priestess/redraws-complete?)

(defn- judgement-discard-card-options [db source-id params]
  (with-update-context updates/judgement-discard-card-options db source-id params))

(defn- judgement-card-maximum [db source-id params]
  (with-update-context updates/judgement-card-maximum db source-id params))

(defn- valid-judgement-card-ids [db source-id params card-ids]
  (with-update-context updates/valid-judgement-card-ids db source-id params card-ids))

(defn- judgement-card-selection-complete? [db source-id params]
  (with-update-context updates/judgement-card-selection-complete? db source-id params))

(def ^:private small-stash-count source-availability/small-stash-count)
(def ^:private source-unavailable-reason
  source-availability/source-unavailable-reason)

(def move-selection selection/move-selection)
(def move-source selection/move-source)
(def move-params selection/move-params)

(defn- update-context []
  (contexts/update-context (facade-context-deps)))

(defn- control-context []
  (contexts/control-context (facade-context-deps)))

(defn move-source-options [db]
  (controls/move-source-options (control-context) db))

(defn move-power-options [db]
  (controls/move-power-options (control-context) db))

(defn move-power [db]
  (controls/move-power (control-context) db))

(defn move-world-copy-options [db]
  (controls/move-world-copy-options (control-context) db))

(defn move-world-copied-power-options [db]
  (controls/move-world-copied-power-options (control-context) db))

(defn move-world-copied-power [db]
  (controls/move-world-copied-power (control-context) db))

(defn move-rod-mode-options [db]
  (controls/move-rod-mode-options (control-context) db))

(defn move-disc-action-count-options [db]
  (controls/move-disc-action-count-options (control-context) db))

(defn move-major-action-count-options [db]
  (controls/move-major-action-count-options (control-context) db))

(defn move-major-action-count [db]
  (controls/move-major-action-count (control-context) db))

(defn move-sword-action-count-options [db]
  (controls/move-sword-action-count-options (control-context) db))

(defn move-devil-action-count-options [db]
  (controls/move-devil-action-count-options (control-context) db))

(defn move-sun-disc-mode-options [db]
  (controls/move-sun-disc-mode-options (control-context) db))

(defn move-fool-reveal-count-options [db]
  (controls/move-fool-reveal-count-options (control-context) db))

(defn move-fool-play-power-options [db]
  (controls/move-fool-play-power-options (control-context) db))

(defn move-fool-play-power [db]
  (controls/move-fool-play-power (control-context) db))

(defn move-fool-reveal-state [db]
  (controls/move-fool-reveal-state (control-context) db))

(defn move-high-priestess-redraw-count-options [db]
  (controls/move-high-priestess-redraw-count-options (control-context) db))

(defn move-high-priestess-redraw-options [db]
  (controls/move-high-priestess-redraw-options (control-context) db))

(defn move-judgement-card-options [db]
  (controls/move-judgement-card-options (control-context) db))

(defn move-judgement-card-maximum [db]
  (controls/move-judgement-card-maximum (control-context) db))

(defn move-control-groups [db]
  (controls/move-control-groups (control-context) db))

(defn move-disc-minion-orientation-required? [db]
  (with-targeting-context targeting/move-disc-minion-orientation-required? db))

(defn move-distance-options [db]
  (with-targeting-context targeting/move-distance-options db))

(defn move-target-piece-options [db]
  (with-targeting-context targeting/move-target-piece-options db))

(defn move-rod-orientation-required? [db]
  (with-targeting-context targeting/move-rod-orientation-required? db))

(defn move-disc-orientation-available? [db]
  (with-targeting-context targeting/move-disc-orientation-available? db))

(defn move-sun-disc-orientation-available? [db]
  (with-targeting-context targeting/move-sun-disc-orientation-available? db))

(defn move-sword-orientation-available? [db]
  (with-targeting-context targeting/move-sword-orientation-available? db))

(defn move-hermit-orientation-required? [db]
  (with-targeting-context targeting/move-hermit-orientation-required? db))

(def move-error updates/move-error)
(def fool-command-map updates/fool-command-map)

(defn- flow-context []
  (contexts/flow-context (facade-context-deps)))

(defn- requirement-complete? [db source-id params requirement]
  (flow/requirement-complete? (flow-context) db source-id params requirement))

(defn- composite-current-action-complete? [db source-id params]
  (flow/composite-current-action-complete?
   (flow-context)
   db
   source-id
   params))

(defn- composite-current-action [db source-id params]
  (flow/composite-current-action (flow-context) db source-id params))

(defn- sword-major-current-action-complete? [db source-id params]
  (flow/sword-major-current-action-complete?
   (flow-context)
   db
   source-id
   params))

(defn- sword-major-current-action [db source-id params]
  (flow/sword-major-current-action (flow-context) db source-id params))

(defn- devil-current-action-complete? [db source-id params]
  (flow/devil-current-action-complete? (flow-context) db source-id params))

(defn- devil-current-action [db source-id params]
  (flow/devil-current-action (flow-context) db source-id params))

(defn- update-move-selection [db f & args]
  (apply with-update-context updates/update-move-selection db f args))

(defn move-ready? [db]
  (with-update-context updates/move-ready? db))

(defn move-missing-fields [db]
  (with-update-context updates/move-missing-fields db))

(defn- prompt-context []
  (contexts/prompt-context (facade-context-deps)))

(defn move-prompt [db]
  (prompt/move-prompt (prompt-context) db))

(defn- ribbon-context []
  (contexts/ribbon-context (facade-context-deps)))

(defn move-action-ribbon [db]
  (ribbon/move-action-ribbon (ribbon-context) db))

(defn select-move-source [db source-id]
  (with-update-context updates/select-move-source db source-id))

(defn cancel-move [db]
  (with-update-context updates/cancel-move db))

(defn select-board-for-active-move [db index]
  (with-update-context updates/select-board-for-active-move db index))

(defn select-move-wasteland-target [db row col]
  (with-update-context updates/select-move-wasteland-target db row col))

(defn select-move-piece [db piece-id]
  (with-update-context updates/select-move-piece db piece-id))

(defn select-move-hand-card [db card-id]
  (with-update-context updates/select-move-hand-card db card-id))

(defn select-move-world-copy [db board-index]
  (with-update-context updates/select-move-world-copy db board-index))

(defn select-move-power [db power]
  (with-update-context updates/select-move-power db power))

(defn select-move-rod-mode [db mode]
  (with-update-context updates/select-move-rod-mode db mode))

(defn select-move-disc-target-kind [db target-kind]
  (with-update-context updates/select-move-disc-target-kind db target-kind))

(defn select-move-sword-target-kind [db target-kind]
  (with-update-context updates/select-move-sword-target-kind db target-kind))

(defn set-move-disc-action-count [db action-count]
  (with-update-context updates/set-move-disc-action-count db action-count))

(defn set-move-sword-action-count [db action-count]
  (with-update-context updates/set-move-sword-action-count db action-count))

(defn set-move-major-action-count [db action-count]
  (with-update-context updates/set-move-major-action-count db action-count))

(defn set-move-devil-action-count [db action-count]
  (with-update-context updates/set-move-devil-action-count db action-count))

(defn set-move-fool-reveal-count [db reveal-count]
  (with-update-context updates/set-move-fool-reveal-count db reveal-count))

(defn reveal-move-fool-card
  ([db]
   (with-update-context updates/reveal-move-fool-card db))
  ([db transition-options]
   (with-update-context updates/reveal-move-fool-card db transition-options)))

(defn skip-move-fool-reveal [db]
  (with-update-context updates/skip-move-fool-reveal db))

(defn play-move-fool-reveal [db]
  (with-update-context updates/play-move-fool-reveal db))

(defn select-move-fool-play-power [db power]
  (with-update-context updates/select-move-fool-play-power db power))

(defn set-move-high-priestess-redraw-count [db redraw-count]
  (with-update-context updates/set-move-high-priestess-redraw-count db redraw-count))

(defn toggle-move-high-priestess-discard-card [db pass-index card-id]
  (with-update-context updates/toggle-move-high-priestess-discard-card db pass-index card-id))

(defn set-move-high-priestess-draw-count [db pass-index draw-count]
  (with-update-context updates/set-move-high-priestess-draw-count db pass-index draw-count))

(defn toggle-move-judgement-card [db card-id]
  (with-update-context updates/toggle-move-judgement-card db card-id))

(defn set-move-minion-orientation [db orientation]
  (with-update-context updates/set-move-minion-orientation db orientation))

(defn select-move-sun-disc-mode [db mode]
  (with-update-context updates/select-move-sun-disc-mode db mode))

(defn set-move-sun-disc-orientation [db orientation]
  (with-update-context updates/set-move-sun-disc-orientation db orientation))

(defn select-move-target-piece [db piece-id]
  (with-update-context updates/select-move-target-piece db piece-id))

(defn select-move-territory-card-source [db territory-card-source]
  (with-update-context updates/select-move-territory-card-source db territory-card-source))

(defn select-move-one-point-card [db card-id]
  (with-update-context updates/select-move-one-point-card db card-id))

(defn select-move-replacement-card [db card-id]
  (with-update-context updates/select-move-replacement-card db card-id))

(defn set-move-orientation [db orientation]
  (with-update-context updates/set-move-orientation db orientation))

(defn set-move-draw-count [db draw-count]
  (with-update-context updates/set-move-draw-count db draw-count))

(defn toggle-move-discard-card [db card-id]
  (with-update-context updates/toggle-move-discard-card db card-id))

(defn set-move-distance [db distance]
  (with-update-context updates/set-move-distance db distance))

(defn- target-options-context []
  (contexts/target-options-context (facade-context-deps)))

(defn move-damage-options [db]
  (target-options/move-damage-options (target-options-context) db))

(defn set-move-damage [db damage]
  (with-update-context updates/set-move-damage db damage))

(defn move-piece-options [db]
  (target-options/move-piece-options (target-options-context) db))

(defn move-hand-card-options [db]
  (target-options/move-hand-card-options (target-options-context) db))

(defn move-discard-card-options [db]
  (target-options/move-discard-card-options (target-options-context) db))

(defn move-source-board-options [db]
  (target-options/move-source-board-options (target-options-context) db))

(defn move-target-board-options [db]
  (target-options/move-target-board-options (target-options-context) db))

(defn move-one-point-card-options [db]
  (target-options/move-one-point-card-options (target-options-context) db))

(defn move-territory-card-source-options [db]
  (target-options/move-territory-card-source-options
   (target-options-context)
   db))

(defn move-disc-target-kind-options [db]
  (target-options/move-disc-target-kind-options (target-options-context) db))

(defn move-sword-target-kind-options [db]
  (target-options/move-sword-target-kind-options (target-options-context) db))

(defn move-replacement-card-options [db]
  (target-options/move-replacement-card-options (target-options-context) db))

(defn move-orientation-options [_db]
  (target-options/move-orientation-options (target-options-context) _db))

(defn- target-context []
  (contexts/target-context (facade-context-deps)))

(defn- replacement-card-expected-value [db source params]
  (targets/replacement-card-expected-value (target-context) db source params))

(defn move-legal-targets [db]
  (targets/move-legal-targets (target-context) db))

(defn- command-context []
  (contexts/command-context (facade-context-deps)))

(defn- gameplay-power-command-for-power [db source params power]
  (commands/gameplay-power-command-for-power (command-context) db source params power))

(defn move-command [db]
  (commands/move-command (command-context) db))

(defn- confirmation-context []
  (contexts/confirmation-context (facade-context-deps)))

(declare move-preview-result)

(defn- preview-context []
  (contexts/preview-context (facade-context-deps)))

(defn move-preview [db]
  (preview/move-preview (preview-context) db))

(defn move-preview-result
  ([db] (move-preview-result db {}))
  ([db transition-options]
   (confirmation/move-preview-result
    (confirmation-context)
    db
    transition-options)))

(defn- facade-context-deps []
  {:active-composite-action-power active-composite-action-power
   :active-power active-power
   :active-sword-major-action-power active-sword-major-action-power
   :board board
   :board-cell-by-index board-cell-by-index
   :board-pieces board-pieces
   :completed-major-actions completed-major-actions
   :composite-current-action composite-current-action
   :composite-current-action-complete? composite-current-action-complete?
   :composite-major-move? composite-major-move?
   :copied-suit-powers copied-suit-powers
   :cup-move? cup-move?
   :cup-target-db cup-target-db
   :default-initial-placement-target-index default-initial-placement-target-index
   :cup-target-territory? cup-target-territory?
   :current-player-hand current-player-hand
   :current-player-id current-player-id
   :current-player-piece? current-player-piece?
   :current-player-pieces current-player-pieces
   :current-player-pieces-on-space current-player-pieces-on-space
   :current-player-territory-source-options
   selection/current-player-territory-source-options
   :death-sword-action-count-option-values death-sword-action-count-option-values
   :devil-action-count-option-values devil-action-count-option-values
   :devil-current-action devil-current-action
   :devil-current-action-complete? devil-current-action-complete?
   :devil-move? devil-move?
   :disc-action-count-option-values disc-action-count-option-values
   :disc-move? disc-move?
   :disc-replacement-card-by-id disc-replacement-card-by-id
   :disc-replacement-card-options-for disc-replacement-card-options-for
   :disc-replacement-card-source-definitions disc-replacement-card-source-definitions
   :disc-replacement-card-source-option-ids disc-replacement-card-source-option-ids
   :disc-target-kinds disc-target-kinds
   :disc-target-piece disc-target-piece
   :disc-territory-target? disc-territory-target?
   :discard-pile discard-pile
   :draw-count-options draw-count-options
   :empty-move-selection empty-move-selection
   :empty-board-target? empty-board-target?
   :fool-active-play? fool-active-play?
   :fool-active-reveal fool-active-reveal
   :fool-active-reveal-card fool-active-reveal-card
   :fool-command-map fool-command-map
   :fool-completed-reveal-count fool-completed-reveal-count
   :fool-completed-reveals fool-completed-reveals
   :fool-move? fool-move?
   :fool-play-power-options fool-play-power-options
   :fool-reveal-count-order fool-reveal-count-order
   :game game
   :game-turn-key game-turn-key
   :gameplay-power-command-for-power gameplay-power-command-for-power
   :hand-trade-major-action-count-definitions hand-trade-major-action-count-definitions
   :hand-trade-major-action-count-option-values hand-trade-major-action-count-option-values
   :hand-trade-major-action-count-source? hand-trade-major-action-count-source?
   :hand-card-by-id hand-card-by-id
   :hanged-man-trade-stage? hanged-man-trade-stage?
   :hermit-destination-complete? hermit-destination-complete?
   :hermit-move? hermit-move?
   :hermit-orientation-required? hermit-orientation-required?
   :hermit-piece-target-selected? hermit-piece-target-selected?
   :hermit-target-piece? hermit-target-piece?
   :hermit-target-selected? hermit-target-selected?
   :hermit-target-territory? hermit-target-territory?
   :hermit-territory-target-selected? hermit-territory-target-selected?
   :hierophant-move? hierophant-move?
   :hierophant-target-piece? hierophant-target-piece?
   :high-priestess-draw-count-options high-priestess-draw-count-options
   :high-priestess-draw-count-options-in-state high-priestess-draw-count-options-in-state
   :high-priestess-hand-card-options high-priestess-hand-card-options
   :high-priestess-move? high-priestess-move?
   :high-priestess-redraw-count-order high-priestess-redraw-count-order
   :high-priestess-redraw-pass high-priestess-redraw-pass
   :high-priestess-redraws-complete? high-priestess-redraws-complete?
   :high-priestess-redraw-state-before-pass high-priestess-redraw-state-before-pass
   :high-priestess-valid-discard-card-ids high-priestess-valid-discard-card-ids
   :high-priestess-valid-discard-card-ids-in-state high-priestess-valid-discard-card-ids-in-state
   :judgement-card-maximum judgement-card-maximum
   :judgement-card-selection-complete? judgement-card-selection-complete?
   :judgement-discard-card-options judgement-discard-card-options
   :judgement-move? judgement-move?
   :justice-trade-stage? justice-trade-stage?
   :major-orient-step? major-orient-step?
   :major-action-count-option-definitions major-action-count-option-definitions
   :major-action-count-option-values major-action-count-option-values
   :manipulation-piece-power? manipulation-piece-power?
   :max-draw-count max-draw-count
   :move-command move-command
   :move-disc-orientation-available? move-disc-orientation-available?
   :move-discard-card-options move-discard-card-options
   :move-error move-error
   :move-fool-play-power-options move-fool-play-power-options
   :move-hand-card-options move-hand-card-options
   :move-hermit-orientation-required? move-hermit-orientation-required?
   :move-judgement-card-options move-judgement-card-options
   :move-distance-options move-distance-options
   :move-damage-options move-damage-options
   :move-one-point-card-options move-one-point-card-options
   :move-orientation-options move-orientation-options
   :move-piece-options move-piece-options
   :move-power move-power
   :move-power-options move-power-options
   :move-power-definitions move-power-definitions
   :move-power-ids-for-card move-power-ids-for-card
   :move-power-order move-power-order
   :move-preview-result move-preview-result
   :move-ready? move-ready?
   :move-params move-params
   :move-rod-orientation-required? move-rod-orientation-required?
   :move-selection move-selection
   :move-source move-source
   :move-source-board-options move-source-board-options
   :move-source-definitions move-source-definitions
   :move-source-order move-source-order
   :move-sun-disc-orientation-available? move-sun-disc-orientation-available?
   :move-sword-orientation-available? move-sword-orientation-available?
   :move-target-board-options move-target-board-options
   :move-target-piece-options move-target-piece-options
   :move-target-wasteland-options move-target-wasteland-options
   :move-world-copy-options move-world-copy-options
   :move-world-copied-power-options move-world-copied-power-options
   :move-prompt move-prompt
   :normalize-high-priestess-redraws normalize-high-priestess-redraws
   :one-point-card-options-for selection/one-point-card-options-for
   :piece-by-id piece-by-id
   :piece-coordinate piece-coordinate
   :replacement-card-expected-value replacement-card-expected-value
   :replacement-card-options-for-source replacement-card-options-for-source
   :replacement-card-source-for-card replacement-card-source-for-card
   :replacement-card-source-option-ids replacement-card-source-option-ids
   :requirement-complete? requirement-complete?
   :rod-mode-definitions rod-mode-definitions
   :rod-mode-order rod-mode-order
   :rod-modes rod-modes
   :rod-move? rod-move?
   :rod-territory-target? rod-territory-target?
   :selected-death-sword-action-count selected-death-sword-action-count
   :selected-devil-action-count selected-devil-action-count
   :selected-disc-action-count selected-disc-action-count
   :selected-disc-replacement-card-source selected-disc-replacement-card-source
   :selected-fool-play-power selected-fool-play-power
   :selected-fool-reveal-count selected-fool-reveal-count
   :selected-major-action-count selected-major-action-count
   :selected-hand-trade-major-action-count selected-hand-trade-major-action-count
   :selected-high-priestess-redraw-count selected-high-priestess-redraw-count
   :selected-power selected-power
   :selected-replacement-card-source selected-replacement-card-source
   :selected-sun-disc-mode selected-sun-disc-mode
   :selected-sword-replacement-card-source selected-sword-replacement-card-source
   :selected-world-copied-power selected-world-copied-power
   :small-stash-count small-stash-count
   :source-card source-card
   :source-unavailable-reason source-unavailable-reason
   :sun-cup-needs-one-point-card? sun-cup-needs-one-point-card?
   :sun-cup-target-kind sun-cup-target-kind
   :sun-cup-target-ready? sun-cup-target-ready?
   :sun-disc-replacement-card-by-id sun-disc-replacement-card-by-id
   :sun-disc-replacement-card-options-for sun-disc-replacement-card-options-for
   :sun-disc-mode-definitions sun-disc-mode-definitions
   :sun-disc-mode-option-ids sun-disc-mode-option-ids
   :sun-disc-orientation-available? sun-disc-orientation-available?
   :sun-disc-target-cell sun-disc-target-cell
   :sun-disc-target-piece sun-disc-target-piece
   :sun-disc-territory-target? sun-disc-territory-target?
   :sun-disc-territory-target-stage? sun-disc-territory-target-stage?
   :sun-move? sun-move?
   :sword-damage-options-for sword-damage-options-for
   :sword-major-current-action sword-major-current-action
   :sword-major-current-action-complete? sword-major-current-action-complete?
   :sword-major-move? sword-major-move?
   :sword-move? sword-move?
   :sword-orientation-available? sword-orientation-available?
   :sword-replacement-card-by-id sword-replacement-card-by-id
   :sword-replacement-card-options-for sword-replacement-card-options-for
   :sword-replacement-card-source-option-ids sword-replacement-card-source-option-ids
   :sword-target-kinds sword-target-kinds
   :sword-target-piece sword-target-piece
   :sword-territory-target? sword-territory-target?
   :target-board-cell target-board-cell
   :territory-card-source-option-ids territory-card-source-option-ids
   :target-resolution-complete? target-resolution-complete?
   :target-space-complete? target-space-complete?
   :update-move-selection update-move-selection
   :valid-discard-card-ids valid-discard-card-ids
   :valid-judgement-card-ids valid-judgement-card-ids
   :valid-board-index? valid-board-index?
   :valid-wasteland-target? valid-wasteland-target?
   :world-copied-card world-copied-card
   :world-copied-power-ids-for-card world-copied-power-ids-for-card
   :world-copy-board-indexes world-copy-board-indexes
   :world-copy-board-cell world-copy-board-cell
   :world-move? world-move?})

(defn confirm-move
  ([db] (confirm-move db {}))
  ([db transition-options]
   (confirmation/confirm-move
    (confirmation-context)
    db
    transition-options)))
