# Direct Move-Entry Interaction Spec

This spec defines the direct manipulation grammar for browser move entry. It does not change Gnostica rules. Every gesture must stage the existing `gnostica.move-selection` fields, show the same legal-target descriptors already exposed by `move-legal-targets`, and confirm through the existing pure facades re-exported by `gnostica.game-state`.

Detailed entry remains the fallback path for every move. A gesture may make a move ready to confirm, but it must never mutate `:game` before the pending move is confirmed.

## Anchors

- App-db state: `:move-selection` holds staged fields; `:gesture-intent` holds gesture-derived pending metadata; `:turn-action` disables additional turn actions after a successful confirmation.
- Gesture adapter: `gnostica.gesture-intent/start-gesture-intent` accepts `{:source ... :target ... :fields ...}` maps, infers the move source, stages fields, and exposes Confirm, Cancel, and Detailed entry through `pending-move-tray-view`.
- Renderer descriptors: `gnostica.move-selection/move-legal-targets` is the shared source for legal and disabled territories, wastelands, pieces, hand cards, discard cards, draw pile, and stash pieces.
- Stable locations: board targets use cell `:index` values, not vector positions; wastelands use `{:kind :wasteland :row ... :col ...}`.

## Object Grammar

Hand card:
- Tap or click: start `:play-hand-card` unless the card is currently acting as a detailed-control choice such as discard, one-point territory card, or replacement card.
- Drag to board territory, wasteland, piece, hand/drop target, discard target, draw pile, or stash target: start `:play-hand-card`, set `:hand-card-id`, infer `:power` only when there is one valid implemented power, and set target fields when the drop target maps unambiguously.
- Drag while the active detailed step asks for a card: fill that card choice instead of starting a new source. This covers draw discards, Cup one-point cards, Disc/Sword/Sun replacements, and Judgement discard-pile draws.

Territory card:
- Tap or click: inspect the territory, open the territory overlay, then feed the selected board index into the active move when the current stage accepts a territory.
- Double-click or drag from a territory card: start `:activate-territory`, set `:source-board-index`, and require a current-player piece on that source territory as the minion.
- Drag a territory card to a legal wasteland while a Rod or Hermit territory move is active: stage `:rod-mode :push-territory` or Hermit target/destination fields according to the active source.

Piece:
- Tap or click: inspect its territory or wasteland if needed, then fill `:piece-id` or `:target-piece-id` when the active stage expects a minion or target piece.
- Drag an owned piece onto a compass direction without a different source: start `:orient-piece`, set `:piece-id`, and require `:orientation`.
- Drag an owned piece from a source context to a territory or wasteland: stage Rod minion movement, Hermit piece relocation, or an ordered major sub-action only when the active card power admits that movement.
- Drag a target piece along a minion direction while Rod push-piece is active: stage `:rod-mode :push-piece`, `:target-piece-id`, and snapped `:distance`.
- Enemy pieces are valid gesture targets for attacks, Cup enemy-piece creation, Hermit, Devil, Justice/Hanged Man hand trade, and Rod push-piece where rules allow. Enemy orientation is never exposed as a choice unless the power is Devil; pure transitions preserve enemy orientation elsewhere.

Stash piece:
- Drag the current player's small stash piece to an empty territory or legal wasteland when the current player has no pieces: start `:place-initial-small`, set the target, and require `:orientation`.
- Disabled state is shown when the current player already has a piece, lacks a small stash piece, the turn action is consumed, the game is finished, or the target is occupied or void.

Draw deck:
- Tap or click: start `:draw-cards`.
- Drag to the pending tray or detailed controls: stage the draw source, then require discard choices and `:draw-count`.
- As a Wheel Cup territory-card source, the draw deck is only legal when the selected Cup variant permits `:territory-card-source :draw-pile-top`.

Discard pile:
- Tap the top discard card only when the active stage asks for it, such as Judgement card selection or Star/Tower replacement. Otherwise it is an inert pile indicator.
- Drag a discard card as a replacement only when the active source permits discard-pile replacement. Invalid cards remain disabled with the exact value or source reason.

## Visual States

Idle:
- Objects look inspectable or draggable only when their object category can start or complete the current move. The cursor, focus ring, and accessible label should agree.

Draggable:
- The object advertises a drag affordance and `data` payload equivalent to the gesture map that would be sent to `start-gesture-intent`.
- Dragging an object starts or refreshes `:gesture-intent` but leaves `:game` unchanged.

Armed source:
- The selected hand card, territory, draw deck, piece, or stash piece is highlighted as the move source.
- The pending tray shows the source summary, current prompt, missing fields, Confirm if ready, Cancel, and Detailed entry.

Legal target:
- Legal territories, wastelands, pieces, hand cards, discard cards, draw deck, and stash pieces use the `:legal` status from `move-legal-targets`.
- Dropping on a legal target fills the matching field and refreshes the pending tray.

Disabled target:
- Disabled targets remain visible and explain why through descriptor `:reason` or `[:error :message]`.
- Dropping on a disabled target stages no mutation and records a structured selection error where the existing state machine would.

Preview:
- Movement previews show snapped cardinal path, distance, destination, and whether the landing space is territory, wasteland, full, void, or blocked by enemy occupancy.
- Mutation previews show before/after size, pips, territory value, replacement-card requirement, and source-card cost.
- Preview data is derived from staged fields and pure resolver checks; it is not a separate rules engine.
- Browser implementation note: `gnostica.move-selection/move-preview` exposes the shared preview view model. The CSS fallback renders spatial path, destination, mutation target, and compass overlays on the board; the Three.js path renders the same preview state in its board overlay with an orientation compass and smoke coverage.

Missing choice:
- When a gesture is incomplete, the pending tray lists the first missing field and alternatives from `gnostica.gesture-intent/alternatives`.
- Common missing choices are power, minion, target kind, action count, distance, damage, orientation, replacement-card source, replacement card, and draw count.

Error:
- Rejections display the structured gameplay or selection error without clearing staged fields.
- Error state keeps Cancel and Detailed entry available.

Pending confirmation:
- A ready gesture exposes the exact `move-command` preview, a primary Confirm action, Cancel, and Detailed entry.
- Confirm dispatches `:gnostica.app/confirm-move`, which may inject shuffle behavior for draw reshuffles and Wheel draw-pile territory creation.

Consumed-turn disabled:
- After a successful move, all move sources and gesture starts are disabled until a successful end-turn transition clears `:turn-action`.

## Ambiguity Rules

- If a card has multiple implemented powers, the gesture must stop at `:power` and show alternatives. Do not guess based only on the target.
- If a gesture could mean Cup territory placement, Cup enemy-piece creation, or Cup wasteland territory creation, the target kind decides only when it is unambiguous. Wasteland creation may still require card source and one-point-card choices.
- If a Rod gesture could mean minion movement, push-piece, or push-territory, the dragged object and active target decide the mode. If the result is ambiguous, require the Rod mode control.
- If Disc or Sword could target either a piece or territory, the drop target decides `:disc-target-kind` or `:sword-target-kind`; otherwise show the target-kind segmented control.
- If a replacement can come from hand or discard pile, require `:replacement-card-source` before accepting a card unless the card's zone uniquely determines the source.
- If Strength, Death, Sun, Chariot, Devil, Justice, or Hanged Man needs an action count, require the count before interpreting later gestures as final actions.
- If World is selected, require the copied major territory first and copied power second when the copied card exposes more than one implemented power.
- If the active stage asks for orientation and the gesture cannot infer it from a compass, ring, or keyboard direction, require the orientation control before confirmation. During an eligible piece or first-placement stash-piece drag, arrow keys may set the pending orientation before drop and `O` cycles through up, north, east, south, and west. After a first-placement drop has staged a territory or wasteland target but before confirmation, the same arrow and `O` hotkeys continue to update the staged orientation.

## Input Modes

Mouse and trackpad:
- Drag starts after a movement threshold so normal clicks still inspect cards or pieces.
- Empty-table gestures keep OrbitControls behavior. Object drags suppress OrbitControls only for that active drag.
- Drop targets update continuously as the pointer moves.

Touch and pen:
- Long-press or press-and-hold arms draggable objects; moving after the hold starts a drag.
- Tap still selects or inspects. When tap could also choose a move field, the active prompt decides the action.
- Orientation uses a radial compass large enough for touch. A drag release may open the compass instead of committing orientation.

Keyboard:
- Every gesture action has a detailed-control equivalent in `gnostica.ui.move-panel`.
- Focused objects can be activated with Enter or Space. Arrow keys or WASD remain board panning when the 3D board has focus and no drag or first-placement pending orientation is active; eligible active piece drags and pending first-placement moves temporarily use arrow keys and `O` for orientation without mutating `:game`.
- The pending tray exposes Confirm, Cancel, and Detailed entry as focusable controls.

Reduced motion:
- Prefer static ghost positions, target outlines, and text status over animated trails.
- Movement path previews may update position immediately without easing.
- Error and pending states should not rely on motion alone.

## Command Examples

Cup:
- Gesture: drag a Cups hand card to a legal empty territory, choose a minion if needed, then choose orientation.
- Staged fields: `:source :play-hand-card`, `:hand-card-id`, `:piece-id`, `:power :cup`, `:target-board-index`, `:orientation`.
- Command shape: `apply-cup-move` receives `{:player-id ..., :source {:kind :hand-card :card-id ... :piece-id ...}, :cup-variant ..., :target {:kind :territory :board-index ...}, :orientation ...}`.

Rod:
- Gesture: activate a Rod territory through an owned minion, drag the minion two cardinal spaces to a legal destination, then choose orientation if the moved piece is owned.
- Staged fields: `:source :activate-territory`, `:source-board-index`, `:piece-id`, `:power :rod`, `:rod-mode :move-minion`, `:distance 2`, optional `:orientation`.
- Command shape: `apply-rod-move` receives `{:mode :move-minion, :distance 2, :rod-variant ..., :orientation ...}` merged with player and source.

Disc:
- Gesture: drag a Disc source onto a territory, then drag a valid +1 replacement card from hand onto that territory.
- Staged fields: `:power :disc`, `:disc-target-kind :territory`, `:target-board-index`, `:replacement-card-source :hand`, `:replacement-card-id`.
- Command shape: `apply-disc-move` receives `{:disc-variant ..., :target {:kind :territory :board-index ...}, :replacement-card-source :hand, :replacement-card-id ...}`.

Sword:
- Gesture: drag a Sword source onto a target piece, choose damage constrained by target pips, then choose survivor orientation only if the survivor is owned by the current player.
- Staged fields: `:power :sword`, `:sword-target-kind :piece`, `:target-piece-id`, `:damage`, optional `:orientation`.
- Command shape: `apply-sword-move` receives `{:sword-variant ..., :target {:kind :piece :piece-id ...}, :damage ..., :orientation ...}`.

Ordered major:
- Gesture: play Lovers from hand, choose a minion, perform the Rod movement gesture, then perform the Cup target gesture.
- Staged fields: `:power :lovers`; completed Rod action is stored under `:major-actions`, then the Cup target fields complete the final action.
- Command shape: `apply-lovers-move` receives `{:source ..., :actions [{:power :rod ...} {:power :cup ...}]}` with the hand source charged once.

Draw major:
- Gesture: play High Priestess from hand, choose one redraw pass, mark two hand cards for discard, and choose draw count two.
- Staged fields: `:power :high-priestess`, `:high-priestess-redraw-count 1`, `:redraws [{:discard-card-ids [...] :draw-count 2}]`.
- Command shape: `apply-high-priestess-move` receives `{:source ..., :redraws [...]}` plus injected `:shuffle-fn` only when needed.

World:
- Gesture: play World from hand, choose a non-World major territory to copy, choose the copied power if needed, then use that power's normal gesture controls.
- Staged fields: `:power :world`, `:copied-board-index`, optional `:copied-power`, plus copied-power fields.
- Command shape: `apply-world-move` receives `{:source ..., :copied-board-index ..., ...copied command fields...}`. World remains the paid source.

Orient:
- Gesture: drag an owned piece to a compass direction without an active card source.
- Staged fields: `:source :orient-piece`, `:piece-id`, `:orientation`.
- Command shape: `apply-orient-move` receives `{:player-id ..., :piece-id ..., :orientation ...}`.

Initial placement:
- Gesture: drag the current player's small stash piece to an empty territory or legal wasteland, then choose orientation.
- Staged fields: `:source :place-initial-small`, target territory or wasteland, `:orientation`.
- Command shape: `apply-initial-placement` receives `{:player-id ..., :target {:kind :territory :board-index ...}}` or `{:target {:kind :wasteland :row ... :col ...}}` plus orientation.

## Detailed Entry Fallback

The fallback entry point is the pending tray's Detailed entry action, backed by `gnostica.gesture-intent/open-detailed-entry`. It keeps the staged gesture fields and opens the existing move panel controls. The app-level direct-manipulation setting can also make Detailed entry the session default; in that mode the move panel is open from setup and gesture-started pending moves open the same detailed controls without clearing staged choices.

Recommend Detailed entry when:
- the gesture is ambiguous across powers, variants, target kinds, action counts, or replacement sources;
- the move needs multi-step ordered actions;
- orientation, damage, distance, draw count, or card selection is still missing;
- a pointer device is unavailable, unreliable, or disabled;
- a structured error needs repair without restarting the move.

Classic staged controls must remain usable even if all direct manipulation affordances are disabled.

Manual accessibility checks for this fallback:
- Disable pointer dragging or enable the Detailed entry default, then complete representative Cup, Rod, Disc, Sword, orient, initial-placement, and major-power selections using only focusable move-panel controls plus board/card clicks.
- Tab through the pending tray and move panel; focus should remain visible on source, power, minion, target, replacement, distance, damage, orientation, draw-count, Confirm, and Cancel controls.
- Start a partial gesture, choose Detailed entry, finish the missing fields from the move panel, and confirm; `:game` should remain unchanged until confirmation.

## Implementation Phases

1. Keep the current card and territory gesture adapter as the base: hand-card and draw-pile gestures, territory source gestures, CSS fallback territory/wasteland drops, pending tray, and Detailed entry handoff.
2. Add piece and stash-piece object descriptors to board and move-panel view models, then wire click, keyboard activation, and CSS fallback drag payloads.
3. Extend Three.js picking from territory card selection to draggable cards, piece meshes, wasteland outlines, and stable drop targets; suppress OrbitControls only during active object drags.
4. Implemented: previews for Rod paths, Hermit relocation, initial placement, and orientation compass selection are exposed through the shared `move-preview` model and rendered in both CSS fallback and Three.js board paths.
5. Implemented initial mutation previews for Disc, Sword, and Sun target/value changes; future work can deepen this into richer piece-size glyphs, replacement-card ghosting, damage markers, and same-target shortcut visuals.
6. Add ordered-major and World action ribbon support so completed sub-actions are visible and editable before confirmation.
7. Add integration and smoke coverage for gesture entry, disabled targets, no mutation on invalid drops, cleanup on unmount, and CSS fallback parity.
