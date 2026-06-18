# Architecture Reference

This is the canonical prose map for Gnostica subsystem ownership. Keep
`README.md` as a quick-start document, `AGENTS.md` as local workflow guidance,
and `MIND_MAP.md` as a compact navigation index.

The exact game rules are in `docs/rules.txt`. The exact command shapes are
exposed through `src/main/gnostica/game_state/command_contracts.cljc`, with
focused schema ownership under `game_state/command_contracts/`. The exact
semantic behavior is in the owning source namespace and its tests.

## Public Boundaries

- `gnostica.game-state` is the stable browser-free gameplay facade.
- `gnostica.app-state` is the stable browser-free app-db facade.
- `gnostica.move-selection` stages browser move choices and commands.
- `gnostica.power-taxonomy` owns shared browser-free card power taxonomy.
- `gnostica.gesture-intent` adapts direct gestures into staged moves.
- `gnostica.app.ids` owns stable `:gnostica.app/...` keyword ids.
- `gnostica.app.events` re-exports public UI event and subscription ids.
- `gnostica.ui.*` namespaces render from composed view-model subscriptions.
- `gnostica.three-board/scene` is the public Three.js board component.
- `gnostica.fixtures` owns local lobby, demo, and smoke fixture data.

## Gameplay State

`src/main/gnostica/game_state.cljc` re-exports public gameplay APIs. Focused
implementation namespaces live under `src/main/gnostica/game_state/`.

- `core` keeps compatibility aliases for existing gameplay helper callers.
- `constants` owns browser-free player, hand, piece, phase, target-score, and
  required-field constants.
- `collections` owns generic collection helpers used across gameplay setup and
  validation.
- `players` owns player lookup, player-map rebuilding, player updates, history,
  turn seeding, and current-player checks.
- `hands` owns hand lookup, hand mutation, and discard-pile mutation.
- `sources` owns compact source summaries and source-card cost application.
- `pieces` owns stash accounting plus active-piece lookup, coordinates, and
  mutation.
- `shared` re-exports compatibility aliases for older focused helper callers.
- `result` and `score` own structured result maps and score derivation.
- `setup` keeps the public setup facade and game creation orchestration.
- `setup.creation` owns player validation, base game construction, and the
  initial hand and board deal.
- `setup.starting-bid` owns official starting-bid ranking, ties, rebids, and
  setup history.
- `setup.redraw` owns bid-card redraw order, hand refill accounting, and
  starting-player rotation.
- `deck` owns deck validation, ordering, and draw-pile refresh validation.
- `board-pieces` owns seeded board-piece validation and stash mirror rebuilding.
- `turn` owns turn availability, challenge resolution, elimination, and endgame.
- `placement` owns initial small-piece placement and orient-piece moves.
- `draw` owns standalone draw/discard plus Fool, High Priestess, and Judgement.
- `cup`, `rod`, `disc`, and `sword` own base suit validation and transitions.
- `disc-major` owns Strength, Star, and Sun orchestration.
- `sword-major` owns Justice, Death, Tower, and Moon orchestration.
- `major` owns reusable major-source charging and ordered sequencing.
- `major-power` owns the `apply-card-power` full-card dispatch point and
  default unavailable-power result; implemented defmethods stay with their
  gameplay-family owners in `draw`, `composite`, `manipulation`, `disc-major`,
  `sword-major`, and `world`.
- `composite` owns Empress, Emperor, Lovers, Chariot, Hanged Man, Temperance.
- `manipulation` owns Hierophant, Hermit, and Devil.
- `world` delegates World copies to existing implemented powers.
- `spatial` owns coordinate math, board lookup, wasteland legality, territory
  movement, and piece-space application.
- `card_source` and `suit-target` hold shared move-resolution helpers.
- `command_contracts` is the public Malli contract facade; its focused
  subnamespaces own primitive/source/target schemas, suit schemas, full-card
  schemas, delegated World/Fool validation, basic draw/orient/placement
  commands, and result validation.
- `gnostica.power-taxonomy` owns suit powers, implemented full-card powers,
  card-id mapping, and World/Fool copied-play eligibility.

Gameplay transitions should stay browser-free, return structured result maps,
append compact history events, and preserve schema, deck, piece, and stash
accounting. UI renderers and re-frame event handlers should call these facades
rather than duplicating rule behavior.

## Setup, Scores, and Turns

`create-game` accepts explicit player specs plus either `:deck-order` or a
deterministic `:shuffle-fn`. It deals six-card hands before the nine-card board
and calls `gnostica.board/initial-board` with `identity` for the remaining board
slice.

Player ids come from `gnostica.pieces/players`. Target scores are 8, 9, or 10.
Optional starting bids are resolved by `setup.starting-bid`; bid-card refill and
winner-first turn rotation are handled by `setup.redraw`. Scores, challenge
resolution, failed-challenge elimination, and last-active-player wins are pure
game-state behavior.

## App State and Re-frame

`src/main/gnostica/app_state.cljc` is the public app-db facade. Focused
implementation lives under `src/main/gnostica/app_state/`.

- `db` owns defaults, normalization, and game initialization.
- `lobby`, `lobby_setup`, `lobby_bidding`, and `lobby_start` own setup flows.
- `lobby_view_models` and `view_models` compose UI-facing derivations.
- `gestures` bridges gesture and keyboard placement state into app-db.
- `moves` wires move-selection confirmation and end-turn app-db handling.
- `facade-exports` owns the public move-facade export tables shared by
  `app-state.moves` and `app_state.cljc`; keep changes covered by
  `facade_exports_test`.
- `facade-macros` owns `def-facade-aliases`, which expands those export tables
  into facade vars for CLJ and CLJS callers.

Top-level UI state includes `:lobby`, `:move-selection`, `:gesture-intent`,
`:keyboard-placement-targeting`, `:direct-manipulation`, `:turn-action`,
`:turn-result`, `:open-panels`, and Three.js runtime or renderer errors.
Board, pieces, card zones, current player, scores, challenges, and winner data
derive from `:game`.

Event handlers inject deterministic shuffle functions through
`gnostica.app.handlers` and `gnostica.deterministic-shuffle`; avoid implicit
randomness inside `reg-event-db` paths.

## Move Selection and Gestures

`gnostica.move-selection` is the staged browser-free move facade. Focused
namespaces under `src/main/gnostica/move_selection/` own state selectors,
active power context, source availability, prompt text, target option derivation,
High Priestess redraw staging, target validation, selection update handlers, the
registry, staging, flow requirements, stage mapping, sequence advancement,
options, controls, legal targets, command building, confirmation, action ribbon,
previews, and helper contexts.

Keep power labels, control kinds, command builder keys, transition facades,
previewability, and renderer control keys in the registry. Card-to-power mapping
and World/Fool copied-play eligibility come from `gnostica.power-taxonomy`.
Move-panel control rendering is split across focused UI renderer namespaces under
`gnostica.ui.move-panel.controls.*`, assembled by
`gnostica.ui.move-panel.controls.registry`, and checked against
`gnostica.ui.move-panel.renderer-registry`. Detailed controls, direct gestures,
CSS fallback, Three.js picking, card zones, and the pending tray should share
`move-legal-targets` descriptors.

Direct manipulation follows `docs/direct_move_entry_spec.md`. Gestures stage
choices without mutating `:game`; only confirmation applies gameplay results.

## Board and Rendering

`gnostica.board` creates the initial territory grid. `gnostica.board-layout`
owns browser-free geometry and stable board-index helpers used by app-state,
move-selection, CSS fallback, Three.js, and tests.

The browser board renders through `gnostica.ui.board`. It chooses
`gnostica.three-board/scene` when the pinned Three.js globals are usable and
falls back to the CSS board otherwise. Three.js internals live under
`src/main/gnostica/three_board/` for runtime detection, resources, controls,
pointer handling, scene graph assembly, accessibility, and lifecycle.

Use stable cell `:index` values for UI board references. Territory destruction
can leave gaps, and new territories can append higher indexes.

## Fixtures

`gnostica.fixtures` owns the local prefilled lobby, dev shared-control option,
dev demo hotkey option, demo board pieces, smoke deck orders, smoke setup, and
browser query-param initialization.

Keep fixture data out of pure game construction and `gnostica.pieces`. Strip
lobby-only controller metadata before calling `gnostica.game-state/create-game`.

## Tests and Verification

`clojure -M:lint` runs the br tracker guard, clj-kondo, and cljfmt verification.
`clojure -M:test` runs `gnostica.test-runner`, unit tests, and feature scenarios.

Feature scenarios live under `features/`; step definitions and shared worlds
live under `test/gnostica/feature_steps.clj` and
`test/gnostica/feature_world.clj`.

Focused regression anchors include starting-bid ranking, ties, rebids, and
redraw order; generalized `:major-action-count` staging for paired majors;
Devil orientation counts; Moon sword-only staging; Magician wildcard routing;
Fool skipped reveals; Wheel draw-pile Cup territory creation; and void cleanup
after Sword territory destruction or Hermit relocation.

For 3D board, CDN, extern, release, or browser-smoke behavior, run
`clojure -M:release` before `clojure -M:smoke`. The smoke runner covers the
Three.js path, CSS fallback paths, card zones, icon modes, move controls,
direct entry, keyboard first placement, help modals, and selected board
interactions.

## Documentation Ownership

- `README.md` gives a quick project overview, commands, and workflow links.
- `AGENTS.md` gives local agent workflow and invariants.
- `MIND_MAP.md` indexes project knowledge with compact node references.
- `docs/architecture.md` is the canonical prose architecture reference.
- `docs/direct_move_entry_spec.md` is canonical for direct manipulation UX.
- `docs/rules.txt` is canonical for local gameplay rules.

When a subsystem moves, update this document and the affected mind-map node.
Avoid copying detailed per-power contracts into multiple entry-point docs.
