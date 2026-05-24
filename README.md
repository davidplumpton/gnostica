# Gnostica

A Clojure and ClojureScript tarot app using re-frame and shadow-cljs. The current app is a local browser implementation focused on board rendering, shared pure game state, and early move-selection flows. `gnostica.app` is the small browser shell/startup entry point; re-frame wiring lives in `gnostica.app.events`, keyboard startup lives under `gnostica.app.*`, fixture/demo init options live in `gnostica.fixtures`, and focused view namespaces under `gnostica.ui.*` render the board fallback, card zones, territory panel, move panel, header, and help dialogs. The Ring server only serves the released app assets; multiplayer sync and state import/export workflows are not implemented yet.

The implementation rules reference is `docs/rules.txt`, the local text copy used while building and testing gameplay behavior. The upstream Gnostica PDF remains the provenance source: https://www.looneylabs.com/sites/default/files/pyramid_rules/Gnostica.pdf. If the local text and upstream PDF differ, treat `docs/rules.txt` as authoritative for this repo's current implementation context until the local copy is deliberately reconciled.

The game was designed by John Cooper, Kory Heath, Kristin Matherly, and Jacob Davenport. It makes use of a deck of Tarot cards and Looney Pyramids (https://www.looneylabs.com/pyramids-home).

## Development

Install Java, the Clojure CLI, and Node.js, then run:

```sh
clojure -M:dev
```

The shadow-cljs dev server serves the app at `http://localhost:8080/index.html`. Node.js is still required by shadow-cljs for browser builds, but the default workflow does not use npm.

For the released Ring server path, run `clojure -M:release` first. `clojure -M:server` serves the existing files under `src/main/resources`, including the compiled browser bundle at `src/main/resources/js/main.js`; it does not rebuild stale or missing JavaScript assets.

The browser runtime loads Three.js and OrbitControls from pinned `three@0.128.0` CDN scripts with SRI hashes and `crossorigin="anonymous"` before the compiled ClojureScript bundle. This keeps browser builds npm-free while still exposing the global `THREE` and `THREE.OrbitControls` values used by the `gnostica.three-board` renderer. The app only enables the 3D renderer when `THREE.REVISION` is `"128"` and OrbitControls is present.

## 3D Board View

The first screen renders the nine-card territory board with Three.js when the CDN globals are available. The public renderer component is `gnostica.three-board/scene`; focused `gnostica.three-board.*` namespaces handle runtime detection, lifecycle orchestration, resource cleanup, camera controls, pointer picking, and scene graph assembly. It uses the shared board and Icehouse piece models, loads tarot card images as textures through `gnostica.three-card-textures`, alternates portrait and landscape card planes over a dark velvet table surface, leaves small gaps between neighboring cards, and uses `OrbitControls` for mouse, trackpad, and touch camera movement. The focused 3D board also supports WASD and arrow-key panning with bounded camera-target movement so the board remains recoverable. Browser-free card coordinate, wasteland detection, board-index, piece slot, pip marker, and piece height helpers live in `gnostica.board-layout` and are covered by Clojure tests. Card picking is handled with a Three.js raycaster and updates the selected territory panel through callbacks from `gnostica.ui.board` into the stable `:gnostica.app/select-board-card` event.

Wasteland spaces are the empty orthogonal neighbors of current territory cards. The Three.js board draws faint dashed outlines for those spaces below the cards and pieces, and the CSS fallback renders a matching velvet play mat with non-interactive dotted outlines behind the card buttons so legal empty spaces remain visible without affecting selection.

Icehouse pieces are represented in shared state by `gnostica.pieces`. The local browser startup opts into demo board pieces from `gnostica.fixtures` so the renderer can display player color, size/pips, up/cardinal orientation, and up to three pieces on one territory without making fixtures part of the production piece model. Three.js renders these as lit four-sided cone meshes over the cards with black edge outlines and one, two, or three pale pip markers near the base; the CSS fallback renders matching color/orientation/pip markers and remains clickable.

If either CDN global is missing, if the loaded Three.js revision is not r128, or if the browser cannot create a WebGL renderer, the app falls back to the CSS board so the game remains usable while surfacing a runtime warning. Advanced compilation support for the CDN global is declared in `src/main/externs/three.ext.js`, including the geometry and line APIs used for piece edge outlines; keep future Three.js add-ons on the same `three@0.128.0` release line unless the CDN scripts, SRI hashes, revision gate, smoke checks, and externs are updated together.

## Shared Game State

The authoritative gameplay state contract starts at the `gnostica.game-state` facade. It is browser-free and stable for `clojure.test`, the custom Gherkin-style feature harness, and re-frame events, while setup and shared state helpers live in `gnostica.game-state.core` and move families live in focused `gnostica.game-state.draw`, `.cup`, `.rod`, and `.placement` namespaces. `create-game` accepts explicit player specs plus either an injected `:deck-order` or deterministic `:shuffle-fn`, enforces two to six known players, deals each player a six-card starting hand before the territory board, reuses `gnostica.board` for the nine-card territory grid, and models phase, players, turn order, pieces/stashes, draw pile, discard pile, setup/bid metadata, and compact history events. `:deck-order` and `:shuffle-fn` order the full source deck once at the `create-game` boundary; after hands are dealt, `create-game` passes the remaining cards to `gnostica.board/initial-board` with `identity` so normal setup does not reshuffle the board cards.

Player ids must reference the six metadata-backed entries in `gnostica.pieces/players` so every legal setup has a display name, Three.js color, and CSS color. Injected decks are validated before board setup: cards must be ordered maps with unique string `:id` values plus nonblank `:title` and `:image` fields needed by rendering and territory details.

Starting deck accounting is explicit: the first six cards per player become player hands, the next nine cards become board territories, the remaining cards become `:draw-pile`, and `:discard-pile` starts empty. Each player map also carries its initial Icehouse `:stash`, mirrored by the existing `[:pieces :stashes]` index. Tests assert that all 78 tarot cards appear exactly once across those zones.

The browser app initializes through `gnostica.app-state/initialize`, which stores the `create-game` result under app-db `:game`. `gnostica.app.events` registers the stable `:gnostica.app/...` event and subscription ids, while `gnostica.app.handlers` keeps event-level initialization and move-confirmation transitions testable with injected shuffle seeds. Browser events inject a seed at the re-frame cofx boundary and convert it through `gnostica.deterministic-shuffle`, so default game setup and draw-pile reshuffles do not rely on implicit randomness inside `reg-event-db` handlers. Board, piece, selected-territory, and current-player subscriptions read through the shared game value instead of maintaining a second top-level board or piece setup path. `app-state/initialize` applies visible demo pieces only when explicit `:demo-board-pieces` options are provided; `gnostica.fixtures/browser-init-options` supplies those options for the local browser demo and smoke deck orders. The pieces are kept in the game state's `[:pieces :on-board]` slot through `gnostica.game-state/with-board-pieces`, so renderer smoke checks still exercise piece layout while player stashes and the `[:pieces :stashes]` mirror are debited consistently.

The main screen also displays the current player's hand, the draw deck, and the discard pile from `:game`. The hand zone shows the current player's card images and titles, the draw deck shows a facedown deck affordance with remaining-card count, and the discard pile shows an empty state until a top discarded card exists. These zones derive from pure gameplay state: the standalone draw/discard turn option uses `apply-draw-move`, while Cup and Rod hand-card move transitions move the source card from hand to discard.

Rule helpers return explicit result maps: successful transitions use `{:ok? true :state ... :events [...]}`, while validation errors use `{:ok? false :error {:code ... :message ... :data ...}}`. The public `gnostica.game-state/apply-draw-move` facade delegates to `gnostica.game-state.draw`; it accepts `:player-id`, optional `:discard-card-ids`, `:draw-count`, and an optional deterministic `:shuffle-fn`, discards selected hand cards, draws toward the six-card hand limit, reshuffles the discard pile when the draw pile is exhausted, preserves deck accounting, and appends history.

`gnostica.game-state/apply-cup-move` delegates to `gnostica.game-state.cup` and accepts an explicit command with `:player-id`, a source map for either a Cup territory or Cup hand card plus acting `:piece-id`, a carried `:cup-variant`, and a target map for an existing territory, enemy piece, or wasteland coordinate. It places the current player's small piece with `:orientation`, targets enemy pieces to create one of that enemy's small pieces with the target's orientation, creates territories in legal wastelands from hand one-point cards or Wheel's optional top draw-pile card, discards hand-source Cup cards, preserves deck and schema invariants, and reports structured rejection errors. Plain Cup and Magician wild-suit Cup use the normal three-piece limit, Empress-style `:cup-unbounded` ignores that target-space limit, and Wheel-only draw-pile territory creation is rejected for other Cup variants.

`gnostica.game-state/apply-rod-move` delegates to `gnostica.game-state.rod` and builds on `resolve-rod-command` for pure Rod movement. It supports moving the acting minion, pushing a target piece, and pushing a target territory in the minion's facing direction, while carrying the resolved `:rod-variant` from plain Wands/Rod, Magician wild suits, or Emperor-style `:rod-unbounded` sources. Piece moves enforce positive distance up to the minion pip count, reject upright minions and void destinations, reject full destination territories for plain and wild-suit Rods, allow `:rod-unbounded` moves into full territories, represent wasteland destinations with explicit `:space` coordinates, allow orientation changes only for moved current-player pieces, and preserve enemy orientations. Territory pushes reject enemy-occupied target territories and enemy-occupied landing wastelands, land only in wasteland spaces, leave pieces behind at the source coordinate, place the moved territory under own pieces already in the landing wasteland, update the territory orientation for the board pattern, discard hand-source Rod cards, and append history events.

`gnostica.game-state/apply-orient-move` and `gnostica.game-state/apply-initial-placement` delegate to `gnostica.game-state.placement`. Orient turns one current-player piece to a legal orientation, preserving the same board or wasteland space and appending history. Initial placement places the current player's first small piece, when they have no pieces on the board, onto an empty territory or empty wasteland in the selected orientation while keeping player and stash mirrors reconciled.

## Gameplay Move Selection

The browser app includes a current-player move panel rendered by `gnostica.ui.move-panel` and driven from shared app-db state in `gnostica.app-state`. Move selection is stored outside `:game` under `:move-selection`, while board, pieces, card zones, hand cards, draw counts, and player metadata continue to derive from the authoritative game state. The staged move-selection engine lives in browser-free `gnostica.move-selection`; `gnostica.app-state` remains the app-db boundary and exposes the UI-facing facade used by re-frame subscriptions and events.

The current flow lets the player choose a move source, then stage the required source territory, hand card, minion, implemented card power, Rod mode, target piece or territory, target wasteland, Wheel territory-card source, one-point territory card, distance, draw count, or orientation. Three.js card picking and the CSS fallback board both dispatch through the same app-state board-selection event so inspection state updates before existing-territory selections are delegated into `gnostica.move-selection`; the move panel exposes Cup enemy-piece targets, Cup and initial-placement wasteland targets, and Rod target-piece controls directly. Confirming completed draw-card moves calls `apply-draw-move` with an explicit shuffle function when the re-frame event boundary supplies one for discard reshuffles; confirming completed territory or hand-card Cup moves carries the selected Cup variant into `apply-cup-move` for current-player small-piece placement, enemy-piece small-piece creation, one-point-card wasteland territory creation, or Wheel top-draw-pile wasteland territory creation; confirming completed Rod minion movement, piece pushing, or territory pushing carries the selected Rod variant into `apply-rod-move`; confirming completed orientation and first-small-placement moves calls `apply-orient-move` or `apply-initial-placement`. Successful confirmations replace app-db `:game`, while rejected confirmations keep the staged selection with the structured gameplay error. Move sources without a pure transition still return an explicit `:move-transition-unavailable` result instead of mutating game state.

## Keyboard Shortcuts

The app header in `gnostica.ui.header` exposes a `?` control, the global listener in `gnostica.app.keyboard` handles browser shortcuts, and `gnostica.ui.help` renders the command and icon dialogs. The dialog lists the currently supported browser shortcuts: `?` opens the dialog, `G` opens a special move icon guide with detailed explanations, `I` toggles Gnostica card icon overlays between always-visible and hover/focus popup modes, WASD or arrow keys move the focused 3D board view, and `Esc` closes open help dialogs.

The `I` shortcut should preserve the current 3D OrbitControls camera view; only the `Reset view` control should reset the board camera.

## Gameplay Schema and Feature Tests

`gnostica.game-schema` provides Malli schemas for the pure gameplay data contract: card references, board cells, player state, six-card hand limits, Icehouse pieces, draw/discard piles, temporary wasteland piece coordinates, post-setup board growth, and top-level game state invariants including piece space references and stash plus active-piece accounting. Use `valid-game?`, `explain-game`, and `assert-valid-game` in tests, Gherkin-style feature steps, and other pure-data boundaries where a readable state-shape failure is more useful than renderer or re-frame behavior.

Gameplay rule examples live in Gherkin-style `.feature` files under `features/` and run through the custom `gnostica.feature-runner`. Step definitions and reusable test-world helpers live under `test/gnostica/feature_steps.clj` and `test/gnostica/feature_world.clj`; they create deterministic games, apply pure actions, inspect state paths, and include Malli explanations in failing step output. Current scenarios cover deterministic setup plus Rod minion movement, piece pushing, territory pushing, distance and destination rejection, enemy-occupied territory-push rejection, own-wasteland landing behavior, and owned/enemy orientation rules. `clojure -M:test` runs these feature scenarios alongside the existing `clojure.test` namespaces.

### Verification

For the 3D board slice, run:

```sh
clojure -M:test
clojure -M:release
clojure -M:smoke
```

`clojure -M:smoke` starts the released Ring app unless `SMOKE_URL` points at an already-running dev or release server. The default local smoke path needs a current `src/main/resources/js/main.js`, normally produced by `clojure -M:release`; if that file is missing or stale, rebuild before running smoke. The smoke runner drives a local headless Chrome/Chromium through the DevTools protocol, so no npm workflow is added. Set `SMOKE_CHROME` or `CHROME_PATH` if Chrome is not in a standard location.

The smoke checks desktop and mobile viewport widths, verifies the r128 Three.js and OrbitControls globals, confirms the canvas screenshot is nonblank with visible board content and dark velvet table pixels, verifies antialiasing is requested and enabled when the browser reports support, waits for nine card texture loads, checks icon texture renderer metadata, checks the twelve initial wasteland outline targets and visible piece edge outline count, fails on happy-path texture/fallback status messages, verifies the reset control is present, confirms WASD and arrow-key board movement updates the camera target, confirms `?` and `G` open their help dialogs, confirms `I` preserves a changed 3D camera distance while toggling icon mode, clicks the center 3D card and checks that the territory panel updates, blocks the pinned CDN scripts to verify the missing-global CSS fallback path, and injects a mismatched Three.js revision to verify that version drift also falls back before user interaction. Pip marker count and placement are covered by the browser-free layout tests.

The smoke checks also assert that the current-player card zones are present in the Three.js and fallback layouts, including six visible hand cards, a non-empty draw deck count, and the initial empty discard pile state.

## Commands

```sh
clojure -M:dev       # start the ClojureScript watcher
clojure -M:release   # compile an optimized browser build
clojure -M:server    # serve existing released assets with Clojure/Ring
clojure -M:test      # run Clojure tests and gameplay feature scenarios
clojure -M:smoke     # run the headless Chrome 3D board smoke check
```

Run `clojure -M:release` before `clojure -M:server` or the default `clojure -M:smoke` path so `src/main/resources/js/main.js` is current. `SMOKE_URL` can point smoke at an already-running dev or release server instead.

## Version Control

This project is initialized for Jujutsu:

```sh
jj status
jj diff
jj describe -m "Describe the current change"
jj commit -m "Commit message"
jj new
```
