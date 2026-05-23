# Gnostica

A Clojure and ClojureScript tarot app using re-frame and shadow-cljs. The current app is a local browser implementation focused on board rendering, shared pure game state, and early move-selection flows. The Ring server only serves the released app assets; multiplayer sync and state import/export workflows are not implemented yet.

Rules are available here https://www.looneylabs.com/sites/default/files/pyramid_rules/Gnostica.pdf

The game was designed by John Cooper, Kory Heath, Kristin Matherly, and Jacob Davenport. It makes use of a deck of Tarot cards and Looney Pyramids (https://www.looneylabs.com/pyramids-home).

## Development

Install Java, the Clojure CLI, and Node.js, then run:

```sh
clojure -M:dev
```

The shadow-cljs dev server serves the app at `http://localhost:8080/index.html`. Node.js is still required by shadow-cljs for browser builds, but the default workflow does not use npm.

The browser runtime loads Three.js and OrbitControls from pinned `three@0.128.0` CDN scripts with SRI hashes and `crossorigin="anonymous"` before the compiled ClojureScript bundle. This keeps browser builds npm-free while still exposing the global `THREE` and `THREE.OrbitControls` values used by the `gnostica.three-board` renderer. The app only enables the 3D renderer when `THREE.REVISION` is `"128"` and OrbitControls is present.

## 3D Board View

The first screen renders the nine-card territory board with Three.js when the CDN globals are available. The renderer lives in `gnostica.three-board/scene`, uses the shared board and Icehouse piece models, loads tarot card images as textures, alternates portrait and landscape card planes over a dark velvet table surface, leaves small gaps between neighboring cards, and uses `OrbitControls` for mouse, trackpad, and touch camera movement. Browser-free card coordinate, wasteland detection, board-index, piece slot, pip marker, and piece height helpers live in `gnostica.board-layout` and are covered by Clojure tests. Card picking is handled with a Three.js raycaster and updates the selected territory panel through callbacks into `gnostica.app`.

Wasteland spaces are the empty orthogonal neighbors of current territory cards. The Three.js board draws faint dashed outlines for those spaces below the cards and pieces, and the CSS fallback renders a matching velvet play mat with non-interactive dotted outlines behind the card buttons so legal empty spaces remain visible without affecting selection.

Icehouse pieces are represented in shared state by `gnostica.pieces`. The current board viewer seeds a small visible position so the renderer can display player color, size/pips, up/cardinal orientation, and up to three pieces on one territory. Three.js renders these as lit four-sided cone meshes over the cards with black edge outlines and one, two, or three pale pip markers near the base; the CSS fallback renders matching color/orientation/pip markers and remains clickable.

If either CDN global is missing, if the loaded Three.js revision is not r128, or if the browser cannot create a WebGL renderer, the app falls back to the CSS board so the game remains usable while surfacing a runtime warning. Advanced compilation support for the CDN global is declared in `src/main/externs/three.ext.js`, including the geometry and line APIs used for piece edge outlines; keep future Three.js add-ons on the same `three@0.128.0` release line unless the CDN scripts, SRI hashes, revision gate, smoke checks, and externs are updated together.

## Shared Game State

The authoritative gameplay state contract starts in `gnostica.game-state`. It is a shared `.cljc` namespace with no browser dependencies, so it can be used from `clojure.test`, the custom Gherkin-style feature harness, and re-frame events. `create-game` accepts explicit player specs plus either an injected `:deck-order` or deterministic `:shuffle-fn`, enforces two to six known players, deals each player a six-card starting hand before the territory board, reuses `gnostica.board` for the nine-card territory grid, and models phase, players, turn order, pieces/stashes, draw pile, discard pile, setup/bid metadata, and compact history events.

Player ids must reference the six metadata-backed entries in `gnostica.pieces/players` so every legal setup has a display name, Three.js color, and CSS color. Injected decks are validated before board setup: cards must be ordered maps with unique string `:id` values plus nonblank `:title` and `:image` fields needed by rendering and territory details.

Starting deck accounting is explicit: the first six cards per player become player hands, the next nine cards become board territories, the remaining cards become `:draw-pile`, and `:discard-pile` starts empty. Each player map also carries its initial Icehouse `:stash`, mirrored by the existing `[:pieces :stashes]` index. Tests assert that all 78 tarot cards appear exactly once across those zones.

The browser app initializes through `gnostica.app-state/initialize`, which stores the `create-game` result under app-db `:game`. Board, piece, selected-territory, and current-player subscriptions read through that shared game value instead of maintaining a second top-level board or piece setup path. The visible demo pieces are kept in the game state's `[:pieces :on-board]` slot so renderer smoke checks still exercise piece layout.

The main screen also displays the current player's hand, the draw deck, and the discard pile from `:game`. The hand zone shows the current player's six card images and titles, the draw deck shows a facedown deck affordance with remaining-card count, and the discard pile shows an empty state until a top discarded card exists. These zones are UI-only: drawing and discarding still require future pure gameplay transitions.

Rule helpers return explicit result maps: successful transitions use `{:ok? true :state ... :events [...]}`, while validation errors use `{:ok? false :error {:code ... :message ... :data ...}}`. `gnostica.game-state/apply-cup-move` is the first pure move transition. It accepts an explicit command with `:player-id`, a source map for either a Cup territory or Cup hand card plus acting `:piece-id`, a target map for either an existing territory or wasteland coordinate, and the required `:orientation` or `:one-point-card-id`. It places the current player's small piece, creates one-point-card territories in legal wastelands, preserves deck and schema invariants, and reports structured rejection errors.

## Gameplay Move Selection

The browser app includes a current-player move panel driven from shared app-db state in `gnostica.app-state`. Move selection is stored outside `:game` under `:move-selection`, while board, pieces, card zones, hand cards, draw counts, and player metadata continue to derive from the authoritative game state.

The current flow lets the player choose a move source, then stage the required source territory, hand card, minion, existing target territory, target wasteland, one-point territory card, draw count, or orientation. Three.js card picking and the CSS fallback board both dispatch through the same board-selection event for existing territories, while the move panel exposes Cup wasteland targets directly. Confirming a completed territory or hand-card Cup move now builds the `apply-cup-move` command for either small-piece placement on an existing territory or one-point-card territory creation in a wasteland, replaces app-db `:game` on success, and keeps the staged selection with the structured gameplay error on rejection. Move sources without a pure transition still return an explicit `:move-transition-unavailable` result instead of mutating game state.

## Keyboard Shortcuts

The app header exposes a `?` control, and pressing `?` opens a keyboard command dialog. The dialog lists the currently supported browser shortcuts: `?` opens the dialog, `I` toggles Gnostica card icon overlays between always-visible and hover/focus popup modes, and `Esc` closes the dialog.

The `I` shortcut should preserve the current 3D OrbitControls camera view; only the `Reset view` control should reset the board camera.

`gnostica.game-schema` provides Malli schemas for the pure gameplay data contract: card references, board cells, player state, six-card hand limits, Icehouse pieces, draw/discard piles, temporary wasteland piece coordinates, post-setup board growth, and top-level game state invariants including piece space references. Use `valid-game?`, `explain-game`, and `assert-valid-game` in tests, Gherkin-style feature steps, and other pure-data boundaries where a readable state-shape failure is more useful than renderer or re-frame behavior.

Gameplay rule examples live in Gherkin-style `.feature` files under `features/` and run through the custom `gnostica.feature-runner`. Step definitions and reusable test-world helpers live under `test/gnostica/feature_steps.clj` and `test/gnostica/feature_world.clj`; they create deterministic games, apply pure actions, inspect state paths, and include Malli explanations in failing step output. `clojure -M:test` runs these feature scenarios alongside the existing `clojure.test` namespaces.

### Verification

For the 3D board slice, run:

```sh
clojure -M:test
clojure -M:release
clojure -M:smoke
```

`clojure -M:smoke` starts the released Ring app unless `SMOKE_URL` points at an already-running dev or release server. It drives a local headless Chrome/Chromium through the DevTools protocol, so no npm workflow is added. Set `SMOKE_CHROME` if Chrome is not in a standard location.

The smoke checks desktop and mobile viewport widths, verifies the r128 Three.js and OrbitControls globals, confirms the canvas screenshot is nonblank with visible board content and dark velvet table pixels, verifies antialiasing is requested and enabled when the browser reports support, waits for nine card texture loads, checks the twelve initial wasteland outline targets and visible piece edge outline count, fails on happy-path texture/fallback status messages, verifies the reset control is present, confirms `I` preserves a changed 3D camera distance while toggling icon mode, clicks the center 3D card and checks that the territory panel updates, blocks the pinned CDN scripts to verify the missing-global CSS fallback path, and injects a mismatched Three.js revision to verify that version drift also falls back before user interaction. Pip marker count and placement are covered by the browser-free layout tests.

The smoke checks also assert that the current-player card zones are present in the Three.js and fallback layouts, including six visible hand cards, a non-empty draw deck count, and the initial empty discard pile state.

## Commands

```sh
clojure -M:dev       # start the ClojureScript watcher
clojure -M:release   # compile an optimized browser build
clojure -M:server    # serve the released app with Clojure/Ring
clojure -M:test      # run Clojure tests and gameplay feature scenarios
clojure -M:smoke     # run the headless Chrome 3D board smoke check
```

## Version Control

This project is initialized for Jujutsu:

```sh
jj status
jj diff
jj describe -m "Describe the current change"
jj commit -m "Commit message"
jj new
```
