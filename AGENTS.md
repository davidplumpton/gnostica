# Agent Notes

## Project Overview

Gnostica is a Clojure and ClojureScript tarot app using re-frame and shadow-cljs.

Use `docs/rules.txt` as the local implementation rules reference. The upstream Gnostica PDF remains the provenance source: https://www.looneylabs.com/sites/default/files/pyramid_rules/Gnostica.pdf. If the local text and upstream PDF differ, treat `docs/rules.txt` as authoritative for current repo behavior until the local copy is deliberately reconciled.

## Development Commands

```sh
clojure -M:dev       # start the ClojureScript watcher
clojure -M:release   # compile an optimized browser build
clojure -M:server    # serve existing released assets with Clojure/Ring
clojure -M:test      # run Clojure tests for shared code
clojure -M:smoke     # run the headless Chrome 3D board smoke check
```

The shadow-cljs dev server serves the app at `http://localhost:8080/index.html`. The default workflow does not use npm.

Run `clojure -M:release` before `clojure -M:server` or the default `clojure -M:smoke` path. Both serve the existing browser bundle from `src/main/resources/js/main.js`; neither command rebuilds stale or missing JavaScript assets. `SMOKE_URL` can point smoke at an already-running dev or release server instead.

## Shared Game State

`src/main/gnostica/game_state.cljc` defines the browser-free gameplay state contract. Use `gnostica.game-state/create-game` when tests, Gherkin-style feature steps, or re-frame events need an authoritative state value with deterministic deck setup and explicit success/error results. It enforces two to six known players backed by `gnostica.pieces/players` metadata, applies `:deck-order` or `:shuffle-fn` to the full source deck once, deals six-card starting hands before the territory board, then calls `gnostica.board/initial-board` with `identity` on the remaining cards so normal game setup does not reshuffle the board slice. It validates injected deck cards for unique ids plus rendering fields, and stores phase, players, turn order, pieces/stashes, draw/discard piles, setup/bid metadata, and history events. Use `gnostica.game-state/with-board-pieces` when seeding demo or test pieces so player stashes and `[:pieces :stashes]` stay reconciled with active pieces.

`gnostica.game-state/apply-cup-move` is the pure Cup gameplay move transition. Keep its command contract explicit: `:player-id`, a `:source` map for either `:territory` or `:hand-card` plus acting `:piece-id`, and a `:target` map for `:territory`, `:piece`, or `:wasteland`. Territory targets require `:orientation` and create the current player's small piece; enemy piece targets preserve the target piece orientation and create one of that enemy's small pieces; wasteland targets require `:one-point-card-id` and create one-point-card territories. It should continue to return structured success/error maps, update player and stash mirrors together, move hand-source cards to discard, create one-point-card wasteland territories without duplicating cards, append history events, and keep renderer/UI concerns out of `game_state.cljc`.

`gnostica.game-state/apply-rod-move` is the pure Rod transition. Keep it layered on `resolve-rod-command`: support `:move-minion`, `:push-piece`, and `:push-territory`; move hand-source Rod cards to discard; update piece locations as either `:space-index` territories or `:space {:kind :wasteland ...}`; enforce non-void piece destinations and the three-piece territory limit; allow orientation only for current-player moved pieces; preserve enemy orientation; reject territory pushes when the target territory or landing wasteland has enemy pieces; leave pieces at the source coordinate when a territory moves; place moved territories under own pieces already in the landing wasteland; update pushed territory orientation from the board pattern; and append history events.

`gnostica.game-state/apply-draw-move` is the pure standalone draw/discard turn transition. Keep its command contract explicit: `:player-id`, optional `:discard-card-ids`, `:draw-count`, and optional deterministic `:shuffle-fn`. It should continue to return structured success/error maps, discard selected hand cards before drawing, draw no more than the six-card hand limit and available cards, reshuffle the discard pile when the draw pile is exhausted, append history events, and preserve unique card accounting across hands, board, draw pile, and discard pile.

`gnostica.game-state/apply-orient-move` is the pure orient-piece transition. Keep its command contract explicit: `:player-id`, `:piece-id`, and `:orientation`. It should continue to require the current player's own piece, accept only legal orientations, preserve the piece's board or wasteland space, append history events, and keep renderer/UI concerns out of `game_state.cljc`.

`gnostica.game-state/apply-initial-placement` is the pure first-small-piece placement transition for a current player with no active pieces. Keep its command contract explicit: `:player-id`, `:target` for either an empty `:territory` or empty `:wasteland`, and `:orientation`. It should continue to reject occupied targets, void targets, players who already have pieces, and missing small stash pieces; on success it should update player and stash mirrors together, place the small piece on a territory or wasteland coordinate, and append history events.

`src/main/gnostica/app_state.cljc` is the browser-free app-db boundary for re-frame initialization. `gnostica.app/::initialize` delegates to `gnostica.app-state/initialize`, stores the successful game under `:game`, and keeps only UI-specific state such as `:selected-board-index` and Three.js errors at the top level. Board, piece, selected territory, and current-player subscriptions should continue to derive from `:game` instead of rebuilding board or piece state in `app.cljs`.

Current-player card-zone subscriptions also derive from `:game`: `gnostica.app-state/card-zones` exposes the current hand, draw pile, discard pile, counts, and top discard card for the browser UI. Keep hand/deck/discard rendering derived from `:game` and do not duplicate those zones at the app-db top level. The standalone draw/discard turn option calls `apply-draw-move`, while Cup and Rod hand-card moves move the source card from hand to discard through their pure transitions.

Move-selection UI state also belongs in `src/main/gnostica/app_state.cljc` under the top-level `:move-selection` key. Keep it as a staged, browser-free state machine over the authoritative `:game` data: source choice, source territory, hand card, implemented card power, minion/piece, Rod mode, target piece, target territory or wasteland, one-point card, distance, orientation, draw count, confirmation, cancellation, and explicit error/result states. Board clicks from Three.js and the CSS fallback should continue to dispatch through shared re-frame events that first update inspection state and then feed existing-territory selections in the active move flow; Cup enemy-piece targets, Cup and initial-placement wasteland targets, and Rod target-piece controls are exposed through the move panel. Draw confirmations should build `apply-draw-move` commands; Cup confirmations for territory and hand-card sources should build `apply-cup-move` commands for existing-territory small-piece placement, enemy-piece small-piece creation, and wasteland territory creation; Rod confirmations should build `apply-rod-move` commands for minion movement, piece pushing, and territory pushing; orient and first-small-placement confirmations should build `apply-orient-move` and `apply-initial-placement` commands. Confirmations update `:game` only on success and preserve the staged selection with structured gameplay errors on rejection. Move sources without pure transitions should continue to surface explicit unavailable results rather than mutating `:game` silently.

Keyboard shortcut UI state also stays browser-free in `src/main/gnostica/app_state.cljc`: `:hotkey-help-open?` tracks the command dialog and `:icon-help-open?` tracks the special move icon guide, while `src/main/gnostica/app.cljs` owns the actual browser keydown listener. Keep the `?` hotkey and header control opening the command dialog, `G` opening the detailed special move icon guide, `Esc` closing open help dialogs, `I` toggling card icon mode, and focused 3D-board WASD/arrow-key movement in sync with the visible command list and smoke checks. Pressing `I` must preserve the current 3D OrbitControls view; only the explicit `Reset view` control should reset the board camera. Board keyboard movement belongs in `gnostica.three-board` and should move the camera target with bounded board-plane panning rather than changing pure gameplay state.

`src/main/gnostica/game_schema.cljc` defines Malli schemas for the pure gameplay data contract. Keep schema coverage focused on browser-free state values: card references, board cells including post-setup territory growth, player maps and six-card hand limits, Icehouse pieces including temporary wasteland coordinates, draw/discard piles, and whole-game invariants such as player counts, turn membership, piece ownership, piece space references, stash ownership, player/piece stash mirror equality, active-piece plus stash totals, and unique card ids. Use `gnostica.game-schema/valid-game?`, `explain-game`, and `assert-valid-game` in tests, Gherkin-style feature steps, and other pure-data boundaries; do not couple these schemas to Three.js or re-frame view state.

## Gameplay Feature Tests

Gherkin-style gameplay scenarios live under `features/` and run through the custom `gnostica.feature-runner` as part of `clojure -M:test` with the normal Clojure unit tests. Keep step definitions in `test/gnostica/feature_steps.clj` and reusable world helpers in `test/gnostica/feature_world.clj`; those helpers are the shared place for deterministic deck order, player setup, pure action application, state lookup, and Malli validation checks. Current feature coverage includes deterministic setup plus Rod minion movement, piece pushing, territory pushing, distance and destination rejection, enemy-occupied territory-push rejection, own-wasteland landing behavior, and owned/enemy orientation rules. Failing steps should return `gnostica.feature-runner/fail` with enough state summary or schema explanation for the scenario and step text to identify the broken rule.

## Browser JavaScript Globals

`src/main/resources/index.html` loads Three.js and OrbitControls from pinned `three@0.128.0` CDN URLs before `/js/main.js`. The script tags carry SRI hashes and `crossorigin="anonymous"`. This keeps the default workflow npm-free while exposing the browser globals `THREE` and `THREE.OrbitControls` to `gnostica.three-board`, which only enables the 3D renderer when `THREE.REVISION` is `"128"` and OrbitControls is present.

`src/main/externs/three.ext.js` declares the current `THREE` global, OrbitControls, and the Three.js APIs used by the app for advanced compilation, including the small circle geometries used for pyramid pip markers and the edge/line APIs used for black pyramid outlines. Keep any future Three.js add-ons on the same `three@0.128.0` release line and use CDN scripts compatible with the global build. If the CDN URLs change, update the SRI hashes, the runtime revision gate in `src/main/gnostica/three_board.cljs`, and the smoke checks together.

## 3D Board Verification

The primary board renderer is `gnostica.three-board/scene` in `src/main/gnostica/three_board.cljs`. It mounts a Three.js renderer into Reagent lifecycle hooks, creates textured card planes from the shared board cells, applies alternating portrait/landscape rotation, leaves small gaps between territories, draws faint dashed wasteland outlines behind cards and pieces, renders pyramid pieces with black edge outlines and one/two/three base pip markers, wires `OrbitControls`, supports focused WASD/arrow-key camera-target panning, and raycasts pointer releases through callbacks supplied by `gnostica.app`.

Browser-free board renderer math lives in `src/main/gnostica/board_layout.cljc`. Keep card positioning, wasteland detection, board-space bounds, outline segments, board-index lookup, visible piece slot limits, pip marker local positions, compass rotations, and piece height math there when the behavior can be tested without WebGL.

For 3D board verification, run `clojure -M:test`, `clojure -M:release`, and `clojure -M:smoke`. The smoke command starts the released Ring app unless `SMOKE_URL` points at an existing dev or release server, so the default local smoke path requires a current `src/main/resources/js/main.js` produced by `clojure -M:release`. It uses a local headless Chrome/Chromium through the DevTools protocol without npm, and accepts `SMOKE_CHROME` or `CHROME_PATH` for a nonstandard browser path. It checks desktop and mobile viewports, r128 Three.js/OrbitControls globals, a nonblank canvas screenshot with visible board content, antialiasing when the browser reports support, nine card texture loads, twelve initial wasteland outline targets, visible piece edge outline count, absence of happy-path texture/fallback status messages, reset control presence, WASD and arrow-key board movement metadata, `?` command help, `G` special move icon help, `I` hotkey icon-mode toggling without resetting a changed camera distance, 3D center-card selection updating the territory panel, and both CSS fallback paths: blocked pinned CDN scripts and an injected mismatched Three.js revision.

The smoke check also verifies that the main card zones remain visible in the Three.js and fallback layouts: six current-player hand cards, a positive draw deck count, and an empty initial discard pile.

## Issue Tracking: br (beads_rust)

**Note:** `br` is non-invasive and never executes git commands. After `br sync --flush-only`, manually commit `.beads/` changes with Jujutsu.

This project uses `br` from beads_rust for issue tracking:

```sh
br ready
br list
br show <id>
br create
br update <id>
br close <id>
br sync --flush-only
```

After syncing beads data, include `.beads/` updates in the current Jujutsu change or commit them explicitly with `jj`.

## Version Control: Jujutsu

This project uses Jujutsu (`jj`) as its VCS. Prefer Jujutsu commands when inspecting or recording work:

```sh
jj status
jj diff
jj describe -m "Describe the current change"
jj commit -m "Commit message"
jj new
```

When starting work, set a Jujutsu change description with `jj describe -m "..."` before editing. At the end of the work, run `jj new` so the completed described change is left behind and the working copy moves to a fresh change.

Do not use git-only workflows unless the user asks for them or a tool requires them.
