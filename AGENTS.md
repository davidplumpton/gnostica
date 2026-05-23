# Agent Notes

## Project Overview

Gnostica is a Clojure and ClojureScript tarot app using re-frame and shadow-cljs.

## Development Commands

```sh
clojure -M:dev       # start the ClojureScript watcher
clojure -M:release   # compile an optimized browser build
clojure -M:server    # serve the released app with Clojure/Ring
clojure -M:test      # run Clojure tests for shared code
clojure -M:smoke     # run the headless Chrome 3D board smoke check
```

The shadow-cljs dev server serves the app at `http://localhost:8080/index.html`. The default workflow does not use npm.

## Shared Game State

`src/main/gnostica/game_state.cljc` defines the browser-free gameplay state contract. Use `gnostica.game-state/create-game` when tests, Cucumber steps, or re-frame events need an authoritative state value with deterministic deck setup and explicit success/error results. It enforces two to six known players backed by `gnostica.pieces/players` metadata, reuses `gnostica.board` for territory setup, validates injected deck cards for unique ids plus rendering fields, and stores phase, players, turn order, pieces/stashes, draw/discard piles, setup/bid metadata, and history events.

`src/main/gnostica/app_state.cljc` is the browser-free app-db boundary for re-frame initialization. `gnostica.app/::initialize` delegates to `gnostica.app-state/initialize`, stores the successful game under `:game`, and keeps only UI-specific state such as `:selected-board-index` and Three.js errors at the top level. Board, piece, selected territory, and current-player subscriptions should continue to derive from `:game` instead of rebuilding board or piece state in `app.cljs`.

## Browser JavaScript Globals

`src/main/resources/index.html` loads Three.js and OrbitControls from pinned `three@0.128.0` CDN URLs before `/js/main.js`. This keeps the default workflow npm-free while exposing the browser globals `THREE` and `THREE.OrbitControls` to `gnostica.three-board`.

`src/main/externs/three.ext.js` declares the current `THREE` global, OrbitControls, and the Three.js APIs used by the app for advanced compilation. Keep any future Three.js add-ons on the same `three@0.128.0` release line and use CDN scripts compatible with the global build.

## 3D Board Verification

The primary board renderer is `gnostica.three-board/scene` in `src/main/gnostica/three_board.cljs`. It mounts a Three.js renderer into Reagent lifecycle hooks, creates textured card planes from the shared board cells, applies alternating portrait/landscape rotation, leaves small gaps between territories, wires `OrbitControls`, and raycasts pointer releases through callbacks supplied by `gnostica.app`.

Browser-free board renderer math lives in `src/main/gnostica/board_layout.cljc`. Keep card positioning, board-index lookup, visible piece slot limits, compass rotations, and piece height math there when the behavior can be tested without WebGL.

For 3D board verification, run `clojure -M:test`, `clojure -M:release`, and `clojure -M:smoke`. The smoke command starts the released Ring app unless `SMOKE_URL` points at an existing dev or release server, uses a local headless Chrome/Chromium through the DevTools protocol without npm, and accepts `SMOKE_CHROME` for a nonstandard browser path. It checks desktop and mobile viewports, r128 Three.js/OrbitControls globals, a nonblank canvas screenshot with visible board content, nine card texture loads, absence of happy-path texture/fallback status messages, reset control presence, 3D center-card selection updating the territory panel, and the CSS fallback path when the pinned CDN scripts are blocked.

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
