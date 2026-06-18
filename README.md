# Gnostica

Gnostica is a local browser implementation of the tarot and Icehouse territory
game using Clojure, ClojureScript, re-frame, shadow-cljs, Three.js, and Ring.
The app currently focuses on local lobby setup, a full-window board, pure
browser-free game state, and staged move confirmation for implemented powers.

The local rules reference is [docs/rules.txt](docs/rules.txt). The upstream
PDF remains provenance:

https://www.looneylabs.com/sites/default/files/pyramid_rules/Gnostica.pdf

When those sources differ, treat `docs/rules.txt` as authoritative for current
repo behavior until the local copy is deliberately reconciled. The concise
architecture reference is [docs/architecture.md](docs/architecture.md).

The game was designed by John Cooper, Kory Heath, Kristin Matherly, and Jacob
Davenport. It uses a Tarot deck and Looney Pyramids.

https://www.looneylabs.com/pyramids-home

## Repository Map

- `src/main/gnostica/game_state.cljc` is the stable gameplay facade.
- `src/main/gnostica/app_state.cljc` is the stable app-db facade.
- `src/main/gnostica/move_selection.cljc` stages browser move commands.
- `src/main/gnostica/three_board.cljs` exposes the Three.js board component.
- `src/main/gnostica/ui/*` renders the lobby, board, panels, header, and help.
- `src/main/gnostica/fixtures.cljc` owns local demo and smoke fixture data.
- `features/` and `test/gnostica/` cover gameplay and browser-free behavior.

Detailed subsystem ownership lives in `docs/architecture.md`; this README is
kept as the quick entry point.

## Development

Install Java, the Clojure CLI, and Node.js. Node is required by shadow-cljs,
but the normal workflow does not use npm commands.

```sh
clojure -M:dev
```

The shadow-cljs dev server serves the app at:

http://localhost:8080/index.html

For the released Ring server path, build first:

```sh
clojure -M:release
clojure -M:server
```

`clojure -M:server` serves existing files under `src/main/resources`; it does
not rebuild stale or missing JavaScript assets.

## Commands

```sh
clojure -M:dev       # start the ClojureScript watcher
clojure -M:release   # compile an optimized browser build
clojure -M:server    # serve released browser assets
clojure -M:lint      # run the br guard, clj-kondo, and cljfmt verification
clojure -M:test      # run Clojure tests and gameplay feature scenarios
clojure -M:smoke     # run the headless Chrome board smoke check
```

Run `clojure -M:release` before `clojure -M:server` or the default
`clojure -M:smoke` path. `SMOKE_URL` can point smoke at an already-running dev
or release server.

## Browser Runtime

`src/main/resources/index.html` loads pinned `three@0.128.0` CDN scripts before
the compiled app bundle. The app enables the 3D renderer only when the expected
Three.js revision and `OrbitControls` global are present; otherwise it falls
back to the CSS board.

Keep the CDN URLs, SRI hashes, runtime revision gate, externs, release build,
and smoke checks aligned when changing Three.js.

## Verification

For normal source changes, run:

```sh
clojure -M:lint
clojure -M:test
```

For 3D board or compiled browser changes, also run:

```sh
clojure -M:release
clojure -M:smoke
```

The lint command first checks the br tracker guard. It fails if
`.beads/embeddeddolt` is missing, is a legacy directory, is not a regular file,
or if `.beads/issues.jsonl` is absent.

## Issue Tracking

This repo tracks work with `br` from beads_rust:

```sh
br ready
br list
br show <id>
br update <id>
br close <id>
br sync --flush-only
```

The repository is intentionally `br`-only. Do not use the legacy Go `bd` CLI
here; active tracker state lives under `.beads/`. The tracked regular file at
`.beads/embeddeddolt` blocks legacy `bd` discovery.

If `.beads/issues.jsonl` is removed by legacy tooling, recover it with:

```sh
br sync --flush-only --force
br doctor
br sync --status
```

After updating issues, run `br sync --flush-only` and include the resulting
`.beads/` changes in the current Jujutsu change.

## Version Control

This project uses Jujutsu:

```sh
jj status
jj diff
jj describe -m "Describe the current change"
jj commit -m "Commit message"
jj new
```

Use Jujutsu only for version control in this repository. Do not run git
commands here; local workflow and tracker handoff assume `jj`-managed history.
