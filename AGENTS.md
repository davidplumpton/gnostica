# Agent Notes

## Project Overview

Gnostica is a Clojure and ClojureScript tarot app using re-frame,
shadow-cljs, Three.js, and Ring.

Use `docs/rules.txt` as the local implementation rules reference. The upstream
Gnostica PDF remains provenance:

https://www.looneylabs.com/sites/default/files/pyramid_rules/Gnostica.pdf

If the local text and upstream PDF differ, treat `docs/rules.txt` as
authoritative for current repo behavior until the local copy is deliberately
reconciled.

Use `docs/architecture.md` as the canonical prose map for subsystem ownership.
Keep README high-level, AGENTS focused on workflow and invariants, and
MIND_MAP compact. Do not reintroduce long duplicated power-contract prose into
these three entry-point docs.

**MANDATORY WORKFLOW:**

Important: use jj for version control. Never use git commands. Always commit with an appropriate message, don't call `jj new` with a message.

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **Run br sync --flush-only** - Export issue updates to `.beads/issues.jsonl`; `br` never runs VCS commands
5. **Only work on one task at a time before committing to VCS**
6. **Clean up**
7. **Verify** - Create an appropriate jj description and then run `jj commit -m <description>`
8. **Hand off** - Provide context for next session
## Mind Map Workflow

Use `MIND_MAP.md` as the compact project knowledge index before substantial
work. Start at the Gnostica project anchors [6-10], then follow inline node
references for the subsystem you are touching.

Nodes [1-5] are preserved generic format notes. Consult them only for syntax
and do not change them.

When you encounter bugs, record useful attempts or findings in the relevant
node. When code or docs changes make an existing node stale, update that node
in the same change. Add new nodes only for genuinely new concepts, keep the
map compact, and reference node IDs when using map context in notes or
handoffs.

## Development Commands

```sh
clojure -M:dev       # start the ClojureScript watcher
clojure -M:release   # compile an optimized browser build
clojure -M:server    # serve existing released assets with Clojure/Ring
clojure -M:lint      # run the br guard, clj-kondo, and cljfmt verification
clojure -M:test      # run Clojure tests and gameplay feature scenarios
clojure -M:smoke     # run the headless Chrome 3D board smoke check
```

The shadow-cljs dev server serves the app at:

http://localhost:8080/index.html

Run `clojure -M:release` before `clojure -M:server` or the default
`clojure -M:smoke` path. Both serve the existing browser bundle from
`src/main/resources/js/main.js`; neither command rebuilds stale JavaScript
assets.

`clojure -M:lint` first runs the br tracker guard, then clj-kondo and cljfmt.
The guard fails when `.beads/embeddeddolt` is missing, is a legacy directory,
is not a regular file, or when `.beads/issues.jsonl` is absent.

## Search and Reading

For search-oriented exploration, first try the code-spelunker `cs` command:

```sh
cs "move-legal-targets" path:src/main/gnostica
cs "apply-world-move" OR "world-major-territories"
```

`cs` is a search tool, not a file reader. When exact surrounding context,
line numbers, or complete sections matter, use line-numbered reads such as:

```sh
nl -ba AGENTS.md | sed -n '1,80p'
```

## Local Architecture Invariants

The exact architecture map lives in `docs/architecture.md`. Keep these boundary
rules in mind while editing:

- `gnostica.game-state` is the browser-free gameplay facade.
- `gnostica.app-state` is the browser-free app-db facade for re-frame callers.
- Pure gameplay transitions return structured `{:ok? ...}` result maps.
- Browser events inject deterministic shuffle functions at handler boundaries.
- Re-frame views consume composed feature-level subscriptions.
- Move selection stays outside `:game` under `:move-selection`.
- Gesture-started pending state stays under `:gesture-intent`.
- Direct-manipulation behavior follows `docs/direct_move_entry_spec.md`.
- Fixture and smoke setup data stays in `gnostica.fixtures`.
- UI board indexes are stable cell `:index` values, not vector positions.
- Renderer/UI concerns stay out of pure gameplay transition namespaces.

For exact command shapes, use
`src/main/gnostica/game_state/command_contracts.cljc`. For semantic behavior,
read the owning transition namespace and tests listed in `docs/architecture.md`.

## Verification Guidance

For ordinary Clojure or ClojureScript changes, run:

```sh
clojure -M:lint
clojure -M:test
```

For 3D board, compiled browser, CDN, extern, or smoke-fixture changes, also
run:

```sh
clojure -M:release
clojure -M:smoke
```

`clojure -M:smoke` starts the released Ring app unless `SMOKE_URL` points at an
already-running server. Set `SMOKE_CHROME` or `CHROME_PATH` if Chrome is not in
a standard location.

## Issue Tracking: br

This repository is intentionally `br`-only. Do not use the legacy Go `bd` CLI
here; active issue state lives in the br SQLite and JSONL files under
`.beads/`.

Useful commands:

```sh
br ready
br list
br show <id>
br update <id>
br close <id>
br sync --flush-only
```

The tracked regular file `.beads/embeddeddolt` blocks legacy `bd` from
auto-discovering an embedded-Dolt workspace and overwriting
`.beads/issues.jsonl`.

If `.beads/issues.jsonl` is accidentally removed, recover it with:

```sh
br sync --flush-only --force
br doctor
br sync --status
```

After changing issues, run `br sync --flush-only` and include the resulting
`.beads/` changes in the current Jujutsu change. `br` does not run git or
Jujutsu commands for you.

## Version Control: Jujutsu

Prefer Jujutsu commands when inspecting or recording work:

```sh
jj status
jj diff
jj describe -m "Describe the current change"
jj commit -m "Commit message"
jj new
```

For inspect-only work, use `jj status`/`jj diff` as needed and do not describe,
commit, or split a change. For tracker-only work, update `br`, run
`br sync --flush-only`, and keep any tracked `.beads/` file changes in the
current Jujutsu change without describing, committing, or splitting it unless
the user asks.

For normal code or docs edits, set a clear `jj describe` message before editing
when the working copy is clean or already yours. If the working copy already has
user-owned changes, do not overwrite the description or split changes unless the
user asks; keep your edits scoped and report any shared-file overlap.

Use `jj commit` only when the user asks you to commit or when the repository
workflow explicitly requires a recorded change. Use `jj new` only after a
commit, or when the user asks you to start a separate change; do not create a
new change just to finish a small inspection or tracker update.

If the user has unrelated local changes, leave them untouched. Do not run
destructive reset or checkout commands unless the user explicitly asks.
