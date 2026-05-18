# Agent Notes

## Project Overview

Gnostica is a Clojure and ClojureScript tarot app using re-frame and shadow-cljs.

## Development Commands

```sh
clojure -M:dev       # start the ClojureScript watcher
clojure -M:release   # compile an optimized browser build
clojure -M:server    # serve the released app with Clojure/Ring
clojure -M:test      # run Clojure tests for shared code
```

The shadow-cljs dev server serves the app at `http://localhost:8080/index.html`. The default workflow does not use npm.

## Browser JavaScript Globals

`src/main/resources/index.html` loads Three.js from the pinned CDN URL `https://cdn.jsdelivr.net/npm/three@0.128.0/build/three.min.js` before `/js/main.js`. This keeps the default workflow npm-free while exposing the browser global `THREE` to ClojureScript.

`src/main/externs/three.ext.js` declares the current `THREE` global for advanced compilation. If future work adds OrbitControls, keep it on the same `three@0.128.0` release line and use a CDN script compatible with the global build.

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
```

Do not use git-only workflows unless the user asks for them or a tool requires them.
