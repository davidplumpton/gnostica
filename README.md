# Gnostica

A Clojure and ClojureScript tarot app using re-frame and shadow-cljs.

## Development

Install Java, the Clojure CLI, and Node.js, then run:

```sh
clojure -M:dev
```

The shadow-cljs dev server serves the app at `http://localhost:8080/index.html`. Node.js is still required by shadow-cljs for browser builds, but the default workflow does not use npm.

The browser runtime loads Three.js and OrbitControls from pinned `three@0.128.0` CDN scripts before the compiled ClojureScript bundle. This keeps browser builds npm-free while still exposing the global `THREE` and `THREE.OrbitControls` values used by the 3D board renderer.

## 3D Board View

The first screen renders the nine-card territory board with Three.js when the CDN globals are available. The renderer lives in `gnostica.app/three-board-scene`, uses the shared board model, loads tarot card images as textures, alternates portrait and landscape card planes, leaves small gaps between neighboring cards, and uses `OrbitControls` for mouse, trackpad, and touch camera movement. Card picking is handled with a Three.js raycaster and updates the selected territory panel.

If either CDN global is missing, the app falls back to the CSS board so the game remains usable while surfacing a runtime warning. Advanced compilation support for the CDN global is declared in `src/main/externs/three.ext.js`; keep future Three.js add-ons on the same `three@0.128.0` release line unless the CDN scripts and externs are updated together.

### Verification

For the 3D board slice, run:

```sh
clojure -M:test
clojure -M:release
```

Browser smoke verification should cover desktop and mobile widths when browser automation is available: the canvas is nonblank, all nine cards are visible, adjacent cards alternate orientation with small gaps, the camera can orbit or zoom and reset, and clicking a 3D card changes the selected territory panel.

## Commands

```sh
clojure -M:dev       # start the ClojureScript watcher
clojure -M:release   # compile an optimized browser build
clojure -M:server    # serve the released app with Clojure/Ring
clojure -M:test      # run Clojure tests for shared code
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
