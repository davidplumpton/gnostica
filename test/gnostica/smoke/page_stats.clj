(ns gnostica.smoke.page-stats
  (:require [clojure.string :as str]
            [gnostica.fixtures :as fixtures]
            [gnostica.icon-layout :as icon-layout]
            [gnostica.icons :as icons]
            [gnostica.keyboard-shortcuts :as shortcuts]
            [gnostica.smoke.browser :as browser])
  (:import [java.io ByteArrayInputStream]
           [java.util Base64]
           [javax.imageio ImageIO]))

(def expected-table-surface-color "#1c0715")
(def expected-table-clear-color "#0a0308")
(def min-velvet-pixels 120)
(def min-dark-table-pixels 120)

(def viewports
  [{:name "desktop" :width 1280 :height 900 :mobile false}
   {:name "mobile" :width 390 :height 844 :mobile true}])

(def app-ready-js
  "Boolean(document.querySelector('.app-shell') || document.querySelector('.setup-error'))")

(def happy-stats-js
  "(() => {
     const header = document.querySelector('.app-header');
     const headerRect = header ? header.getBoundingClientRect() : null;
     const shell = document.querySelector('.app-shell');
     const shellRect = shell ? shell.getBoundingClientRect() : null;
     const board = document.querySelector('.board-three');
     const canvas = document.querySelector('.board-three__canvas');
     const boardRect = board ? board.getBoundingClientRect() : null;
     const rect = canvas ? canvas.getBoundingClientRect() : null;
     const antialiasSupported = (() => {
       const probe = document.createElement('canvas');
       const context = probe.getContext('webgl2', {antialias: true})
         || probe.getContext('webgl', {antialias: true})
         || probe.getContext('experimental-webgl', {antialias: true});
       const attributes = context && context.getContextAttributes ? context.getContextAttributes() : null;
       return Boolean(attributes && attributes.antialias);
     })();
     const cardZones = document.querySelector('.card-zones');
     const cardZonesRect = cardZones ? cardZones.getBoundingClientRect() : null;
     const status = Array.from(document.querySelectorAll('.board-3d-status.is-error')).map((node) => node.textContent.trim());
     const imageResourceCount = performance.getEntriesByType('resource')
       .filter((entry) => /\\/images\\/.*\\.png(?:$|\\?)/.test(entry.name)).length;
     const iconMetrics = (selector) => {
       const icon = document.querySelector(selector);
       const stack = icon ? icon.closest('.gnostica-icon-stack') : null;
       const face = icon ? icon.closest('.card-face') : null;
       const iconRect = icon ? icon.getBoundingClientRect() : null;
       const faceRect = face ? face.getBoundingClientRect() : null;
       const stackStyle = stack ? getComputedStyle(stack) : null;
       const gap = stackStyle ? Number.parseFloat(stackStyle.rowGap || stackStyle.gap || '') : -1;
       return {
         scale: stack ? Number(stack.dataset.iconScale || -1) : -1,
         iconWidth: iconRect ? iconRect.width : 0,
         faceWidth: faceRect ? faceRect.width : 0,
         widthRatio: iconRect && faceRect && faceRect.width > 0 ? iconRect.width / faceRect.width : 0,
         gap: Number.isFinite(gap) ? gap : -1
       };
     };
     return {
       url: location.href,
       viewportWidth: window.innerWidth,
       viewportHeight: window.innerHeight,
       viewportAvailableHeight: Math.round(window.innerHeight - (headerRect ? headerRect.height : 0)),
       shellClientWidth: shellRect ? Math.round(shellRect.width) : 0,
       shellClientHeight: shellRect ? Math.round(shellRect.height) : 0,
       cardIconMode: (document.querySelector('.app-shell') || {}).dataset.cardIconMode || null,
       threeRevision: window.THREE ? window.THREE.REVISION : null,
       orbitControls: Boolean(window.THREE && window.THREE.OrbitControls),
       board: Boolean(board),
       boardCardIconMode: board ? board.dataset.cardIconMode : null,
       boardCardCount: board ? Number(board.dataset.boardCardCount || -1) : -1,
       majorIconCardCount: board ? Number(board.dataset.majorIconCardCount || -1) : -1,
       majorIconCount: board ? Number(board.dataset.majorIconCount || -1) : -1,
       cardIconScale: board ? Number(board.dataset.cardIconScale || -1) : -1,
       cardIconSize: board ? Number(board.dataset.cardIconSize || -1) : -1,
       cardTextureSupportedIconCount: board ? Number(board.dataset.cardTextureSupportedIconCount || -1) : -1,
       cardTextureMaxIconCount: board ? Number(board.dataset.cardTextureMaxIconCount || -1) : -1,
       cardTextureIconStackFits: board ? board.dataset.cardTextureIconStackFits === 'true' : false,
       wastelandCount: board ? Number(board.dataset.wastelandCount || -1) : -1,
       visiblePieceCount: board ? Number(board.dataset.visiblePieceCount || -1) : -1,
       pieceEdgeOutlineCount: board ? Number(board.dataset.pieceEdgeOutlineCount || -1) : -1,
       selectedIndex: board ? Number(board.dataset.selectedBoardIndex || -1) : -1,
       tableSurfaceColor: board ? board.dataset.tableSurfaceColor : null,
       tableClearColor: board ? board.dataset.tableClearColor : null,
       textureErrorCount: board ? Number(board.dataset.textureErrorCount || -1) : -1,
       fallback: Boolean(document.querySelector('.board-fallback')),
       canvas: Boolean(canvas),
       canvasWidth: canvas ? canvas.width : 0,
       canvasHeight: canvas ? canvas.height : 0,
       boardClientWidth: boardRect ? Math.round(boardRect.width) : 0,
       boardClientHeight: boardRect ? Math.round(boardRect.height) : 0,
       canvasClientWidth: rect ? Math.round(rect.width) : 0,
       canvasClientHeight: rect ? Math.round(rect.height) : 0,
       antialiasRequested: board ? board.dataset.antialiasRequested === 'true' : false,
       antialiasEnabled: board ? board.dataset.antialiasEnabled === 'true' : false,
       antialiasSupported,
       minZoomDistance: board ? Number(board.dataset.minZoomDistance || -1) : -1,
       maxZoomDistance: board ? Number(board.dataset.maxZoomDistance || -1) : -1,
       cameraDistance: board ? Number(board.dataset.cameraDistance || -1) : -1,
       cameraTargetX: board ? Number(board.dataset.cameraTargetX || -999) : -999,
       cameraTargetY: board ? Number(board.dataset.cameraTargetY || -999) : -999,
       reset: Boolean(document.querySelector('.board-three__reset')),
       cardZones: Boolean(cardZones),
       cardZonesVisible: Boolean(cardZonesRect && cardZonesRect.width > 0 && cardZonesRect.height > 0),
       handCardCount: document.querySelectorAll('.hand-card').length,
       handMajorIconStackCount: document.querySelectorAll('.hand-card .gnostica-icon-stack').length,
       handMajorIconCount: document.querySelectorAll('.hand-card .gnostica-icon').length,
       handIconMetrics: iconMetrics('.hand-card .gnostica-icon'),
       drawCount: cardZones ? Number(cardZones.dataset.drawCount || -1) : -1,
       discardCount: cardZones ? Number(cardZones.dataset.discardCount || -1) : -1,
       status,
       imageResourceCount
     };
   })()")

(def canvas-rect-js
  "(() => {
     const canvas = document.querySelector('.board-three__canvas');
     if (!canvas) return null;
     const rect = canvas.getBoundingClientRect();
     return {
       x: rect.left,
       y: rect.top,
       width: rect.width,
       height: rect.height,
       centerX: rect.left + (rect.width / 2),
       centerY: rect.top + (rect.height / 2)
     };
   })()")

(def selection-js
  "(() => {
     const board = document.querySelector('.board-three');
     const panel = document.querySelector('.territory-panel');
     return {
       selectedIndex: board ? Number(board.dataset.selectedBoardIndex || -1) : -1,
       panelText: panel ? panel.innerText : ''
     };
   })()")

(def fallback-stats-js
  "(() => {
     const status = document.querySelector('.board-3d-status');
     const stage = document.querySelector('.board-fallback .board-stage');
     const fallbackFace = document.querySelector('.board-fallback .card-face');
     const cardZones = document.querySelector('.card-zones');
     const cardZonesRect = cardZones ? cardZones.getBoundingClientRect() : null;
     const iconMetrics = (selector) => {
       const icon = document.querySelector(selector);
       const stack = icon ? icon.closest('.gnostica-icon-stack') : null;
       const face = icon ? icon.closest('.card-face') : null;
       const iconRect = icon ? icon.getBoundingClientRect() : null;
       const faceRect = face ? face.getBoundingClientRect() : null;
       const stackStyle = stack ? getComputedStyle(stack) : null;
       const gap = stackStyle ? Number.parseFloat(stackStyle.rowGap || stackStyle.gap || '') : -1;
       return {
         scale: stack ? Number(stack.dataset.iconScale || -1) : -1,
         iconWidth: iconRect ? iconRect.width : 0,
         faceWidth: faceRect ? faceRect.width : 0,
         widthRatio: iconRect && faceRect && faceRect.width > 0 ? iconRect.width / faceRect.width : 0,
         gap: Number.isFinite(gap) ? gap : -1
       };
     };
     return {
       cardIconMode: (document.querySelector('.app-shell') || {}).dataset.cardIconMode || null,
       threeRevision: window.THREE ? window.THREE.REVISION : null,
       orbitControls: Boolean(window.THREE && window.THREE.OrbitControls),
       fallback: Boolean(document.querySelector('.board-fallback')),
       boardCardIconMode: fallbackFace ? fallbackFace.dataset.iconMode : null,
       cssCards: document.querySelectorAll('.board-fallback .board-card').length,
       cssMajorIconStackCount: document.querySelectorAll('.board-fallback .board-card .gnostica-icon-stack').length,
       cssMajorIconCount: document.querySelectorAll('.board-fallback .board-card .gnostica-icon').length,
       cssBoardIconMetrics: iconMetrics('.board-fallback .board-card.is-portrait .gnostica-icon'),
       cssWastelands: document.querySelectorAll('.board-fallback .board-wasteland').length,
       cssWastelandPieceMarkers: document.querySelectorAll('.board-fallback .board-wasteland.has-pieces .board-piece').length,
       canvas: Boolean(document.querySelector('.board-three__canvas')),
       tableSurfaceColor: stage ? stage.dataset.tableSurfaceColor : null,
       tableClearColor: stage ? stage.dataset.tableClearColor : null,
       cardZones: Boolean(cardZones),
       cardZonesVisible: Boolean(cardZonesRect && cardZonesRect.width > 0 && cardZonesRect.height > 0),
       handCardCount: document.querySelectorAll('.hand-card').length,
       handMajorIconStackCount: document.querySelectorAll('.hand-card .gnostica-icon-stack').length,
       handMajorIconCount: document.querySelectorAll('.hand-card .gnostica-icon').length,
       handIconMetrics: iconMetrics('.hand-card .gnostica-icon'),
       drawCount: cardZones ? Number(cardZones.dataset.drawCount || -1) : -1,
       discardCount: cardZones ? Number(cardZones.dataset.discardCount || -1) : -1,
       statusText: status ? status.textContent.trim() : '',
       panelText: (document.querySelector('.territory-panel') || {}).innerText || ''
     };
   })()")

(def popup-mode-js
  "(() => {
     const visible = (node) => {
       if (!node) return false;
       const rect = node.getBoundingClientRect();
       const style = getComputedStyle(node);
       return rect.width > 0
         && rect.height > 0
         && style.visibility !== 'hidden'
         && Number(style.opacity || 0) > 0.8;
     };
     const text = (node) => node ? node.textContent.trim() : '';
     const focusStats = (target, popoverSelector) => {
       if (target && target.focus) target.focus();
       const popover = document.querySelector(popoverSelector);
       return {
         visible: visible(popover),
         iconCount: popover ? popover.querySelectorAll('.gnostica-icon').length : 0,
         itemCount: popover ? popover.querySelectorAll('.card-icon-popover__item').length : 0,
         text: text(popover)
       };
     };
     const shell = document.querySelector('.app-shell');
     const board = document.querySelector('.board-three');
     const fallback = document.querySelector('.board-fallback');
     const fallbackFace = document.querySelector('.board-fallback .card-face');
     const handFace = document.querySelector('.hand-card.has-gnostica-icons .card-face');
     const textureStatus = Array.from(document.querySelectorAll('.board-3d-status.is-error'))
       .map((node) => node.textContent.trim())
       .filter((message) => message.includes('Texture load failed'));
     const fallbackTwoIconCard = document.querySelector('.board-fallback .board-card .card-icon-popover[data-icon-count=\"2\"]');
     const fallbackThreeIconCard = document.querySelector('.board-fallback .board-card .card-icon-popover[data-icon-count=\"3\"]');
     const hand = focusStats(handFace, '.hand-card.has-gnostica-icons .card-icon-popover');
     const boardTwo = focusStats(
       board || (fallbackTwoIconCard ? fallbackTwoIconCard.closest('.board-card') : null),
       board ? '.board-three-icon-popover .card-icon-popover' : '.board-fallback .board-card .card-icon-popover[data-icon-count=\"2\"]'
     );
     const boardThree = focusStats(
       fallbackThreeIconCard ? fallbackThreeIconCard.closest('.board-card') : null,
       '.board-fallback .board-card .card-icon-popover[data-icon-count=\"3\"]'
     );
     return {
       appMode: shell ? shell.dataset.cardIconMode : null,
       boardMode: board ? board.dataset.cardIconMode : (fallbackFace ? fallbackFace.dataset.iconMode : null),
       cameraDistance: board ? Number(board.dataset.cameraDistance || -1) : -1,
       textureErrorCount: board ? Number(board.dataset.textureErrorCount || -1) : 0,
       textureStatus,
       fallback: Boolean(fallback),
       togglePressed: (document.querySelector('.card-icon-mode-toggle') || {}).getAttribute('aria-pressed'),
       handStackCount: document.querySelectorAll('.hand-card .gnostica-icon-stack').length,
       boardStackCount: document.querySelectorAll('.board-fallback .board-card .gnostica-icon-stack').length,
       hand,
       boardTwo,
       boardThree
     };
   })()")

(def hotkey-help-js
  "(() => {
     const visible = (node) => {
       if (!node) return false;
       const rect = node.getBoundingClientRect();
       const style = getComputedStyle(node);
       return rect.width > 0
         && rect.height > 0
         && style.visibility !== 'hidden'
         && style.display !== 'none'
         && Number(style.opacity || 1) > 0.8;
     };
     const dialog = document.querySelector('.hotkey-help-dialog');
     const overlay = document.querySelector('.hotkey-help-overlay');
     const keyLabels = Array.from(document.querySelectorAll('.hotkey-command kbd'))
       .map((node) => node.textContent.trim());
     return {
       overlayVisible: visible(overlay),
       dialogVisible: visible(dialog),
       role: dialog ? dialog.getAttribute('role') : null,
       ariaModal: dialog ? dialog.getAttribute('aria-modal') : null,
       title: (document.querySelector('#hotkey-help-title') || {}).textContent || '',
       commandCount: document.querySelectorAll('.hotkey-command').length,
       keyLabels,
       text: dialog ? dialog.textContent : ''
     };
   })()")

(def icon-help-js
  "(() => {
     const visible = (node) => {
       if (!node) return false;
       const rect = node.getBoundingClientRect();
       const style = getComputedStyle(node);
       return rect.width > 0
         && rect.height > 0
         && style.visibility !== 'hidden'
         && style.display !== 'none'
         && Number(style.opacity || 1) > 0.8;
     };
     const dialog = document.querySelector('.icon-help-dialog');
     const overlay = document.querySelector('.icon-help-overlay');
     return {
       overlayVisible: visible(overlay),
       dialogVisible: visible(dialog),
       role: dialog ? dialog.getAttribute('role') : null,
       ariaModal: dialog ? dialog.getAttribute('aria-modal') : null,
       title: (document.querySelector('#icon-help-title') || {}).textContent || '',
       itemCount: document.querySelectorAll('.icon-help-item').length,
       iconCount: document.querySelectorAll('.icon-help-item .gnostica-icon').length,
       text: dialog ? dialog.textContent : ''
     };
   })()")

(def mismatched-three-js
  "window.THREE = {REVISION: '999', OrbitControls: function OrbitControls() {}};")

(defn major-icons-smoke-url [url]
  (str url
       (if (str/includes? url "?") "&" "?")
       fixtures/smoke-query-param
       "="
       fixtures/smoke-major-icons-mode))

(defn velvet-pixel? [argb]
  (let [r (bit-and (bit-shift-right argb 16) 0xff)
        g (bit-and (bit-shift-right argb 8) 0xff)
        b (bit-and argb 0xff)]
    (and (>= r 24)
         (>= b 18)
         (<= g 80)
         (<= b 130)
         (> r g)
         (> b g)
         (>= (- r g) 12)
         (>= (- b g) 4))))

(defn dark-table-pixel? [argb]
  (let [r (bit-and (bit-shift-right argb 16) 0xff)
        g (bit-and (bit-shift-right argb 8) 0xff)
        b (bit-and argb 0xff)]
    (and (<= r 18)
         (<= g 14)
         (<= b 20))))

(defn screenshot-pixel-stats! [client {:strs [x y width height]}]
  (let [result (browser/cdp-command! client
                                     "Page.captureScreenshot"
                                     {"format" "png"
                                      "clip" {"x" x
                                              "y" y
                                              "width" width
                                              "height" height
                                              "scale" 1}})
        bytes (.decode (Base64/getDecoder) ^String (get result "data"))
        image (ImageIO/read (ByteArrayInputStream. bytes))]
    (if-not image
      {"ok" false
       "error" "Chrome returned screenshot bytes that ImageIO could not decode."}
      (let [image-width (.getWidth image)
            image-height (.getHeight image)
            step-x (max 1 (quot image-width 80))
            step-y (max 1 (quot image-height 80))
            colors (volatile! (transient #{}))
            velvet-pixels (volatile! 0)
            dark-table-pixels (volatile! 0)
            sampled (volatile! 0)]
        (doseq [sample-x (range 0 image-width step-x)
                sample-y (range 0 image-height step-y)]
          (vswap! sampled inc)
          (let [argb (.getRGB image sample-x sample-y)]
            (when (velvet-pixel? argb)
              (vswap! velvet-pixels inc))
            (when (dark-table-pixel? argb)
              (vswap! dark-table-pixels inc))
            (vswap! colors conj! argb)))
        {"ok" true
         "width" image-width
         "height" image-height
         "sampledPixels" @sampled
         "velvetPixels" @velvet-pixels
         "darkTablePixels" @dark-table-pixels
         "distinctColors" (count (persistent! @colors))}))))

(defn pixel-ok? [stats]
  (and (true? (get stats "ok"))
       (>= (long (or (get stats "sampledPixels") 0)) 100)
       (>= (long (or (get stats "distinctColors") 0)) 16)
       (>= (long (or (get stats "velvetPixels") 0)) min-velvet-pixels)
       (>= (long (or (get stats "darkTablePixels") 0)) min-dark-table-pixels)))

(defn antialias-ready? [stats]
  (and (true? (get stats "antialiasRequested"))
       (or (false? (get stats "antialiasSupported"))
           (true? (get stats "antialiasEnabled")))))

(defn roughly= [expected actual tolerance]
  (<= (Math/abs (- (double expected) (double actual))) tolerance))

(defn dom-icon-layout-ready? [metrics]
  (let [width-ratio (double (or (get metrics "widthRatio") 0))
        gap (double (or (get metrics "gap") -1))
        icon-width (double (or (get metrics "iconWidth") 0))]
    (and (map? metrics)
         (= icon-layout/card-icon-scale
            (long (or (get metrics "scale") -1)))
         (roughly= (/ icon-layout/dom-card-icon-width-percent 100.0)
                   width-ratio
                   0.03)
         (roughly= icon-layout/dom-card-icon-gap-px gap 1.0)
         (pos? icon-width))))

(defn three-icon-layout-ready? [stats]
  (and (= icon-layout/card-icon-scale
          (long (or (get stats "cardIconScale") -1)))
       (= icon-layout/texture-card-icon-size
          (long (or (get stats "cardIconSize") -1)))
       (= (count icons/icon-ids)
          (long (or (get stats "cardTextureSupportedIconCount") -1)))
       (= icon-layout/max-card-icon-count
          (long (or (get stats "cardTextureMaxIconCount") -1)))
       (true? (get stats "cardTextureIconStackFits"))))

(defn happy-ready? [stats]
  (let [visible-piece-count (long (or (get stats "visiblePieceCount") -1))
        piece-edge-outline-count (long (or (get stats "pieceEdgeOutlineCount") -1))
        viewport-width (long (or (get stats "viewportWidth") 0))
        viewport-height (long (or (get stats "viewportAvailableHeight") 0))
        shell-width (long (or (get stats "shellClientWidth") 0))
        shell-height (long (or (get stats "shellClientHeight") 0))
        board-width (long (or (get stats "boardClientWidth") 0))
        board-height (long (or (get stats "boardClientHeight") 0))
        canvas-width (long (or (get stats "canvasClientWidth") 0))
        canvas-height (long (or (get stats "canvasClientHeight") 0))]
    (and (= "128" (get stats "threeRevision"))
         (= "always" (get stats "cardIconMode"))
         (true? (get stats "orbitControls"))
         (true? (get stats "board"))
         (<= (- viewport-width 2) shell-width)
         (<= (- viewport-height 2) shell-height)
         (<= (- shell-width 2) board-width)
         (<= (- shell-height 2) board-height)
         (<= (- board-width 2) canvas-width)
         (<= (- board-height 2) canvas-height)
         (= "always" (get stats "boardCardIconMode"))
         (= 2 (long (or (get stats "majorIconCardCount") -1)))
         (= 5 (long (or (get stats "majorIconCount") -1)))
         (three-icon-layout-ready? stats)
         (= 12 (get stats "wastelandCount"))
         (= (count fixtures/smoke-board-pieces) visible-piece-count)
         (= visible-piece-count piece-edge-outline-count)
         (= expected-table-surface-color (get stats "tableSurfaceColor"))
         (= expected-table-clear-color (get stats "tableClearColor"))
         (false? (get stats "fallback"))
         (true? (get stats "canvas"))
         (pos? canvas-width)
         (pos? canvas-height)
         (antialias-ready? stats)
         (roughly= 3.2 (double (or (get stats "minZoomDistance") -1)) 0.001)
         (roughly= 10.0 (double (or (get stats "maxZoomDistance") -1)) 0.001)
         (<= 3.2 (double (or (get stats "cameraDistance") -1)) 10.0)
         (number? (get stats "cameraTargetX"))
         (number? (get stats "cameraTargetY"))
         (true? (get stats "reset"))
         (true? (get stats "cardZones"))
         (true? (get stats "cardZonesVisible"))
         (= 6 (long (or (get stats "handCardCount") -1)))
         (= 1 (long (or (get stats "handMajorIconStackCount") -1)))
         (= 1 (long (or (get stats "handMajorIconCount") -1)))
         (dom-icon-layout-ready? (get stats "handIconMetrics"))
         (pos? (long (or (get stats "drawCount") 0)))
         (zero? (long (or (get stats "discardCount") -1)))
         (empty? (get stats "status"))
         (>= (long (or (get stats "imageResourceCount") 0)) 9))))

(defn camera-distance-changed? [initial-stats stats]
  (let [initial-distance (double (or (get initial-stats "cameraDistance") -1))
        current-distance (double (or (get stats "cameraDistance") -1))]
    (and (pos? initial-distance)
         (pos? current-distance)
         (not (roughly= initial-distance current-distance 0.05)))))

(defn camera-distance-preserved? [expected-stats stats]
  (let [expected-distance (double (or (get expected-stats "cameraDistance") -1))
        current-distance (double (or (get stats "cameraDistance") -1))]
    (and (pos? expected-distance)
         (pos? current-distance)
         (roughly= expected-distance current-distance 0.02))))

(defn camera-target-x-changed? [initial-stats stats]
  (let [initial-x (double (or (get initial-stats "cameraTargetX") -999))
        current-x (double (or (get stats "cameraTargetX") -999))]
    (not (roughly= initial-x current-x 0.05))))

(defn camera-target-y-changed? [initial-stats stats]
  (let [initial-y (double (or (get initial-stats "cameraTargetY") -999))
        current-y (double (or (get stats "cameraTargetY") -999))]
    (not (roughly= initial-y current-y 0.05))))

(defn fallback-ready? [stats]
  (and (nil? (get stats "threeRevision"))
       (= "always" (get stats "cardIconMode"))
       (false? (get stats "orbitControls"))
       (true? (get stats "fallback"))
       (= "always" (get stats "boardCardIconMode"))
       (false? (get stats "canvas"))
       (= expected-table-surface-color (get stats "tableSurfaceColor"))
       (= expected-table-clear-color (get stats "tableClearColor"))
       (= 9 (get stats "cssCards"))
       (= 2 (long (or (get stats "cssMajorIconStackCount") -1)))
       (= 5 (long (or (get stats "cssMajorIconCount") -1)))
       (dom-icon-layout-ready? (get stats "cssBoardIconMetrics"))
       (= 12 (get stats "cssWastelands"))
       (= 1 (long (or (get stats "cssWastelandPieceMarkers") -1)))
       (true? (get stats "cardZones"))
       (true? (get stats "cardZonesVisible"))
       (= 6 (long (or (get stats "handCardCount") -1)))
       (= 1 (long (or (get stats "handMajorIconStackCount") -1)))
       (= 1 (long (or (get stats "handMajorIconCount") -1)))
       (dom-icon-layout-ready? (get stats "handIconMetrics"))
       (pos? (long (or (get stats "drawCount") 0)))
       (zero? (long (or (get stats "discardCount") -1)))
       (str/includes? (or (get stats "statusText") "") "Three.js is unavailable")))

(defn mismatch-ready? [stats]
  (and (= "999" (get stats "threeRevision"))
       (= "always" (get stats "cardIconMode"))
       (true? (get stats "orbitControls"))
       (true? (get stats "fallback"))
       (= "always" (get stats "boardCardIconMode"))
       (false? (get stats "canvas"))
       (= expected-table-surface-color (get stats "tableSurfaceColor"))
       (= expected-table-clear-color (get stats "tableClearColor"))
       (= 9 (get stats "cssCards"))
       (= 2 (long (or (get stats "cssMajorIconStackCount") -1)))
       (= 5 (long (or (get stats "cssMajorIconCount") -1)))
       (dom-icon-layout-ready? (get stats "cssBoardIconMetrics"))
       (= 12 (get stats "cssWastelands"))
       (true? (get stats "cardZones"))
       (true? (get stats "cardZonesVisible"))
       (= 6 (long (or (get stats "handCardCount") -1)))
       (= 1 (long (or (get stats "handMajorIconStackCount") -1)))
       (= 1 (long (or (get stats "handMajorIconCount") -1)))
       (dom-icon-layout-ready? (get stats "handIconMetrics"))
       (pos? (long (or (get stats "drawCount") 0)))
       (zero? (long (or (get stats "discardCount") -1)))
       (str/includes? (or (get stats "statusText") "") "revision 999 is incompatible")))

(defn popup-mode-ready? [stats]
  (let [hand (get stats "hand")
        board-two (get stats "boardTwo")
        board-three (get stats "boardThree")
        fallback? (true? (get stats "fallback"))]
    (and (= "popup" (get stats "appMode"))
         (= "popup" (get stats "boardMode"))
         (= "false" (get stats "togglePressed"))
         (zero? (long (or (get stats "handStackCount") -1)))
         (zero? (long (or (get stats "textureErrorCount") -1)))
         (empty? (get stats "textureStatus"))
         (true? (get hand "visible"))
         (= 1 (long (or (get hand "iconCount") -1)))
         (str/includes? (or (get hand "text") "") "Sword, rod, cup, or disc")
         (true? (get board-two "visible"))
         (= 2 (long (or (get board-two "iconCount") -1)))
         (str/includes? (or (get board-two "text") "") "Rod")
         (or (not fallback?)
             (and (true? (get board-three "visible"))
                  (= 3 (long (or (get board-three "iconCount") -1)))
                  (str/includes? (or (get board-three "text") "")
                                 "Orient a target piece"))))))

(defn hotkey-help-open-ready? [stats]
  (let [labels (set (get stats "keyLabels"))
        expected-labels (set (shortcuts/hotkey-command-labels))
        board-command (:command (shortcuts/command-by-id :pan-board-view))]
    (and (true? (get stats "overlayVisible"))
         (true? (get stats "dialogVisible"))
         (= "dialog" (get stats "role"))
         (= "true" (get stats "ariaModal"))
         (str/includes? (get stats "title") "Keyboard Commands")
         (= (count shortcuts/hotkey-commands)
            (long (or (get stats "commandCount") -1)))
         (every? #(contains? labels %) expected-labels)
         (str/includes? (or (get stats "text") "") board-command))))

(defn icon-help-open-ready? [stats]
  (and (true? (get stats "overlayVisible"))
       (true? (get stats "dialogVisible"))
       (= "dialog" (get stats "role"))
       (= "true" (get stats "ariaModal"))
       (str/includes? (get stats "title") "Special Move Icons")
       (= (count icons/icon-ids) (long (or (get stats "itemCount") -1)))
       (= (count icons/icon-ids) (long (or (get stats "iconCount") -1)))
       (str/includes? (or (get stats "text") "") "Create a small piece")
       (str/includes? (or (get stats "text") "") "Any major arcana power")))

(defn icon-help-closed-ready? [stats]
  (and (false? (get stats "overlayVisible"))
       (false? (get stats "dialogVisible"))
       (zero? (long (or (get stats "itemCount") -1)))))

(defn hotkey-help-closed-ready? [stats]
  (and (false? (get stats "overlayVisible"))
       (false? (get stats "dialogVisible"))
       (zero? (long (or (get stats "commandCount") -1)))))

(defn center-card-selected? [selection]
  (str/includes? (or (get selection "panelText") "") "Row 2, Column 2"))

(defn focus-three-board! [client]
  (when-not (true? (browser/evaluate! client
                                      "(() => {
                                         const board = document.querySelector('.board-three');
                                         if (!board) return false;
                                         board.focus();
                                         return document.activeElement === board;
                                       })()"))
    (throw (ex-info "The Three.js board could not be focused for keyboard movement."
                    {}))))
