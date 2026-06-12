(ns gnostica.smoke.page-stats.probes
  (:require [gnostica.smoke.page-stats.common-js :as common-js]))

(def viewports
  [{:name "desktop" :width 1280 :height 900 :mobile false}
   {:name "mobile" :width 390 :height 844 :mobile true}])
(def narrow-mobile-viewport
  {:name "narrow-mobile" :width 320 :height 844 :mobile true})
(def short-mobile-viewport
  {:name "short-mobile" :width 390 :height 360 :mobile true})
(def app-ready-js
  "Boolean(document.querySelector('.app-shell') || document.querySelector('.setup-error'))")
(def happy-stats-js
  (common-js/script
   "     const header = document.querySelector('.app-header');
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
     const appStatus = document.querySelector('.app-status');
     const appScores = document.querySelector('.app-scores');
     const mobileGameContext = document.querySelector('.mobile-game-context');
     const status = Array.from(document.querySelectorAll('.board-3d-status.is-error')).map((node) => node.textContent.trim());
     const imageResourceCount = performance.getEntriesByType('resource')
       .filter((entry) => /\\/images\\/.*\\.png(?:$|\\?)/.test(entry.name)).length;
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
       boardAriaLabel: board ? board.getAttribute('aria-label') : '',
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
       appStatusVisible: visible(appStatus),
       appStatusText: text(appStatus),
       appScoresVisible: visible(appScores),
       appScoresText: text(appScores),
       mobileGameContext: Boolean(mobileGameContext),
       mobileGameContextVisible: visible(mobileGameContext),
       mobileGameContextText: text(mobileGameContext),
       mobileGameContextAriaLabel: mobileGameContext ? mobileGameContext.getAttribute('aria-label') : '',
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
     };"))
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
  (common-js/script
   "     const status = document.querySelector('.board-3d-status');
     const stage = document.querySelector('.board-fallback .board-stage');
     const fallbackFace = document.querySelector('.board-fallback .card-face');
     const cardZones = document.querySelector('.card-zones');
     const cardZonesRect = cardZones ? cardZones.getBoundingClientRect() : null;
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
     };"))
(def popup-mode-js
  (common-js/script
   "     const focusStats = (target, popoverSelector) => {
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
     };"))
(def hotkey-help-js
  (common-js/script
   "     const dialog = document.querySelector('.hotkey-help-dialog');
     const overlay = document.querySelector('.hotkey-help-overlay');
     const close = document.querySelector('.hotkey-help-dialog__close');
     const background = document.querySelector('.app-modal-scope');
     const active = document.activeElement;
     const keyLabels = Array.from(document.querySelectorAll('.hotkey-command kbd'))
       .map((node) => node.textContent.trim());
     return {
       overlayVisible: visible(overlay),
       dialogVisible: visible(dialog),
       role: dialog ? dialog.getAttribute('role') : null,
       ariaModal: dialog ? dialog.getAttribute('aria-modal') : null,
       backgroundInert: background ? Boolean(background.inert || background.hasAttribute('inert')) : false,
       backgroundAriaHidden: background ? background.getAttribute('aria-hidden') : null,
       appCardIconMode: (document.querySelector('.app-shell') || {}).dataset.cardIconMode || null,
       activeTag: active ? active.tagName : null,
       activeClass: active && active.className ? String(active.className) : '',
       activeText: active ? active.textContent.trim() : '',
       activeInDialog: Boolean(dialog && active && dialog.contains(active)),
       closeFocused: Boolean(close && active === close),
       boardFocused: Boolean(active && active.classList && active.classList.contains('board-three')),
       hotkeyToggleFocused: Boolean(active && active.classList && active.classList.contains('hotkey-help-toggle')),
       lastEscape: window.__gnosticaLastHelpEscape || null,
       title: (document.querySelector('#hotkey-help-title') || {}).textContent || '',
       commandCount: document.querySelectorAll('.hotkey-command').length,
       keyLabels,
       text: dialog ? dialog.textContent : ''
     };"))
(def icon-help-js
  (common-js/script
   "     const dialog = document.querySelector('.icon-help-dialog');
     const overlay = document.querySelector('.icon-help-overlay');
     const close = document.querySelector('.icon-help-dialog__close');
     const background = document.querySelector('.app-modal-scope');
     const active = document.activeElement;
     return {
       overlayVisible: visible(overlay),
       dialogVisible: visible(dialog),
       role: dialog ? dialog.getAttribute('role') : null,
       ariaModal: dialog ? dialog.getAttribute('aria-modal') : null,
       backgroundInert: background ? Boolean(background.inert || background.hasAttribute('inert')) : false,
       backgroundAriaHidden: background ? background.getAttribute('aria-hidden') : null,
       appCardIconMode: (document.querySelector('.app-shell') || {}).dataset.cardIconMode || null,
       activeTag: active ? active.tagName : null,
       activeClass: active && active.className ? String(active.className) : '',
       activeText: active ? active.textContent.trim() : '',
       activeInDialog: Boolean(dialog && active && dialog.contains(active)),
       closeFocused: Boolean(close && active === close),
       boardFocused: Boolean(active && active.classList && active.classList.contains('board-three')),
       iconToggleFocused: Boolean(active && active.classList && active.classList.contains('icon-help-toggle')),
       lastEscape: window.__gnosticaLastHelpEscape || null,
       title: (document.querySelector('#icon-help-title') || {}).textContent || '',
       itemCount: document.querySelectorAll('.icon-help-item').length,
       iconCount: document.querySelectorAll('.icon-help-item .gnostica-icon').length,
       text: dialog ? dialog.textContent : ''
     };"))
(def icon-help-toggle-rect-js
  "(() => {
     const button = document.querySelector('.icon-help-toggle');
     if (!button) return null;
     const rect = button.getBoundingClientRect();
     return {
       centerX: rect.left + (rect.width / 2),
       centerY: rect.top + (rect.height / 2)
     };
   })()")
(def pending-tray-stats-js
  (common-js/script
   "     const tray = document.querySelector('.pending-move-tray');
     const panel = document.querySelector('.move-panel');
     const board = document.querySelector('.board-three');
     const stage = document.querySelector('.board-fallback .board-stage');
     const previewPiece = document.querySelector('.board-move-preview__piece');
     const previewSurface = board || stage;
     const buttons = Array.from(tray ? tray.querySelectorAll('button') : []);
     const confirm = buttonByText(buttons, 'Confirm');
     const cancel = buttonByText(buttons, 'Cancel');
     const detailed = buttonByText(buttons, 'Detailed entry');
     const steps = Array.from(document.querySelectorAll('.move-panel .move-step'));
     const orientationStep = steps.find((step) => text(step.querySelector('.move-step__header span')) === 'Orientation');
     const selectedOrientation = orientationStep
       ? orientationStep.querySelector('button.move-chip[aria-pressed=\"true\"]')
       : null;
     return {
       active: Boolean(tray),
       status: text(document.querySelector('.pending-move-tray__status')),
       summary: text(document.querySelector('.pending-move-tray__summary')),
       missingCount: document.querySelectorAll('.pending-move-tray__missing li').length,
       errorText: text(document.querySelector('.pending-move-tray .move-error')),
       confirmVisible: Boolean(confirm),
       canConfirm: Boolean(confirm && !confirm.disabled),
       cancelVisible: Boolean(cancel),
       detailedVisible: Boolean(detailed),
       detailedPressed: detailed ? detailed.getAttribute('aria-pressed') : null,
       panelOpen: Boolean(panel),
       panelActive: Boolean(panel && panel.classList.contains('is-active')),
       selectedOrientation: text(selectedOrientation),
       previewTargetKind: previewSurface ? previewSurface.dataset.movePreviewTargetKind : (previewPiece ? previewPiece.dataset.previewSpaceKind : null),
       previewTargetBoardIndex: previewSurface && previewSurface.dataset.movePreviewTargetBoardIndex !== ''
         ? Number(previewSurface.dataset.movePreviewTargetBoardIndex)
         : (previewPiece && previewPiece.dataset.previewBoardIndex ? Number(previewPiece.dataset.previewBoardIndex) : null),
       previewTargetRow: previewSurface && previewSurface.dataset.movePreviewTargetRow !== ''
         ? Number(previewSurface.dataset.movePreviewTargetRow)
         : (previewPiece && previewPiece.dataset.previewRow ? Number(previewPiece.dataset.previewRow) : null),
       previewTargetCol: previewSurface && previewSurface.dataset.movePreviewTargetCol !== ''
         ? Number(previewSurface.dataset.movePreviewTargetCol)
         : (previewPiece && previewPiece.dataset.previewCol ? Number(previewPiece.dataset.previewCol) : null),
       previewPlacementOrientation: previewSurface ? previewSurface.dataset.movePreviewPlacementOrientation : (previewPiece ? previewPiece.dataset.previewOrientation : null),
       boardPointerDragEnabled: board ? board.dataset.pointerDragEnabled === 'true' : null,
       fallbackPointerDragEnabled: stage ? stage.dataset.pointerDragEnabled === 'true' : null
     };"))
(def touch-input-probe-stats-js
  "(() => {
     const events = window.__gnosticaSmokeTouchEvents || [];
     const counts = events.reduce((acc, event) => {
       acc[event.type] = (acc[event.type] || 0) + 1;
       return acc;
     }, {});
     const pointerTypes = Array.from(new Set(events
       .map((event) => event.pointerType)
       .filter(Boolean)));
     return {
       eventCount: events.length,
       touchStartCount: counts.touchstart || 0,
       touchMoveCount: counts.touchmove || 0,
       touchEndCount: counts.touchend || 0,
       pointerTouchCount: events.filter((event) => event.pointerType === 'touch').length,
       pointerTypes,
       recent: events.slice(-12)
     };
   })()")
(def hand-card-touch-point-js
  "(() => {
     const card = document.querySelector('.hand-card[draggable=\"true\"]');
     if (!card) {
       return {ok: false, reason: 'No draggable hand card found.'};
     }
     const rect = card.getBoundingClientRect();
     return {
       ok: rect.width > 0 && rect.height > 0,
       x: rect.left + rect.width / 2,
       y: rect.top + rect.height / 2,
       cardTitle: card.querySelector('.hand-card__title') ? card.querySelector('.hand-card__title').textContent.trim() : null,
       rect: {x: rect.left, y: rect.top, width: rect.width, height: rect.height}
     };
   })()")
(def direct-drop-fallback-stats-js
  (common-js/script
   "     const status = document.querySelector('.board-3d-status');
     const stage = document.querySelector('.board-fallback .board-stage');
     const movePanel = document.querySelector('.move-panel');
     const sourceButtons = Array.from(document.querySelectorAll('.move-source-option'));
     const firstPieceSource = sourceButtons.find((button) => text(button).includes('Place first piece'));
     const firstPieceIcon = firstPieceSource ? firstPieceSource.querySelector('.move-source-option__piece') : null;
     const firstPieceBody = firstPieceIcon ? firstPieceIcon.querySelector('.move-source-option__piece-body') : null;
     const firstPiecePip = firstPieceIcon ? firstPieceIcon.querySelector('.move-source-option__piece-pip') : null;
     const firstPieceBodyStyle = firstPieceBody ? getComputedStyle(firstPieceBody) : null;
     return {
       fallback: Boolean(document.querySelector('.board-fallback')),
       canvas: Boolean(document.querySelector('.board-three__canvas')),
       cssCards: document.querySelectorAll('.board-fallback .board-card').length,
       cssWastelands: document.querySelectorAll('.board-fallback .board-wasteland').length,
       pieceCount: document.querySelectorAll('.board-fallback .board-piece').length,
       wastelandPieceCount: document.querySelectorAll('.board-fallback .board-wasteland.has-pieces .board-piece').length,
       northPieceCount: document.querySelectorAll('.board-fallback .board-piece.is-north').length,
       eastPieceCount: document.querySelectorAll('.board-fallback .board-piece.is-east').length,
       smallPieceCount: document.querySelectorAll('.board-fallback .board-piece.is-small').length,
       rosePieceCount: document.querySelectorAll('.board-fallback .board-piece[data-piece-id^=\"rose-\"]').length,
       pendingActive: Boolean(document.querySelector('.pending-move-tray')),
       movePanelOpen: Boolean(movePanel),
       movePanelActive: Boolean(movePanel && movePanel.classList.contains('is-active')),
       firstPieceSourceVisible: Boolean(firstPieceSource),
       firstPieceSourceDisabled: firstPieceSource ? firstPieceSource.disabled : null,
       firstPieceSourceText: text(firstPieceSource),
       firstPieceSourceIconShape: firstPieceIcon ? firstPieceIcon.dataset.pieceShape : null,
       firstPieceSourceIconClipPath: firstPieceBodyStyle ? firstPieceBodyStyle.clipPath : null,
       firstPieceSourceIconColor: firstPieceBodyStyle ? firstPieceBodyStyle.backgroundColor : null,
       firstPieceSourceIconPipVisible: Boolean(firstPiecePip),
       pointerDragEnabled: stage ? stage.dataset.pointerDragEnabled === 'true' : null,
       tableSurfaceColor: stage ? stage.dataset.tableSurfaceColor : null,
       tableClearColor: stage ? stage.dataset.tableClearColor : null,
       statusText: status ? status.textContent.trim() : ''
     };"))
(def direct-drop-three-stats-js
  (common-js/script
   "     const board = document.querySelector('.board-three');
     const canvas = document.querySelector('.board-three__canvas');
     const movePanel = document.querySelector('.move-panel');
     const sourceButtons = Array.from(document.querySelectorAll('.move-source-option'));
     const firstPieceSource = sourceButtons.find((button) => text(button).includes('Place first piece'));
     const firstPieceIcon = firstPieceSource ? firstPieceSource.querySelector('.move-source-option__piece') : null;
     const firstPieceBody = firstPieceIcon ? firstPieceIcon.querySelector('.move-source-option__piece-body') : null;
     const firstPiecePip = firstPieceIcon ? firstPieceIcon.querySelector('.move-source-option__piece-pip') : null;
     const firstPieceBodyStyle = firstPieceBody ? getComputedStyle(firstPieceBody) : null;
     const sourceDragGhost = document.querySelector('.move-source-drag-ghost');
     const sourceDragGhostBody = sourceDragGhost ? sourceDragGhost.querySelector('.move-source-drag-ghost__body') : null;
     const sourceDragGhostBodyStyle = sourceDragGhostBody ? getComputedStyle(sourceDragGhostBody) : null;
     return {
       fallback: Boolean(document.querySelector('.board-fallback')),
       canvas: Boolean(canvas),
       pieceCount: board ? Number(board.dataset.visiblePieceCount || 0) : null,
       pendingActive: Boolean(document.querySelector('.pending-move-tray')),
       movePanelOpen: Boolean(movePanel),
       movePanelActive: Boolean(movePanel && movePanel.classList.contains('is-active')),
       firstPieceSourceVisible: Boolean(firstPieceSource),
       firstPieceSourceDisabled: firstPieceSource ? firstPieceSource.disabled : null,
       firstPieceSourceText: text(firstPieceSource),
       firstPieceSourceIconShape: firstPieceIcon ? firstPieceIcon.dataset.pieceShape : null,
       firstPieceSourceIconClipPath: firstPieceBodyStyle ? firstPieceBodyStyle.clipPath : null,
       firstPieceSourceIconColor: firstPieceBodyStyle ? firstPieceBodyStyle.backgroundColor : null,
       firstPieceSourceIconPipVisible: Boolean(firstPiecePip),
       pointerDragEnabled: board ? board.dataset.pointerDragEnabled === 'true' : null,
       sourceDragGhostVisible: Boolean(sourceDragGhost),
       sourceDragGhostShape: sourceDragGhost ? sourceDragGhost.dataset.pieceShape : null,
       sourceDragGhostOrientation: sourceDragGhost ? sourceDragGhost.dataset.orientation : null,
       sourceDragGhostClipPath: sourceDragGhostBodyStyle ? sourceDragGhostBodyStyle.clipPath : null,
       dragActive: board ? board.dataset.dragActive === 'true' : null,
       cameraDistance: board ? Number(board.dataset.cameraDistance || -1) : -1,
       cameraTargetX: board ? Number(board.dataset.cameraTargetX || -999) : -999,
       cameraTargetY: board ? Number(board.dataset.cameraTargetY || -999) : -999,
       dragGhostVisible: board ? board.dataset.dragPiecePreviewVisible === 'true' : false,
       dragPiecePreviewVisible: board ? board.dataset.dragPiecePreviewVisible === 'true' : false,
       dragPiecePreviewSize: board ? board.dataset.dragPiecePreviewSize : null,
       dragPiecePreviewPlayerId: board ? board.dataset.dragPiecePreviewPlayerId : null,
       dragPiecePreviewOrientation: board ? board.dataset.dragPiecePreviewOrientation : null,
	       dragTargetKind: board ? board.dataset.dragTargetKind : null,
	       dragTargetStatus: board ? board.dataset.dragTargetStatus : null,
	       dragTargetHighlightCount: board ? Number(board.dataset.dragTargetHighlightCount || 0) : null
	     };"))
(def initial-placement-three-drag-points-js
  (common-js/script
   "     const sourceButtons = Array.from(document.querySelectorAll('.move-source-option'));
     const source = sourceButtons.find((button) => text(button).includes('Place first piece'));
     const target = document.querySelector('.board-three__canvas');
     if (!source || !target) {
       return {
         ok: false,
         reason: source ? 'No Three.js canvas target found.' : 'No Place first piece source found.',
         sourceCount: sourceButtons.length,
         canvasCount: document.querySelectorAll('.board-three__canvas').length
       };
     }
     const sourceRect = source.getBoundingClientRect();
     const targetRect = target.getBoundingClientRect();
     const sourcePoint = {
       x: sourceRect.left + sourceRect.width / 2,
       y: sourceRect.top + sourceRect.height / 2
     };
     const targetPoint = {
       x: targetRect.left + targetRect.width / 2,
       y: targetRect.top + targetRect.height / 2
     };
     return {
       ok: true,
       source: sourcePoint,
       target: targetPoint,
       mid: {
         x: (sourcePoint.x + targetPoint.x) / 2,
         y: (sourcePoint.y + targetPoint.y) / 2
       },
       sourceText: text(source)
     };"))
(def three-piece-drag-points-js
  "(() => {
     const canvas = document.querySelector('.board-three__canvas');
     if (!canvas || !window.THREE) {
       return {ok: false, reason: 'Three.js canvas is unavailable.'};
     }
     const rect = canvas.getBoundingClientRect();
     if (rect.width <= 0 || rect.height <= 0) {
       return {ok: false, reason: 'Three.js canvas has no measurable bounds.'};
     }
     const camera = new THREE.PerspectiveCamera(45, rect.width / rect.height, 0.1, 100);
     camera.up.set(0, 0, 1);
     camera.position.set(0, -4.8, 5.9);
     camera.lookAt(new THREE.Vector3(0, 0, 0));
     camera.updateMatrixWorld();
     camera.updateProjectionMatrix();
     const project = (x, y, z) => {
       const point = new THREE.Vector3(x, y, z).project(camera);
       return {
         x: rect.left + ((point.x + 1) / 2) * rect.width,
         y: rect.top + ((1 - point.y) / 2) * rect.height
       };
     };
     const source = project(-1.39, 1.36, 0.12);
     return {
       ok: source.x >= rect.left
         && source.x <= rect.right
         && source.y >= rect.top
         && source.y <= rect.bottom,
       source,
       target: {x: source.x + 32, y: source.y + 18},
       canvas: {x: rect.left, y: rect.top, width: rect.width, height: rect.height}
     };
   })()")
