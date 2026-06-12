(ns gnostica.smoke.page-stats.actions
  (:require [gnostica.smoke.browser :as browser]
            [gnostica.smoke.page-stats.common-js :as common-js]))

(def help-dialog-escape-keydown-js
  "(() => {
     const target = document.activeElement || document.body;
     const event = new KeyboardEvent('keydown', {
       key: 'Escape',
       code: 'Escape',
       bubbles: true,
       cancelable: true
     });
     const dispatched = target.dispatchEvent(event);
     const close = document.querySelector('.hotkey-help-dialog__close, .icon-help-dialog__close');
     const closeRect = close ? close.getBoundingClientRect() : null;
     const result = {
       targetTag: target ? target.tagName : null,
       targetClass: target && target.className ? String(target.className) : '',
       canceled: !dispatched,
       closeClicked: false,
       closeRect: closeRect ? {
         centerX: closeRect.left + (closeRect.width / 2),
         centerY: closeRect.top + (closeRect.height / 2)
       } : null
     };
     window.__gnosticaLastHelpEscape = result;
     return result;
   })()")
(def move-panel-hand-card-step-js
  (common-js/script
   "     const moveToggle = document.querySelector('.panel-toggle[aria-controls=\"move-panel\"]');
     if (!document.querySelector('.move-panel') && moveToggle) {
       moveToggle.click();
     }
     const sourceButtons = Array.from(document.querySelectorAll('.move-source-option'));
     const handSource = sourceButtons.find((button) => button.textContent.includes('Play hand card'));
     if (handSource && handSource.getAttribute('aria-pressed') !== 'true') {
       handSource.click();
     }
     const panel = document.querySelector('.move-panel');
     const selectedSource = document.querySelector('.move-source-option[aria-pressed=\"true\"]');
     const steps = Array.from(document.querySelectorAll('.move-panel .move-step'));
     const stepLabels = steps.map((step) => text(step.querySelector('.move-step__header span')));
     const handStep = steps.find((step) => text(step.querySelector('.move-step__header span')) === 'Hand card');
     return {
       panelOpen: Boolean(panel),
       panelActive: panel ? panel.classList.contains('is-active') : false,
       selectedSourceLabel: text(selectedSource ? selectedSource.querySelector('.move-source-option__label') : null),
       prompt: text(document.querySelector('.move-panel__prompt')),
       stepCount: steps.length,
       stepLabels,
       handCardStep: Boolean(handStep),
       handCardChoiceCount: handStep ? handStep.querySelectorAll('.move-chip').length : 0
     };"))
(def short-mobile-gameplay-reachability-js
  (common-js/script
   "     const scrollRoot = document.scrollingElement || document.documentElement;
     const moveToggle = document.querySelector('.panel-toggle[aria-controls=\"move-panel\"]');
     const cardsToggle = document.querySelector('.panel-toggle[aria-controls=\"cards-panel\"]');
     if (!document.querySelector('.move-panel') && moveToggle) {
       moveToggle.click();
     }
     if (!document.querySelector('.card-zones') && cardsToggle) {
       cardsToggle.click();
     }
     window.scrollTo(0, 0);
     const header = document.querySelector('.app-header');
     const board = document.querySelector('.board-three');
     const shell = document.querySelector('.app-shell');
     const topHeaderVisible = visibleInViewport(header);
     const topBoardVisible = visibleInViewport(board);
     const requestedScrollY = Math.max(0, scrollRoot.scrollHeight - scrollRoot.clientHeight);
     window.scrollTo(0, requestedScrollY);
     const sideStack = document.querySelector('.side-stack');
     if (sideStack) {
       sideStack.scrollTop = 0;
     }
     const cardZones = document.querySelector('.card-zones');
     const movePanel = document.querySelector('.move-panel');
     const moveSource = document.querySelector('.move-source-option');
     const sideStackStyle = sideStack ? getComputedStyle(sideStack) : null;
     const bodyStyle = getComputedStyle(document.body);
     const rootStyle = getComputedStyle(document.documentElement);
     return {
       viewportWidth: window.innerWidth,
       viewportHeight: window.innerHeight,
       bodyOverflowY: bodyStyle.overflowY,
       rootOverflowY: rootStyle.overflowY,
       scrollHeight: scrollRoot.scrollHeight,
       clientHeight: scrollRoot.clientHeight,
       requestedScrollY,
       scrollYAfter: Math.round(window.scrollY || scrollRoot.scrollTop || 0),
       shellHeight: shell ? Math.round(shell.getBoundingClientRect().height) : 0,
       moveToggle: Boolean(moveToggle),
       cardsToggle: Boolean(cardsToggle),
       topHeaderVisible,
       topBoardVisible,
       cardsPanelReachable: visibleInViewport(cardZones),
       movePanelReachable: visibleInViewport(movePanel),
       moveSourceReachable: visibleInViewport(moveSource),
       sideStackVisible: visibleInViewport(sideStack),
       sideStackOverflowY: sideStackStyle ? sideStackStyle.overflowY : null,
       sideStackClientHeight: sideStack ? sideStack.clientHeight : 0,
       sideStackScrollHeight: sideStack ? sideStack.scrollHeight : 0
     };"))
(def keyboard-placement-start-js
  (common-js/script
   "     const sourceButtons = Array.from(document.querySelectorAll('.move-source-option'));
     const source = sourceButtons.find((button) => text(button).includes('Place first piece'));
     if (!source) {
       return {
         started: false,
         reason: 'No Place first piece source found.',
         sourceCount: sourceButtons.length
       };
     }
     source.focus();
     const event = new KeyboardEvent('keydown', {
       key: 'Enter',
       code: 'Enter',
       bubbles: true,
       cancelable: true
     });
     const dispatched = source.dispatchEvent(event);
     return {
       started: true,
       dispatched,
       focused: document.activeElement === source,
       sourceText: text(source)
     };"))
(def open-detailed-entry-js
  (common-js/script
   "     const buttons = Array.from(document.querySelectorAll('.pending-move-tray button'));
     const detailed = buttonByText(buttons, 'Detailed entry');
     if (detailed) detailed.click();
     return Boolean(detailed);"))
(def touch-input-probe-init-js
  "(() => {
     window.__gnosticaSmokeTouchEvents = [];
     const classText = (node) => node && node.className ? String(node.className) : '';
     const record = (event) => {
       const events = window.__gnosticaSmokeTouchEvents || (window.__gnosticaSmokeTouchEvents = []);
       events.push({
         type: event.type,
         pointerType: event.pointerType || null,
         targetClass: classText(event.target),
         touchCount: event.touches ? event.touches.length : null,
         changedTouchCount: event.changedTouches ? event.changedTouches.length : null
       });
       if (events.length > 120) events.shift();
     };
     ['touchstart', 'touchmove', 'touchend', 'touchcancel']
       .forEach((type) => window.addEventListener(type, record, true));
     ['pointerdown', 'pointermove', 'pointerup', 'pointercancel']
       .forEach((type) => window.addEventListener(type, record, true));
   })();")
(def reset-touch-input-probe-js
  "(() => {
     window.__gnosticaSmokeTouchEvents = [];
     return true;
   })()")
(def cancel-pending-move-js
  (common-js/script
   "     const buttons = Array.from(document.querySelectorAll('.pending-move-tray button'));
     const cancel = buttonByText(buttons, 'Cancel');
     if (cancel) cancel.click();
     return Boolean(cancel);"))
(def close-move-panel-js
  "(() => {
     const close = document.querySelector('.move-panel .panel-close');
     if (close) close.click();
     return Boolean(close);
   })()")
(def hand-card-drag-js
  "(() => {
     const card = document.querySelector('.hand-card[draggable=\"true\"]');
     if (!card) {
       return {started: false, reason: 'No draggable hand card found.'};
     }
     const dataTransfer = new DataTransfer();
     const event = new DragEvent('dragstart', {
       bubbles: true,
       cancelable: true,
       dataTransfer
     });
     const dispatched = card.dispatchEvent(event);
     return {
       started: true,
       dispatched,
       draggable: card.getAttribute('draggable'),
       payload: dataTransfer.getData('application/gnostica-gesture')
     };
   })()")
(def fallback-piece-drag-js
  "(() => {
     const piece = document.querySelector('.board-fallback .board-piece[draggable=\"true\"]');
     if (!piece) {
       return {started: false, reason: 'No draggable CSS fallback piece found.'};
     }
     const dataTransfer = new DataTransfer();
     const event = new DragEvent('dragstart', {
       bubbles: true,
       cancelable: true,
       dataTransfer
     });
     const dispatched = piece.dispatchEvent(event);
     return {
       started: true,
       dispatched,
       pieceId: piece.dataset.pieceId || null,
       draggable: piece.getAttribute('draggable'),
       payload: dataTransfer.getData('application/gnostica-gesture')
     };
   })()")
(def initial-placement-three-drop-js
  (common-js/promise-script
   "     const sourceButtons = Array.from(document.querySelectorAll('.move-source-option'));
     const source = sourceButtons.find((button) => text(button).includes('Place first piece'));
     const target = document.querySelector('.board-three__canvas');
     if (!source || !target) {
       resolve({
         dropped: false,
         reason: source ? 'No Three.js canvas drop target found.' : 'No Place first piece source found.',
         sourceCount: sourceButtons.length,
         canvasCount: document.querySelectorAll('.board-three__canvas').length
       });
       return;
     }
     const rect = target.getBoundingClientRect();
     const dataTransfer = new DataTransfer();
     const eventInit = {
       bubbles: true,
       cancelable: true,
       dataTransfer,
	       clientX: rect.left + rect.width / 2,
	       clientY: rect.top + rect.height / 2
	     };
	     const dragStart = new DragEvent('dragstart', eventInit);
	     const dragStartDispatched = source.dispatchEvent(dragStart);
	     requestAnimationFrame(() => requestAnimationFrame(() => {
	       const board = document.querySelector('.board-three');
	       if (board) board.focus();
	       const orientationKey = new KeyboardEvent('keydown', {
	         key: 'ArrowRight',
	         code: 'ArrowRight',
	         bubbles: true,
	         cancelable: true
	       });
	       const orientationKeyDispatched = board ? board.dispatchEvent(orientationKey) : false;
	       requestAnimationFrame(() => requestAnimationFrame(() => {
	       const protectedTransfer = {
	         types: Array.from(dataTransfer.types),
	         dropEffect: 'move',
	         effectAllowed: dataTransfer.effectAllowed,
	         getData: () => '',
	         setData: () => {},
	         setDragImage: () => {}
	       };
	       const dragOver = new MouseEvent('dragover', eventInit);
	       Object.defineProperty(dragOver, 'dataTransfer', {value: protectedTransfer});
	       const dragOverDispatched = target.dispatchEvent(dragOver);
	       requestAnimationFrame(() => requestAnimationFrame(() => {
	         const board = document.querySelector('.board-three');
	         const beforeDrop = {
	           boardDragActiveBeforeDrop: board ? board.dataset.dragActive === 'true' : null,
	           dragTargetKindBeforeDrop: board ? board.dataset.dragTargetKind : null,
	           dragTargetStatusBeforeDrop: board ? board.dataset.dragTargetStatus : null,
	           dragTargetHighlightCountBeforeDrop: board ? Number(board.dataset.dragTargetHighlightCount || 0) : null,
	           dragPiecePreviewVisibleBeforeDrop: board ? board.dataset.dragPiecePreviewVisible === 'true' : null,
	           dragPiecePreviewSizeBeforeDrop: board ? board.dataset.dragPiecePreviewSize : null,
	           dragPiecePreviewPlayerIdBeforeDrop: board ? board.dataset.dragPiecePreviewPlayerId : null,
	           dragPiecePreviewOrientationBeforeDrop: board ? board.dataset.dragPiecePreviewOrientation : null
	         };
	         const drop = new DragEvent('drop', eventInit);
	         const dropDispatched = target.dispatchEvent(drop);
	         resolve(Object.assign({
	           dropped: true,
	           dragStartDispatched,
	           orientationKeyDispatched,
	           dragOverDispatched,
	           dropDispatched,
	           sourceText: text(source),
	           payload: dataTransfer.getData('application/gnostica-gesture'),
	           fallbackPayload: dataTransfer.getData('text/plain'),
	           target: {x: eventInit.clientX, y: eventInit.clientY},
	           ghostVisible: beforeDrop.dragPiecePreviewVisibleBeforeDrop,
	           ghostPlayerId: beforeDrop.dragPiecePreviewPlayerIdBeforeDrop,
	           ghostPieceSize: beforeDrop.dragPiecePreviewSizeBeforeDrop,
	           ghostOrientation: beforeDrop.dragPiecePreviewOrientationBeforeDrop,
	           ghostClassName: null,
	           ghostLeft: null,
	           ghostTop: null
	         }, beforeDrop));
	       }));
	       }));
	     }));"))
(def initial-placement-drop-js
  (common-js/promise-script
   "     const sourceButtons = Array.from(document.querySelectorAll('.move-source-option'));
     const source = sourceButtons.find((button) => text(button).includes('Place first piece'));
     const target = document.querySelector('.board-fallback .board-card');
     if (!source || !target) {
       resolve({
         dropped: false,
         reason: source ? 'No CSS fallback board-card drop target found.' : 'No Place first piece source found.',
         sourceCount: sourceButtons.length,
         targetCount: document.querySelectorAll('.board-fallback .board-card').length
       });
       return;
     }
     const dataTransfer = new DataTransfer();
	     const dragStart = new DragEvent('dragstart', {
	       bubbles: true,
	       cancelable: true,
	       dataTransfer
	     });
	     const dragStartDispatched = source.dispatchEvent(dragStart);
	     requestAnimationFrame(() => requestAnimationFrame(() => {
	       const protectedTransfer = {
	         types: Array.from(dataTransfer.types),
	         dropEffect: 'move',
	         effectAllowed: dataTransfer.effectAllowed,
	         getData: () => '',
	         setData: () => {},
	         setDragImage: () => {}
	       };
	       const dragOver = new MouseEvent('dragover', {
	         bubbles: true,
	         cancelable: true
	       });
	       Object.defineProperty(dragOver, 'dataTransfer', {value: protectedTransfer});
	       const dragOverDispatched = target.dispatchEvent(dragOver);
	       requestAnimationFrame(() => requestAnimationFrame(() => {
	         const stage = document.querySelector('.board-fallback .board-stage');
	         const beforeDrop = {
	           boardDragActiveBeforeDrop: stage ? stage.dataset.dragActive === 'true' : null,
	           dragHoverKindBeforeDrop: stage ? stage.dataset.dragHoverKind : null,
	           dragHoverStatusBeforeDrop: stage ? stage.dataset.dragHoverStatus : null,
	           dragHoverTargetCountBeforeDrop: document.querySelectorAll('.board-fallback .board-card.is-drag-hover-target, .board-fallback .board-wasteland.is-drag-hover-target').length,
	           visibleLegalTargetCountBeforeDrop: document.querySelectorAll('.board-fallback .board-card.is-legal-target, .board-fallback .board-wasteland.is-legal-target').length,
	           visibleDisabledTargetCountBeforeDrop: document.querySelectorAll('.board-fallback .board-card.is-disabled-target, .board-fallback .board-wasteland.is-disabled-target').length,
	           dropTargetCountBeforeDrop: document.querySelectorAll('.board-fallback .board-card.is-drop-target, .board-fallback .board-wasteland.is-drop-target').length
	         };
	         const drop = new DragEvent('drop', {
	           bubbles: true,
	           cancelable: true,
	           dataTransfer
	         });
	         const dropDispatched = target.dispatchEvent(drop);
	         resolve(Object.assign({
	           dropped: true,
	           dragStartDispatched,
	           dragOverDispatched,
	           dropDispatched,
	           sourceText: text(source),
	           targetText: text(target),
	           payload: dataTransfer.getData('application/gnostica-gesture')
		         }, beforeDrop));
		       }));
		     }));"))
(def initial-placement-click-wasteland-js
  (common-js/promise-script
   "     const sourceButtons = Array.from(document.querySelectorAll('.move-source-option'));
     const source = sourceButtons.find((button) => text(button).includes('Place first piece'));
     if (!source) {
       resolve({
         clicked: false,
         reason: 'No Place first piece source found.',
         sourceCount: sourceButtons.length
       });
       return;
     }
     const sourceClicked = clickNode(source);
     requestAnimationFrame(() => requestAnimationFrame(() => {
       const target = document.querySelector('.board-fallback .board-wasteland[data-move-target-status=\"legal\"]');
       const targetClicked = clickNode(target);
       requestAnimationFrame(() => requestAnimationFrame(() => {
         const orientationButtons = Array.from(document.querySelectorAll('.move-chip'));
         const east = orientationButtons.find((button) => text(button) === 'East');
         const eastClicked = clickNode(east);
         requestAnimationFrame(() => requestAnimationFrame(() => {
           const actions = Array.from(document.querySelectorAll('.move-panel .move-actions button'));
           const confirm = actions.find((button) => text(button) === 'Confirm');
           const canConfirm = Boolean(confirm && !confirm.disabled);
           if (canConfirm) confirm.click();
           resolve({
             clicked: Boolean(sourceClicked && targetClicked && eastClicked && canConfirm),
             sourceClicked,
             targetClicked,
             eastClicked,
             confirmClicked: canConfirm,
             targetRole: target ? target.getAttribute('role') : null,
             targetStatus: target ? target.dataset.moveTargetStatus : null,
             targetTabIndex: target ? target.getAttribute('tabindex') : null,
             targetCount: document.querySelectorAll('.board-fallback .board-wasteland').length,
             legalTargetCount: document.querySelectorAll('.board-fallback .board-wasteland[data-move-target-status=\"legal\"]').length
           });
         }));
       }));
     }));"))
(def confirm-pending-move-js
  (common-js/script
   "     const buttons = Array.from(document.querySelectorAll('.pending-move-tray button'));
     const confirm = buttonByText(buttons, 'Confirm');
     const canConfirm = Boolean(confirm && !confirm.disabled);
     if (canConfirm) confirm.click();
     return {
       clicked: canConfirm,
       confirmVisible: Boolean(confirm),
       confirmDisabled: confirm ? confirm.disabled : null
     };"))
(def mismatched-three-js
  "window.THREE = {REVISION: '999', OrbitControls: function OrbitControls() {}};")
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
