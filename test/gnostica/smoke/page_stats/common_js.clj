(ns gnostica.smoke.page-stats.common-js)

(def dom-helpers-js
  "const visible = (node) => {
     const rect = node ? node.getBoundingClientRect() : null;
     const style = node ? getComputedStyle(node) : null;
     const opacity = style ? Number.parseFloat(style.opacity || '1') : 0;
     return Boolean(rect
       && rect.width > 0
       && rect.height > 0
       && style.display !== 'none'
       && style.visibility !== 'hidden'
       && opacity > 0);
   };
   const visibleInViewport = (node) => {
     const rect = node ? node.getBoundingClientRect() : null;
     const style = node ? getComputedStyle(node) : null;
     const opacity = style ? Number.parseFloat(style.opacity || '1') : 0;
     return Boolean(rect
       && rect.width > 0
       && rect.height > 0
       && rect.right > 0
       && rect.left < window.innerWidth
       && rect.bottom > 0
       && rect.top < window.innerHeight
       && style.display !== 'none'
       && style.visibility !== 'hidden'
       && opacity > 0);
   };
   const text = (node) => node ? node.textContent.trim().replace(/\\s+/g, ' ') : '';
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
   const buttonByText = (buttons, label) => buttons.find((button) => text(button) === label);
   const clickNode = (node) => {
     if (!node) return false;
     node.click();
     return true;
   };")

(defn script [body]
  (str "(() => {\n" dom-helpers-js "\n" body "\n})()"))

(defn promise-script [body]
  (str "(() => new Promise((resolve) => {\n" dom-helpers-js "\n" body "\n}))()"))
