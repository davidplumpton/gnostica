(ns gnostica.ui.help
  (:require [clojure.string :as str]
            [gnostica.app.events :as events]
            [gnostica.cards :as cards]
            [gnostica.icon-view :as icon-view]
            [gnostica.icons :as icons]
            [gnostica.keyboard-shortcuts :as shortcuts]
            [re-frame.core :as rf]
            [reagent.core :as r]))

(def ^:private focusable-selector
  (str "a[href], area[href], input:not([disabled]), select:not([disabled]), "
       "textarea:not([disabled]), button:not([disabled]), "
       "iframe, object, embed, [contenteditable], "
       "[tabindex]:not([tabindex='-1'])"))

(defn- visible-element? [element]
  (let [rect (.getBoundingClientRect element)
        style (js/window.getComputedStyle element)]
    (and (pos? (.-width rect))
         (pos? (.-height rect))
         (not= "none" (.-display style))
         (not= "hidden" (.-visibility style)))))

(defn- focusable-elements [root]
  (if root
    (->> (js/Array.from (.querySelectorAll root focusable-selector))
         array-seq
         (filter visible-element?)
         vec)
    []))

(defn- focus-element! [element]
  (when (and element (.-focus element))
    (.focus element)))

(defn- restore-focus! [element]
  (when (and element
             (.-body js/document)
             (.contains (.-body js/document) element))
    (focus-element! element)))

(defn- focus-initial-element! [dialog]
  (let [target (or (some-> dialog
                           (.querySelector "[data-modal-initial-focus]"))
                   (first (focusable-elements dialog))
                   dialog)]
    (focus-element! target)))

(defn- trap-tab! [dialog event]
  (when dialog
    (let [focusable (focusable-elements dialog)
          active (.-activeElement js/document)
          first-element (first focusable)
          last-element (last focusable)
          backward? (.-shiftKey event)]
      (when (seq focusable)
        (cond
          (= 1 (count focusable))
          (do
            (.preventDefault event)
            (focus-element! first-element))

          (or (nil? active)
              (not (.contains dialog active)))
          (do
            (.preventDefault event)
            (focus-element! (if backward? last-element first-element)))

          (and backward? (= active first-element))
          (do
            (.preventDefault event)
            (focus-element! last-element))

          (and (not backward?) (= active last-element))
          (do
            (.preventDefault event)
            (focus-element! first-element)))))))

(defn- dispatch-close! [close-event]
  (rf/dispatch-sync [close-event])
  (r/flush))

(defn- escape-key-event? [event]
  (or (= "Escape" (.-key event))
      (= "Esc" (.-key event))
      (= "Escape" (.-code event))
      (= 27 (.-keyCode event))
      (= 27 (.-which event))))

(defn- handle-dialog-key-down! [dialog close-event event]
  (cond
    (escape-key-event? event)
    (do
      (.preventDefault event)
      (.stopPropagation event)
      (dispatch-close! close-event))

    (= "Tab" (.-key event))
    (trap-tab! dialog event)))

(defn- make-modal-dialog [display-name]
  (let [dialog-node (atom nil)
        previous-focus (atom nil)
        close-event-ref (atom nil)
        keydown-listener (atom nil)]
    (r/create-class
     {:display-name display-name
      :component-did-mount
      (fn [_]
        (reset! previous-focus (.-activeElement js/document))
        (let [listener #(handle-dialog-key-down!
                         @dialog-node
                         @close-event-ref
                         %)]
          (reset! keydown-listener listener)
          (.addEventListener js/document "keydown" listener true))
        (focus-initial-element! @dialog-node))
      :component-will-unmount
      (fn [_]
        (when-let [listener @keydown-listener]
          (.removeEventListener js/document "keydown" listener true))
        (let [restore-target @previous-focus]
          (js/setTimeout #(restore-focus! restore-target) 0))
        (reset! dialog-node nil)
        (reset! previous-focus nil)
        (reset! close-event-ref nil)
        (reset! keydown-listener nil))
      :reagent-render
      (fn [{:keys [overlay-class dialog-class title-id close-event]} & children]
        (reset! close-event-ref close-event)
        [:div
         {:class overlay-class
          :role "presentation"
          :on-click #(dispatch-close! close-event)}
         (into
          [:section
           {:class dialog-class
            :ref #(reset! dialog-node %)
            :role "dialog"
            :aria-modal "true"
            :aria-labelledby title-id
            :tab-index -1
            :on-click #(.stopPropagation %)
            :on-key-down #(handle-dialog-key-down!
                           @dialog-node
                           close-event
                           %)}]
          children)])})))

(def ^:private hotkey-modal-dialog
  (make-modal-dialog "HotkeyHelpDialog"))

(def ^:private icon-modal-dialog
  (make-modal-dialog "IconHelpDialog"))

(defn hotkey-help-dialog [open?]
  (when open?
    [hotkey-modal-dialog
     {:overlay-class "hotkey-help-overlay"
      :dialog-class "hotkey-help-dialog"
      :title-id "hotkey-help-title"
      :close-event events/close-hotkey-help}
     [:div.hotkey-help-dialog__header
      [:h2#hotkey-help-title "Keyboard Commands"]
      [:button.hotkey-help-dialog__close
       {:type "button"
        :data-modal-initial-focus true
        :aria-label "Close keyboard commands"
        :on-click #(dispatch-close! events/close-hotkey-help)}
       "Close"]]
     [:dl.hotkey-command-list
      (for [{:keys [keys command]} shortcuts/hotkey-commands]
        ^{:key command}
        [:div.hotkey-command
         [:dt
          (for [key-label keys]
            ^{:key key-label}
            [:kbd key-label])]
         [:dd command]])]]))

(defn- card-titles-for-icon [icon-id]
  (->> cards/deck
       (filter #(some #{icon-id} (:gnostica-icons %)))
       (map :title)
       vec))

(defn icon-help-dialog [open?]
  (when open?
    [icon-modal-dialog
     {:overlay-class "icon-help-overlay"
      :dialog-class "icon-help-dialog"
      :title-id "icon-help-title"
      :close-event events/close-icon-help}
     [:div.icon-help-dialog__header
      [:h2#icon-help-title "Special Move Icons"]
      [:button.icon-help-dialog__close
       {:type "button"
        :data-modal-initial-focus true
        :aria-label "Close special move icon guide"
        :on-click #(dispatch-close! events/close-icon-help)}
       "Close"]]
     [:div.icon-help-list
      (for [{:keys [id label description]} (icons/icon-glossary-items)]
        (let [card-titles (card-titles-for-icon id)]
          ^{:key (name id)}
          [:div.icon-help-item
           [:span.icon-help-item__icon
            [icon-view/gnostica-icon id]]
           [:div.icon-help-item__body
            [:h3 label]
            [:p.icon-help-item__description description]
            (when (seq card-titles)
              [:p.icon-help-item__cards
               [:span "Cards"]
               (str " " (str/join ", " card-titles))])]]))]]))

(defn help-dialogs []
  (let [{:keys [hotkey-help-open? icon-help-open?]} @(rf/subscribe [events/help-dialogs-view])]
    [:<>
     [hotkey-help-dialog hotkey-help-open?]
     [icon-help-dialog icon-help-open?]]))
