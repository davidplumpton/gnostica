(ns gnostica.ui.card
  (:require [clojure.string :as str]
            [gnostica.icon-layout :as icon-layout]
            [gnostica.icon-view :as icon-view]
            [gnostica.icons :as icons]))

(defn- card-icon-stack [card]
  (when-let [icon-ids (seq (icons/present-icon-ids (:gnostica-icons card)))]
    [:span.gnostica-icon-stack
     {:aria-hidden "true"
      :style (icon-layout/dom-icon-stack-style)
      :data-icon-scale icon-layout/card-icon-scale
      :data-icon-count (count icon-ids)
      :data-icon-ids (str/join "," (map name icon-ids))
      :title (icons/icon-stack-label icon-ids)}
     (for [[position icon-id] (map-indexed vector icon-ids)]
       ^{:key (str (:id card) "-" position "-" (name icon-id))}
       [icon-view/gnostica-icon icon-id])]))

(defn card-icon-summary [card]
  (when-let [icon-ids (seq (icons/present-icon-ids (:gnostica-icons card)))]
    (icons/icon-stack-label icon-ids)))

(defn- card-aria-label [card alt-text]
  (str alt-text
       (when-let [summary (card-icon-summary card)]
         (str ", special moves: " summary))))

(defn- class-names [& classes]
  (->> classes
       (remove str/blank?)
       (str/join " ")))

(defn card-face
  ([card class-name card-icon-mode]
   (card-face card class-name (:title card) card-icon-mode {}))
  ([card class-name alt-text card-icon-mode]
   (card-face card class-name alt-text card-icon-mode {}))
  ([card class-name alt-text card-icon-mode {:keys [focusable?]}]
   (let [has-icons? (boolean (seq (icons/present-icon-ids (:gnostica-icons card))))
         popup-mode? (= :popup card-icon-mode)]
     [:span.card-face
      {:class (class-names class-name
                           (when has-icons? "has-gnostica-icons"))
       :data-icon-mode (name card-icon-mode)
       :aria-label (when (and focusable? has-icons?)
                     (card-aria-label card alt-text))
       :tabIndex (when (and focusable? has-icons?)
                   0)}
      [:img.card-face__image
       {:src (:image card)
        :alt alt-text
        :draggable "false"}]
      (if popup-mode?
        [icon-view/card-icon-popover card]
        [card-icon-stack card])])))
