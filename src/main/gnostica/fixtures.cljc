(ns gnostica.fixtures
  (:require [clojure.string :as str]
            [gnostica.cards :as cards]
            [gnostica.pieces :as pieces]))

(def smoke-query-param "gnostica-smoke")

(def smoke-major-icons-mode "major-icons")

(def dev-shared-control-query-param "gnostica-dev-shared-control")

(def shared-local-controller
  {:id "local-dev"
   :name "Local dev"})

(def demo-board-pieces
  [{:id :rose-scout
    :player-id :rose
    :space-index 0
    :size :small
    :orientation :east}
   {:id :indigo-minion
    :player-id :indigo
    :space-index 4
    :size :medium
    :orientation :up}
   {:id :gold-charger
    :player-id :gold
    :space-index 4
    :size :large
    :orientation :north}
   {:id :teal-guard
    :player-id :teal
    :space-index 4
    :size :small
    :orientation :west}
   {:id :rose-striker
    :player-id :rose
    :space-index 8
    :size :medium
    :orientation :south}])

(defn demo-board-pieces-for [player-specs]
  (let [player-ids (set (map :id player-specs))]
    (filterv #(contains? player-ids (:player-id %))
             demo-board-pieces)))

(defn demo-init-options
  ([] {:demo-board-pieces demo-board-pieces})
  ([player-specs]
   {:demo-board-pieces (demo-board-pieces-for player-specs)}))

(def default-browser-lobby-player-specs
  (mapv #(select-keys % [:id :name])
        (take 2 pieces/players)))

(defn lobby-init-options []
  {:start-in-lobby? true
   :player-specs default-browser-lobby-player-specs})

(defn shared-local-control-init-options [enabled?]
  (when enabled?
    {:start-in-lobby? true
     :player-specs default-browser-lobby-player-specs
     :local-controller shared-local-controller}))

(defn major-icon-smoke-deck-order []
  (let [major-hand-card (cards/card-by-id "magician")
        major-board-cards [(cards/card-by-id "chariot")
                           (cards/card-by-id "devil")]
        minor-cards (filter #(= :minor (:arcana %)) cards/deck)
        current-hand-minors (take 5 minor-cards)
        other-hand-cards (take 30 (drop 5 minor-cards))
        board-minors (take 7 (drop 35 minor-cards))
        used-card-ids (set (map :id (concat [major-hand-card]
                                            major-board-cards
                                            current-hand-minors
                                            other-hand-cards
                                            board-minors)))
        remaining-cards (remove #(contains? used-card-ids (:id %)) cards/deck)]
    (vec (concat [major-hand-card]
                 current-hand-minors
                 other-hand-cards
                 major-board-cards
                 board-minors
                 remaining-cards))))

(defn smoke-init-options [smoke-mode]
  (when (= smoke-major-icons-mode smoke-mode)
    {:start-in-lobby? false
     :bypass-lobby? true
     :player-specs (mapv #(select-keys % [:id :name]) pieces/players)
     :demo-board-pieces demo-board-pieces
     :game-options {:deck-order (major-icon-smoke-deck-order)}}))

(defn merge-init-options [& options]
  (reduce (fn [merged option]
            (if-not option
              merged
              (let [game-options (merge (:game-options merged)
                                        (:game-options option))]
                (cond-> (merge merged (dissoc option :game-options))
                  (seq game-options)
                  (assoc :game-options game-options)))))
          {}
          options))

#?(:cljs
   (defn- smoke-mode-from-search [search]
     (let [params (js/URLSearchParams. (or search ""))]
       (.get params smoke-query-param))))

#?(:cljs
   (defn- truthy-query-param? [params param-name]
     (contains? #{"1" "true" "yes" "on"}
                (some-> (.get params param-name)
                        str/lower-case))))

#?(:cljs
   (defn- shared-local-control-from-search [search]
     (let [params (js/URLSearchParams. (or search ""))]
       (truthy-query-param? params dev-shared-control-query-param))))

#?(:cljs
   (defn browser-init-options
     ([] (browser-init-options (.. js/window -location -search)))
     ([search]
      (merge-init-options (lobby-init-options)
                          (shared-local-control-init-options
                           (shared-local-control-from-search search))
                          (smoke-init-options (smoke-mode-from-search search))))))
