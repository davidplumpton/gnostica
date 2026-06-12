(ns gnostica.app-state.db
  (:require [gnostica.board :as board]
            [gnostica.game-state :as game-state]
            [gnostica.gesture-intent :as gesture-intent]
            [gnostica.move-selection :as move-selection]
            [gnostica.pieces :as pieces]))

(def default-player-specs
  (mapv #(select-keys % [:id :name]) pieces/players))

(def default-lobby-player-specs
  (subvec default-player-specs 0 game-state/min-players))

(def default-selected-board-index 0)

(def default-card-icon-mode :always)

(def default-hotkey-help-open? false)

(def default-icon-help-open? false)

(def default-dev-demo-hotkeys? false)

(def target-score-options
  (vec (sort game-state/allowed-target-scores)))

(def panel-ids
  #{:cards :move :territory})

(def default-open-panels
  #{:cards})

(def default-three-runtime-status
  {:ok? false
   :code :three-unchecked
   :revision nil
   :expected-revision nil
   :message "Three.js runtime status has not been checked yet."})

(def default-direct-manipulation
  {:pointer-drag-enabled? true
   :detailed-entry-available? true
   :detailed-entry-default? false})

(def card-icon-modes
  #{:always :popup})

(def empty-move-selection move-selection/empty-move-selection)

(def dev-demo-territory-columns 10)

(def dev-demo-territory-start-row -2)

(def dev-demo-territory-start-col -3)

(def max-dev-demo-territory-count 78)

(defn normalize-open-panels [open-panels]
  (set (filter panel-ids open-panels)))

(defn normalize-dev-demo-hotkeys [opts]
  (true? (:dev-demo-hotkeys? opts)))

(defn dev-demo-hotkeys? [app-db]
  (true? (:dev-demo-hotkeys? app-db)))

(defn state-with-demo-board-pieces [state opts]
  (if (contains? opts :demo-board-pieces)
    (game-state/with-board-pieces state (:demo-board-pieces opts))
    state))

(defn normalize-card-icon-mode [mode]
  (if (contains? card-icon-modes mode)
    mode
    default-card-icon-mode))

(defn normalize-three-runtime-status [status]
  (let [status (if (map? status) status {})]
    (merge default-three-runtime-status
           (select-keys status [:code :revision :expected-revision :message])
           {:ok? (true? (:ok? status))})))

(defn normalize-direct-manipulation [{:keys [direct-manipulation
                                             direct-manipulation-enabled?]
                                      :as _opts}]
  (let [settings (if (map? direct-manipulation)
                   direct-manipulation
                   {})]
    (cond-> (merge default-direct-manipulation
                   (select-keys settings [:pointer-drag-enabled?
                                          :detailed-entry-available?
                                          :detailed-entry-default?]))
      (contains? settings :pointer-drag-enabled?)
      (update :pointer-drag-enabled? true?)

      (contains? settings :detailed-entry-available?)
      (update :detailed-entry-available? true?)

      (contains? settings :detailed-entry-default?)
      (update :detailed-entry-default? true?)

      (contains? _opts :direct-manipulation-enabled?)
      (assoc :pointer-drag-enabled? (true? direct-manipulation-enabled?)))))

(defn base-db
  [{:keys [selected-board-index card-icon-mode open-panels three-runtime-status]
    :as opts
    :or {selected-board-index default-selected-board-index
         card-icon-mode default-card-icon-mode
         open-panels default-open-panels}}]
  (let [direct-manipulation (normalize-direct-manipulation opts)]
    {:selected-board-index selected-board-index
     :card-icon-mode (normalize-card-icon-mode card-icon-mode)
     :open-panels (cond-> (normalize-open-panels open-panels)
                    (and (:detailed-entry-available? direct-manipulation)
                         (:detailed-entry-default? direct-manipulation))
                    (conj :move))
     :hotkey-help-open? default-hotkey-help-open?
     :icon-help-open? default-icon-help-open?
     :dev-demo-hotkeys? (normalize-dev-demo-hotkeys opts)
     :move-selection (empty-move-selection)
     :gesture-intent gesture-intent/empty-gesture-intent
     :three-runtime-status (normalize-three-runtime-status three-runtime-status)
     :direct-manipulation direct-manipulation
     :three-texture-errors []}))

(defn initialize-game-db [db opts player-specs game-options]
  (let [result (game-state/create-game player-specs (or game-options {}))]
    (if (:ok? result)
      (-> db
          (assoc :game (state-with-demo-board-pieces (:state result) opts))
          (dissoc :turn-action)
          (dissoc :setup-error :lobby))
      (-> db
          (assoc :setup-error (:error result))
          (dissoc :game :turn-action)))))

(defn game [db]
  (:game db))

(defn setup-error [db]
  (:setup-error db))

(defn- game-zone-cards [state]
  (concat
   (mapcat :hand (:players state))
   (map :card (:board state))
   (:draw-pile state)
   (:discard-pile state)))

(defn- ordered-setup-cards [state]
  (let [deck-card-ids (vec (get-in state [:setup :deck-card-ids]))
        cards-by-id (into {} (map (juxt :id identity)) (game-zone-cards state))
        cards (mapv cards-by-id deck-card-ids)]
    (when (and (seq cards)
               (every? some? cards)
               (<= (count cards) max-dev-demo-territory-count))
      cards)))

(defn- dev-demo-territory-coordinate [index]
  {:row (+ dev-demo-territory-start-row
           (quot index dev-demo-territory-columns))
   :col (+ dev-demo-territory-start-col
           (mod index dev-demo-territory-columns))})

(defn- dev-demo-territory-cell [index card]
  (let [{:keys [row col]} (dev-demo-territory-coordinate index)]
    {:index index
     :row row
     :col col
     :orientation (board/orientation-for row col)
     :face :up
     :card card}))

(defn- clear-player-hands [state]
  (let [players (mapv #(assoc % :hand []) (:players state))]
    (assoc state
           :players players
           :players-by-id (into {} (map (juxt :id identity) players)))))

(defn layout-shuffled-deck-as-territories [app-db]
  (if-not (and (dev-demo-hotkeys? app-db)
               (:game app-db))
    app-db
    (if-let [ordered-cards (ordered-setup-cards (:game app-db))]
      (let [board-cells (vec (map-indexed dev-demo-territory-cell
                                          ordered-cards))
            event {:type :demo/deck-laid-out-as-territories
                   :card-count (count board-cells)}]
        (-> app-db
            (update :game
                    (fn [state]
                      (-> state
                          clear-player-hands
                          (assoc :board board-cells
                                 :draw-pile []
                                 :discard-pile [])
                          (game-state/append-history event)
                          game-state/with-current-scores)))
            (assoc :move-selection (empty-move-selection)
                   :gesture-intent gesture-intent/empty-gesture-intent)
            (dissoc :turn-action :turn-result)))
      app-db)))
