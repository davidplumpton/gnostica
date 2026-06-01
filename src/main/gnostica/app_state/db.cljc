(ns gnostica.app-state.db
  (:require [gnostica.game-state :as game-state]
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

(defn normalize-open-panels [open-panels]
  (set (filter panel-ids open-panels)))

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
