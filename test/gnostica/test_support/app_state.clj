(ns gnostica.test-support.app-state
  (:require [gnostica.app.subscriptions :as app-subscriptions]
            [gnostica.app-state :as app-state]))

(def test-player-specs
  [{:id :rose}
   {:id :indigo}])

(def rose-source-piece
  {:id :rose-scout
   :player-id :rose
   :space-index 0
   :size :small
   :orientation :east})

(def rose-hand-piece
  {:id :rose-striker
   :player-id :rose
   :space-index 8
   :size :medium
   :orientation :south})

(def rose-rod-minion
  {:id :rose-rod-minion
   :player-id :rose
   :space-index 3
   :size :medium
   :orientation :east})

(def rose-rod-target
  {:id :rose-rod-target
   :player-id :rose
   :space-index 5
   :size :small
   :orientation :north})

(def indigo-rod-target
  {:id :indigo-rod-target
   :player-id :indigo
   :space-index 4
   :size :small
   :orientation :north})

(def rose-hand-cup-territory-piece
  (assoc rose-hand-piece
         :space-index 6
         :orientation :north))

(def rose-hand-cup-enemy-piece
  (assoc rose-hand-piece
         :space-index 3
         :orientation :east))

(defn contains-data-value? [needle data]
  (boolean
   (some #(= needle %)
         (tree-seq coll? seq data))))

(defn move-control-group-summary [db]
  (mapv #(select-keys % [:type :power :action-power])
        (:control-groups (app-state/move-panel-view db))))

(defn move-panel-subscription-view [db]
  (app-subscriptions/move-panel-view db [:gnostica.app/move-panel-view]))

(defn action-ribbon-step-summary [view]
  (mapv #(select-keys % [:power :status :board-index :compound?])
        (get-in view [:action-ribbon :steps])))

(defn alternative-by-field [tray field]
  (some #(when (= field (:field %)) %)
        (:alternatives tray)))

(def expected-major-power-options
  {"fool" [:fool]
   "magician" [:cup :rod :disc :sword]
   "high-priestess" [:high-priestess]
   "empress" [:empress :cup]
   "emperor" [:emperor :rod]
   "hierophant" [:hierophant]
   "lovers" [:lovers :cup :rod]
   "chariot" [:chariot :rod]
   "justice" [:justice :sword]
   "hermit" [:hermit]
   "wheeloffortune" [:cup]
   "strength" [:disc]
   "hangedman" [:hanged-man :rod]
   "death" [:death :sword]
   "temperance" [:temperance :cup]
   "devil" [:devil]
   "tower" [:tower :sword]
   "star" [:disc]
   "moon" [:moon :rod :sword]
   "sun" [:cup :disc :sun]
   "judgement" [:judgement]
   "world" [:world]})

(defn source-option [db source-id]
  (some #(when (= source-id (:id %)) %)
        (app-state/move-source-options db)))
