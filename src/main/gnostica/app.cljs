(ns gnostica.app
  (:require [gnostica.app.events :as events]
            [gnostica.fixtures :as fixtures]
            [gnostica.ui.board :as board-ui]
            [gnostica.ui.card-zones :as card-zones-ui]
            [gnostica.ui.header :as header-ui]
            [gnostica.ui.help :as help-ui]
            [gnostica.ui.lobby :as lobby-ui]
            [gnostica.ui.move-panel :as move-panel-ui]
            [gnostica.ui.territory :as territory-ui]
            [re-frame.core :as rf]
            [reagent.dom :as rdom]))

(defn setup-error-panel [error]
  [:main.app-shell.is-setup-error
   [:section.setup-error
    {:role "alert"}
    [:p.eyebrow "Setup error"]
    [:h1 "Game setup failed"]
    [:p.setup-error__message (:message error)]
    (when (seq (:data error))
      [:pre.setup-error__data (pr-str (:data error))])]])

(defn app []
  (let [{:keys [setup-error card-icon-mode open-panels lobby?]} @(rf/subscribe [events/app-view])
        panel-open? #(contains? open-panels %)]
    [:<>
     [header-ui/app-header]
     (if setup-error
       [setup-error-panel setup-error]
       (if lobby?
         [lobby-ui/local-lobby]
         [:main.app-shell
          {:data-card-icon-mode (name card-icon-mode)}
          [:div.play-stack
           [board-ui/board-stage]]
          (when (panel-open? :cards)
            [card-zones-ui/card-zones])
          (when (or (panel-open? :move)
                    (panel-open? :territory))
            [:div.side-stack
             (when (panel-open? :move)
               [move-panel-ui/move-panel])
             (when (panel-open? :territory)
               [territory-ui/territory-panel])])]))
     [help-ui/help-dialogs]]))

(defn mount! []
  (rf/dispatch-sync [events/install-keyboard-shortcuts])
  (rdom/render [app] (.getElementById js/document "app")))

(defn reload! []
  (mount!))

(defn init []
  (rf/dispatch-sync [events/initialize (fixtures/browser-init-options)])
  (mount!))
