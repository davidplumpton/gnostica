(ns gnostica.app
  (:require [gnostica.app.events :as events]
            [gnostica.fixtures :as fixtures]
            [gnostica.ui.board :as board-ui]
            [gnostica.ui.card-zones :as card-zones-ui]
            [gnostica.ui.header :as header-ui]
            [gnostica.ui.help :as help-ui]
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
  (let [{:keys [setup-error card-icon-mode]} @(rf/subscribe [events/app-view])]
    [:<>
     [header-ui/app-header]
     (if setup-error
       [setup-error-panel setup-error]
       [:main.app-shell
        {:data-card-icon-mode (name card-icon-mode)}
        [:div.play-stack
         [board-ui/board-stage]
         [card-zones-ui/card-zones]]
        [:div.side-stack
         [move-panel-ui/move-panel]
         [territory-ui/territory-panel]]])
     [help-ui/help-dialogs]]))

(defn mount! []
  (rf/dispatch-sync [events/install-keyboard-shortcuts])
  (rdom/render [app] (.getElementById js/document "app")))

(defn reload! []
  (mount!))

(defn init []
  (rf/dispatch-sync [events/initialize (fixtures/browser-init-options)])
  (mount!))
