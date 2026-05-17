(ns gnostica.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.not-modified :refer [wrap-not-modified]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.util.response :as response])
  (:gen-class))

(defn- index-response []
  (-> (response/resource-response "index.html")
      (response/content-type "text/html; charset=utf-8")))

(defn- browser-route? [{:keys [request-method uri]}]
  (and (= :get request-method)
       (or (= "/" uri)
           (not (re-find #"\.[^/]+$" uri)))))

(defn- routes [request]
  (when (browser-route? request)
    (index-response)))

(defn- wrap-not-found [handler]
  (fn [request]
    (or (handler request)
        {:status 404
         :headers {"content-type" "text/plain; charset=utf-8"}
         :body "Not found"})))

(def app
  (-> routes
      (wrap-resource "")
      wrap-content-type
      wrap-not-modified
      wrap-not-found))

(defn- port []
  (try
    (Long/parseLong (or (System/getenv "PORT") "8080"))
    (catch NumberFormatException _
      8080)))

(defn -main [& _]
  (let [port (port)]
    (println (str "Serving Gnostica on http://localhost:" port))
    (jetty/run-jetty #'app {:port port :join? true})))

