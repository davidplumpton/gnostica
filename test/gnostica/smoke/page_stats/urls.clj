(ns gnostica.smoke.page-stats.urls
  (:require [clojure.string :as str]
            [gnostica.fixtures :as fixtures]))

(defn major-icons-smoke-url [url]
  (str url
       (if (str/includes? url "?") "&" "?")
       fixtures/smoke-query-param
       "="
       fixtures/smoke-major-icons-mode))
(defn direct-drop-smoke-url [url]
  (str url
       (if (str/includes? url "?") "&" "?")
       fixtures/smoke-query-param
       "="
       fixtures/smoke-direct-drop-mode))
