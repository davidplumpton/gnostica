(ns react
  (:require [goog.object :as gobj]))

(def createElement (gobj/get js/React "createElement"))
(def Fragment (gobj/get js/React "Fragment"))
(def Children (gobj/get js/React "Children"))
(def Component (gobj/get js/React "Component"))
(def createRef (gobj/get js/React "createRef"))
(def useEffect (gobj/get js/React "useEffect"))
(def useRef (gobj/get js/React "useRef"))
(def useState (gobj/get js/React "useState"))
(def memo (gobj/get js/React "memo"))
