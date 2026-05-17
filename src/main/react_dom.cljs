(ns react-dom
  (:require [goog.object :as gobj]))

(def findDOMNode (gobj/get js/ReactDOM "findDOMNode"))
(def render (gobj/get js/ReactDOM "render"))
(def unmountComponentAtNode (gobj/get js/ReactDOM "unmountComponentAtNode"))
