(ns gnostica.game-state.command-contracts.primitives
  (:require [clojure.string :as str]
            [gnostica.pieces :as pieces]))

(defn enum-schema [values]
  (into [:enum] values))

(defn closed-map [& entries]
  (into [:map {:closed true}] entries))

(defn contains-any? [command ks]
  (boolean (some #(contains? command %) ks)))

(defn- nonblank-string? [value]
  (and (string? value)
       (not (str/blank? value))))

(defn- non-negative-int? [value]
  (and (int? value)
       (not (neg? value))))

(defn- positive-int? [value]
  (and (int? value)
       (pos? value)))

(def NonBlankString
  [:fn {:error/message "must be a nonblank string"} nonblank-string?])

(def NonNegativeInt
  [:fn {:error/message "must be a non-negative integer"} non-negative-int?])

(def PositiveInt
  [:fn {:error/message "must be a positive integer"} positive-int?])

(def PlayerId :keyword)

(def PieceId :keyword)

(def CardId NonBlankString)

(def BoardIndex NonNegativeInt)

(def Orientation
  (enum-schema pieces/legal-orientations))

(def ShuffleFn
  [:fn {:error/message "must be callable"} ifn?])
