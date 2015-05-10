(ns cramble.utils
  (:require [cramble.bin :as b]))

(def fromCharCode (.-fromCharCode js/String))

(defn array-to-string 
  "Convert the bytes in `ba` to a String."
  [ba]
  (.apply fromCharCode nil ba))

(defn read-nstr 
  "Read `n` bytes from `stream` as a String."
  [n stream]
  (.apply fromCharCode nil (clj->js (for [i (range n)] (b/read-byte stream)))))
