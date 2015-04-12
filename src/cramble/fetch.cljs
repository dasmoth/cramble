(ns cramble.fetch
  (:require [cljs.core.async :refer [put! close! chan]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn get-binary-range 
  "Fetch `uri` from `min` to `max`, passing the result as an ArrayBuffer to `chan`, or
   to a newly-created channel if not specified.  Returns the channel."
  ([uri]
     (get-binary-range uri nil nil))
  ([uri min max]
     (get-binary-range uri min max (chan 1)))
  ([uri min max chan]
     (let [xhr (js/XMLHttpRequest.)]
       (.open xhr "GET" uri)
       (if (and min max)
         (.setRequestHeader xhr "Range" (str "bytes=" min "-" max)))
       (set! (.-responseType xhr) "arraybuffer")
       (.addEventListener
        xhr
        "load"
        (fn []
          (println "onload" (.-status xhr))
          (let [r (.-response xhr)]
            (put! chan r)
            (close! chan))))
       (.send xhr)
       chan)))
