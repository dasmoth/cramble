(ns cramble.core
  (:require [cljs.core.async :refer [put! close! chan]]
            [cramble.fetch :refer (get-binary-range)]
            [cramble.bin :as b]
            [clojure.string :as str])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn- parse-block [stream]
  (let [method     (b/read-byte stream)
        type       (b/read-byte stream)
        content-id (b/read-itf8 stream)
        size       (b/read-itf8 stream)
        rawsize    (b/read-itf8 stream)]
    {:method      method
     :type        type
     :content-id  content-id
     :size        size
     :raw-size    rawsize}))

(defn- parse-container [blk]
  (let [length        (b/read-int blk)
        ref-id        (b/read-itf8 blk)
        start         (b/read-itf8 blk)
        span          (b/read-itf8 blk)
        num-records   (b/read-itf8 blk)
        rcd-counter   (b/read-itf8 blk)
        read-bases    (b/read-itf8 blk)
        num-blks      (b/read-itf8 blk)
        landmarks     (b/read-array b/read-itf8 blk)]
    {:ref-id      ref-id
     :start       start
     :span        span
     :num-records num-records
     :rcd-counter rcd-counter
     :read-bases  read-bases
     :num-blks    num-blks
     :landmarks   landmarks
     :length      length}))

(def fromCharCode (.-fromCharCode js/String))

(defn array-to-string [ba]
  (.apply fromCharCode nil ba))

(defn read-cram
  "Read headers from a CRAM file at uri.  Returns a promise channel which will
   receive an object which can be used for further querying the CRAM."
  [uri]
  (go
    (let [header (->> (get-binary-range uri 0 26)
                      (<!)
                      (b/make-binary-stream))
          magic (b/read-int header)
          major (b/read-byte header)
          minor (b/read-byte header)
          name (str/join (map #(.fromCharCode js/String %) 
                              (filter (complement zero?) (map #(b/read-byte header) (range 20)))))]
      (cond 
       (not= magic 0x4d415243)
       (js/Error. (str "Not a CRAM file.  Magic=" (.toString magic 16)))

       (not= major 2)
       (js/Error. (str "Unsupported CRAM version" major))

       :default
       (let [c1-start  26
             c1-stream  (->> (get-binary-range uri c1-start (+ c1-start 100))
                             (<!)
                             (b/make-binary-stream))
             c1-head    (parse-container c1-stream)
             b1-head    (parse-block c1-stream)
             b1-start   (+ c1-start (b/tell c1-stream))
             bam-header (->> (get-binary-range uri (+ b1-start 4) (+ b1-start (:size b1-head)))
                             (<!)
                             (js/Uint8Array.)
                             (array-to-string))]
         {:major major
          :minor minor
          :name name
          :bam-header bam-header
          :c2-offset (+ (:blocks-start c1-head) (:length c1-head))})))))
    
    
    
