(ns cramble.core
  (:require [cljs.core.async :refer [put! close! chan]]
            [cramble.fetch :refer (get-binary-range)]
            [cramble.bin :as b]
            [clojure.string :as str]
            [cramble.encodings :as enc]
            [clojure.browser.repl :as repl]
            [jszlib :as zlib])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def fromCharCode (.-fromCharCode js/String))

(defn array-to-string [ba]
  (.apply fromCharCode nil ba))

(defn read-nstr [n stream]
  (.apply fromCharCode nil (clj->js (for [i (range n)] (b/read-byte stream)))))

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

(defn- read-map [stream key-reader fmtmap]
  (let [size (b/read-itf8 stream)
        n    (b/read-itf8 stream)]
    (loop [cnt    0
           result {}]
      (if (>= cnt n)
        result
        (let [key (key-reader stream)
              fmt (fmtmap key)]
          (println key)
          (if fmt
            (recur 
             (inc cnt)
             (assoc result key (fmt stream)))
            (do
              (println "Unknown key" key)
              result)))))))

(def ^:private pres-map-format
  {"RN" b/read-bool
   "AP" b/read-bool
   "RR" b/read-bool
   "TD" (partial b/read-array b/read-byte)    ;; needs more?
   "SM" (fn [stream]
          (vec (for [i (range 5)] (b/read-byte stream))))})

(def ^:private dse-map-format*
  {"BF" enc/read-int-encoding
   "AP" enc/read-int-encoding
   "FP" enc/read-int-encoding
   "RL" enc/read-int-encoding
   "DL" enc/read-int-encoding
   "NF" enc/read-int-encoding
   "BA" enc/read-byte-encoding
   "QS" enc/read-byte-encoding
   "FC" enc/read-byte-encoding
   "FN" enc/read-int-encoding
   "BS" enc/read-byte-encoding
   "IN" enc/read-byte-array-encoding
   "RG" enc/read-int-encoding
   "MQ" enc/read-int-encoding
   "TL" enc/read-int-encoding
   "RN" enc/read-byte-array-encoding
   "NS" enc/read-int-encoding
   "NP" enc/read-int-encoding
   "TS" enc/read-int-encoding
   "MF" enc/read-int-encoding
   "CF" enc/read-int-encoding
   "TM" enc/read-int-encoding
   "RI" enc/read-int-encoding
   "RS" enc/read-int-encoding
   "PD" enc/read-int-encoding
   "HC" enc/read-int-encoding
   "SC" enc/read-byte-array-encoding})

(defn- dse-map-format [key]
  (or (dse-map-format* key)
      (fn [stream]
        (let [e (b/read-byte stream)
              param-len (b/read-itf8 stream)]
          (b/skip-bytes stream param-len)
          {:type :unknown :e e}))))

(defn parse-comp-header [data]
  (let [data     (b/make-binary-stream data)
        pres-map (read-map data (partial read-nstr 2) pres-map-format)
        dse-map  (read-map data (partial read-nstr 2) dse-map-format)
        tag-map  (read-map 
                    data
                    (fn [stream]
                      (let [k (b/read-itf8 stream)]
                        (.fromCharArray 
                           js/String
                           (bit-and (bit-shift-right k 16) 0xff)
                           (bit-and (bit-shift-right k 8) 0xff)
                           (bit-and k 0xff))))
                    (constantly enc/read-byte-array-encoding))]
    {:pres-map pres-map
     :dse-map  dse-map
     :tag-map  tag-map}))


(defn read-container
  [uri offset]
  (println "In read-container" uri offset)
  (go
    (let [ch-data     (->> (get-binary-range uri offset (+ offset 1000))
                           (<!)
                           (b/make-binary-stream))
          header      (parse-container ch-data)
          b1-head     (parse-block ch-data)
          b1-start    (+ offset (b/tell ch-data))
          comp-header (->> (get-binary-range uri b1-start (+ b1-start (:size b1-head)))
                           (<!)
                           (parse-comp-header))]
      (assoc header :comp comp-header))))

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
             c2-start   (+ c1-start (b/tell c1-stream) (:length c1-head))
             b1-head    (parse-block c1-stream)
             b1-start   (+ c1-start (b/tell c1-stream))
             bam-header (->> (get-binary-range uri (+ b1-start 4) (+ b1-start (:size b1-head)))
                             (<!)
                             (js/Uint8Array.)
                             (array-to-string))]
         {:uri uri
          :major major
          :minor minor
          :name name
          :bam-header bam-header
          :c2-offset c2-start})))))
  
(defrecord CRAIRecord 
    [seq-id 
     ali-start 
     ali-span 
     container-start 
     slice-start
     slice-len])
  
(defn read-crai
  [uri]
  (go
    (->>
     (-> (get-binary-range uri)
         (<!)
         (js/jszlib_inflate_buffer 10)
         (js/Uint8Array.)
         (array-to-string)
         (str/split #"\n"))
     (map
      (fn [line]
        (let [[seq-id ali-start ali-span container-start slice-start slice-len]
              (str/split line #"\t")]
          (CRAIRecord. 
           (js/parseInt seq-id)
           (js/parseInt ali-start)
           (js/parseInt ali-span)
           (js/parseInt container-start)
           (js/parseInt slice-start)
           (js/parseInt slice-len))))))))
