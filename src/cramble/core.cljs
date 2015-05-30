(ns cramble.core
  (:require [cljs.core.async :refer [put! close! chan]]
            [cramble.fetch :refer (get-binary-range)]
            [cramble.bin :as b]
            [cramble.bits :as bits]
            [cramble.utils :refer (array-to-string read-nstr)]
            [clojure.string :as str]
            [cramble.encodings :as enc]
            [cramble.decode :as dec]
            [clojure.browser.repl :as repl]
            [jszlib :as zlib])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(defrecord CRAIRecord 
    [seq-id 
     ali-start 
     ali-span 
     container-start 
     slice-start
     slice-len])

(defn- parse-block [stream]
  (let [method     (b/read-byte stream)
        type       (b/read-byte stream)
        content-id (b/read-itf8 stream)
        size       (b/read-itf8 stream)
        rawsize    (b/read-itf8 stream)
        offset     (b/tell stream)]
    {:method      method
     :type        type
     :content-id  content-id
     :size        size
     :raw-size    rawsize
     :offset      offset}))

(defn- block-content [data offset header & {:keys [stream] :or {stream :bytes}}]
  (let [[data offset] (case (:method header)
                        0
                        [data (:offset header)]

                        1
                        [(js/jszlib_inflate_buffer data (+ (:offset header) 10)) 0]
                        
                        2
                        (throw (js/Error. "Currently don't support BZIP2"))
                        
                        ;; default
                        (throw (js/Error. (str "Unknown compression method " (:method header)))))]
    (case stream
      :bytes
      (b/make-binary-stream data offset)

      :bits
      (bits/make-bit-stream data offset)

      (throw (js/Error. (str "Unknown stream type " stream))))))

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
   "TD" (fn [stream]
          (loop [td      (b/read-array b/read-byte stream)
                 tagsets []]
            (println td)
            (if (seq td)
              (let [[tags tail] (split-with (complement zero?) td)]
                (recur
                 (rest tail)
                 (conj tagsets
                   (map #(array-to-string (clj->js %)) (partition 3 tags)))))
              tagsets)))
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
   "CF" enc/read-byte-encoding
   "TM" enc/read-int-encoding
   "RI" enc/read-int-encoding
   "RS" enc/read-int-encoding
   "PD" enc/read-int-encoding
   "HC" enc/read-int-encoding
   "SC" enc/read-byte-array-encoding
   "TC" enc/read-byte-encoding
   "TN" enc/read-int-encoding})


(defn- dse-map-format [key]
  (or (dse-map-format* key)
      (fn [stream]
        (let [e (b/read-byte stream)
              param-len (b/read-itf8 stream)]
          (b/skip-bytes stream param-len)
          (println {:key key :type :unknown :e e})))))

(defn parse-comp-header [data]
  (let [data     (b/make-binary-stream data)
        pres-map (read-map data (partial read-nstr 2) pres-map-format)
        dse-map  (read-map data (partial read-nstr 2) dse-map-format)
        tag-map  (read-map 
                    data
                    (fn [stream]
                      (let [k (b/read-itf8 stream)]
                        (.fromCharCode
                           js/String
                           (bit-and (bit-shift-right k 16) 0xff)
                           (bit-and (bit-shift-right k 8) 0xff)
                           (bit-and k 0xff))))
                    (constantly enc/read-byte-array-encoding))
        offset (b/tell data)]
    (println "PresMap" pres-map)
    (println "TagMap" (keys tag-map))
    {:pres-map pres-map
     :dse-map  dse-map
     :tag-map  tag-map}))


(defn read-container
  [uri offset]
  (go
    (let [ch-data     (->> (get-binary-range uri offset (+ offset 1000))
                           (<!)
                           (b/make-binary-stream))
          header      (parse-container ch-data)
          content-start (b/tell ch-data)
          b1-head     (parse-block ch-data)
          b1-start    (+ offset (b/tell ch-data))
          comp-header (->> (get-binary-range uri b1-start (+ b1-start (:size b1-head)))
                           (<!)
                           (parse-comp-header))]
      (assoc header :comp comp-header
                    :content-start content-start))))

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


(defn- parse-slice-header [data]
  (let [ref-id        (b/read-itf8 data)
        align-start   (b/read-itf8 data)
        align-span    (b/read-itf8 data)
        num-records   (b/read-itf8 data)
        rcd-counter   (b/read-itf8 data)
        num-blocks    (b/read-itf8 data)
        block-ids     (b/read-array b/read-itf8 data)
        embed-ref     (b/read-itf8 data)
        ref-md5       (str/join
                       (for [i (range 16)]
                         (.toString (b/read-byte data) 16)))]
    {:ref-id      ref-id
     :align-start align-start
     :align-span  align-span
     :num-records num-records
     :rcd-counter rcd-counter
     :num-blocks  num-blocks
     :block-ids   block-ids
     :embed-ref   embed-ref
     :ref-md5     ref-md5}))


(defn read-slice [cram slice]
  (go
    (let [{:keys [uri]} cram
          {:keys [container-start slice-start slice-len]} slice
          cont (<! (read-container uri container-start)) ;; should probably cache the last few?
          slice-start (+ container-start (:content-start cont) slice-start)
          data (<! (get-binary-range uri 
                                     slice-start 
                                     (+ slice-start slice-len)))
          slice-block (parse-block (b/make-binary-stream data))
          slice-header-data (block-content data 0 slice-block)
          slice-header (parse-slice-header slice-header-data)

          core-block-start (+ (:offset slice-block) (:size slice-block))
          core-block (parse-block (b/make-binary-stream data core-block-start))
          core-data (block-content data core-block-start core-block :stream :bits)

          alt-data (loop [[a & rest] (:block-ids slice-header)   ;; CHECK extra block in here somewhere?
                          alts {}
                          offset (+ (:offset core-block) (:size core-block))]
                     (if a
                       (let [block (parse-block (b/make-binary-stream data offset))
                             data (block-content data offset block)]
                         (recur rest 
                                (assoc alts (:content-id block) data)
                                (+ (:offset block) (:size block))))
                       alts))
          decoder (dec/make-decoder 
                   (:dse-map (:comp cont)) 
                   (:tag-map (:comp cont))
                   (:pres-map (:comp cont))
                   core-data 
                   alt-data)]
      (println slice-header)
      (try
        (loop [cnt  (:num-records slice-header)
               pos  (:align-start slice-header)
               rcds []]
          (if (> cnt 0)
            (let [r (dec/decode-record decoder pos)]
              (recur 
               (dec cnt) 
               (aget r "align_start")
               ;(.-align_start r)  ;; Do we need to do something special if `r` is unmapped?
               (conj rcds r)))
            rcds))
        (catch js/Error e
          (println "error" (.-stack e)))))))
      
(defn read-region [cram crai req-seq-id start end]
  (let [slices (filter (fn [{:keys [seq-id ali-start ali-span]}]
                         (and (= seq-id req-seq-id)
                              (<= ali-start end)
                              (>= (+ ali-start ali-span) start)))
                       crai)]
    (println "Fetching " slices)
    (go-loop [[slice & slices] slices
              results []]
      (if slice
        (recur slices
               (concat 
                results
                (filter (fn [read]
                          (let [ali-start (aget read "align_start")
                                ali-end (+ ali-start (aget read "read_len"))]
                            (and (<= ali-start end)
                                 (>= ali-end start))))
                        (<! (read-slice cram slice)))))
        results))))
