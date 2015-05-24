(ns cramble.decode
  (:require [cljs.core.async :refer [put! close! chan]]
            [cramble.bin :as b]
            [cramble.utils :refer (array-to-string)]
            [clojure.string :as str])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defprotocol CRAMDecoder
  (r-bit-flags         [_]  "bit flags (int)")                                ;; BF
  (r-align-start       [_]  "in-seq positions, zero-based (int)")             ;; AP
  (r-feature-pos       [_]  "in-read positions (int)")                        ;; FP
  (r-read-length       [_]  "read lengths (int)")                             ;; RL
  (r-deletion-length   [_]  "base-pair deletion lengths (int)")               ;; DL
  (r-next-fragment     [_]  "number of records to the next fragment (int)")   ;; NF
  (r-base              [_]  "bases (byte)")                                   ;; BA
  (r-qual-score        [_]  "quality scores (byte)")                          ;; QS
  (r-feature-code      [_]  "read feature codes (byte)")                      ;; FC
  (r-feature-count     [_]  "number of read features (int)")                  ;; FN
  (r-base-subst        [_]  "base substitution codes (byte)")                 ;; BS
  (r-insertion         [_]  "inserted bases (byte[])")                        ;; IN
  (r-read-group        [_]  "read groups, -1 means no group (int)")           ;; RG
  (r-mapping-quality   [_]  "mapping quality scores (int)")                   ;; MQ
  (r-tag-ids           [_]  "list of tag ids (int)")                          ;; TL
  (r-read-name         [_]  "read names (byte[])")                            ;; RN
  (r-next-frag-seq     [_]  "next fragment ref seq ID (int)")                 ;; NS
  (r-next-frag-pos     [_]  "alignment position for the next frament (int)")  ;; NP
  (r-template-size     [_]  "template size (int)")                            ;; TS
  (r-mate-flags        [_]  "next mate bit flags (int)")                      ;; MF
  (r-compression-flags [_]  "compression bit flags (int)")                    ;; CF
  (r-test-mark         [_]  "a prefix before each record, for debugging (int)") ;; TM
  (r-reference-id      [_]  "Reference ID from the BAM header (int)")         ;; RI
  (r-reference-skip    [_]  "Number of skipped bases for 'N' features (int)") ;; RS
  (r-padding           [_]  "Number of padded bases (int)")                   ;; PD
  (r-hard-clip         [_]  "Number of hard-clipped bases (int)")             ;; HC
  (r-soft-clip         [_]  "Soft-clipped bases (byte[])"))                   ;; SC

(defn make-decoder [dse-map core-stream alt-streams]
  (let [{:strs [BF AP FP RL DL NF BA QS FC FN
                BS IN RG MQ TL RN NS NP TS
                MF CF TM RI RS PD HC SC]}
        dse-map]
    (reify 
      CRAMDecoder
      (r-bit-flags [_]          (BF core-stream alt-streams))
      (r-align-start [_]        (AP core-stream alt-streams))
      (r-feature-pos [_]        (FP core-stream alt-streams))
      (r-read-length [_]        (RL core-stream alt-streams))
      (r-deletion-length [_]    (DL core-stream alt-streams))
      (r-next-fragment [_]      (NF core-stream alt-streams))
      (r-base [_]               (BA core-stream alt-streams))
      (r-qual-score [_]         (QS core-stream alt-streams))
      (r-feature-code [_]       (FC core-stream alt-streams))
      (r-feature-count [_]      (FN core-stream alt-streams))
      (r-base-subst [_]         (BS core-stream alt-streams))
      (r-insertion [_]          (IN core-stream alt-streams))
      (r-read-group [_]         (RG core-stream alt-streams))
      (r-mapping-quality [_]    (MQ core-stream alt-streams))
      (r-tag-ids [_]            (TL core-stream alt-streams))
      (r-read-name [_]          (RN core-stream alt-streams))
      (r-next-frag-seq [_]      (NS core-stream alt-streams))
      (r-next-frag-pos [_]      (NP core-stream alt-streams))
      (r-template-size [_]      (TS core-stream alt-streams))
      (r-mate-flags [_]         (MF core-stream alt-streams))
      (r-compression-flags [_]  (CF core-stream alt-streams))
      (r-test-mark [_]          (TM core-stream alt-streams))
      (r-reference-id [_]       (RI core-stream alt-streams))
      (r-reference-skip [_]     (RS core-stream alt-streams))
      (r-padding [_]            (PD core-stream alt-streams))
      (r-hard-clip [_]          (HC core-stream alt-streams))
      (r-soft-clip [_]          (SC core-stream alt-streams)))))
      
(defn- decode-feature [d]
  (let [code (r-feature-code d)
        pos (r-feature-pos d)
        f    {:pos pos}]
    (case code
      0x42
      (assoc f
        :base (r-base d)
        :qual (r-qual-score d))
      
      0x58
      (assoc f
        :subst (r-base-subst d))
      
      0x49
      (assoc f
        :insert (r-insertion d))
      
      0x44
      (assoc f
        :deletion (r-deletion-length d))

      0x69
      (assoc f
        :insert [(r-base d)])

      0x51
      (assoc f
        :qual (r-qual-score d))

      0x4e
      (assoc f 
        :ref-skip (r-reference-skip d))

      0x53
      (assoc f
        :soft-clip (array-to-string (clj->js (r-soft-clip d))))

      0x50
      (assoc f
        :padding (r-padding d))

      0x48
      (assoc f
        :hard-clip (r-hard-clip d))

      ;; default
      (throw (js/Error. (str "Unknown seq-feature type " code))))))

(defn decode-record [d start-pos]
  (let [cram-flags     (r-bit-flags d)
        comp-flags     (r-compression-flags d)
        ref-id         (if false
                         (r-reference-id d))
        read-len       (r-read-length d)
        align-start    (r-align-start d)
        read-group     (r-read-group d)
        qual-score     (if false
                         (r-qual-score d))
        read-name      (if false
                         (array-to-string (clj->js (r-read-name d))))
        mate           (cond
                         (not= (bit-and comp-flags 0x2) 0)
                         (let [mate-flags     (r-mate-flags d)
                               mate-name      (array-to-string (clj->js (r-read-name d)))
                               mate-ref       (r-next-frag-seq d)
                               mate-start     (r-next-frag-pos d)
                               template-size  (r-template-size d)]
                           {:mate-flags mate-flags
                            :mate-name  mate-name
                            :mate-ref   mate-ref
                            :mate-start mate-start
                            :template-size template-size})
                         
                         (not= (bit-and comp-flags 0x4) 0)
                         (r-next-fragment d))
        tag-ids        (r-tag-ids d)
        ;; TBD tag data
        num-features   (r-feature-count d)
        features       (vec (for [f (range num-features)]
                              (decode-feature d)))
        map-quality    (r-mapping-quality d)]
    {:cram-flags cram-flags
     :comp-flags comp-flags
     :ref-id ref-id
     :read-len read-len
     :align-start (+ start-pos align-start)
     :read-group read-group
     :qual-score qual-score
     :read-name  read-name
     :mate mate
     :tag-ids tag-ids
     :num-features num-features
     :map-qual map-quality
     :features features}))
