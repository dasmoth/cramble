(ns cramble.decode
  (:require [cljs.core.async :refer [put! close! chan]]
            [cramble.bin :as b]
            [clojure.string :as str])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defprotocol CRAMDecoder
  (r-bit-flags         [_]  "bit flags (int)")                                ;; BF
  (r-align-start       [_]  "in-seq positions, zero-based (int)")             ;; AP
  (r-feature-pos       [_]  "in-read positions (int)")                        ;; FP
  (r-read-length       [_]  "read lengths (int)")                             ;; RL
  (r-deletion-length   [_]  "base-pair deletion lengths (int)")               ;; DL
  (r-next-fragment     [_]  "number of records to the next fragment (int)")   ;; NF
  (r-bases             [_]  "bases (byte)")                                   ;; BA
  (r-qual-scores       [_]  "quality scores (byte)")                          ;; QS
  (r-feature-code      [_]  "read feature codes (byte)")                      ;; FC
  (r-feature-count     [_]  "number of read features (int)")                  ;; FN
  (r-base-subst        [_]  "base substitution codes (byte)")                 ;; BS
  (r-insertion         [_]  "inserted bases (byte[])")                        ;; IN
  (r-read-groups       [_]  "read groups, -1 means no group (int)")           ;; RG
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
      (r-bases [_]              (BA core-stream alt-streams))
      (r-qual-scores [_]        (QS core-stream alt-streams))
      (r-feature-code [_]       (FC core-stream alt-streams))
      (r-feature-count [_]      (FN core-stream alt-streams))
      (r-base-subst [_]         (BS core-stream alt-streams))
      (r-insertion [_]          (IN core-stream alt-streams))
      (r-read-groups [_]        (RG core-stream alt-streams))
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
      
