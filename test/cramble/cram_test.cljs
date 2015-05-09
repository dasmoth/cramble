(ns cramble.cram-test
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [cemerick.cljs.test
                    :refer (is deftest testing done)])
  (:require [cljs.core.async :refer [put! close! chan]]
            [cemerick.cljs.test :as t]
            [cramble.core :refer (read-cram read-container read-crai read-slice)]))

(deftest ^:async test-bad-cram 
  (go
    (is (instance? js/Error (<! (read-cram "http://www.biodalliance.org/datasets/subset22-sorted.bam"))))
    (done)))

(deftest ^:async test-connect-cram
  (go 
    (let [cram (<! (read-cram "http://www.biodalliance.org/datasets/cramtests/tiny.cram"))]
      (println (dissoc cram :bam-header))
      (is (= (:name cram) "tiny.sam"))
      ;; somethign with header
      (let [container (<! (read-container (:uri cram) (:c2-offset cram)))]
        (println (keys container))
        (is container)
        (println (:end (:comp container)))
        (println (keys (:comp container)))
        (println (:pres-map (:comp container)))
        (println (keys (:dse-map (:comp container))))
        (done)))))

(deftest ^:async test-connect-crai
  (go
    (let [crai (<! (read-crai "http://www.biodalliance.org/datasets/cramtests/tiny.cram.crai"))]
      (is crai)
      (println crai)
      (done))))

(deftest ^:async test-read-slice
  (go
    (let [cram (<! (read-cram "http://www.biodalliance.org/datasets/cramtests/tiny.cram"))
          crai (<! (read-crai "http://www.biodalliance.org/datasets/cramtests/tiny.cram.crai"))
          slice-header (<! (read-slice cram (first crai)))]
      (is slice-header)
      (println slice-header)
      (done))))
      
