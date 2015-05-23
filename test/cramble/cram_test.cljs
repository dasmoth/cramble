(ns cramble.cram-test
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [put! close! chan]]
            [cljs.test :refer-macros (is deftest testing async)]
            [cramble.core :refer (read-cram read-container read-crai read-slice)]))

(deftest test-bad-cram 
 (async done
  (go
    (is (instance? js/Error (<! (read-cram "http://www.biodalliance.org/datasets/subset22-sorted.bam"))))
    (done))))

(deftest test-connect-cram
 (async done
  (go 
    (let [cram (<! (read-cram "http://www.biodalliance.org/datasets/cramtests/tiny.cram"))]
      (println (dissoc cram :bam-header))
      (is (= (:name cram) "tiny.sam"))
      ;; somethign with header
      (let [container (<! (read-container (:uri cram) (:c2-offset cram)))]
        (is container)
        (done))))))

(deftest test-connect-crai
 (async done
  (go
    (let [crai (<! (read-crai "http://www.biodalliance.org/datasets/cramtests/tiny.cram.crai"))]
      (is crai)
      (println crai)
      (done)))))

(deftest test-read-slice
 (async done
  (go
    (let [cram (<! (read-cram "http://www.biodalliance.org/datasets/cramtests/tiny.cram"))
          crai (<! (read-crai "http://www.biodalliance.org/datasets/cramtests/tiny.cram.crai"))
          slice-header (<! (read-slice cram (first crai)))]
      (is slice-header)
      (println slice-header)
      (done)))))
      
