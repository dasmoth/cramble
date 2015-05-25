(ns cramble.cram-test
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [put! close! chan]]
            [cljs.test :refer-macros (is deftest testing async run-tests)]
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
          slice-data (<! (read-slice cram (first crai)))]
      (is slice-data)
      #_(doseq [r (take 10 (drop 80 slice-data))]
        (println r)
        (println))
      (done)))))
      
(deftest test-read-large
 (async done
  (go
    (let [cram (<! (read-cram "http://www.biodalliance.org/datasets/cramtests/adipose.cram"))
          crai (<! (read-crai "http://www.biodalliance.org/datasets/cramtests/adipose.cram.crai"))
          slice-data (<! (read-slice cram (first crai)))]
      (is slice-data)
      (println "got" (count slice-data) "reads")
      (doseq [r (take 10 slice-data)]
        (println r)
        (println))
      (done)))))


(defn run []
  (run-tests 'cramble.cram-test))

#_(enable-console-print!)
#_(doseq [x (range 10)]
  (println x)
  (run))
