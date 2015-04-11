(ns cramble.cram-test
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [cemerick.cljs.test
                    :refer (is deftest testing done)])
  (:require [cljs.core.async :refer [put! close! chan]]
            [cemerick.cljs.test :as t]
            [cramble.core :refer (read-cram)]))

(deftest ^:async test-bad-cram 
  (go
    (is (instance? js/Error (<! (read-cram "http://www.biodalliance.org/datasets/subset22-sorted.bam"))))
    (done)))
  
  
(deftest ^:async test-connect-cram
  (go 
    (let [cram (<! (read-cram "http://www.biodalliance.org/datasets/cramtests/tiny.cram"))]
      (is (= (:name cram) "tiny.sam"))
      (println (:bam-header cram))
      (done))))
