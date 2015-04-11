(ns cramble.bits-test
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)])
  (:require [cemerick.cljs.test :as t]
            [cramble.bits :refer (read-bits make-bit-stream)]))

(deftest bit-stream
  (let [data (js/Uint8Array. #js [0xb0 0x70])
        bits (make-bit-stream (.-buffer data))]
    (is (= (read-bits bits 1) 1))
    (is (= (read-bits bits 1) 0))
    (is (= (read-bits bits 2) 3))
    (is (= (read-bits bits 8) 7))))
