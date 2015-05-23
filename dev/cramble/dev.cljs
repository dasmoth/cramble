(ns cramble.dev
  (:require [clojure.browser.repl :as repl]
            [cramble.core :as cramble]
            [cljs.test :refer-macros [run-tests run-all-tests]]
            cramble.bits-test
            cramble.cram-test))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

