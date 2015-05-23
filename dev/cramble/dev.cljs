(ns cramble.dev
  (:require [clojure.browser.repl :as repl]
            [cramble.core :as cramble]))

(defonce conn
  (repl/connect "http://localhost:9000/repl"))

