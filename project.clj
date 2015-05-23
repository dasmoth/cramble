(defproject cramble "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.7.0-RC1"]
                 [org.clojure/clojurescript "0.0-3269"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [ring "1.3.2"]
                 [ring/ring-defaults "0.1.3"]
                 [compojure "1.3.1"]
                 [com.cemerick/piggieback "0.1.5"]
                 [weasel "0.6.0"]]

  :plugins [[lein-cljsbuild "1.0.5"]
            [com.cemerick/clojurescript.test "0.3.3"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src" "test"]
              :compiler {
                :output-to "target/cljs/testable.js"
                :source-map "target/cljs/testable.js.map"
                :externs ["jszlib-externs.js"]
                :foreign-libs [{:file "jszlib/js/inflate.js"
                                :provides ["jszlib"]}]}}]
     :test-commands {"unit-tests" ["slimerjs" :runner 
                                   "target/cljs/testable.js"]}})
              
