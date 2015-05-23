(defproject cramble "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.7.0-RC1"]
                 [org.clojure/clojurescript "0.0-3269"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [compojure "1.3.1"]]

  :plugins [[lein-cljsbuild "1.0.5"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src" "test"]
              :compiler {
                :output-to "target/cljs/testable.js"
                :source-map "target/cljs/testable.js.map"
                :externs ["jszlib-externs.js"]
                :foreign-libs [{:file "jszlib/js/inflate.js"
                                :provides ["jszlib"]}]}}]})
              
