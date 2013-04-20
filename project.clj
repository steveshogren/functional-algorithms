(defproject resume-timeline "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [hiccup "1.0.2"]
                 [criterium "0.3.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [compojure "1.1.3"]]
  :plugins [[lein-ring "0.7.5"]]
  :ring {:handler resume-timeline.handler/app}
  :profiles
  {:dev {:dependencies [[ring-mock "0.1.3"]]}})
