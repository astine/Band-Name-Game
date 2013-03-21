(defproject band-names "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [roul "0.2.0"]
                 [enlive "1.1.1"]
                 [incanter "1.4.1"]
                 [compojure "1.1.5"]
                 [inflections "0.8.0"]]
  :plugins [[lein-ring "0.8.2"]]
  :ring {:handler band-names.server/app}
  :profiles {:dev {:dependencies [[ring-mock "0.1.3"]]}})
