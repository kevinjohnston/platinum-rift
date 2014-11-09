(defproject platinum-rift "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [cider/cider-nrepl "0.7.0"]
                 [instaparse "1.3.4"]
                 [criterium "0.4.3"]]
  :main ^:skip-aot platinum-rift.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
