(defproject hexer "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://github.com/TimothyStiles/hexer"
  :license {:name "Apache Software License"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [thi.ng/geom "0.0.908"]
                 [thi.ng/math "0.1.4"]]
  :main ^:skip-aot hexer.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
