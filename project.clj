(defproject metace "0.1.0-SNAPSHOT"
  :description "meta circular evaluator in clojure"
  :url "https://github.com/zjhmale/metace"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot metace.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
