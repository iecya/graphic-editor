(defproject graphic-editor "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [prismatic/schema "1.1.3"]]
  :main ^:skip-aot graphic-editor.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
