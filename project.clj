(defproject lemmalearnerclj "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.1.0"]
                 [org.clojure/data.priority-map "1.2.0"]]
  :plugins [[lein-cloverage "1.2.2"] [lein-try "0.4.3"]]
  :main ^:skip-aot lemmalearnerclj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
