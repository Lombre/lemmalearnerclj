(defproject lemmalearnerclj "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.1.0"]
                 [org.clojure/data.priority-map "1.2.0"]
                 [org.clojure/data.json "2.5.0"]
                 [com.clojure-goes-fast/clj-async-profiler "1.2.0"]
                 [parallel "0.10"]
                 [metosin/jsonista "0.3.8"]
                 [philoskim/debux "0.9.1"]
                 [org.clojure/data.priority-map "1.2.0"]]
  :plugins [[lein-cloverage "1.2.2"] [lein-try "0.4.3"]]
  :jvm-opts ["-Djdk.attach.allowAttachSelf"] ; To enable profiling
  :java-source-paths ["src/java" "test/java"]
  :main ^:skip-aot lemmalearnerclj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Djdk.attach.allowAttachSelf"]}})
