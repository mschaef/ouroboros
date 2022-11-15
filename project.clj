(defproject ouroboros "0.0.1-SNAPSHOT"
  :description "A subset of Clojure implemented in Clojure"
  :license { :name "Copyright East Coast Toolworks (c) 2022" }

  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/tools.logging "1.2.3"]
                 [com.taoensso/timbre "5.2.1"]]

  :main ouroboros.main
  :aot [ouroboros.main]

  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "--no-sign"]
                  ["tar"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]])
