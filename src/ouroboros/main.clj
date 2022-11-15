(ns ouroboros.main
  (:gen-class :main true)
  (:require [ouroboros.logging :as logging]
            [taoensso.timbre :as log]))

(defn -main [& args]
  (logging/setup-logging {:development-mode true} [])
  (log/info "end run."))
