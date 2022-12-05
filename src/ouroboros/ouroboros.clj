(ns ouroboros.ouroboros
  (:require [ouroboros.oeval :as oeval]
            [ouroboros.default-env :as env]))

(defn interp-eval [ form ]
  (oeval/oeval form env/default))

