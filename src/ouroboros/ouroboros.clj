(ns ouroboros.ouroboros
  (:require [ouroboros.oeval :as oeval]
            [ouroboros.default-env :as env]))

(defn interp-eval
  ([form env]
   (oeval/oeval form env))

  ([ form ]
   (interp-eval form env/default)))

(defn interp-load
  ([ forms env ]
   (oeval/oload forms env))

  ([ forms ]
   (interp-load forms env/default)))
