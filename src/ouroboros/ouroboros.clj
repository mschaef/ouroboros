(ns ouroboros.ouroboros
  (:use ouroboros.util)
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

(defn interp-load-string
  ([ string env ]
   (oeval/oload (string-forms string) env))

  ([ string ]
   (interp-load-string string env/default)))
