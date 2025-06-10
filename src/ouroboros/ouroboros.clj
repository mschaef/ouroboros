(ns ouroboros.ouroboros
  (:require [clojure.edn :as edn]
            [ouroboros.oeval :as oeval]
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

(defn- string-reader [string]
  (java.io.PushbackReader. (java.io.StringReader. string)))

(def edn-seq
  (let [eof-sentinal (gensym "end-of-file")]
    (fn [stream]
      (let [val (edn/read {:eof eof-sentinal} stream)]
        (if (= val eof-sentinal)
          nil
          (cons val (lazy-seq (edn-seq stream))))))))

(defn- string-forms [string]
  (edn-seq (string-reader string)))

(defn interp-load-string
  ([ string env ]
   (oeval/oload (string-forms string) env))

  ([ string ]
   (interp-load-string string env/default)))
