(ns ouroboros.util
  (:require [clojure.edn :as edn]))

(defn- string-reader [string]
  (java.io.PushbackReader. (java.io.StringReader. string)))

(def form-seq
  (let [eof-sentinal (gensym "end-of-file")]
    (fn [stream]
      (let [val (binding [*read-eval* false]
                  (read {:eof eof-sentinal} stream))]
        (if (= val eof-sentinal)
          nil
          (cons val (lazy-seq (form-seq stream))))))))

(defn string-forms [string]
  (form-seq (string-reader string)))
