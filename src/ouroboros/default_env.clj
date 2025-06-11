(ns ouroboros.default-env
  (:use ouroboros.util
        ouroboros.oeval))

(def default
  (oload (string-forms (slurp (clojure.java.io/resource "bootstrap.clj")))
         (merge
          (oimport-syms
           * + - / < <= = > >= assoc assoc-in bigdec bigint boolean boolean?
           butlast byte bytes char char-escape-string char-name-string conj
           constantly count counted? cycle disj double? drop empty? even?
           false? ffirst first flatten float float? format get get-in hash
           inc int int? into key keys last list list* list? long longs map?
           max merge merge-with min mod nat-int? neg-int? neg? next nfirst
           nil? not not-any? not-empty not-every? nth num number? numerator
           odd? partition partition-all pos-int? pos? quot rand rand-int
           rand-nth random-sample range ratio? rational? rationalize re-find
           re-groups re-matcher re-pattern re-seq rem remove rest reverse
           rseq rsubseq second seq seq seq seq? shuffle split-at str string?
           take take-nth true? update update-in vec zero?)

          ;; These are necessary for the Clojure reader's quasiquote
          ;; syntax to work properly.
          (assoc
           (oimport-syms clojure.core/seq clojure.core/concat clojure.core/list clojure.core/vector)
           ;; apply is handled differently because we do not want to
           ;; escape the interpreter by using the official Clojure
           ;; apply
           'clojure.core/apply apply))))
