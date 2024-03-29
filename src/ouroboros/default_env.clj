(ns ouroboros.default-env
  (:use ouroboros.oeval))

(def interpreter-definitions
  '(
    (def* defmacro
      (macro* (fn[ name formals & code ]
                (list 'def* name (list 'macro* (list* 'fn formals code))))))

    (defmacro defn [ name formals & code ]
      (list 'def* name (list* 'fn formals code)))))

(def default
  (oload interpreter-definitions
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
          take take-nth true? update update-in vec zero?)))

