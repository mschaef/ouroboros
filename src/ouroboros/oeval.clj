(ns ouroboros.oeval)

(defn- fail [ & args ]
  (throw (.RuntimeException (str args))))

(defn oeval [ form env ]
  (if (symbol? form)
    (if (contains? env form)
      (env form)
      (fail "Unbound symbol " env))
    form))
