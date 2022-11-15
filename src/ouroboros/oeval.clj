(ns ouroboros.oeval)

(defn- fail [ & args ]
  (throw (.RuntimeException (str args))))

(defn- envlookup [ var env ]
  (if (contains? env var)
    (env var)
    (fail "Unbound global variable: " var)))

(defn oeval [ form env ]
  (if (symbol? form)
    (envlookup form env)
    form))
