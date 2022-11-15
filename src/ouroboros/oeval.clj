(ns ouroboros.oeval)

(defn- fail [ & args ]
  (throw (RuntimeException. (apply str args))))

(defn- envlookup [ var env ]
  (if (contains? env var)
    (env var)
    (fail "Unbound global variable: " var)))

(defn oeval [ form env ]
  (cond
    (symbol? form)
    (envlookup form env)

    (seq? form)
    (let [ [ fun-pos arg ] form ]
      (if (= fun-pos 'quote)
        arg
        (fail "Invalid function call: " form)))

    :else
    form))
