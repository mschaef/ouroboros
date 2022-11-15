(ns ouroboros.oeval)

(defn oeval [ form env ]
  (if (symbol? form)
    (env form)
    form))
