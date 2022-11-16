(ns ouroboros.oeval)

(defn- fail [ & args ]
  (throw (RuntimeException. (apply str args))))

(defn- envlookup [ var env ]
  (if (contains? env var)
    (env var)
    (fail "Unbound global variable: " var)))

(defn- oapply [ fun args ]
  (get fun (first args)))

(declare oeval)

(defn- oeval-list [ form env ]
  (let [ [ fun-pos & args ] form ]
    (cond
      (= fun-pos 'quote)
      (first args)

      :else
      (oapply (oeval fun-pos env) (map #(oeval % env) args)))))

(defn oeval [ form env ]
  (cond
    (symbol? form)
    (envlookup form env)

    (seq? form)
    (oeval-list form env)

    :else
    form))
