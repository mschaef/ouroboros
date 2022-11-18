(ns ouroboros.oeval)

(defn- fail [ & args ]
  (throw (RuntimeException. (apply str args))))

(defn- envlookup [ var env ]
  (if (contains? env var)
    (env var)
    (fail "Unbound global variable: " var)))

(defn- oapply [ fun args ]
  (if (map? fun)
    (get fun (first args))
    (get (first args) fun)))

(declare oeval)

(defn- oeval-list [ form env ]
  (if (empty? form)
    form
    (let [ [ fun-pos & args ] form ]
      (cond
        (= fun-pos 'quote)
        (first args)

        :else
        (oapply (oeval fun-pos env) (map #(oeval % env) args))))))

(defn oeval [ form env ]
  (cond
    (symbol? form)
    (envlookup form env)

    (vector? form)
    (vec (map #(oeval % env) form))

    (map? form)
    (into {} (for [[k v] form]
               [(oeval k env) (oeval v env)]))

    (seq? form)
    (oeval-list form env)

    :else
    form))
