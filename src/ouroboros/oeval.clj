(ns ouroboros.oeval)

(defn- fail [ & args ]
  (throw (RuntimeException. (apply str args))))

(defn- envlookup [ var env ]
  (if (contains? env var)
    (env var)
    (fail "Unbound global variable: " var)))

(defn- oapply [ fun args ]
  (cond
    (or (map? fun) (vector? fun))
    (get fun (first args))

    (or (symbol? fun) (keyword? fun))
    (get (first args) fun)

    (fn? fun)
    (apply fun args)

    :else
    (fail "Cannot apply: " (if (nil? fun) "nil" fun))))

(declare oeval)

(defn- oeval-if [ [ condition-clause then-clause else-clause ] env ]
  (if (oeval condition-clause env)
    (oeval then-clause env)
    (oeval else-clause env)))

(defn- oeval-do [ forms env ]
  (loop [retval nil forms forms]
    (if (empty? forms)
      retval
      (recur (oeval (first forms) env)
             (rest forms)))))

(defn- oeval-and [ forms env ]
  (loop [retval true forms forms]
    (if (or (empty? forms) (not retval))
      retval
      (recur (oeval (first forms) env)
             (rest forms)))))

(defn- oeval-list [ form env ]
  (if (empty? form)
    form
    (let [ [ fun-pos & args ] form ]
      (case fun-pos
        quote
        (first args)

        if
        (oeval-if args env)

        do
        (oeval-do args env)

        and
        (oeval-and args env)

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
