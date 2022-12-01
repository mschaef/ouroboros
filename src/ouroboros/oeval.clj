(ns ouroboros.oeval)

(defn- fail [ & args ]
  (throw (RuntimeException. (apply str args))))

(defrecord ODef [ var val macro? ])

(defn odefinition? [ val ]
  (instance? ODef val))

(defrecord OFunction [ formals code env ])

(defn ofunction? [ val ]
  (instance? OFunction val))

(defn- envlookup [ var env ]
  (if (contains? env var)
    (env var)
    (fail "Unbound global variable: " var)))

(declare oeval-do)

(defn- oapply-ofn [ fun actuals env ]
  (oeval-do (:code fun)
            (merge (:env fun)
                   (zipmap (:formals fun) actuals))))

(defn- oapply [ fun actuals env ]
  (cond
    (ofunction? fun)
    (oapply-ofn fun actuals env)

    (or (map? fun) (vector? fun))
    (get fun (first actuals))

    (or (symbol? fun) (keyword? fun))
    (get (first actuals) fun)

    (fn? fun)
    (apply fun actuals)

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

(defn- oeval-or [ forms env ]
  (loop [retval false forms forms]
    (if (or (empty? forms) retval)
      retval
      (recur (oeval (first forms) env)
             (rest forms)))))

(defn- oeval-let [ [ bindings & forms ] env ]
  (loop [bindings bindings
         forms forms
         env env]
    (if (empty? bindings)
      (oeval-do forms env)
      (let [ [ var var-form & remaining-bindings ] bindings ]
        (when (not (symbol? var))
          (fail "Bad let binding name: " var))
        (recur remaining-bindings forms (assoc env var (oeval var-form env)))))))

(defn- oeval-fn [ [ formals & forms ] env ]
  (OFunction. formals forms env))

(defn- oeval-def* [ [ var macro? defn-form ] env ]
  (when (not (symbol? var))
    (fail "Cannot define: " var))
  (let [definition (oeval defn-form env)]
    (when (and macro? (not (ofunction? definition)))
      (fail "Macros must be defined to be functions: " var))
    (ODef. var definition macro?)))

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

        or
        (oeval-or args env)

        let
        (oeval-let args env)

        fn
        (oeval-fn args env)

        def*
        (oeval-def* args env)

        (oapply (oeval fun-pos env)
                (map #(oeval % env) args)
                env)))))

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

(defn oload [ forms env ]
  (reduce (fn [ env form ]
            (let [ result (oeval form env) ]
              (if (odefinition? result)
                (assoc env (:var result) (:val result))
                env)))
          env
          forms))
