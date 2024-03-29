(ns ouroboros.oeval)

(defn- fail [ & args ]
  (throw (RuntimeException. (apply str args))))

(defrecord ODef [ var val macro? ])

(defn odefinition? [ val ]
  (instance? ODef val))

(defrecord OFunction [ formals code env ])

(defn ofunction? [ val ]
  (instance? OFunction val))

(defrecord OMacro [ val ])

(defn omacro? [ val ]
  (instance? OMacro val))

(defn- envlookup [ var env ]
  (if (contains? env var)
    (env var)
    (fail "Unbound global variable: " var)))

(defn macrolookup [ var env ]
  (and (contains? env var)
       (let [ val (env var) ]
         (and (omacro? val)
              (.val val)))))

(defn extend-env! [ env var val ]
  (assoc env var val))

(defn oimport-syms-form [ syms ]
  (into {}
        (map (fn [ sym ]
               (when (not (symbol? sym))
                 (fail "Cannot import non-symbol: " sym))
               `[ '~sym ~sym])
             syms)))

(defmacro oimport-syms [ & syms ]
  (oimport-syms-form syms))

(declare oeval-do)

(defn- bind-formal-arguments [ formals actuals ]
  (loop [env {}
         f formals
         a actuals]

    (cond
      (= '& (first f))
      (assoc env (second f) a)

      (and (empty? f) (empty? a))
      env

      (or (empty? f) (empty? a))
      (fail "Incorrect number of arguments: " (count actuals))

      :else
      (recur (assoc env (first f) (first a))
             (rest f)
             (rest a)))))

(defn- oapply-ofn [ fun actuals env ]
  (oeval-do (:code fun)
            (merge (:env fun)
                   (bind-formal-arguments (:formals fun) actuals))))

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
  (doseq [ formal formals ]
    (when (not (symbol? formal))
      (fail "Invalid formal argument: " formal)))
  (OFunction. formals forms env))

(defn- oeval-def* [ [ var defn-form ] env ]
  (when (not (symbol? var))
    (fail "Cannot define: " var))
  (let [definition (oeval defn-form env)]
    (ODef. var definition false)))

(defn- oeval-macro* [ [ defn-form ] env ]
  (let [definition (oeval defn-form env)]
    (when (not (ofunction? definition))
      (fail "Macros must be functions: " definition))
    (OMacro. definition)))

(defn- oeval-list [ form env ]
  (if (empty? form)
    form
    (let [ [ fun-pos & args ] form ]
      (if-let [ macro (macrolookup fun-pos env)]
        (oeval (oapply macro (rest form) env) env)
        (case fun-pos
          quote  (first args)
          if     (oeval-if args env)
          do     (oeval-do args env)
          and    (oeval-and args env)
          or     (oeval-or args env)
          let    (oeval-let args env)
          fn     (oeval-fn args env)
          def*   (oeval-def* args env)
          macro* (oeval-macro* args env)

          (oapply (oeval fun-pos env)
                  (map #(oeval % env) args)
                  env))))))

(defn oeval [ form env ]
  ;; this should fail if env is not a map
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
  ;; this should fail if env is not a map
  (reduce (fn [ env form ]
            (let [ result (oeval form env) ]
              (if (odefinition? result)
                (extend-env! env (:var result) (:val result))
                env)))
          env
          forms))
