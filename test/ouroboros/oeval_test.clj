(ns ouroboros.oeval-test
  (:use ouroboros.oeval)
  (:require [clojure.test :refer :all]))

(defn order-test-env []
  (let [order-state (atom [])]
    {'current-order-state order-state
     'order-step (fn [ step-id return-value ]
                   (swap! order-state conj step-id)
                   return-value)}))

(defn execution-order [ env ]
  @(env 'current-order-state))


(deftest oimport-environment
  (testing "An empty oimport produces an empty environment"
    (is (= {} (oimport-syms))))

  (testing "An oimport with a symbol produces an environemnt with that symbol bound to the current definition"
    (is (= {'+ +} (oimport-syms +))))

  (testing "An oimport with n symbols produces an environemnt with those symbols bound"
    (is (= {'+ + '* * '- -} (oimport-syms + * -))))

  (testing "oimport cannot be used for anything other than symbols"
    (is (thrown-with-msg? RuntimeException #"Cannot import non-symbol: 42"
                          (oimport-syms-form [42])))))

(def math-env (oimport-syms + list))

(deftest scalar-oeval
  (testing "The empty list evaluates to itself."
    (is (= (oeval '() {}) '())))

  (testing "Nil evaluates to itself."
    (is (= (oeval nil {}) nil)))

  (testing "True evaluates to itself."
    (is (= (oeval true {}) true)))

  (testing "False evaluates to itself."
    (is (= (oeval false {}) false)))

  (testing "Number evaluates to itself."
    (is (= (oeval 42 {}) 42)))

  (testing "String evaluates to itself."
    (is (= (oeval "xyzzy" {}) "xyzzy")))

  (testing "Keyword evaluates to itself."
    (is (= (oeval :keyword {}) :keyword))))

(deftest symbol-oevel
  (let [env '{x 3 y 4}]
    (testing "Symbol evaluates to global environment binding"
      (is (= (oeval 'x env) 3)))

    (testing "Error is sigaled when a sysmbol is missing in the global environment"
      (is (thrown-with-msg? RuntimeException #"Unbound global variable: missing"
                            (oeval 'missing env))))))

(deftest quote-oeval
  (let [env '{x 3 y 4}]
    (testing "Quoted symbol evaluates to symbol and not bound value"
      (is (= (oeval ''x env) 'x)))

    (testing "Quoted symbol missing in global environment evaluates to symbol without error"
      (is (= (oeval ''missing env) 'missing)))))

(deftest vector-oeval
  (let [env '{x 3 y 4}]
    (testing "Empty vectors are evaluated to themselves"
      (is (= (oeval '[] env) '[])))

    (testing "Vectors with scalars are evaluated to themselves"
      (is (= (oeval '[11 22] env) '[11 22])))

    (testing "Embedded variables in vectors are evaluated"
      (is (= (oeval '[x y] env) '[3 4])))

    (testing "Quote works inside vectors"
      (is (= (oeval '['x x 'y y] env) '[x 3 y 4])))))

(deftest map-oeval
  (let [env '{x 3 y 4}]
    (testing "Empty maps are evaluated to themselves"
      (is (= (oeval '{} {})))

    (testing "Maps with scalars are evaluated to themselves"
      (is (= (oeval '{'a 65 'b 66} env) '{a 65 b 66})))

    (testing "Embedded variables in maps are evaluated"
      (is (= (oeval '{'a x 'b y} env) '{a 3 b 4})))

    (testing "Quote works inside maps"
      (is (= (oeval '{'a 'x 'b y} env) '{a x b 4}))))))

(deftest invalid-apply-error
  (testing "A number cannot be applied."
    (is (thrown-with-msg? RuntimeException #"Cannot apply: 42"
                          (oeval '(42) {}))))

  (testing "The boolean true cannot be applied."
    (is (thrown-with-msg? RuntimeException #"Cannot apply: true"
                          (oeval '(true) {}))))

  (testing "The boolean false cannot be applied."
    (is (thrown-with-msg? RuntimeException #"Cannot apply: false"
                          (oeval '(false) {}))))

  (testing "nil cannot be applied."
    (is (thrown-with-msg? RuntimeException #"Cannot apply: nil"
                          (oeval '(nil) {}))))

  (testing "The empty list cannot be applied."
    (is (thrown-with-msg? RuntimeException #"Cannot apply: ()"
                          (oeval '(nil) {})))))

(deftest map-apply
  (let [env '{map {x 3 y 4}}]
    (testing "A map in function position can be applied as a function from key to value"
      (is (= (oeval '(map 'x) env) 3)))))

(deftest map-apply
  (let [env '{v [:a :b :c :d]}]
    (testing "A vector in function position can be applied as a function from index to value"
      (is (= (oeval '(v 0) env) :a)))))

(deftest keyword-apply
  (let [env '{map {:x 3 :y 4}}]
    (testing "A keyword in function position can be applied as an index into a map"
      (is (= (oeval '(:x map) env) 3)))))

(deftest symbol-apply
  (let [env '{map {x 3 y 4}}]
    (testing "A keyword in function position can be applied as an index into a map"
      (is (= (oeval '('x map) env) 3)))))

(deftest function-application-var-lookup
  (let [env '{m {:x 3 :y 4}
              index :x}]
    (testing "Arguments to function applications are evaluated"
      (is (= (oeval '(m index) env) 3)))))

(deftest recursive-function-application
  (let [env '{m {:map {:x 3 :y 4} :index :x}}]
    (testing "All positions of function applications are recursively evaluated"
      (is (= (oeval '((m :map) (m :index)) env) 3)))))

(deftest native-fn-apply
  (let [env {'underlying-+ +}]
    (testing "A function native to the underlying Clojure can be applied"
      (is (= (oeval '(underlying-+ 1 2) env) 3)))))

(deftest if-special-form-return
  (testing "Then branch in an if returns if conditional is true"
    (is (= (oeval '(if true 1 2) {}) 1)))

  (testing "Else branch in an if returns if conditional is false"
    (is (= (oeval '(if false 1 2) {}) 2)))

  (testing "Then branch in an if returns if conditional is true, even without an else cuase"
    (is (= (oeval '(if true 1) {}) 1)))

  (testing "An if returns nil conditional is false and there is no else clause"
    (is (= (oeval '(if false 1) {}) nil))))

(deftest if-special-form-evaluation
  (let [env (order-test-env)]
    (testing "Only the then branch in an if form is evaluated if the condition is true"
      (oeval '(if true
                (order-step :then 0)
                (order-step :else 0))
             env)
      (is (= [:then] (execution-order env)))))

  (let [env (order-test-env)]
    (testing "Only the else branch in an if form is evaluated if the condition is false"
      (oeval '(if false
                (order-step :then 0)
                (order-step :else 0))
             env)
      (is (= [:else] (execution-order env))))))

(deftest do-special-form-evaluation
  (testing "An empty do block evaluates to nil"
    (is (nil? (oeval '(do) {}))))

  (testing "A single form do block evaluates as that form"
    (is (= 42 (oeval '(do 42) {}))))

  (testing "A multiple form do block evaluates as the result of the last formform"
    (is (= 3 (oeval '(do 1 2 3) {}))))

  (let [env (order-test-env)]
    (testing "Do block forms are executed in order"
      (oeval '(do
                (order-step 1 0)
                (order-step 2 0)
                (order-step 3 0))
             env)
      (is (= [1 2 3] (execution-order env))))))

(deftest and-special-form-evaluation
  (testing "An empty and form evaluates to true"
    (is (= true (oeval '(and) {}))))

  (testing "An and form with one true clause evaluates to that clause"
    (is (= 42 (oeval '(and 42) {}))))

  (testing "An and form with one false clause evaluates to false"
    (is (false? (oeval '(and false) {}))))

  (testing "A true and form with multiple true clauses evaluates to the last clause"
    (is (= 3 (oeval '(and 1 2 3) {}))))

  (testing "A false and form with multiple true clauses evaluates to false"
    (is (false? (oeval '(and false 2 3) {})))
    (is (false? (oeval '(and 1 false 3) {})))
    (is (false? (oeval '(and 1 2 false) {}))))

  (let [env (order-test-env)]
    (testing "And forms short-circuit evaluation"
      (oeval '(and
               (order-step 1 true)
               (order-step 2 false)
               (order-step 3 true))
             env)
      (is (= [1 2] (execution-order env))))))

(deftest or-special-form-evaluation
  (testing "An empty or form evaluates to falsee"
    (is (false? (oeval '(or) {}))))

  (testing "An or form with one true clause evaluates to that clause"
    (is (= 42 (oeval '(or 42) {}))))

  (testing "An or form with one false clause evaluates to false"
    (is (false? (oeval '(or false) {}))))

  (testing "A true or form with multiple true clauses evaluates to the first clause"
    (is (= 1 (oeval '(or 1 2 3) {}))))

  (testing "A true or form with an initial true clause evaluates to that clause"
    (is (= 1 (oeval '(or 1 false false) {}))))

  (let [env (order-test-env)]
    (testing "Or forms short-circuit evaluation"
      (oeval '(or
               (order-step 1 false)
               (order-step 2 true)
               (order-step 3 false))
             env)
      (is (= [1 2] (execution-order env))))))

(deftest let-without-bindings
  (testing "An empty let with empty bindings evaluates to nil"
    (is (nil? (oeval '(let []) {}))))

  (testing "A let with empty bindings and one body form evaluates to its body"
    (is (= 1 (oeval '(let [] 1) {}))))

  (testing "A let with empty bindings evaluates to its body"
    (is (= 3 (oeval '(let [] 1 2 3) {}))))

  (let [env (order-test-env)]
    (testing "Let forms evaluate their bodies in order."
      (is (= 33 (oeval '(let []
                          (order-step 1 11)
                          (order-step 2 22)
                          (order-step 3 33))
                       env)))
      (is (= [1 2 3] (execution-order env))))))

(deftest let-with-bindings
  (testing "Let creates a single variable binding available within its body"
    (is (= 42 (oeval '(let [x 42] x) {}))))

  (testing "Let creates multiple variable bindings available within its body"
    (is (= [42 43 44]
           (oeval '(let [x 42 y 43 z 44] [x y z]) {}))))

  (testing "Let bindings are available within definition forms for subsequent bindings in the same form"
    (is (= [42 43 44]
           (oeval '(let [x 42 y (+ x 1) z (+ y 1)] [x y z]) math-env))))

  (testing "Let requires binding names to be symbols"
    (is (thrown-with-msg? RuntimeException #"Bad let binding name: 42"
                          (oeval '(let [42 42] x) {})))))

(deftest let-with-function-bindings
  (testing "Let can be used to bind to functions"
    (is (= 4 (oeval '(let [add +] (add 1 3))
                    math-env)))))

(deftest fn-zero-arity
  (testing "An anonymous function can be created and called"
    (is (= 42 (oeval '((fn [] 42)) {})))))

(deftest fn-1-arity
  (testing "An anonymous function with an argument can be created and called"
    (is (= 42 (oeval '((fn [ x ] x) 42) {})))))

(deftest fn-2-arity
  (testing "An anonymous function with two arguments can be created and called"
    (is (= 7 (oeval '((fn [ x y ] (+ x y)) 3 4) math-env)))))


(deftest fn-1-arity-closed
  (testing "A function is closed over its lexical environment"
    (is (= 42 (oeval '(((fn [ x ] (fn [] x)) 42))
                     {})))))

(deftest lexical-scope
  (testing "Bindings created by function application are confined to their lexical scope"
    (is (= 1 (oeval '(let [x 1
                           fa (fn [] x)
                           fb (fn [ x ] (fa))]
                       (fb 2))
                    {}))))

    (testing "Bindings created by let forms are confined to their lexical scope"
    (is (= 1 (oeval '(let [x 1
                           fa (fn [] x)
                           fb (fn [ ] (let [ x 2 ]  (fa)))]
                       (fb))
                     {})))))

(deftest var-defining-form
  (testing "A definition form returns a definition instance"
    (is (odefinition? (oeval '(def* x 2) {}))))

  (testing "A definition form includes the symbol being defined"
    (is (= 'x (:var (oeval '(def* x 3) {})))))

  (testing "A definition form evaluates its definition"
    (is (= 4 (:val (oeval '(def* x (+ 3 1)) math-env)))))

  (testing "A definition form defaults the macro flag to false"
    (is (= false (:macro? (oeval '(def* x (+ 3 1)) math-env)))))

  (testing "A definition form can only defime a symbol"
    (is (thrown-with-msg? RuntimeException #"Cannot define: 42"
                          (oeval '(def* 42 x) {})))))

(deftest macro-defining-form
  (testing "A definition form defaults the macro flag to false"
    (is (= true (:macro? (oeval '(defm* x (fn [] 3)) {})))))

  (testing "A macro definition form can only defime a symbol"
    (is (thrown-with-msg? RuntimeException #"Cannot define: 42"
                          (oeval '(defm* 42 (fn [] x)) {}))))

  (testing "A macro definition must be a function"
    (is (thrown-with-msg? RuntimeException
                          #"Macros must be defined to be functions: x"
                          (oeval '(defm* x "not-a-function") {})))))

(deftest set-macro-flag
  (testing "Setting the macro flag in an empty environment adds to the set of macro symbols"
    (is (= {:macros #{'x}}
           (set-macro-flag! {} 'x true))))

  (testing "Clearing the macro flag in an empty environment removes from the set of macro symbols"
    (is (= {:macros #{}}
           (set-macro-flag! {:macros #{'x}} 'x false)))))

(deftest load-statement
  (testing "An empty load form does not alter the environment"
    (is (= {} (oload [] {}))))

  (testing "An load form with a single definition adds that definition"
    (is (= '{x 4 :macros #{}} (oload '[(def* x 4)] {}))))

  (testing "An load form with two definitions adds those definitions"
    (is (= '{x 4 y 3 :macros #{}} (oload '[(def* x 4) (def* y 3)] {}))))

  (testing "An load form can define a function"
    (let [ env (oload '[(def* double (fn [ x ] (+ x x)))] math-env) ]
      (= 4 (oeval '(double 2) env))))

  (testing "An load form with a standard definition does not set the macro flag"
    (is (not (macro-defn? (oload '[(def* x 4)] {})
                          'x))))

  (testing "An load form with a macro definition sets the macro flag"
    (is (macro-defn? (oload '[(defm* x (fn [ x ] (+ x x)))] {})
                          'x))))

(deftest macro-expansion
  (testing "A macro is expanded during evaluation and the result of the macro evaluated"
    (let [ env-w-macro (oload '[(defm* add (fn [ arg-0 arg-1 ]
                                             (list '+ arg-0 arg-1)))]
                              math-env)]
      (is (= 5 (oeval '(add 2 3) env-w-macro))))))
