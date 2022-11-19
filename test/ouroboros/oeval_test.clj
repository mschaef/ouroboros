(ns ouroboros.oeval-test
  (:use ouroboros.oeval)
  (:require [clojure.test :refer :all]))

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
  (let [env {'then-clause (atom false)
             'else-clause (atom false)
             'reset! reset!}]
    (testing "Only the then branch in an if form is evaluated if the condition is true"
      (oeval '(if true
                (reset! then-clause true)
                (reset! else-clause true))
             env)
      (is (= [true false]
             [@(env 'then-clause)
              @(env 'else-clause)]))))

  (let [env {'then-clause (atom false)
             'else-clause (atom false)
             'reset! reset!}]
    (testing "Only the else branch in an if form is evaluated if the condition is false"
      (oeval '(if false
                (reset! then-clause true)
                (reset! else-clause true))
             env)
      (is (= [false true]
             [@(env 'then-clause)
              @(env 'else-clause)])))))

