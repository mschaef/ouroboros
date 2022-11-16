(ns ouroboros.oeval-test
  (:use ouroboros.oeval)
  (:require [clojure.test :refer :all]))

(deftest scalar-oeval
  (testing "Number evaluates to itself."
    (is (= (oeval 42 {}) 42)))

  (testing "String evaluates to itself."
    (is (= (oeval "xyzzy" {}) "xyzzy")))

  (testing "Vector evaluates to itself."
    (is (= (oeval [1 2] {}) [1 2])))

  (testing "Map evaluates to itself."
    (is (= (oeval {:x 3 :y 4} {}) {:x 3 :y 4}))))

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
      (is (= (oeval ''missing env) 'missing)))

    (testing "Quote does not work in the first position of a vector"
      (is (= (oeval '[quote x] env) '[quote x])))))

(deftest map-apply
  (let [env '{map {x 3 y 4}}]
    (testing "A map in function position can be evaluated as a function from key to value"
      (is (= (oeval '(map 'x) env) 3)))))
