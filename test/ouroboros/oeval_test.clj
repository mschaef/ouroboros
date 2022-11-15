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
