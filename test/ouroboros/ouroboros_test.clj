(ns ouroboros.ouroboros-test
  (:use ouroboros.ouroboros)
  (:require [clojure.test :refer :all]))

(deftest default-environment
  (testing "The default environment contains math operations"
    (is (= 31 (interp-eval '(+ 3 (* 4 7)))))))

(deftest function-definition
  (testing "It is possible to define a function in the default enviornment"
    (let [ env (interp-load '((defn test-incr [ x ] (+ x 1))))]
      (is (= 12 (interp-eval '(test-incr 11) env)))))

  (testing "It is possible to define multiple functiosn in the default enviornment"
    (let [ env (interp-load '((defn test-incr [ x ] (+ x 1))
                              (defn test-mul [ x ] (* x 3))))]
      (is (= 36 (interp-eval '(test-mul (test-incr 11)) env))))))
