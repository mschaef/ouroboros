(ns ouroboros.ouroboros-test
  (:use ouroboros.ouroboros)
  (:require [clojure.test :refer :all]))

(deftest default-environment
  (testing "The default environment contains math operations"
    (is (= 31 (interp-eval '(+ 3 (* 4 7)))))))

(deftest load-into-default-environment
  (testing "It is possible to define a constant"
    (let [ env (interp-load '((def value 42)))]
      (is (= 42 (interp-eval 'value env)))))

  (testing "It is possible to define a constant with an expression"
    (let [ env (interp-load '((def value (+ 12 23))))]
      (is (= 35 (interp-eval 'value env)))))

  (testing "It is possible to define a function"
    (let [ env (interp-load '((defn test-incr [ x ] (+ x 1))))]
      (is (= 12 (interp-eval '(test-incr 11) env)))))

  (testing "It is possible to define multiple functions"
    (let [ env (interp-load '((defn test-incr [ x ] (+ x 1))
                              (defn test-mul [ x ] (* x 3))))]
      (is (= 36 (interp-eval '(test-mul (test-incr 11)) env))))))

(deftest string-load-into-default-environment
  (testing "It is possible to define a constant"
    (let [ env (interp-load-string "(def value 42)")]
      (is (= 42 (interp-eval 'value env)))))

  (testing "It is possible to define a constant with an expression"
    (let [ env (interp-load-string "(def value (+ 12 23))")]
      (is (= 35 (interp-eval 'value env)))))

  (testing "It is possible to refer to variables defined in previous expressions"
    (let [env (interp-load-string
               (str
                "(def pt {})"
                "(def pt (assoc pt :x 3))"
                "(def pt (assoc pt :y 4))"))]
      (is (= {:x 3 :y 4} (interp-eval 'pt env)))))

  (testing "It is possible to define a function"
    (let [ env (interp-load-string "(defn test-incr [ x ] (+ x 1))")]
      (is (= 10 (interp-eval '(test-incr 9) env)))))

  (testing "It is possible to define multiple functions"
    (let [ env (interp-load-string
                (str
                 "(defn test-incr [ x ] (+ x 11))"
                 "(defn test-mul [ x ] (* x 3))"))]
      (is (= 102 (interp-eval '(test-mul (test-incr 23)) env))))))
