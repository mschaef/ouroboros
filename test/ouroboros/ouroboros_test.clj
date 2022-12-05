(ns ouroboros.ouroboros-test
  (:use ouroboros.ouroboros)
  (:require [clojure.test :refer :all]))

(deftest default-environment
  (testing "The default environment contains math operations"
    (is (= 31 (interp-eval '(+ 3 (* 4 7)))))))
