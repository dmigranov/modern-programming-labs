(ns lab2-1.core-test
  (:require [clojure.test :refer :all]
            [lab2-1.core :refer :all]))


(def epsilon-test 1e-2)
(defn close? [x y] (< (Math/abs (- x y)) epsilon-test))



(defn exp-integral-expected [x] (dec (exp x)))
(deftest integration-operator-test-exp
  (testing "exponent integration" ;testing is for accumulating groups of assertions
    (let [exp-integral-got (integration-operator-memo exp 0.1)]
      (is (close? (exp-integral-expected 0) (exp-integral-got 0)))
      (is (close? (exp-integral-expected 0.1) (exp-integral-got 0.1)))
      (is (close? (exp-integral-expected 0.12) (exp-integral-got 0.12)))
      (is (close? (exp-integral-expected 0.15) (exp-integral-got 0.15)))
      (is (close? (exp-integral-expected 0.199) (exp-integral-got 0.199)))
      (is (close? (exp-integral-expected 0.2) (exp-integral-got 0.2)))
      (is (close? (exp-integral-expected 0.201) (exp-integral-got 0.201)))
      (is (close? (exp-integral-expected 1) (exp-integral-got 1))))
    ))

(defn cos-integral-expected [x] (sin x))
(deftest integration-operator-test-cos
  (testing "cosinus integration"
    (let [cos-integral-got (integration-operator-memo cos 0.1)]
      (is (close? (cos-integral-expected 0) (cos-integral-got 0)))
      (is (close? (cos-integral-expected 0.1) (cos-integral-got 0.1)))
      (is (close? (cos-integral-expected 0.12) (cos-integral-got 0.12)))
      (is (close? (cos-integral-expected 0.15) (cos-integral-got 0.15)))
      (is (close? (cos-integral-expected 0.199) (cos-integral-got 0.199)))
      (is (close? (cos-integral-expected 0.2) (cos-integral-got 0.2)))
      (is (close? (cos-integral-expected 0.201) (cos-integral-got 0.201)))
      (is (close? (cos-integral-expected 1) (cos-integral-got 1))))
    ))

(deftest trapezoid-rule-test
  (testing "trapezoid rule"
    (letfn [(one-func [x] 1)]
      (is (close? (trapezoid-rule one-func 4 9) 5))
      (is (close? (trapezoid-rule one-func -4 6) 10))
      (is (close? (trapezoid-rule one-func -0.2 0.7) 0.9)))))