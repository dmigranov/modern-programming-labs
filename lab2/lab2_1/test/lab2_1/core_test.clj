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
      (is (close? (exp-integral-expected 0.2) (exp-integral-got 0.2)))
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
      (is (close? (cos-integral-expected 0.2) (cos-integral-got 0.2)))
      (is (close? (cos-integral-expected 1) (cos-integral-got 1))))
    ))
