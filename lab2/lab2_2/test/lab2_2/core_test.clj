(ns lab2-2.core-test
  (:require [clojure.test :refer :all]
            [lab2-2.core :refer :all]))


(defn exp-integral-expected [x] (dec (exp x)))

(deftest integration-operator-test-exp
  (testing "exponent integration"
    (is (= 0 1))))
