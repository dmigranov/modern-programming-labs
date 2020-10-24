(ns lab4.core-test
  (:require [clojure.test :refer :all]
            [lab4.core :refer :all]))

(deftest variable-test
  (testing "Variables"
    (is (variable? (variable ::x)))
    (is (= ::x (variable-name (variable ::x))))
    (is (same-variables?
         (variable :x)
         (variable :x)))
    (is (not (same-variables?
                   (variable :x)
                   (variable :y))))))

(deftest implication-test
  (testing "Implication"
    (is (implication? (implication ::x ::y)))
    (is (= ::x (first (args (implication ::x ::y)))))
    ))

(deftest negation-test
  (testing "Negation"
    (is (negation? (negation ::x)))
    (is (= ::x (first (args (negation ::x)))))))