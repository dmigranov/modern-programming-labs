(ns lab4.core-test
  (:require [clojure.test :refer :all]
            [lab4.core :refer :all]))

(deftest variable-test
  (testing "Variables"
    (is (variable? (variable ::x)))
    (is (= ::x (variable-name (variable ::x))))
    (is (same-variables?
         (variable ::x)
         (variable ::x)))
    (is (not (same-variables?
                   (variable ::x)
                   (variable ::y))))))

(deftest implication-test
  (testing "Implication"
    (is (implication? (implication (variable :x) (variable :y))))
    (is (= :x (variable-name (first (args (implication (variable :x) (variable :y)))))))
    ))

(deftest negation-test
  (testing "Negation"
    (is (negation? (negation (variable :x))))
    (is (= :x (variable-name (first (args (negation (variable :x)))))))))

(deftest disjunction-test
  (testing "Disjunction"
    (is (disjunction? (disjunction (variable :x) (variable :y))))
    (is (disjunction? (disjunction (variable :x) (variable :y) (variable :z))))
    (is (= :x (variable-name (first (args (disjunction (variable :x) (variable :y) (variable :z)))))))
    ))

(deftest conjunction-test
  (testing "Conjunction"
    (is (conjunction? (conjunction (variable :x) (variable :y))))
    (is (conjunction? (conjunction (variable :x) (variable :y) (variable :z))))
    (is (= :x (variable-name (first (args (conjunction (variable :x) (variable :y) (variable :z)))))))
    ))