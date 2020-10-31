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

(deftest to-dnf-test
  (testing "To DNF"
    (is (=
         (to-dnf (negation (disjunction (implication (variable :x) (variable :y)) (negation (implication (variable :y) (variable :z))))))
         '(:disj (:conj (:var :x) (:neg (:var :y))) (:conj (:var :x) (:neg (:var :y)) (:var :z)))))

    (is (=
         (to-dnf (disjunction (variable :x) (variable :y)))
         '(:disj (:var :x) (:var :y))))

    (is (=
         (to-dnf (disjunction (variable :x) (variable :y) (variable :z) (variable :d)))
         '(:disj (:var :x) (:var :y) (:var :z) (:var :d))))

    (is (=
         (to-dnf (conjunction (variable :x) (variable :y)))
         '(:conj (:var :x) (:var :y))))


    (is (=
         (to-dnf (conjunction (variable :x) (variable :y) (variable :z) (variable :d)))
         '(:conj (:var :d) (:var :x) (:var :y) (:var :z))))

    (is (=
         (to-dnf (conjunction (disjunction (variable :x) (variable :y)) (disjunction (variable :z) (variable :s))))
         '(:disj (:conj (:var :x) (:var :z)) (:conj (:var :s) (:var :x)) (:conj (:var :y) (:var :z)) (:conj (:var :s) (:var :y)))))

    (is (=
         (to-dnf (conjunction (disjunction (variable :x) (variable :y)) (disjunction (variable :z) (variable :s))))
         '(:disj (:conj (:var :x) (:var :z)) (:conj (:var :s) (:var :x)) (:conj (:var :y) (:var :z)) (:conj (:var :s) (:var :y)))))

    (is (=
         (to-dnf (variable :x))
         '(:var :x)))

    (is (=
         (to-dnf (conjunction (variable :x) (variable :x)))
         '(:var :x)))

    (is (=
         (to-dnf (conjunction (variable :x) (negation (variable :x))))
         '(:false)))


    (is (=
         (to-dnf log-true)
         '(:true)))

    (is (=
         (to-dnf log-false)
         '(:false)))))