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
