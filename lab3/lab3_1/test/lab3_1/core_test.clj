(ns lab3-1.core-test
  (:require [clojure.test :refer :all]
            [lab3-1.core :refer :all]))

(deftest partition-test
  (testing "Partitioning lists"
    (let [list-a (range 1 10)
          list-b (range 1 11)]
      (is (= (my-partition 2 list-a) '((1 2) (3 4) (5 6) (7 8) (9))))
      (is (= (my-partition 3 list-a) '((1 2 3) (4 5 6) (7 8 9))))
      (is (= (my-partition 4 list-b) '((1 2 3 4) (5 6 7 8) (9 10))))
      (is (= (my-partition 4 (list)) '()))
      (is (= (my-partition 4 (list 1)) '((1))))
      )
    ))
