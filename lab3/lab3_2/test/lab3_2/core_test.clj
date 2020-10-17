(ns lab3-2.core-test
  (:require [clojure.test :refer :all]
            [lab3-2.core :refer :all]))

(deftest my-filter-future-finite-test
  (testing "Filtering using my-filter-future"
    (let [list-a (range 1 10)
          list-b (range 1 11)]
      (is (= (my-filter-future-finite even? list-a 2) '(2 4 6 8)))
      (is (= (my-filter-future-finite even? list-b 2) '(2 4 6 8 10)))
      (is (= (my-filter-future-finite odd? list-a 2) '(1 3 5 7 9)))
      (is (= (my-filter-future-finite odd? list-b 2) '(1 3 5 7 9)))
      (is (= (my-filter-future-finite odd? list-a 16) '(1 3 5 7 9)))
      (is (= (my-filter-future-finite odd? '() 16) '()))
      (is (= (my-filter-future-finite odd? '(2) 16) '()))
      )))