(ns lab3-2.core
  (:gen-class))



;бесконечная последовательность натуральных чисел
(def naturals
  (lazy-seq
   (cons 1 (map inc naturals))))

;из 3.1: не lazy, так как reduce и он будет пытаться всё размотать - StackOverflow
(defn my-filter [pred coll] (reduce
   (fn [acc elem] (if (pred elem) (concat acc (list elem)) acc))
   (list) coll))

;lazy; (seq x) is the recommended idiom for testing if a collection is not empty
;if the test fails when-let return nil, and lazy-sequence is terminated
(defn my-filter-lazy [pred coll]
  (lazy-seq (when-let [s (seq coll)]
              (if (pred (first s))
                (cons (first s) (my-filter-lazy pred (rest s)))
                (my-filter-lazy pred (rest s))))))

(comment ;плохо: превращает конечную в бесконечную
(defn my-partition-old [n coll]
  (->>
   (iterate (fn [[current-part tail]] [(take n tail), (drop n tail)]) [(take n coll) (drop n coll)])
   (map first)
   (map (fn [elem] (if (= elem (list)) nil elem)))
   ) ;если конечная - то take k - количество списков
  )
 )

(defn my-partition [n coll]
  (lazy-seq (when-let [s (seq coll)]
              (cons (take n s) (my-partition n (drop n s))))))

(def base-thread-number 4)
(def base-batch-size 1000)
(defn my-filter-future-lazy
  ([pred coll thread-number] (->>
                              (my-partition (if-let [test (nth coll base-batch-size nil)]
                                              2 ;если существует элемент на позиции base-batch-size
                                              3 ;если не существует элемент, то есть точно конечная
                                              ) coll)
                              ))
  ([pred coll] (my-filter-future-lazy pred coll base-thread-number)))


(defn -main
  [& args]
  (println "Hello, World!"))
