(ns lab3-2.core
  (:gen-class))

(defn my-partition-old [n coll] ;плохо: превращает конечную в бесконечную
  (->>
   (iterate (fn [[current-part tail]] [(take n tail), (drop n tail)]) [(take n coll) (drop n coll)])
   (map first)
   (map (fn [elem] (if (= elem (list)) nil elem)))))




;бесконечная последовательность натуральных чисел
(def naturals
  (lazy-seq
   (cons 1 (map inc naturals))))



;из 3.1: не lazy, так как reduce и он будет пытаться всё размотать - StackOverflow
(defn my-filter-no-lazy [pred coll] (reduce
                             (fn [acc elem] (if (pred elem) (concat acc (list elem)) acc))
                             (list) coll))

;lazy; (seq x) is the recommended idiom for testing if a collection is not empty
;if the test fails when-let return nil, and lazy-sequence is terminated
(defn my-filter-lazy [pred coll]
  ;(println "THREAD"  (. (Thread/currentThread) getName))
  (lazy-seq (when-let [s (seq coll)]
              (if (pred (first s))
                (cons (first s) (my-filter-lazy pred (rest s)))
                (my-filter-lazy pred (rest s))))))

(defn my-partition-lazy [n coll]
  (lazy-seq (when-let [s (seq coll)]
              (cons (take n s) (my-partition-lazy n (drop n s))))))


(def base-thread-number 2)
(def base-batch-size 20000)

(defn my-filter-lazy-no-parallel [pred coll ]
  (when-let [s (seq coll)]
    (lazy-cat (my-filter-lazy pred (take base-batch-size s)) (my-filter-lazy-no-parallel pred (drop base-batch-size s)))))
   


;идея: взять take некий батч размером и отфильтроват ппраллельно его bass-thread-number тредами, потом так же следующий кусок)
;todo: распареллилть батчи (брать count batch = (take base-batch-size s))

(defn my-filter-future-finite [pred coll thread-number]
  (->>
   coll
   ;todo: случай нуля
   (my-partition-lazy (Math/ceil (/ (count coll) thread-number)))
   (map (fn [elem] (future (doall (my-filter-lazy pred elem)))))
   (doall)
   ;(map deref)
   ;(apply concat)
   (mapcat deref)))

(defn my-filter-lazy-parallel
  ([pred coll thread-number]
   (when-let [s (seq coll)]
     (lazy-cat (my-filter-future-finite pred (take base-batch-size s) thread-number) (my-filter-lazy-parallel pred (drop base-batch-size s) thread-number))))
   ([pred coll] (my-filter-lazy-parallel pred coll base-thread-number)))



;(def infinite-size 4000)
;(defn my-filter-future-lazy-bad
;  ([pred coll thread-number] (->>
;                              (my-partition (if-let [test (nth coll infinite-size nil)]
;                                              base-batch-size ;если существует элемент на позиции infinite-size, делим на куски по batch-size
;                                              (Math/ceil (/ (count coll) thread-number)) ;если не существует элемент, то есть точно конечная и можно поделить поровну
;                                              )coll)
;                              (map (fn [elem] (future (my-filter pred elem))))
;                              (map deref)))
;  ([pred coll] (my-filter-future-lazy-bad pred coll base-thread-number)))



(defn -main
  [& args]
  (println "TIME TEST")

  
   (let [n 40000, thread-number 2, thread-number-double (* 2 thread-number), thread-number-4 (* 4 thread-number)]

     (doall (my-filter-lazy-no-parallel even? (range 0 n)))
     (doall (my-filter-lazy-parallel even? (range 0 n) thread-number))
     (doall (my-filter-lazy-parallel even? (range 0 n) thread-number-double))
     (doall (my-filter-lazy-parallel even? (range 0 n) thread-number-4))

     (println "LAZY, NO PARALLEL," n "elems")
     (time (doall (my-filter-lazy-no-parallel even? (range 0 n))))
     (time (doall (my-filter-lazy-no-parallel even? (range 0 n))))
     (time (doall (my-filter-lazy-no-parallel even? (range 0 n))))
     (time (doall (my-filter-lazy-no-parallel even? (range 0 n))))
     (time (doall (my-filter-lazy-no-parallel even? (range 0 n))))


     (println "LAZY, PARALLEL," n "elems," thread-number "threads")
     (time (doall (my-filter-lazy-parallel even? (range 0 n) thread-number)))
     (time (doall (my-filter-lazy-parallel even? (range 0 n) thread-number)))
     (time (doall (my-filter-lazy-parallel even? (range 0 n) thread-number)))
     (time (doall (my-filter-lazy-parallel even? (range 0 n) thread-number)))
     (time (doall (my-filter-lazy-parallel even? (range 0 n) thread-number)))
     

     (println "LAZY, PARALLEL," n "elems," thread-number-double "threads")
     (time (doall (my-filter-lazy-parallel even? (range 0 n) thread-number-double)))
     (time (doall (my-filter-lazy-parallel even? (range 0 n) thread-number-double)))
     (time (doall (my-filter-lazy-parallel even? (range 0 n) thread-number-double)))

     (println "LAZY, PARALLEL," n "elems," thread-number-4 "threads")
     (time (doall (my-filter-lazy-parallel even? (range 0 n) thread-number-4)))
     (time (doall (my-filter-lazy-parallel even? (range 0 n) thread-number-4)))
     (time (doall (my-filter-lazy-parallel even? (range 0 n) thread-number-4)))

     (println))
   
   (shutdown-agents)
  )
