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

(def base-thread-number 2)
(def base-batch-size 4000)


;из 3.1: не lazy, так как reduce и он будет пытаться всё размотать - StackOverflow
(defn my-filter-no-lazy [pred coll] (reduce
                             (fn [acc elem] (if (pred elem) (concat acc (list elem)) acc))
                             (list) coll))

(defn divide-collection [n acc rest]
  (if (> (count rest) 0)
    (recur n (concat acc (list (take n rest))) (drop n rest))
    acc))
(defn my-partition [n coll]
  (divide-collection n (list) coll))
(defn my-filter-parallel-no-lazy
  ([pred coll thread-number] (->>
                              (my-partition (Math/ceil (/ (count coll) thread-number)) coll)
                              (map (fn [elem] (future (my-filter-no-lazy pred elem))))
                              (doall)
                              (map deref)
                              (reduce concat)))
  ([pred coll] (my-filter-parallel-no-lazy pred coll base-thread-number)))


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




(defn my-filter-lazy-no-parallel [pred coll ]
  (when-let [s (seq coll)]
    (lazy-cat (my-filter-lazy pred (take base-batch-size s)) (my-filter-lazy-no-parallel pred (drop base-batch-size s)))))
   


;идея: взять take некий батч размером и отфильтроват ппраллельно его bass-thread-number тредами, потом так же следующий кусок)
;todo: распареллилть батчи (брать count batch = (take base-batch-size s))
(defn my-filter-future-finite [pred coll thread-number]
  (->>
   coll
   (my-partition-lazy (Math/ceil (/ (count coll) thread-number))) ;todo: случай нуля
   (map (fn [elem] (future (doall (my-filter-lazy pred elem)))))
   (doall)
   (mapcat deref) ;(map deref) (apply concat)
   (doall)))    

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
  (println "TIME TEST, FINITE")

  (let [n 4000, thread-number 2, thread-number-double (* 2 thread-number), thread-number-4 (* 4 thread-number)]

    (doall (my-filter-lazy-parallel even? (range 0 n) thread-number))
    (doall (my-filter-lazy-parallel even? (range 0 n) thread-number-double))
    (doall (my-filter-lazy-parallel even? (range 0 n) thread-number-4))

    (println "LAZY, PARALLEL," n "elems," thread-number "threads")
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

    (println "OLD FILTER, NO LAZY, NO PARALLEL," n "elems")
    (time (doall (my-filter-no-lazy even? (range 0 n))))
    (time (doall (my-filter-no-lazy even? (range 0 n))))
    (time (doall (my-filter-no-lazy even? (range 0 n))))

    (println "3.1 FILTER, NO LAZY,  PARALLEL," n "elems," thread-number "threads")
    (time (doall (my-filter-parallel-no-lazy even? (range 0 n) thread-number)))
    (time (doall (my-filter-parallel-no-lazy even? (range 0 n) thread-number)))
    (time (doall (my-filter-parallel-no-lazy even? (range 0 n) thread-number)))

    (println "3.1 FILTER, NO LAZY,  PARALLEL," n "elems," thread-number-double "threads")
    (time (doall (my-filter-parallel-no-lazy even? (range 0 n) thread-number-double)))
    (time (doall (my-filter-parallel-no-lazy even? (range 0 n) thread-number-double)))
    (time (doall (my-filter-parallel-no-lazy even? (range 0 n) thread-number-double)))

    (println))

  (println "TIME TEST, INFINITE")

  (let [n 20000, thread-number 2, thread-number-double (* 2 thread-number), thread-number-4 (* 4 thread-number)
        lnp (my-filter-lazy-no-parallel even? naturals)
        lp (my-filter-lazy-parallel even? naturals thread-number)
        lp2 (my-filter-lazy-parallel even? naturals thread-number-double)
        lp4 (my-filter-lazy-parallel even? naturals thread-number-4)]

    (doall (take n (my-filter-lazy-parallel even? naturals thread-number)))
    (doall (take n (my-filter-lazy-parallel even? naturals thread-number-double)))
    (doall (take n (my-filter-lazy-parallel even? naturals thread-number-4)))

    (println "LAZY, PARALLEL," n "elems," thread-number "threads")
    (time (doall (take n (my-filter-lazy-parallel even? naturals thread-number))))
    (time (doall (take n (my-filter-lazy-parallel even? naturals thread-number))))
    (time (doall (take n (my-filter-lazy-parallel even? naturals thread-number))))

    (println "LAZY, PARALLEL," n "elems," thread-number-double "threads")
    (time (doall (take n (my-filter-lazy-parallel even? naturals thread-number-double))))
    (time (doall (take n (my-filter-lazy-parallel even? naturals thread-number-double))))
    (time (doall (take n (my-filter-lazy-parallel even? naturals thread-number-double))))

    (println "LAZY, PARALLEL," n "elems," thread-number-4 "threads")
    (time (doall (take n (my-filter-lazy-parallel even? naturals thread-number-4))))
    (time (doall (take n (my-filter-lazy-parallel even? naturals thread-number-4))))
    (time (doall (take n (my-filter-lazy-parallel even? naturals thread-number-4))))

    (println))


  (shutdown-agents))
