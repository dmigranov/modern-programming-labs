(ns lab3-1.core
  (:gen-class))

(def base-thread-number 4)

(defn my-filter [pred coll]
  (reduce
   (fn [acc elem]
     (if (pred elem)
       (concat acc (list elem))
       acc))
   (list) coll))

(defn divide-collection [n acc rest]
  (if (> (count rest) 0)
    (recur n (concat acc (list (take n rest))) (drop n rest))
    acc))

(defn my-partition [n coll] ;каждый кусок из n элементов NOT LAZY
  (divide-collection n (list) coll))

(defn my-filter-future
  ([pred coll thread-number] (->>
                              (my-partition (Math/ceil (/ (count coll) thread-number)) coll)
                              (map (fn [elem] (future (my-filter pred elem))))
                              (doall)
                              (map deref)
                              (reduce concat)))
  ([pred coll] (my-filter-future pred coll base-thread-number)))

(defn -main
  [& args]
  (println "TIME TEST")
  (doall (my-filter even? (range 0 2000)))
  (doall (my-filter-future even? (range 0 2000) 4))
  (doall (my-filter-future even? (range 0 2000) 8))
  
  (let [n 500, thread-number 4, thread-number-double (* 2 thread-number)]
    (println "NO FUTURE," n "elems:")
    (time (doall (my-filter even? (range 0 n))))
    (println "WITH FUTURE," n "elems," thread-number "threads")
    (time (doall (my-filter-future even? (range 0 n) thread-number)))
    (println "WITH FUTURE," n "elems," thread-number-double "threads")
    (time (doall (my-filter-future even? (range 0 n) thread-number-double)))
    (println))

  (let [n 2000, thread-number 4, thread-number-double (* 2 thread-number)]
    (println "NO FUTURE," n "elems:")
    (time (doall (my-filter even? (range 0 n))))
    (println "WITH FUTURE," n "elems," thread-number "threads")
    (time (doall (my-filter-future even? (range 0 n) thread-number)))
    (println "WITH FUTURE," n "elems," thread-number-double "threads")
    (time (doall (my-filter-future even? (range 0 n) thread-number-double)))
    (println))
  
  (let [n 4000, thread-number 4, thread-number-double (* 2 thread-number)]
    (println "NO FUTURE," n "elems:")
    (time (doall (my-filter even? (range 0 n))))
    (println "WITH FUTURE," n "elems," thread-number "threads")
    (time (doall (my-filter-future even? (range 0 n) thread-number)))
    (println "WITH FUTURE," n "elems," thread-number-double "threads")
    (time (doall (my-filter-future even? (range 0 n) thread-number-double)))
    (println))

  (shutdown-agents))
