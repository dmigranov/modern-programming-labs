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
  ([pred coll] (my-filter-future pred coll base-thread-number))
  )


;deref = @
;When applied to a future, will block if computation not complete. then return value



;todo: затестить на восьми тредах
(defn -main
  [& args]
  (println "TIME TEST")
  (my-filter even? (range 0 10))
  (my-filter-future even? (range 0 10))
  (let [n 500]
    (println "NO FUTURE," n "elems:")
    (time (doall (my-filter even? (range 0 n))))

    (println "WITH FUTURE," n "elems," thread-count "threads")
    (time (doall (my-filter-future even? (range 0 n)))))

  (println)

  (let [n 1000]
    (println "NO FUTURE," n "elems:")
    (time (doall (my-filter even? (range 0 n))))

    (println "WITH FUTURE," n "elems," thread-count "threads")
    (time (doall (my-filter-future even? (range 0 n)))))

  (println)

  (let [n 2000]
    (println "NO FUTURE," n "elems:")
    (time (doall (my-filter even? (range 0 n))))

    (println "WITH FUTURE," n "elems," thread-count "threads")
    (time (doall (my-filter-future even? (range 0 n)))))

  (shutdown-agents))
