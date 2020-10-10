(ns lab3-1.core
  (:gen-class))

(defn filter-future [pred coll]
  
  )

(defn partition-reduce-func [n]
  (fn [coll elem])
  )


(defn divide-collection [n acc rest]
  (if (> (count rest) 0)
    (recur n (concat acc (list (take n rest))) (drop n rest))
    acc))

(defn my-partition [n coll] ;каждый кусок из n элементов
  ;take and drop
  ;может, заредбюсить?
  (divide-collection n (list) coll)
  )


(defn -main
  [& args]
  (println "Hello, World!"))
