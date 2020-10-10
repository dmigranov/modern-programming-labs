(ns lab3-1.core
  (:gen-class))

(defn filter-future [pred coll]
  
  )

(defn divide-collection [n acc rest]
  (if (> (count rest) 0)
    (recur n (concat acc (list (take n rest))) (drop n rest))
    acc))

(defn my-partition [n coll] ;каждый кусок из n элементов
  (divide-collection n (list) coll)
  )

;deref = @
;When applied to a future, will block if computation not complete. then return value

(defn -main
  [& args]
  (println "Hello, World!"))
