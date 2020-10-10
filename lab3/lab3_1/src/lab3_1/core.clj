(ns lab3-1.core
  (:gen-class))

(def thread-count 4)

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


(defn my-filter-future [pred coll]
  (my-partition (Math/ceil (/ (count coll) thread-count)) coll)
  )


;deref = @
;When applied to a future, will block if computation not complete. then return value

(defn -main
  [& args]
  (println "Hello, World!"))
