(ns lab1-3.core
  (:gen-class))

;(reduce f coll)
;(reduce f val coll)

(defn my-map [func coll]    ;для одного списка
  (reduce 
   (fn [acc elem]
     (concat acc (list (func elem))))
   (list)
   coll
   ))

(defn my-filter [pred coll]
  (reduce
   (fn [acc elem]
     (if (pred elem)
       (concat acc (list elem)) ; then
       acc ;else
       ))
   (list)
   coll))

(defn -main
  [& args]
  (println (my-filter (fn [n] (= 0 (mod n 3))) (range 0 11)))
  (println (my-map (fn [x] (* x x)) (range 0 11)))
  )
