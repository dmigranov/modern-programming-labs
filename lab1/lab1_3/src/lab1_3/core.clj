(ns lab1-3.core
  (:gen-class))

;(reduce f coll)
;(reduce f val coll)

(defn my-map [func coll]    ;для одного списка
  (reduce 
   (fn [c elem]
     (concat c (list (func elem))))
   (list)
   coll
   ))

(defn my-filter [pred coll]
  (reduce
   (fn [c elem]
     (if (pred elem)
       (concat c (list elem)) ; then
       c ;else
       ))
   (list)
   coll))

(defn -main
  [& args]
  (println (my-filter (fn [n] (= 0 (mod n 3))) (range 0 11)))
  
  
  )
