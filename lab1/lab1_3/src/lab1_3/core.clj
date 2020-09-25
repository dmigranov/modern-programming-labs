(ns lab1-3.core
  (:gen-class))

;(reduce f coll)
;(reduce f val coll)
; использовать в качестве функции reduce - concat

(defn my-map [func coll]    ;для одного списка

  nil)

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
  (println (my-filter (fn [n] (= 0 (mod n 3))) (range 0 11))))
