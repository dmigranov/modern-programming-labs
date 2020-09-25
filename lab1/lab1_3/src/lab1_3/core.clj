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
      (conj c elem) ; then
      c ;else
      )
    )
  (list)
  coll
  ))

(defn -main
  [& args]
  (println "Hello, World!"))
