(ns lab2-1.core
  (:gen-class))


;0 to x: a = 0, b = x; h - шаг, n = x/h, тогда точки нумеруются [0, n] (включая n)
;func(x)
(defn integrate-func [func x h]
  
  (* h (+ (/ (+ (func 0) (func x)) 2.) (reduce + ())))
  )


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
