(ns lab2-1.core
  (:gen-class))


;0 to x: a = 0, b = x; h - шаг, n = x/h, тогда точки нумеруются [0, n] (включая n)
;func(x)
;h = x/n
(defn integrate-func [func x n]
  (let [h (double (/ x n))]
    (* h (reduce + (map (fn [grid-number]
                          (if (or (= grid-number 0) (= grid-number n))
                            (/ (func (* n h)) 2.) ;then
                            (func (* n h))))        ;else
                        (range 0 (inc n)))))
    ))

(defn square [x] (* x x))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
