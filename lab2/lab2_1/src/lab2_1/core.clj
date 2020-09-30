(ns lab2-1.core
  (:gen-class))


;0 to x: a = 0, b = x; h - шаг, n = x/h, тогда точки нумеруются [0, n] (включая n)
;func(x)
;h = x/n
(defn integrate-simple [func x n]
  (let [h (double (/ x n))]
    (* h (reduce + (map (fn [grid-number]
                          (if (or (= grid-number 0) (= grid-number n))
                            (/ (func (* grid-number h)) 2.) ;then
                            (func (* grid-number h))))        ;else
                        (range 0 (inc n)))))
    ))

;идея
;Есть fixed_step - Допустим, 1 - это основная сетка
;но если мы тыкаем в 1.5 то в качестве шага надо взять 0.5, базируясь на имеющейся уже (возможно) 1



(defn integrate-memo [func x n]
 (let [h (double (/ x n))]
   
   
   
   ))

(defn integration-operator f [func]
  (fn integrated-func [x]
    
    ))



(defn square [x] (* x x))

(defn exp [x] (Math/pow Math/E x))

(defn sin [x] (Math/sin x))

(def pi Math/PI)

(defn -main
  [& args]
  (println "Hello, World!"))
