(ns lab2-1.core
  (:gen-class))

(defn square [x] (* x x))

(defn exp [x] (Math/pow Math/E x))

(defn sin [x] (Math/sin x))

(def pi Math/PI)

;0 to x: a = 0, b = x; h - шаг, n = x/h (h = x/n), тогда точки нумеруются [0, n] (включая n)
(defn integrate-no-memo [func x n]
  (let [h (double (/ x n))]
    (* h (reduce + (map (fn [grid-number]
                          (if (or (= grid-number 0) (= grid-number n))
                            (/ (func (* grid-number h)) 2.) ;then
                            (func (* grid-number h))))        ;else
                        (range 0 (inc n)))))
    ))

;даёт площадь куска от a до b - не надо мемоизировать, мемоизировать над суммы таких кусков?
(defn trapezoid-rule [f a b]
  (println (str "Calculating integral from "  a " to " b "..."))
  (* (- b a) (/ (+ (f a) (f b)) 2.)))

;идея
;Есть fixed_step - Допустим, 1 - это основная сетка
;но если мы тыкаем в 1.5 то в качестве шага надо взять 0.5, базируясь на имеющейся уже (возможно) 1


;тупой вариант
;в предположении что функции вызывается только для точек лежащих на сетке
;значит x - гарантированно делится на h; в качестве значений берем и мемоизируем kh
(def calculate-integral-sum-simple (memoize (fn [func k h] 
                                      (if (> k 0)
                                        (+ (trapezoid-rule func (* (dec k) h) (* k h)) (calculate-integral-sum-simple func (dec k) h))
                                        0))))

(defn integrate-memo-simple [func x h]
  (let [k (Math/round (/ x h))]
    (calculate-integral-sum-simple func k h)
    ))

(def fixed_h 0.1)

(defn integration-operator-simple [func]
  (fn [x]
    (integrate-memo-simple func x fixed_h)
    ))


(defn -main
  [& args]
  (println "Hello, World!"))
