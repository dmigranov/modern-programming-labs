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

;даёт площадь куска под функцией от a до b
(defn trapezoid-rule [f a b]
  (* (- b a) (/ (+ (f a) (f b)) 2.)))

(defn trapezoid-rule-debug [f a b]
  (println (str "Calculating integral from "  a " to " b "..."))
  (trapezoid-rule f a b))

(defn calculate-integral-sum-no-memo [func k h]
  (if (> k 0)
    (+ (trapezoid-rule-debug func (* (dec k) h) (* k h)) (calculate-integral-sum-no-memo func (dec k) h))
    0))

(def calculate-integral-sum-memo (memoize (fn [func k h] 
                                      (if (> k 0)
                                        (+ (trapezoid-rule func (* (dec k) h) (* k h)) (calculate-integral-sum-memo func (dec k) h))
                                        0))))

;тупой вариант
;в предположении что функции вызывается только для точек лежащих на сетке
;значит x - гарантированно делится на h; в качестве значений берем и мемоизируем kh
(defn integrate-memo-simple [func x h]
  (let [k (Math/round (/ x h))]
    (calculate-integral-sum-memo func k h)
    ))

;похоже, но если мы тыкаем за сетку, то досчитывается и прибавляется значение от ближайшего значения сетки 
;(но оно не мемоизируется, мемоизируется только значения интеграал для узлов сетки)
(defn integrate-memo [func x h]
  (let [k (Math/round (/ x h))]
    ;todo
    (calculate-integral-sum-memo func k h)
    ))

(def fixed_h 0.1)
(defn integration-operator
  ([func step] (fn [x]
                 (integrate-memo func x step)))
  ([func] (integration-operator func fixed_h))
  )


(defn -main
  [& args]
  (println "Hello, World!"))
