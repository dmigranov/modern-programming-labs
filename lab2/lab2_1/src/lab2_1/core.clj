(ns lab2-1.core
  (:gen-class))

(defn square [x] (* x x))
(defn exp [x] (Math/pow Math/E x))
(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(def pi Math/PI)

;даёт площадь куска под функцией от a до b
(defn trapezoid-rule [f a b]
  ;(println (str "Calculating integral from "  a " to " b "...")) ;закоментить при замерах времени
  (* (- b a) (/ (+ (f a) (f b)) 2.)))


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

(def fixed_h 0.1)
(def epsilon (* fixed_h 1e-5))
(defn close [x y]
  (< (Math/abs (- x y)) epsilon)
  )

;похоже, но если мы тыкаем за сетку, то досчитывается и прибавляется значение от ближайшего значения сетки 
;(но оно не мемоизируется, мемоизируется только значения интеграал для узлов сетки)
(defn integrate-memo [func x h]
  (let [k (Math/round (/ x h))] ;это если будет, например, x = 7.9999 при h = 1. сокращать вниз сразу нельзя
    (if (close x (* k h))
      (calculate-integral-sum-memo func k h) ;если пренебрежимо близко - то попали в сетку
      (let [l (Math/floor (/ x h))]    ;иначе округляем x/h вниз (l) и считаем интеграл от l*h до x и плюсуем 
        (+
         (calculate-integral-sum-memo func l h)
         (trapezoid-rule func (* l h) x) ;интеграл от l*h до x
         )))))

(defn integration-operator
  ([func step] (fn [x]
                 (integrate-memo func x step)))
  ([func] (integration-operator func fixed_h))
  )





(defn calculate-integral-sum-no-memo [func k h]
  (if (> k 0)
    (+ (trapezoid-rule func (* (dec k) h) (* k h)) (calculate-integral-sum-no-memo func (dec k) h))
    0))

(defn integrate-no-memo [func x h]
  )

(defn integration-operator-no-memo
  ([func step] (fn [x]
                 (integrate-no-memo func x step)))
  ([func] (integration-operator-no-memo func fixed_h)))


(defn -main
  [& args]
  (let [values (range 0.0 0.5 0.02)]
    (println "e^x: expected vs what we got")
    (println (map dec (map exp values)))
    (println (map (integration-operator exp) values))
    (println "cos(x): expected vs what we got")
    (println (map sin values)) ;интеграл косинуса - синус
    (println (map (integration-operator cos) values))))
