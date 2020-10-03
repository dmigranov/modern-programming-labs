(ns lab2-1.core
  (:gen-class))

(defn square [x] (* x x))
(defn exp [x] (Math/pow Math/E x))
(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(def pi Math/PI)


(def fixed_h 0.1)
(defn close [x y epsilon] (< (Math/abs (- x y)) epsilon))


;площадь куска под функцией от a до b
(defn trapezoid-rule [f a b]
  ;(println (str "Calculating integral from "  a " to " b "...")) ;закоментить при замерах времени
  (* (- b a) (/ (+ (f a) (f b)) 2.)))


(def calculate-integral-sum-memo (memoize (fn [func k h]
                                            (if (> k 0)
                                              (+ (trapezoid-rule func (* (dec k) h) (* k h)) (calculate-integral-sum-memo func (dec k) h))
                                              0))))
(defn integrate-memo [func x h]
  (let [k (Math/round (/ x h))] ;это если будет, например, x = 7.9999 при h = 1. сокращать вниз сразу нельзя
    (if (close x (* k h) (* h 1e-5))
      (calculate-integral-sum-memo func k h) ;если пренебрежимо близко - то попали в сетку
      (let [l (Math/floor (/ x h))]    ;иначе округляем x/h вниз (l) и считаем интеграл от l*h до x и плюсуем 
        (+ (calculate-integral-sum-memo func l h) (trapezoid-rule func (* l h) x))))))

(defn integration-operator-memo
  ([func step] (fn [x]
                 (integrate-memo func x step)))
  ([func] (integration-operator-memo func fixed_h)))



(defn calculate-integral-sum-no-memo [func k h]
  (if (> k 0)
    (+ (trapezoid-rule func (* (dec k) h) (* k h)) (calculate-integral-sum-no-memo func (dec k) h))
    0))

(defn integrate-no-memo [func x h]
  (let [k (Math/round (/ x h))]
    (if (close x (* k h) (* h 1e-5))
      (calculate-integral-sum-no-memo func k h)
      (let [l (Math/floor (/ x h))]
        (+ (calculate-integral-sum-no-memo func l h) (trapezoid-rule func (* l h) x))))))

(defn integration-operator-no-memo
  ([func step] (fn [x]
                 (integrate-no-memo func x step)))
  ([func] (integration-operator-no-memo func fixed_h)))



(defn calc-graphic-memo [h end-x]
  
  )

(defn calc-graphic-no-memo [h end-x]
  
  )


(defn -main
  [& args]
  (let [values (range 0.0 0.5 0.02)]
    ;откомментить в правиле трапеции
    (println "No memo vs memo")
    (println "No memo:")
    ((integration-operator-no-memo exp) 1.35)
    ((integration-operator-no-memo exp) 1.85)
    (println "Memo:")
    ((integration-operator-memo exp) 1.35)
    ((integration-operator-memo exp) 1.85)
    ))


