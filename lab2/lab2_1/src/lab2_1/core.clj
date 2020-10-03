(ns lab2-1.core
  (:gen-class))

(defn square [x] (* x x))
(defn exp [x] (Math/pow Math/E x))
(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))
(def pi Math/PI)


(def fixed_h 0.1)
(defn close [x y epsilon] (< (Math/abs (- x y)) epsilon))



(def h-eps 1e-5) ;множитель на который умножается эпсилон при проверке близости к сетке
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
    (if (close x (* k h) (* h h-eps))
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
    (if (close x (* k h) (* h h-eps))
      (calculate-integral-sum-no-memo func k h)
      (let [l (Math/floor (/ x h))]
        (+ (calculate-integral-sum-no-memo func l h) (trapezoid-rule func (* l h) x))))))

(defn integration-operator-no-memo
  ([func step] (fn [x]
                 (integrate-no-memo func x step)))
  ([func] (integration-operator-no-memo func fixed_h)))



(defn calc-integral-graphic-memo [func graphic-step end-x]
  (map (integration-operator-memo func) (range 0 (+ end-x graphic-step) graphic-step)))

(defn calc-integral-graphic-no-memo [func graphic-step end-x]
  (map (integration-operator-no-memo func) (range 0 (+ end-x graphic-step) graphic-step)))



(defn -main [& args]
  (println "Time testing:")
  (let [graphic-step 0.1
        border 100]
    (println (str "Graphic of cos from 0 to " border " with step " graphic-step " (integration step 0.1)"))
    (println "NO MEMO:")
    (println (time (calc-integral-graphic-no-memo cos graphic-step border)))
    (println "MEMO:")
    (println (time (calc-integral-graphic-memo cos graphic-step border)))
    
    
    ))