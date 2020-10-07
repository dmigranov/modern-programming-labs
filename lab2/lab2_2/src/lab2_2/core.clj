(ns lab2-2.core
  (:gen-class))


(defn exp [x] (Math/pow Math/E x))
(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))





(def fixed_h 0.1)
(defn close [x y epsilon] (< (Math/abs (- x y)) epsilon))
(def h-eps 1e-5) ;множитель на который умножается эпсилон при проверке близости к сетке



(defn trapezoid-rule [f a b]
  ;(println (str "Calculating integral from "  a " to " b "..."))
  (* (- b a) (/ (+ (f a) (f b)) 2.)))

(defn lazy-list-of-integral-sums [func h]
  ;(println "lazy-list-of-integral-sums called")
  (->>
   ;(iterate (fn [pair] [(inc (first pair)) (+ (second pair) (trapezoid-rule func (* (first pair) h) (* (inc (first pair)) h)))]) [0 0.]) ;[nh, ...]
   (iterate (fn [pair] (let [new-x (+ (first pair) h)]
                         [new-x
                          (+ (second pair) (trapezoid-rule func (first pair) new-x))]))
            [0., 0.])
   (map second)))

;(declare lazy-sums)
(defn integrate-lazy [func x h lazy-sums]
  (let [k (Math/round (/ x h))]
    (if (close x (* k h) (* h h-eps))
      (nth lazy-sums k) ;then - просто тупо берем из списка
      (let [l (Math/floor (/ x h))] (+ (nth lazy-sums l) (trapezoid-rule func (* l h) x))))))

; два варианта
; 1-ый (плохой, тк считает повторно):
; lazy-list-of-integral-sums вызывается много раз
(defn integration-operator-slow
  ([func step] (fn [x] (integrate-lazy func x step (lazy-list-of-integral-sums func step))))
  ([func] (integration-operator-slow func fixed_h)))

; 2-ый - повторного пересчитывания нет, все считается оптимально:
; (lazy-list-of-integral-sums func step) вызывается единожды для всех вызовов
(defn integration-operator
  ([func step lazy-sums] (fn [x] (integrate-lazy func x step lazy-sums)))
  ([func step] (integration-operator func step (lazy-list-of-integral-sums func step)))
  ([func] (integration-operator func fixed_h)))

(defn calc-integral-graphic-lazy [func integration-step graphic-step end-x]
  (map (integration-operator func integration-step) (range 0 (+ end-x graphic-step) graphic-step)))

(defn calc-integral-graphic-lazy-slow [func integration-step graphic-step end-x]
  (map (integration-operator-slow func integration-step) (range 0 (+ end-x graphic-step) graphic-step)))





(defn calculate-integral-sum-no-opti [func k h]
  (if (> k 0)
    (+ (trapezoid-rule func (* (dec k) h) (* k h)) (calculate-integral-sum-no-opti func (dec k) h))
    0))

(defn integrate-no-opti [func x h]
  (let [k (Math/round (/ x h))]
    (if (close x (* k h) (* h h-eps))
      (calculate-integral-sum-no-opti func k h)
      (let [l (Math/floor (/ x h))]
        (+ (calculate-integral-sum-no-opti func l h) (trapezoid-rule func (* l h) x))))))

(defn integration-operator-no-opti
  ([func step] (fn [x]
                 (integrate-no-opti func x step)))
  ([func] (integration-operator-no-opti func fixed_h)))

(defn calc-integral-graphic-no-opti [func integration-step graphic-step end-x]
  (map (integration-operator-no-opti func integration-step) (range 0 (+ end-x graphic-step) graphic-step)))


(defn -main [& args]
  (println "Time testing:")
  (let [graphic-step 0.1
        integration-step 0.1
        border 100]
    (println (str "Graphic of cos from 0 to " border " with step " graphic-step "; integration step " integration-step))
    (doall (calc-integral-graphic-no-opti cos integration-step graphic-step 0.2))
    (doall (calc-integral-graphic-lazy cos integration-step graphic-step 0.2))

    (println "LAZY SEQ AND ITERATE:")
    (time (doall (calc-integral-graphic-lazy cos integration-step graphic-step border)))

    (println "NO LAZY:")
    (time (doall (calc-integral-graphic-no-opti cos integration-step graphic-step border)))))
