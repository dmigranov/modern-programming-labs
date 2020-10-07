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
  (->>
   ;(iterate (fn [pair] [(inc (first pair)) (+ (second pair) (trapezoid-rule func (* (first pair) h) (* (inc (first pair)) h)))]) [0 0.]) ;[nh, ...]
   (iterate (fn [pair] (let [new-x (+ (first pair) h)]
                         [new-x
                          (+ (second pair) (trapezoid-rule func (first pair) new-x))])) 
            [0., 0.])
   (map second))
  )

(defn integrate-lazy [func x h]
  (let [k (Math/round (/ x h))
        lazy-sums (lazy-list-of-integral-sums func h)]
    
    (if (close x (* k h) (* h h-eps))
      (nth lazy-sums k) ;then - просто тупо берем из списка
      () ;else - берем из списка и добавляем кусок)
      )
    
    
    ))

(defn integration-operator
  ([func step] (fn [x]
                 (integrate-lazy func x step)))
  ([func] (integration-operator func fixed_h)))



(defn -main
  [& args]
  (println "Hello, World!"))
