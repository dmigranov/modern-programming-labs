(ns lab2-2.core
  (:gen-class))


;площадь куска под функцией от a до b
(defn trapezoid-rule [f a b]
  ;(println (str "Calculating integral from "  a " to " b "..."))
  (* (- b a) (/ (+ (f a) (f b)) 2.)))



(defn lazy-list-of-integral-sums [func h]
   (iterate (fn [pair] (+ (second pair) (trapezoid-rule func (first pair) (+ (first pair) h)))) [0 0.])
  )

(defn integrate-lazy [func x h]



    )

(def fixed_h 0.1)
(defn integration-operator
  ([func step] (fn [x]
                 (integrate-lazy func x step)))
  ([func] (integration-operator func fixed_h)))



(defn -main
  [& args]
  (println "Hello, World!"))
