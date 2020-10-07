(ns lab2-2.core
  (:gen-class))


(defn exp [x] (Math/pow Math/E x))
(defn sin [x] (Math/sin x))
(defn cos [x] (Math/cos x))


(defn trapezoid-rule [f a b]
  ;(println (str "Calculating integral from "  a " to " b "..."))
  (* (- b a) (/ (+ (f a) (f b)) 2.)))


(defn lazy-list-of-integral-sums [func h]
  ;можно хранить пары не с n, а сразу nh?
   (iterate (fn [pair] [(inc (first pair)) (+ (second pair) (trapezoid-rule func (* (first pair) h) (* (inc (first pair)) h)))]) [0 0.])
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
