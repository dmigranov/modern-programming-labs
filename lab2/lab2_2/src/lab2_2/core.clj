(ns lab2-2.core
  (:gen-class))


;площадь куска под функцией от a до b
(defn trapezoid-rule [f a b]
  ;(println (str "Calculating integral from "  a " to " b "..."))
  (* (- b a) (/ (+ (f a) (f b)) 2.)))



(defn integrate-lazy [func x h]

  (let 
   [
    ;lazy-integral-sums (lazy-seq ())
    lazy-integral-sums (iterate (fn [current-integral-sum] (+ current-integral-sum)) 0)
    ]
    
    
    
    
    ))

(def fixed_h 0.1)
(defn integration-operator
  ([func step] (fn [x]
                 (integrate-lazy func x step)))
  ([func] (integration-operator func fixed_h)))



(defn -main
  [& args]
  (println "Hello, World!"))
