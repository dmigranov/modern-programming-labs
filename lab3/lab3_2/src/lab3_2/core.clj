(ns lab3-2.core
  (:gen-class))

;lazy: iterate [текущая-часть, хвост]
(defn my-partition [n coll]
  (->>
   (iterate (fn [[current-part tail]] [(take n tail), (drop n tail)]) [(take n coll) (drop n coll)])
   (map first)
   (take k)) ;k - количество списков
  )


(defn -main
  [& args]
  (println "Hello, World!"))
