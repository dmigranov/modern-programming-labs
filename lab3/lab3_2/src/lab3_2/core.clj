(ns lab3-2.core
  (:gen-class))

;lazy: iterate [текущая-часть, хвост]
;(defn my-partition [n coll]
;  (->>
;   (iterate (fn [[current-part tail]] [(take n tail), (drop n tail)]) [(take n coll) (drop n coll)])
;   (map first)
;   (map (fn [elem] (if (= elem (list)) nil elem)))
;   ) ;если конечная - то take k - количество списков
;  )
;превращает конечную в бесконечную...

(defn my-partition [n coll]
(lazy-seq ))
;todo


(defn -main
  [& args]
  (println "Hello, World!"))
