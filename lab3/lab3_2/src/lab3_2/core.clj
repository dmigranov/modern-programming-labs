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
  ;(seq x) is the recommended idiom for testing if a collection is not empty
  (lazy-seq (when-let [s (seq coll)]
              (cons (take n s) (my-partition n (drop n s))))))

(defn -main
  [& args]
  (println "Hello, World!"))
