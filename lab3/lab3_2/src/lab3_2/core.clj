(ns lab3-2.core
  (:gen-class))

(def base-thread-number 4)

;for testing
(def naturals
  (lazy-seq
   (cons 1 (map inc naturals))))

(defn my-filter [pred coll] 
  (reduce
   (fn [acc elem]
     (if (pred elem)
       (concat acc (list elem))
       acc))
   (list) coll))
;(def filt-nat (my-filter even? naturals))
;(take 10 filt-nat)
;Error printing return value (StackOverflowError) (он пытается реализовать всю коллекцию в reduce)
;todo: make lazy

(defn my-filter-lazy [pred coll]
  (lazy-seq (when-let [s (seq coll)]
              (if (pred (first s))
                (cons (first s) (my-filter-lazy pred (rest s)))
                (my-filter-lazy pred (rest s))))))
;а тут все работает


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

;(defn my-filter-future-lazy
;  ([pred coll thread-number] (->>
;                              ))
;  ([pred coll] (my-filter-future-lazy pred coll base-thread-number)))


(defn -main
  [& args]
  (println "Hello, World!"))
