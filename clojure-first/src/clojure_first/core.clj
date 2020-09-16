(ns clojure-first.core
  (:gen-class))



(defn fact
  [arg]
  (if (> arg 1) (* arg (fact (- arg 1))) 1))

(defn words
  [alphabet n]
  (let [cardinality (count alphabet)])
  (if (> n 1)
    ()    ;then: взять words с n-1  и прибавить буковки слева справа
    (alphabet)    ;else
    )

  (list n (inc n))
  )

(defn generate-word-list
  [alphabet n word]
  (if (< (.length word) n)
    ()   ;then
    (list)   ;else
  )
  )
  



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def c 2)
  (def result (+ c c))
  
  (println (fact result))
  )




