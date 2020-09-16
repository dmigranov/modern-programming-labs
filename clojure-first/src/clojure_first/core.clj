(ns clojure-first.core
  (:gen-class))



(defn fact
  [arg]
  (if (> arg 1) (* arg (fact (- arg 1))) 1))

(defn words
  [alphabet n]
  
  (if (> n 1)
    (cons )    ;then: взять words с n-1  и прибавить буковки слева справа
    alphabet    ;else
    )

  (list n (inc n))
  )


(defn generate-words
  [list n]

  (if (> n 1)
    (cons)    ;then: взять words с n-1  и прибавить буковки слева справа
    list    ;else
    )

  (list n (inc n)))


(defn generate-word-list
  [word n]

  (if (> n 1)
    (cons)    ;then: взять words с n-1  и прибавить буковки слева справа
    list    ;else
    )

  (list n (inc n)))

(defn traverse-list [words]
  (if (> (count words) 0)
    (concat (generate-word-list (first words) 0)

          (traverse-list (rest words)))

    (list)))



  



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def c 2)
  (def result (+ c c))
  
  (println (fact result))
  )




