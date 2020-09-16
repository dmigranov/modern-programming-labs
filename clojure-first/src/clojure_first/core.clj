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


;todo
(defn generate-words
  [list n]

  (if (> n 1)
    (cons)    ;then: traverse-list?
    list    ;else
    )

  (list n (inc n)))


(defn generate-word-list
  [alphabet word n]
  (let [first-letter (get word 0)]) ;char
  (if (> n 1)
    (cons)    ;then: взять words с n-1  и прибавить буковки слева справа
    list    ;else
    )

  (list n (inc n)))

(defn generate-atomic-words
  [alphabet]
  (if (> (count alphabet) 0)
    (cons (str (first alphabet)) (generate-atomic-words (rest alphabet)))
    (list)))


(defn traverse-list [alphabet words n]
  (if (> (count words) 0)
    (concat (generate-word-list alphabet (first words) 0)
          (traverse-list alphabet (rest words) (dec n)))
    (list))
)

;!!!
(defn main-word-function
  [alphabet n]
  (traverse-list alphabet (generate-atomic-words alphabet) n)
  )


  



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def c 2)
  (def result (+ c c))
  
  (println (fact result))
  )




