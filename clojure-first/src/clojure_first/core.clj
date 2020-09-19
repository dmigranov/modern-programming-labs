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
    (cons)    ;then: traverse-list?
    list    ;else
    )

  (list n (inc n))
)

;todo
;;then: взять words с n-1  и прибавить буковки слева справа
(defn generate-word-list
  [alphabet word n]
  (let [first-letter (get word 0)]) ;char

    (if (> (count alphabet) 0)
      (cons (str (first alphabet) word)
              (generate-word-list (rest alphabet) word n)) 
      (list))    


)

(defn traverse-list [alphabet words n]
  (
   if (> (count words) 0)
    (concat (generate-word-list alphabet (first words) n)
          (traverse-list alphabet (rest words) n))
    (list)
  )
)

(defn generate-atomic-words
  [alphabet]
  (
  if (> (count alphabet) 0)
    (cons (str (first alphabet)) (generate-atomic-words (rest alphabet)))
    (list)
  )
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




