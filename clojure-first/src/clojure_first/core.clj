(ns clojure-first.core
  (:gen-class))



(defn fact
  [arg]
  (if (> arg 1) (* arg (fact (- arg 1))) 1))


;todo
;;then: взять words с n-1  и прибавить буковки слева справа
(defn generate-word-list
  [alphabet word]
  (if (> (count alphabet) 0)
    (concat
     (if (not= (get word 0) (first alphabet)) (list (str (first alphabet) word)) (list))
     (generate-word-list (rest alphabet) word)
     )
    (list))
)

(defn traverse-list [alphabet words n]
  (
   if (> (count words) 0)
    (concat (generate-word-list alphabet (first words))
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




