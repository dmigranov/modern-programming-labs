(ns clojure-first.core
  (:gen-class))

(defn generate-word-list
  [alphabet word]
  (if (> (count alphabet) 0)
    (concat
     (if (not= (get word 0) (first alphabet)) (list (str (first alphabet) word)) (list))
     (generate-word-list (rest alphabet) word)
     )
    (list))
)

(defn traverse-list [alphabet words]
  (
   if (> (count words) 0)
    (concat (generate-word-list alphabet (first words))
          (traverse-list alphabet (rest words)))
    (list)
  )
)

(defn lenghthen-words [alphabet words n]
  (cond 
    (> n 2) (lenghthen-words alphabet (traverse-list alphabet words) (dec n))
    (= n 2) (traverse-list alphabet words)
    :else words
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
  ;(traverse-list alphabet (generate-atomic-words alphabet) n)
  (lenghthen-words alphabet (generate-atomic-words alphabet) n)
  )

(defn -main
  [& args]

  (println (main-word-function '(\r \s) 2))
  )




