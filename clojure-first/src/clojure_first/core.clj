(ns clojure-first.core
  (:gen-class))

(defn multiply-and-lenghten-word [word alphabet]
  (if (> (count alphabet) 0)
    (concat
     (if (not= (get word 0) (first alphabet)) (list (str (first alphabet) word)) (list))
     (multiply-and-lenghten-word word (rest alphabet))
     )
    (list))
)

(defn lengthen-words-by-one [alphabet words]
  (
   if (> (count words) 0)
    (concat (multiply-and-lenghten-word (first words) alphabet)
          (lengthen-words-by-one alphabet (rest words)))
    (list)
  )
)

(defn lengthen-words-recursively [alphabet words n]
  (cond 
    (> n 2) (lengthen-words-recursively alphabet (lengthen-words-by-one alphabet words) (dec n))
    (= n 2) (lengthen-words-by-one alphabet words)
    :else words
    )
)

(defn generate-atomic-words [alphabet]
  (
  if (> (count alphabet) 0)
    (cons (str (first alphabet)) (generate-atomic-words (rest alphabet)))
    (list)
  )
)

(defn generate-words [alphabet n] ;!!!entry!!!
  (lengthen-words-recursively alphabet (generate-atomic-words alphabet) n)
)

(defn -main [& args]
  (println (generate-words '(\a \b \c) 3))
)