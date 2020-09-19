(ns clojure-first.core
  (:gen-class))

(defn multiply-word [word alphabet]
  (if (> (count alphabet) 0)
    (concat
     (if (not= (get word 0) (first alphabet)) (list (str (first alphabet) word)) (list))
     (multiply-word word (rest alphabet))
     )
    (list))
)

(defn traverse-list [alphabet words]
  (
   if (> (count words) 0)
    (concat (multiply-word (first words) alphabet)
          (traverse-list alphabet (rest words)))
    (list)
  )
)

(defn lengthen-words [alphabet words n]
  (cond 
    (> n 2) (lengthen-words alphabet (traverse-list alphabet words) (dec n))
    (= n 2) (traverse-list alphabet words)
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
  (lengthen-words alphabet (generate-atomic-words alphabet) n)
)

(defn -main [& args]
  (println (generate-words '(\r \s) 2))
)