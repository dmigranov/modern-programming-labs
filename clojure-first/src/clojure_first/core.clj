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
  (if (> (count words) 0)
    (concat (multiply-and-lenghten-word (first words) alphabet)
            (lengthen-words-by-one alphabet (rest words)))
    (list)))

(defn lengthen-words-by-one-tail 
  ([alphabet words] (lengthen-words-by-one-tail alphabet words (list)))
  ([alphabet words lengthened-words] 
   (if (> (count words) 0)
     (recur alphabet (rest words) (concat lengthened-words (multiply-and-lenghten-word (first words) alphabet)))
     lengthened-words)) ;else
  )


(defn lengthen-words-recursively [alphabet words n]
  (cond
    (> n 2) (lengthen-words-recursively alphabet (lengthen-words-by-one alphabet words) (dec n))
    (= n 2) (lengthen-words-by-one alphabet words)
    :else words))

(defn lengthen-words-recursively-tail [alphabet words n]
  (cond
    (> n 2) (recur alphabet (lengthen-words-by-one alphabet words) (dec n))
    (= n 2) (lengthen-words-by-one alphabet words)
    :else words))


(defn generate-atomic-words [alphabet]
  (
  if (> (count alphabet) 0)
    (cons (str (first alphabet)) (generate-atomic-words (rest alphabet)))
    (list)
  )
)

(defn generate-atomic-words-tail 
  ([alphabet] (generate-atomic-words-tail alphabet (list)))
  ([alphabet atomic-words]
   (if (> (count alphabet) 0)
     (recur (rest alphabet) (concat atomic-words (list (str (first alphabet)))))
     atomic-words ;else
     )))


(defn generate-words [alphabet n] ;!!!entry!!!
  (lengthen-words-recursively alphabet (generate-atomic-words alphabet) n)
)

(defn generate-words-tail [alphabet n] ;!!!entry!!!
  (lengthen-words-recursively-tail alphabet (generate-atomic-words-tail alphabet) n))


(defn -main [& args]
  (println (generate-words '(\a \b \c) 3))
)