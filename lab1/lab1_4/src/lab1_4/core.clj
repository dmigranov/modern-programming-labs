(ns lab1-4.core
  (:gen-class))

(defn generate-atomic-words [alphabet]
  (map (fn [char] (str char)) alphabet))

(defn filter-alphabet [alphabet word]
  (filter
   (fn [alphabet-letter] (not= (get word 0) alphabet-letter)) ;predicate
   alphabet))

(defn generate-lengthened-words [word alphabet] ; 1 слово -> (|alphabet| - 1) удлиненных слов
  (reduce (fn [coll letter] (concat coll (list (str letter word))))
          (list)
          (filter-alphabet alphabet word)) 
  )

(defn reduce-func [words alphabet]
  ; будет список списков, поэтому надо их все конкатенировать
  (reduce concat (map (fn [word]
                        (generate-lengthened-words word alphabet)) words)))

(defn my-repeat [n elem]
  (map (fn [num] elem) (range 0 n))) 

(defn generate-words [alphabet n] 
  (reduce reduce-func (generate-atomic-words alphabet) (my-repeat (dec n) alphabet))
  ;(reduce reduce-func (my-repeat n (generate-atomic-words alphabet))) ;можно так но надо переделать filter-alphabet
  )

(defn -main
  [& args]
  (println (generate-words '(\a \b \c \d) 5)))
