(ns lab1-4.core
  (:gen-class))

(defn generate-atomic-words [alphabet]
  (map (fn [char] (str char)) alphabet))

(defn filter-alphabet [alphabet word]
  (filter
   (fn [alphabet-letter] (not= (get word 0) alphabet-letter)) ;predicate
   alphabet))

(defn generate-lengthened-words [word alphabet]
  (reduce (fn [coll letter] (concat coll (list (str letter word)))) (list) (filter-alphabet alphabet word)) 
  )

(defn reduce-func [words alphabet]
  (reduce concat (map (fn [word]
                  (generate-lengthened-words word alphabet)) words)))

(defn my-repeat [n elem]
  (map (fn [num] elem) (range 0 n))) 

;map reduce filter
(defn generate-words [alphabet n] 
  (reduce reduce-func (generate-atomic-words alphabet) (my-repeat (dec n) alphabet))
  ;reduce - подаем на вход список атомарных букв?
  )

(defn -main
  [& args]
  (println "Hello, World!"))
