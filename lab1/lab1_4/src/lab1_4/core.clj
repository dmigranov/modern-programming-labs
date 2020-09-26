(ns lab1-4.core
  (:gen-class))


(defn generate-atomic-words [alphabet]
  (map (fn [char] (str char)) alphabet))

(defn filter-alphabet [alphabet word]
  (filter
   (fn [alphabet-letter] (not= (get word 0) (alphabet-letter))) ;predicate
   alphabet))

;map reduce filter
(defn generate-words [alphabet n] 
  
  
  )


(defn -main
  [& args]
  (println "Hello, World!"))
