(ns lab1-4.core
  (:gen-class))


(defn generate-atomic-words [alphabet]
  (map (fn [char] (str char)) alphabet))

(defn filter-alphabet [alphabet word]
  (filter
   (fn [alphabet-letter] (not= (get word 0) alphabet-letter)) ;predicate
   alphabet))

;поступает на вход список ("a" "b" "c")
(defn reduce-func [atomic-words] 
  
  )

(defn my-repeat [n elem]
  (map (fn [num] elem) (range 0 n))) 

;map reduce filter
(defn generate-words [alphabet n] 
  (reduce reduce-func (my-repeat n (generate-atomic-words alphabet)))
  ;reduce - подаем на вход список атомарных букв?
  )


(defn -main
  [& args]
  (println "Hello, World!"))
