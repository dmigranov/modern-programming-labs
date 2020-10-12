(ns lab4.core
  (:gen-class))
; :  - keyword
; :: - keyword в пользовательском неймспейсу

(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  (= (first expr) ::var))

(defn variable-name [var]
  (second name))

(defn same-variables? [v1 v2]
  (and 
   (variable? v1)
   (variable? v2)
   (= (variable-name v1) (variable-name v2))))

;constant logical values (false and true) as operators of arity zero...

(defn log-or [expr1 expr2 & rest]
  (concat (list ::or expr1 expr2) rest))

(defn log-or? [expr] 
  (= ::or (first expr)))

(defn log-and [expr1 expr2 & rest]
  (concat (list ::and expr1 expr2) rest))

(defn log-and? [expr]
  (= ::and (first expr)))

(defn args [expr] (rest expr))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
