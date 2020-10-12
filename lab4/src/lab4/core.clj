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
  (second var))

(defn same-variables? [v1 v2]
  (and 
   (variable? v1)
   (variable? v2)
   (= (variable-name v1) (variable-name v2))))

;constant logical values (false and true) as operators of arity zero...

(defn disjunction [expr rest]
  (cons ::disj (cons expr rest)))

(defn disjunction? [expr] 
  (= ::disj (first expr)))

(defn conjunction [expr & rest]
  (cons ::conj (cons expr rest)))

(defn conjunction? [expr]
  (= ::conj (first expr)))

(defn implication [expr1 expr2]
  (list ::impl expr1 expr2))

(defn implication? [expr]
  (= ::impl (first expr)))

(defn negation [expr]
  (list ::neg expr))

(defn negation? [expr]
  (= ::neg (first expr)))

(defn args [expr] (rest expr))

(defn -main
  [& args]
  (println "Hello, World!"))
