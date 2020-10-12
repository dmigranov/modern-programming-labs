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

(defn disjunction [expr & rest]
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

(def log-true (list ::true))

(defn log-true? [expr] 
  (= ::true (first expr)))

(def log-false (list ::false))

(defn log-false? [expr]
  (= ::false (first expr)))

(defn args [expr] (rest expr))


;избавление ото всех нестандартных операций типа импликации
;в виде списка чтобы можно было добавлять новые
;проблема: так не получится с импликацийй вложенной внутрь!
;надо все правила проработать
(declare to-dnf-tier-1)
(def tier-1-rules (list
                   [(fn [expr] (implication? expr))
                    (fn [expr] (let [e-args (args expr), x (first e-args), y (second e-args)] (disjunction (negation (to-dnf-tier-1 x)) (to-dnf-tier-1 y))))]
                   [(fn [expr] (conjunction? expr))
                    (fn [expr] (let [e-args (args expr)] (apply conjunction (map to-dnf-tier-1 e-args))))]
                   [(fn [expr] (disjunction? expr))
                    (fn [expr] (let [e-args (args expr)] (apply disjunction (map to-dnf-tier-1 e-args))))]
                   [(fn [expr] (negation? expr))
                    (fn [expr] (let [arg (second expr)] (negation (to-dnf-tier-1 arg))))]
                   [(fn [expr] (or (variable? expr) (log-true? expr) (log-false? expr)))
                    (fn [expr] expr)])) 

;а тут может можно объединить законы де моргана и двойное отрицание?
(declare to-dnf-tier-2)
(def tier-2-rules (list
                   [(fn [expr] (and (negation? expr) (conjunction? (second expr))))
                    (fn [expr] (let [neg-arg (second expr)] (apply disjunction (->> (args neg-arg)
                                                                                  (map to-dnf-tier-2)
                                                                                  (map (fn [elem] (negation elem)))))))]
                   [(fn [expr] (and (negation? expr) (disjunction (second expr))))
                    (fn [expr] expr)]  ; TODO



                   [(fn [expr] (conjunction? expr))
                    (fn [expr] (let [e-args (args expr)] (apply conjunction (map to-dnf-tier-2 e-args))))]
                   [(fn [expr] (disjunction? expr))
                    (fn [expr] (let [e-args (args expr)] (apply disjunction (map to-dnf-tier-2 e-args))))]
                   [(fn [expr] (negation? expr))
                    (fn [expr] (let [arg (second expr)] (negation (to-dnf-tier-2 arg))))]
                   [(fn [expr] (or (variable? expr) (log-true? expr) (log-false? expr)))
                    (fn [expr] expr)]))



(defn to-dnf-tier [expr rules]
  ((some (fn [[rule-cond rule-transform]]
           (if (rule-cond expr)
             rule-transform ; это функция
             false)) rules) expr))

(defn to-dnf-tier-1 [expr] (to-dnf-tier expr tier-1-rules))
(defn to-dnf-tier-2 [expr] (to-dnf-tier expr tier-2-rules))

(defn to-dnf [expr]
  (->> expr
       to-dnf-tier-1
       to-dnf-tier-2
       ))



;todo signify expr var val

(defn -main
  [& args]
  (println "Hello, World!"))
