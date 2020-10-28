(ns lab4.core
  (:gen-class))
; :  - keyword
; :: - keyword в пользовательском неймспейсу

(defn ^{:doc "Creates a variable"}
  variable [name]
  {:pre [(keyword? name)]}
  (list :var name))

(defn ^{:doc "Checks if expr is a variable"}
  variable?  [expr]
  (= (first expr) :var))

(defn ^{:doc "Returns variable name"} variable-name
  [var]
  (second var))

(defn ^{:doc "Checks if variables are the same"} same-variables?
  [v1 v2]
  (and
   (variable? v1)
   (variable? v2)
   (= (variable-name v1) (variable-name v2))))

(defn disjunction-internal [expr & rest]
  (cons :disj (cons expr rest)))

(defn ^{:doc "Creates binary disjunction"} disjunction-binary
  [expr1 expr2]
  (list :disj expr1 expr2))

(defn ^{:doc "Creates n-ary disjunction, if n = 1 returns expr"} disjunction
  [expr & others]
  (if (not-empty others)
    ((fn [expr1 expr2] (list :disj expr1 expr2)) expr (apply disjunction others))
    expr))

(defn ^{:doc "Checks if expr is a disjunction"} disjunction?
  [expr]
  (= :disj (first expr)))

(defn conjunction-internal [expr & rest]
  (cons :conj (cons expr rest)))  

(defn ^{:doc "Creates binary conjunction"} conjunction-binary
  [expr1 expr2]
  (list :conj expr1 expr2))

(defn ^{:doc "Creates n-ary conjunction, if n = 1 returns expr"} conjunction
  [expr & others]
  (if (not-empty others)
    ((fn [expr1 expr2] (list :conj expr1 expr2)) expr (apply conjunction others))
    expr))

(defn ^{:doc "Checks if expr is a conjunction"} conjunction?
  [expr]
  (= :conj (first expr)))

(defn ^{:doc "Creates implication"} implication
  [expr1 expr2]
  (list :impl expr1 expr2))

(defn ^{:doc "Checks if expr is a implication"} implication?
  [expr]
  (= :impl (first expr)))

(defn ^{:doc "Creates negation of expr"} negation
  [expr]
  (list :neg expr))

(defn ^{:doc "Checks if expr is a negation"} negation?
  [expr]
  (= :neg (first expr)))

(def ^{:doc "Returns logical true"} log-true (list :true))

(defn ^{:doc "Checks if expr is true"} log-true? [expr]
  (= :true (first expr)))

(def ^{:doc "Returns logical false"} log-false (list :false))

(defn ^{:doc "Checks if expr is false"} log-false? [expr]
  (= :false (first expr)))

(defn ^{:doc "Returns args of expr"} args [expr] (rest expr))

(defn ^{:doc "Checks if x is variable or variable's negation"} atomic-expression? [x] ;variable or variable's negation
  (or (variable? x) (and (negation? x) (variable? (first (args x))))))

;избавление ото всех нестандартных операций типа импликации
;в виде списка чтобы можно было добавлять новые (!!! РАСШИРЯЕМОСТЬ !!!)
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

(declare to-dnf-tier-2)
(def tier-2-rules (list
                   ;де моргана
                   
                   [(fn [expr] (and (negation? expr) (conjunction? (second expr))))
                    (fn [expr] (let [neg-arg (second expr)] (to-dnf-tier-2 (apply disjunction (->> (args neg-arg)
                                                                                                   (map (fn [elem] (negation elem))))))))]
                   [(fn [expr] (and (negation? expr) (disjunction? (second expr))))
                    (fn [expr] (let [neg-arg (second expr)] (to-dnf-tier-2 (apply conjunction (->> (args neg-arg)
                                                                                                   (map (fn [elem] (negation elem))))))))]
                   ;двойное отрицание
                   
                   [(fn [expr] (and (negation? expr) (negation? (second expr))))
                    (fn [expr] (let [arg (first (args (second expr)))] (to-dnf-tier-2 arg)))]
                   
                   [(fn [expr] (conjunction? expr))
                    (fn [expr] (let [e-args (args expr)] (apply conjunction (map to-dnf-tier-2 e-args))))]
                   [(fn [expr] (disjunction? expr))
                    (fn [expr] (let [e-args (args expr)] (apply disjunction (map to-dnf-tier-2 e-args))))]
                   [(fn [expr] (negation? expr))
                    (fn [expr] (let [arg (second expr)] (negation (to-dnf-tier-2 arg))))]
                   [(fn [expr] (or (variable? expr) (log-true? expr) (log-false? expr)))
                    (fn [expr] expr)]))

(declare to-dnf-tier-3)
(def tier-3-rules (list
                   ;дистрибутивность слева
                   ;(и (или a b) c)
                   [(fn [expr] (and (conjunction? expr) (disjunction? (second expr))))
                    (fn [expr] (let [conj-args (args expr), disj (first conj-args), disj-args (args disj)
                                     a (first disj-args), b (second disj-args), c (second conj-args)]
                                 (to-dnf-tier-3 (disjunction (conjunction a c) (conjunction b c)))))]

                   ;дистрибутивность справа
                   ;(и c (или a b))
                   [(fn [expr] (and (conjunction? expr) (disjunction? (nth expr 2))))
                    (fn [expr] (let [conj-args (args expr), disj (second conj-args), disj-args (args disj)
                                     a (first disj-args), b (second disj-args), c (first conj-args)]
                                 (to-dnf-tier-3 (disjunction (conjunction c a) (conjunction c b)))))]

                   [(fn [expr] (and (negation? expr) (negation? (second expr))))
                    (fn [expr] (let [arg (first (args (second expr)))] (to-dnf-tier-3 arg)))]
                   [(fn [expr] (conjunction? expr))
                    (fn [expr] (let [e-args (args expr)] (apply conjunction (map to-dnf-tier-3 e-args))))]
                   [(fn [expr] (disjunction? expr))
                    (fn [expr] (let [e-args (args expr)] (apply disjunction (map to-dnf-tier-3 e-args))))]
                   [(fn [expr] (negation? expr))
                    (fn [expr] (let [arg (second expr)] (negation (to-dnf-tier-3 arg))))]
                   [(fn [expr] (or (variable? expr) (log-true? expr) (log-false? expr)))
                    (fn [expr] expr)]))



(declare to-dnf-tier-unite)
(def tier-unite-rules (list

                       ;[(fn [expr] (and (conjunction? expr) (conjunction? (first (args expr)))))
                       ; (fn [expr] (let [conj (first (args expr)), conj-args (args conj)
                       ;                  a (first conj-args), b (second conj-args), c (second (args expr))]
                       ;              (to-dnf-tier-unite (conjunction-internal a b c))))]
                       ;[(fn [expr] (and (conjunction? expr) (conjunction? (second (args expr)))))
                       ; (fn [expr] (let [conj (second (args expr)), conj-args (args conj)
                       ;                  a (first conj-args), b (second conj-args), c (first (args expr))]
                       ;              (to-dnf-tier-unite (conjunction-internal c a b))))]

                       ;это плохо так как если много то перестает работать так как не обязательно на первом или втором месте

                       [(fn [expr] (and (conjunction? expr) (some conjunction? (args expr))))
                        (fn [expr] (let [conj (some (fn [elem] (if (conjunction? elem) elem nil)) (args expr)), conj-args (args conj)]
                                     (to-dnf-tier-unite (apply conjunction-internal (concat (remove (fn [elem] (= elem conj)) (args expr)) conj-args)))))]

                       [(fn [expr] (and (disjunction? expr) (some disjunction? (args expr))))
                        (fn [expr] (let [disj (some (fn [elem] (if (disjunction? elem) elem nil)) (args expr)), disj-args (args disj)]
                                     (to-dnf-tier-unite (apply disjunction-internal (concat (remove (fn [elem] (= elem disj)) (args expr)) disj-args)))))]


                       [(fn [expr] (conjunction? expr))
                        (fn [expr] (let [e-args (args expr)] (apply conjunction-internal (map to-dnf-tier-unite e-args))))]
                       [(fn [expr] (disjunction? expr))
                        (fn [expr] (let [e-args (args expr)] (apply disjunction-internal (map to-dnf-tier-unite e-args))))]
                       [(fn [expr] (negation? expr))
                        (fn [expr] (let [arg (second expr)] (negation (to-dnf-tier-unite arg))))]
                       [(fn [expr] (or (variable? expr) (log-true? expr) (log-false? expr)))
                        (fn [expr] expr)]))

(defn unnegate-variable [x]
    {:pre [(or (variable? x) (and (negation? x) (variable? (first (args x)))))]}
  (if (variable? x)
    x
    (first (args x)))
  )

(declare to-dnf-tier-sort)
(def tier-sort-rules (list
                       [(fn [expr] (conjunction? expr))
                        (fn [expr] (let [c-args (args expr)]
                                     (apply conjunction-internal (sort (fn [x y]
                                                                         (let [a (unnegate-variable x) b (unnegate-variable y)]
                                                                           (compare (variable-name a) (variable-name b)))) c-args))))]
                       
                       [(fn [expr] (disjunction? expr))
                        (fn [expr] (let [e-args (args expr)] (apply disjunction-internal (map to-dnf-tier-sort e-args))))]
                       
                      [(fn [expr] (atomic-expression? expr))
                       (fn [expr] expr)]
                      
                      ))


(defn to-dnf-tier [expr rules]
  ((some (fn [[rule-cond rule-transform]]
           (if (rule-cond expr)
             rule-transform ; это функция
             false)) rules) expr))

(defn to-dnf-tier-1 [expr] (to-dnf-tier expr tier-1-rules))
(defn to-dnf-tier-2 [expr] (to-dnf-tier expr tier-2-rules))
(defn to-dnf-tier-3 [expr] (to-dnf-tier expr tier-3-rules))
(defn to-dnf-tier-unite [expr] (to-dnf-tier expr tier-unite-rules))
(defn to-dnf-tier-sort [expr] (to-dnf-tier expr tier-sort-rules))


(defn simplify-disjunct-recur [simplified rest-conjuncts]
  (if (> (count rest-conjuncts) 1)
    (let [c1 (first rest-conjuncts)
          c2 (second rest-conjuncts)]
      (cond
        (= c1 c2) (recur simplified (concat (list c1) (drop 2 rest-conjuncts))) ;они оба одна переменная с одним знаком
        (= (unnegate-variable c1) (unnegate-variable c2)) log-false ;одна переменна, но с противоположными знаками
        :else (recur (concat simplified (list c1)) (rest rest-conjuncts)) ;разные
        ))
    (concat simplified rest-conjuncts) ;else
    ))

(defn simplify-disjunct [disjunct] ;x & not y & y ...
  (let [simplified (simplify-disjunct-recur (list :conj) (if (atomic-expression? disjunct)
                                                           (list disjunct)
                                                           (rest disjunct)))]
    (if (conjunction? simplified)
      (if (> (count (args simplified)) 1)
        simplified
        (second simplified))
      simplified)
    ))

(defn to-dnf-tier-simplify-disjuncts [expr] 
  ;todo: ошибка для одиночных переменных
  (let [disjuncts (args expr)]
    (apply disjunction-internal (map simplify-disjunct disjuncts)))
  )

;это необходимо, поскольку: ((a or b) and c) and d
;это раскорется при первом применении tier-3 в ((a and c) or (b and c)) and d
;но рекурсивно спускаясь мы пропустим возможность примениит это правило ко всему выражению!
(defn to-dnf-tier-3-cycle [expr]
 (let [new-expr (to-dnf-tier-3 expr)]
   (if (= new-expr expr)
     new-expr
     (recur new-expr))) 
   )


(defn ^{:doc "Returns DNF of expr"} to-dnf [expr]
  (->> expr
       to-dnf-tier-1
       to-dnf-tier-2 ;2 и 3 нельзя объединить в одну тк тогда могут быть не замечены некоторые правила дистрибутивности
       to-dnf-tier-3-cycle
       to-dnf-tier-unite
       to-dnf-tier-sort
       to-dnf-tier-simplify-disjuncts

       ;tier4 - поиск одинаковых переменных, плюс избавление от единиц и нулей?
       ))

(declare signify-expression)
(defn signify-rules [var val]
  (list
     ;todo: найти все вхождения переменной var, заменить на val и привести к нормальной форме

   [(fn [expr] (and (variable? expr) (same-variables? var expr)))
    (fn [expr] val)]
   [(fn [expr] (conjunction? expr))
    (fn [expr] (let [e-args (args expr)] (apply conjunction-internal (map (fn [elem] (signify-expression var val elem)) e-args))))]
   [(fn [expr] (disjunction? expr))
    (fn [expr] (let [e-args (args expr)] (apply disjunction-internal (map (fn [elem] (signify-expression var val elem)) e-args))))]
   [(fn [expr] (negation? expr))
    (fn [expr] (let [arg (second expr)] (negation (signify-expression var val arg))))])
  )

(defn signify-expression [var val expr] (to-dnf-tier (signify-rules var val) expr))

(defn ^{:doc "Signifies variable var in expr"} signify [expr var val]

  (->>
   expr
   (signify-expression var val)
   to-dnf)
  )

(defn -main
  [& args]
  (println (to-dnf (negation (disjunction (implication (variable ::x) (variable ::y)) (negation (implication (variable ::y) (variable ::z)))))))
  )