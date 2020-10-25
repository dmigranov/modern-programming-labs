(ns lab4.core
  (:gen-class))
; :  - keyword
; :: - keyword в пользовательском неймспейсу

(defn variable [name]
  {:pre [(keyword? name)]}
  (list :var name))

(defn variable? [expr]
  (= (first expr) :var))

(defn variable-name [var]
  (second var))

(defn same-variables? [v1 v2]
  (and
   (variable? v1)
   (variable? v2)
   (= (variable-name v1) (variable-name v2))))

(defn disjunction-internal [expr & rest]
  (cons :disj (cons expr rest)))

(defn disjunction [expr1 expr2]
  (list :disj expr1 expr2))

(defn disjunction-many [expr & others]
  (if (not-empty others)
    (disjunction expr (apply disjunction-many others))
    expr
    )
  )

(defn disjunction? [expr]
  (= :disj (first expr)))

(defn conjunction-internal [expr & rest]
  (cons :conj (cons expr rest)))  

(defn conjunction [expr1 expr2]
  (list :conj expr1 expr2))

(defn conjunction-many [expr & others]
  (if (not-empty others)
    (conjunction expr (apply conjunction-many others))
    expr))

(defn conjunction? [expr]
  (= :conj (first expr)))

(defn implication [expr1 expr2]
  (list :impl expr1 expr2))

(defn implication? [expr]
  (= :impl (first expr)))

(defn negation [expr]
  (list :neg expr))

(defn negation? [expr]
  (= :neg (first expr)))

(def log-true (list :true))

(defn log-true? [expr]
  (= :true (first expr)))

(def log-false (list :false))

(defn log-false? [expr]
  (= :false (first expr)))

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


                       [(fn [expr] (and (conjunction? expr)))
                        (fn [expr] (let [c-args (args expr)]
                                     (apply conjunction-internal (sort (fn [x y]
                                                                         (let [a (unnegate-variable x) b (unnegate-variable y)]
                                                                           (compare (variable-name a) (variable-name b)))) c-args))))]
                       
                       [(fn [expr] (disjunction? expr))
                        (fn [expr] (let [e-args (args expr)] (apply disjunction-internal (map to-dnf-tier-sort e-args))))]
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

;это необходимо, поскольку: ((a or b) and c) and d
;это раскорется при первом применении tier-3 в ((a and c) or (b and c)) and d
;но рекурсивно спускаясь мы пропустим возможность примениит это правило ко всему выражению!
(defn to-dnf-tier-3-cycle [expr]
 (let [new-expr (to-dnf-tier-3 expr)]
   (if (= new-expr expr)
     new-expr
     (recur new-expr))) 
   )

(defn to-dnf [expr]
  (->> expr
       to-dnf-tier-1
       to-dnf-tier-2 ;2 и 3 нельзя объединить в одну тк тогда могут быть не замечены некоторые правила дистрибутивности
       to-dnf-tier-3-cycle
       to-dnf-tier-unite
       to-dnf-tier-sort
       ;tier4 - поиск одинаковых переменных, плюс избавление от единиц и нулей?
       ))




(defn signify [expr var val]
  ;todo: найти все вхождения переменной var, заменить на val и привести к нормальной форме
  )

(defn -main
  [& args]
  (println "Hello, World!")
  (to-dnf (negation (disjunction (implication (variable ::x) (variable ::y)) (negation (implication (variable ::y) (variable ::z))))))

  )