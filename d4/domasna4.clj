;; Grupa 1


;; Zadacha a

(defn atomic? "Check if V is atomic" [V]
  (not (coll? V)))


;; Zadacha b

(defn member? "Check if x is present in lst" [x lst]
  (if (not= lst '())
    (if (not= x (first lst))
      (member? x (rest lst)) true)
    false))


;; Zadacha v

(defn count-elements "Iterate through lst and return the count of elements" [lst curr]
  (if (not= lst '()) (count-elements (rest lst) (+ 1 curr)) curr
  ))

(defn my-count "Return count of elements at level 0 in lst" [lst]
  (count-elements lst 0))


;; Zadacha g

(defn append "Merge lst1 and lst2" [lst1 lst2]
  (if (not= lst1 '())
    (cons (first lst1) (append (rest lst1) lst2))
    lst2
  ))

;; Zadacha d

(defn zip "Combine lst1 and lst2 in one list of pairs where the first element
  is a part of lst1 and the second of lst2" [lst1 lst2]
  (if (and (not= lst1 '()) (not= lst2 '()))
    (cons (list (first lst1) (first lst2)) (zip (rest lst1) (rest lst2)))
    '()
  ))


;; Zadacha gj

(defn lookup "Find the value (second element in the pair) in list-of-pairs matching the key(first element in pair)" [key list-of-pairs]
  (if (not= list-of-pairs '())
    (if (not= key (first (first list-of-pairs)))
      (lookup key (rest list-of-pairs)) (first (rest (first list-of-pairs))))
    nil))

;; Zadacha e

(defn my-merge "Merge two (ascedingly) sorted lists into one sorted list" [lst1 lst2]
  (if (and (not= lst1 '()) (not= lst2 '()))
    (if (< (first lst1) (first lst2))
    (cons (first lst1) (my-merge (rest lst1) lst2))
    (cons (first lst2) (my-merge lst1 (rest lst2))))
    (if (not= lst1 '()) (cons (first lst1) (my-merge (rest lst1) lst2))
      (if (not= lst2 '()) (cons (first lst2) (my-merge lst1 (rest lst2))) nil))
  ))

;; Zadacha zh

(defn count-all "Count all atomical elements in lst in all levels" [lst]
  (if (not= lst '())
      (if (atomic? (first lst))
        (+ (count-all (rest lst)) 1)
        (+ (count-all (first lst)) (count-all (rest lst)))
      ) 0))

;; Zadacha z

(defn my-drop "Remove first n elements in lst" [n lst]
  (if (and (> n 0) (not= lst '()))
    (my-drop (- n 1)(rest lst)) lst
  ))

;; Zadacha dz

(defn my-take "Get list of first n elements in lst" [n lst]
  (if(and (> n 0) (not= lst '()))
    (cons (first lst) (my-take (- n 1) (rest lst))) '()
  ))

;; Zadacha i

(defn my-reverse "Reverse lst" [lst]
  (if (not= lst '())
    (append (my-reverse (rest lst)) (list (first lst)))
    lst))

;; Zadacha j

(defn remove-number "Remove all appearances of n in lst" [lst n]
  (if (not= lst '())
    (if (= (first lst) n)
      (remove-number (rest lst) n)
      (cons (first lst) (remove-number (rest lst) n)))
    lst))

(defn remove-duplicates "Remove duplicates from lst" [lst]
  (if (not= lst '())
    (cons (first lst) (remove-duplicates (remove-number (rest lst) (first lst))))
    lst))

;; Zadacha k
(defn merge-lists "Merge lst1 and lst2" [lst1 lst2]
  (if (not= lst1 '())
  (cons (first lst1) (merge-lists (rest lst1) lst2))
  lst2
  ))

(defn my-flatten "Remove one level of parentheses in lst" [lst]
  (if (not= lst '())
    (
      if (atomic? (first lst))
     (cons (first lst) (my-flatten (rest lst)))
     (merge-lists (first lst) (my-flatten (rest lst)))
    )
    lst))


;; Grupa 2


;; Zadacha a

(defn buzz-map "If a number is divisible by 7 or contains 7 return :buzz or the number itself" [num]
  (cond (= (mod num 7) 0) :buzz
        (member?  \7 (seq (str num))) :buzz
        :else num))

(defn buzz  "Replace all numbers divisible by 7 and containing 7 in them with :buzz"[list-of-ints]
  (map buzz-map list-of-ints))

;; Zadacha b

(defn divisors-of "Find all divisors of n" [n]
  (filter #(= (mod n %) 0) (range 2 n)))

;; Zadacha v

(defn longest-reduce "Get the longest of two strings and if they are equal the first one" [s1 s2]
  (cond (= (max (count s1) (count s2)) (count s1)) s1
        :else s2))

(defn longest "Find the longest string" [list-of-strings]
  (reduce longest-reduce nil list-of-strings))


;; Grupa 3

;; Zadacha a

(defn my-map "Map function" [f lst]
  (cond (not (empty? lst)) (cons (f (first lst)) (my-map f (rest lst)))
        :else lst))

;; Zadacha b

(defn my-filter "Filter function" [pred lst]
  (cond (empty? lst) lst
        (pred (first lst)) (cons (first lst) (my-filter pred (rest lst)))
        :else (my-filter pred (rest lst))))

;; Zadacha v

(defn my-reduce "Reduce function" ([f lst] (my-reduce f (first lst) (rest lst)))
  ([f value? lst] (cond (empty? lst) value?
    :else (my-reduce f (f value? (first lst)) (rest lst)))))

;; Zadacha g

(defn my-flat-map "Flat-map" ([f lst] (cond (empty? lst) '() :else (my-flat-map f (f (first lst)) (rest lst))))
  ([f value lst]
   (cond (empty? lst) value
     :else (my-flat-map f (merge-lists value (f (first lst))) (rest lst)))))
