(ns user (:use clojure.test))


(deftest test-atomic?
 (is (= true (atomic? 42))) ;Dali tochno gi prepoznava atomichnite vrednosti
 (is (= false (atomic? [1 2 3]))) ;Dali tochno prepoznava listi
 (is (= false (atomic? {:a 1 :b 2}))) ;Dali tochno prepoznava mapi
 (is (= true (atomic? "hello"))) ;Dali tochno prepoznava stringovi
 (is (= false (atomic? [1 [2 3] 4])))) ; Dali tochno prepoznava listi

(deftest test-member?
  (is (= true (member? 1 [1 2 3 4 5]))) ; Dali tochno kje prepoznae chlen
  (is (= false (member? 6 []))) ; Dali tochno kje proveri prazen vektor
  (is (= false (member? 10 [1 2 3 4 5]))) ; Dali tochno kje vidi deka chlen ne pripagja na vektorot
  (is (= true (member? 2 [1 2 2 3 4]))) ; Dva pati chlen dali kje pravi problem
  (is (= true (member? 6 '(5 6 7 8))))) ; Dali raboti so listi

(deftest test-my-count
  (is (= 3 (my-count '(a d f)))) ;Dali tochno kje prepoznae ednostavna lista
  (is (= 0 (my-count '()))) ;Dali tochno kje prepoznae prazna lista
  (is (= 4 (my-count '(a () b (c () d))))) ;Dali tochno kje prepoznae vgnezdeni list
  (is (= 5 (my-count '(a b () d 5)))) ;Dali kje pomine na test so meshani tipovi
  (is (= 5 (my-count '(a 2 (b c 3) d e))))) ;Dali kje pomine na test so simboli i brojki

(deftest test-append
  (is (= '(1 2 3 1 2) (append '(1 2 3) '(1 2)))) ;Dali tochno kje raboti so ednostavni listi
  (is (= '() (append '() '()))) ;Dali tochno kje raboti so prazni listi
  (is (= '(a (1 2) c f) (append '(a) '((1 2) c f)))) ;Dali tochno kje raboti so vgnezdeni listi
  (is (= '(x 0 y (2 3) :c) (append '(x 0) '(y (2 3) :c)))) ; Dali tochno kje raboti so meshani pod tipovi i vgnezdeni listi
  (is (= '(1 a 2 :b x) (append '(1 a) '(2 :b x))))) ; Dali tochno kje raboti so 2 meshani pod tipovi


(deftest test-zip
  (is (= '((x 1) (y 2) (z 3)) (zip '(x y z) '(1 2 3)))) ;Dali kje raboti tochno so ednostaven primer
  (is (= '() (zip '() '()))) ;Dali kje raboti tochno so prazna lista
  (is (= '((x (1 2)) (y (3 4)) (z (5 6))) (zip '(x y z) '((1 2) (3 4) (5 6))))) ;Dali kje raboti tochno so vgnezdeni listi
  (is (= '((x 1) (y 2) (z 3)) (zip '(x y z) '(1 2 3 4)))) ;Dali kje raboti tochno ako vo vtorata lista ima povekje chlenovi
  (is (= '((:a 1) (y "two") ("z" true)) (zip '(:a y "z" :d) '(1 "two" true))))) ;Dali kje raboti tochno ako vo prvata lista ima povekje chlenovi


(deftest test-lookup
  (is (= "y-value" (lookup :y '((:x "x-value") (:y "y-value") (:z "z-value"))))) ;Dali kje raboti so postoechki kluch
  (is (= nil (lookup :w '((:x "x-value") (:y "y-value") (:z "z-value"))))) ;Dali kje raboti so nepostoechki kluch
  (is (= "z-value" (lookup '(:z :x) '(((:x :y) "x-value") ((:y :z) "y-value") ((:z :x) "z-value"))))) ;Dali kje raboti so lista kako kluch
  (is (= nil (lookup :a '()))) ;Dali kje raboti so prazna lista
  (is (= true (lookup true '((false false) (true true) (nil nil)))))) ;Dali kje raboti so booleans kako kluchevi

(deftest test-my-merge
  (is (= '(1 2 3 4 5 6) (my-merge '(1 2 3) '(4 5 6)))) ;Dali kje raboti so ednostaven primer
  (is (= '(1 2 3) (my-merge '(1 2 3) '()))) ;Dali kje raboti so prazna vtora lista
  (is (= '(1 2 3) (my-merge '() '(1 2 3)))) ; Dali kje raboti so prazna prva lista
  (is (= '(2 3 5 5 7 8 8 9) (my-merge '(2 5 7 8) '(3 5 8 9)))) ;Dali kje raboti so duplikat elementi
  (is (= '(-10 -5 0 3 7 12 15 20 25 30 40 50 60) (my-merge '(-10 0 7 12 20 30) '(-5 3 15 25 40 50 60)))) ;Dali kje raboti so negativni broevi
  )

(deftest test-count-all
  (is (= 8 (count-all '(1 (2 (3 (4 (5 (6 (7 8)))))))))) ;Dali kje raboti so ednostaven primer
  (is (= 4 (count-all '(:a :b :a :b)))) ;Dali kje raboti so simboli koi se povtoruvaat
  (is (= 5 (count-all '(true false nil true false)))) ;Dali kje raboti so nil i booleans
  (is (= 8 (count-all '(:a 42 [] "x" [:b :c] true nil (5))))) ;Dali kje raboti so vgnezdeni listi, vektori i drugi meshani pod tipovi
  (is (= 10 (count-all '((:a 1 :b 2) [:c 3 :d 4] [:e :f])))) ;Dali kje raboti so samo vgnezdeni listi
  )

(deftest test-my-drop
  (is (= '(c d e) (my-drop 2 '(a b c d e)))) ;Dali kje raboti so ednostaven primer
  (is (= '() (my-drop 4 '(1 2 3 4)))) ;Dali kje raboti so brojot na elementi vo listata
  (is (= '() (my-drop 5 '(:a :b :c)))) ;Dali kje raboti so broj pogolem od brojot na elementi na listata
  (is (= '() (my-drop 2 '()))) ;Dali kje raboti so prazna lista
  (is (= '(a b c d e) (my-drop 0 '(a b c d e)))) ;Dali kje raboti so 0
  )

(deftest test-my-take
  (is (= '(x y) (my-take 2 '(x y z a b)))) ;Dali kje raboti so ednostaven primer
  (is (= '(x y z) (my-take 5 '(x y z)))) ;Dali kje raboti ako se baraat poishe od shto ima elementi vo listata
  (is (= '() (my-take 3 '()))) ;Dali kje raboti so prazna lista
  (is (= '() (my-take 0 '(x y z)))) ;Dali kje raboti so 0
  (is (= '(x y z a b) (my-take 5 '(x y z a b)))) ;Dali kje raboti so brojot na elementi vo listata
  )

(deftest test-my-reverse
  (is (= '(5 4 3 2 1) (my-reverse '(1 2 3 4 5)))) ;Dali kje raboti so ednostaven primer
  (is (= '((x y z) 3 2 1) (my-reverse '(1 2 3 (x y z))))) ; Dali kje raboti so vgnezdena lista
  (is (= '(() :z :y :x) (my-reverse '(:x :y :z ())))) ;Dali kje raboti so prazna vgnezdena lista
  (is (= '() (my-reverse '()))) ;Dali kje raboti so prazna lista
  (is (= '(true false true) (my-reverse '(true false true)))) ;Dali raboti so booleans
  )

(deftest test-remove-duplicates
  (is (= '(1 2 3 4) (remove-duplicates '(1 2 3 1 4 1 2)))) ;Dali raboti so ednostaven primer
  (is (= '(x y z (a b)) (remove-duplicates '(x y z x (a b) (a b) z)))) ;Dali raboti so listi vgnezdeni
  (is (= '(true false) (remove-duplicates '(true false true true false)))) ;Dali raboti so booleans
  (is (= '() (remove-duplicates '()))) ;Dali raboti so prazna lista
  (is (= '(1 :a "hello" true) (remove-duplicates '(1 :a "hello" true :a 1 "hello" true)))) ;Dali raboti so meshani podatochni tipovi
  )

(deftest test-my-flatten
  (is (= '((1 1) (2 3) (5 7)) (my-flatten '(((1 1) (2 3)) ((5 7)))))) ;Dali raboti so ednostaven primer
  (is (= '(:a 42 "x" true nil 5) (my-flatten '((:a 42) "x" (true nil) [5])))) ;Dali raboti so primeri koi sodrzhat i nevgnezdeni elementi
  (is (= '() (my-flatten '()))) ;Dali raboti so prazna lista
  (is (= '(x y z) (my-flatten '(x y z)))) ;Dali raboti so lista koja ne sodrzhi vgnezdeni elementi
  (is (= '(true false true) (my-flatten '((true false true))))) ;Dali raboti so lista koja sodrzhi samo eden vgnezden element
  )

(deftest test-buzz
  (is (= '(1 2 3 :buzz :buzz :buzz)
          (buzz '(1 2 3 14 21 28)))) ;Dali raboti so broevi delivi so 7
  (is (= '(:buzz :buzz 31 :buzz)
          (buzz '(17 27 31 37)))) ;Dali raboti so broevi koi sodrzhat 7
  (is (= '() (buzz '()))) ;Dali raboti so prazna lista
  (is (= '(1 2 3 4 :buzz 6 :buzz 8 9 10 11 :buzz 13 :buzz 15)
          (buzz '(1 2 3 4 -7 6 -14 8 9 10 11 -17 13 -21 15)))) ;Dali raboti so negativni broevi
  (is (= '(1 2 3 5 8 9)
          (buzz '(1 2 3 5 8 9)))); Dali raboti so listi koi nemaat broevi delivi so 7 ili sodrzhat 7
  )

(deftest test-divisors-of
  (is (= '() (divisors-of 13))) ;Dali raboti so prosti broevi
  (is (= '(2 3 4 6) (divisors-of 12))) ;Dali raboti so slozheni broevi
  (is (= '() (divisors-of 1))) ;Dali raboti so 1
  (is (= '(2 4 5 10 20 25 50) (divisors-of 100))) ;Dali raboti so golemi broevi
  (is (= '() (divisors-of 2))) ;Dali raboti so mali broevi
  )

(deftest test-longest
  (is (= "abc" (longest '("a" "abc" "xy")))) ;Dali raboti so ednostavni primeri
  (is (= "xyz" (longest '("xyz" "aba" "abc" "123")))) ;Dali raboti so stringovi so ista dolzhina
  (is (= "a" (longest '("a")))) ;Dali raboti so ednoelementni listi
  (is (= "1 2 3 4" (longest '("a" "1 2 3 4")))) ;Dali raboti so stringovi so prazni mesta i broevi
  (is (= "12345678999" (longest '("aBcDeFGH" "123AB" "ABCDEFG" "11111" "12345678999" "           ")))) ;Dali raboti so podolgi nizi
  )

(deftest test-my-map
  (is (= '(1 2 3 4 5) (my-map + '(1 2 3 4 5)))) ;Dali raboti so ednostavni primeri
  (is (= '(1.0 4.0 9.0 16.0 25.0) (my-map #(Math/pow % 2) '(1 2 3 4 5)))) ;Dali raboti so lambda funkcija
  (is (= '("ABC" "DEF" "GHI") (my-map clojure.string/upper-case '("abc" "def" "ghi")))) ;Dali raboti so stringovi
  (is (= '(-1 -2 -3 -4 -5) (my-map #(- %) '(1 2 3 4 5)))) ;Dali raboti so negativni broevi
  (is (= '("abc1" "abc2" "abc3") (my-map #(str "abc" %) '(1 2 3)))) ;Dali raboti so konkatenacija
  )


(deftest test-my-filter
  (is (= '(2 4 8) (my-filter even? '(2 4 7 8 3)))) ;Dali raboti so ednostavni primeri
  (is (= '("ABC" "DEF" "GHI") (my-filter #(= (clojure.string/upper-case %) %) '("ABC" "xyz" "DEF" "tij" "GHI")))) ;Dali raboti so stringovi
  (is (= '(2 8) (my-filter pos? '(2 -21 8 -15 -19)))) ;Dali raboti so negativni broevi
  (is (= '(a b) (my-filter symbol? '(1 2 a 5 b 11 :c 3)))) ;Dali raboti so simboli
  (is (= '() (my-filter even? '()))) ;Dali raboti so prazna lista
  )

(deftest test-my-reduce
  (is (= 15 (my-reduce + 0 '(1 2 3 4 5)))) ;Dali raboti so ednostaven primer
  (is (= 120 (my-reduce * '(1 2 3 4 5)))) ; Dali raboti bez daden pocheten value
  (is (= 0 (my-reduce - 10 '(1 2 3 4)))) ;Dali raboti so daden pocheten value
  (is (= "abc cde" (my-reduce str "" '("abc" " " "cde")))) ;Dali raboti so stringovi
  (is (= "1===2===3===4" (my-reduce (fn [x y] (str x "===" y)) '(1 2 3 4)))) ;Dali raboti so korisnichki def funkcija
  )

(deftest test-my-flat-map
  (is (= '(1 2 3 4 5) (my-flat-map #(list %) '(1 2 3 4 5)))) ;Dali raboti so ednostavni primeri
  (is (= '(1 1 2 2 3 3 4 4 5 5) (my-flat-map #(list % %) '(1 2 3 4 5)))) ;Dali ja izramnuva
  (is (= '("HELLO" "WORLD") (my-flat-map #(list (clojure.string/upper-case %)) '("hello" "world")))) ;Dali raboti so stringovi
  (is (= '(("prefix1" "1a1" "prefix2" "2a2" "prefix3" "3c3") (my-flat-map #(list (str "prefix" %) (str % "a" %)) '("1" "2" "3"))))) ;Dali raboti so poslozheni primeri
  (is (= '() (my-flat-map #(list %) '()))) ;Dali raboti so prazna lista
  )


(run-tests)
