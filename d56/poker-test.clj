(ns user (:use clojure.test))

(deftest test-suit
  (is (= "C" (suit "TC"))) ; so suit C
  (is (= "D" (suit "7D"))) ; so suit D
  (is (= "S" (suit "5S"))) ; so suit S
  (is (= "H" (suit "8H"))) ; so suit H
  (is (= nil (suit "Invalid"))) ;nevaliden suit
  )

(deftest test-rank
  (is (= 7 (rank "7H"))) ;so broj
  (is (= 10 (rank "TS"))) ;so 10
  (is (= 11 (rank "JD"))) ;so Jack
  (is (= 12 (rank "QC"))) ;so Queen
  (is (= 14 (rank "AH"))) ; so Ace
  )

(deftest test-pair?
  (is (pair? '("KS" "KD" "9H" "4D" "TC"))) ; dali raboti so par kings
  (is (not (pair? '("AC" "AH" "AD" "7S" "2C")))) ; dali raboti koga ima 3 of a kind
  )

(deftest test-three-of-a-kind?
  (is (three-of-a-kind? '("7D" "7H" "7C" "AC" "KH"))) ; Dali raboti koga ima three of a kind
  (is (not (three-of-a-kind? '("3C" "3H" "3D" "3S" "9C")))) ;Dali raboti koga ima 4
  )

(deftest test-four-of-a-kind?
  (is (four-of-a-kind? '("AH" "AD" "AS" "AC" "5H"))) ;test za validno
  (is (not (four-of-a-kind? '("5S" "6D" "7D" "8H" "9C")))) ;test za nevalidno
  )

(deftest test-flush?
  (is (flush? '("2H" "4H" "7H" "9H" "TH"))) ;test za validna kombinacija
  (is (not (flush? '("2S" "AS" "4S" "3S" "5S")))) ;test za nevalidna kombinacija
  )

(deftest test-full-house?
  (is (full-house? '("5H" "5S" "5D" "6S" "6H"))) ;validna kombinacija
  (is (not (full-house? '("AH" "AS" "AD" "KS" "JC")))) ;nevalidna
  )

(deftest test-two-pairs?
  (is (two-pairs? '("KH" "QD" "2S" "KS" "QC")))  ;validna
  (is (not (two-pairs? '("2H" "3S" "2S" "3H" "2D")))) ;nevalidna
  )

(deftest test-straight?
  (is (straight? '("2S" "AD" "3C" "4S" "5H"))) ;validna
  (is (not (straight? '("2H" "4S" "7C" "9D" "10H")))) ;nevalidna
  )

(deftest test-straight-flush?
  (is (straight-flush? '("TS" "JS" "KS" "AS" "QS"))) ;validna
  (is (not (straight-flush? '("2H" "4S" "7C" "9D" "TH")))) ;nevalidna
  )

(deftest test-value
  (is (= (value '("2H" "3H" "4H" "5H" "6H")) 8)) ;straight flush test
  (is (= (value '("2S" "2H" "2D" "2C" "7H")) 7)) ; four of a kind test
  (is (= (value '("KH" "KS" "QC" "QD" "2S")) 2)) ; two-pairs test
  (is (= (value '("TS" "JD" "QC" "KS" "AH")) 4)) ; straight test
  (is (= (value '("2H" "3H" "4H" "5H" "7H")) 5)) ; flush test
  )
(deftest test-kickers
  (is (= (kickers '("2H" "6S" "3C" "4D" "5D")) '(6 5 4 3 2))) ;nitu edno ispolneto
  (is (= (kickers '("5H" "AD" "5C" "7D" "AS")) '(14 5 7))) ;two-pairs ispolneto
  (is (= (kickers '("KH" "KS" "KC" "QD" "QS")) '(13 12))) ;full-house
  (is (= (kickers '("2S" "2H" "2D" "2C" "7H")) '(2 7))) ;four of a kind
  (is (= (kickers '("9D" "9H" "9S" "7C" "5S")) '(9 7 5))) ;three-of-a-kind
  )

(deftest test-higher-kicker?
  (is (higher-kicker? '(6 5 4 3 2) '(5 4 3 2 1))) ;da vrakja true
  (is (not (higher-kicker? '(14 5 7) '(14 5 7)))) ; da vrakja false
  )

(deftest test-beats?
  (is (beats? '("TS" "JD" "QC" "KS" "AH") '("KH" "QS" "QC" "QD" "2S"))) ; da vrakja true
  (is (nil? (beats? '("2H" "3H" "4H" "5H" "7H") '("2H" "3H" "4H" "5H" "6H")))) ; da vrakja false
  )

(deftest test-winning-hand
  ; Test so kombinacija two-pairs i straight
  (is (= (winning-hand '("KH" "KS" "QC" "QD" "2S") '("TS" "JD" "QC" "KS" "AH")) '("TS" "JD" "QC" "KS" "AH"))) ;
  ; Test so 2 pobednici (isti kombinacii od karti)
  (is (= (winning-hand '("2H" "3H" "4H" "5H" "7H") '("2H" "3H" "4H" "5H" "6H") '("2H" "3H" "4H" "5H" "6H")) '(("2H" "3H" "4H" "5H" "6H") ("2H" "3H" "4H" "5H" "6H"))))
  (is (nil? (winning-hand)))
  ; Test so 2 pobednici (razlichni kombinacii od karti)
  (is (= (winning-hand '("2S" "2H" "2D" "2C" "7H") '("2S" "2H" "2D" "2C" "7D")) '(("2S" "2H" "2D" "2C" "7H") ("2S" "2H" "2D" "2C" "7D"))))
  ;Test eden pobednik - sporeduvanje na podredeni listi na hands so ednakvi vrednosti
  (is (= (winning-hand '("2S" "2H" "2D" "2C" "7H") '("AD" "AH" "AS" "AC" "5S") '("KH" "KS" "QC" "QD" "2S")) '("AD" "AH" "AS" "AC" "5S")))
  )


(run-tests)
