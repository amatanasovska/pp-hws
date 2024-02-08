(ns user (:use clojure.test))

(deftest test-solve
 (is (= '((1 2 3) (3 1 2) (2 3 1)) (solve '((1 2 3) (1 2 3) (1 2 3))))) ;izbran e kako ednostaven primer za prachki koi imaat reshenie
 (is (= nil (solve '((1 2 2) (2 2 2) (3 2 1))))) ;izbran e kako ednostaven primer za prachki koi nemaat reshenie i boite se povtoruvaat
 (is (= nil (solve '((:red :yellow :blue) (:blue :yellow :red) (:blue :red :yellow))))) ;primer prachki koi nemaat reshenie i za koristenje na keywords kako iminja na boi
 (is (= '((:r :y :b :g) (:b :r :g :y) (:y :g :y :b) (:g :b :r :r))
         (solve '((:r :y :b :g) (:r :g :y :b) (:y :b :y :g) (:r :r :g :b)))))  ;primer prachki koi imaat reshenie i za koristenje na keywords kako iminja na boi
  (is (= nil (solve '(("A" "B" "C" "D") ("D" "B" "A" "C") ("A" "B" "C" "D") ("D" "B" "A" "C")))))  ;primer prachki koi nemaat reshenie i za koristenje na stringovi kako iminja na boi
  (is (= '(("A" "B" "C" "D") ("D" "A" "B" "C") ("C" "D" "A" "B") ("B" "C" "D" "A"))  ;primer prachki koi nemaat reshenie i za koristenje na stringovi kako iminja na boi
          (solve '(("A" "B" "C" "D") ("A" "B" "C" "D") ("A" "B" "C" "D") ("A" "B" "C" "D")))))
  (is (= '((1 2 3 4) (2 3 4 1) (3 4 1 2) (4 1 2 3)) (solve '((1 2 3 4) (2 3 4 1) (3 4 1 2) (4 1 2 3)))))  ; ednostaven primer za prachki koi imaat reshenie
  (is (= '((1 1 1 2 2) (3 3 3 1 1) (2 2 2 4 4) (4 4 4 3 3)) (solve '((1 1 1 2 2) (1 3 3 3 1) (4 2 2 2 4) (3 4 4 4 3))))) ; ednostaven primer za prachki koi imaat reshenie i brojot na kelii vo prachki != broj na prachki i boite se povtoruvaat
  (is (= '((1 2 3 4 5) (3 1 5 2 4) (2 5 4 3 1) (4 3 1 5 2) (5 4 2 1 3)) ; pokompleksen primer za prachki koi imaat reshenie
         (solve '((1 2 3 4 5) (5 2 4 3 1) (1 2 5 4 3) (2 4 3 1 5) (5 4 2 1 3)))))
  (is (= '((1 4 2 5 3) (4 3 5 2 1) (3 2 4 1 5) (5 1 3 4 2) (2 5 1 3 4)) ; pokompleksen primer za prachki koi imaat reshenie
         (solve '((1 4 2 5 3) (3 5 2 1 4) (1 5 3 2 4) (3 4 2 5 1) (2 5 1 3 4))))))

(run-tests)
