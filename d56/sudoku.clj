;; Problem 1

(defn map-to-vect "Map all the rows in a matrix and the matrix itself in a vector" [matrix]
  (into [] (map #(into [] %) matrix)))

;; Baranata transform funkcija

(defn transform "Requested transform function" [matrix]
  (map-to-vect (for [x matrix] (for [y x]
                    (if (= y 0)
                      #{1 2 3 4 5 6 7 8 9}
                      #{y})))))

(defn remove-number "Remove number n from lst" [lst n]
  (if (not (empty? lst))
    (if (= (first lst) n)
      (remove-number (rest lst) n)
      (cons (first lst) (remove-number (rest lst) n)))
    '()))

(defn transpose-matrix "Transposing matrix - used to delete element from a column using remove-from-row additionally"
  [matrix]
  (if (not (empty? (first matrix))) (cons (map first matrix)
        (transpose-matrix (map rest matrix))) '()))

(defn equality-cond "Check if the set contains only one element and equal to el, if not remove el from set-pos" [set-pos el]
  (if (not= set-pos #{el}) (into #{} (remove-number set-pos el)) set-pos))

(defn remove-from-row "Remove element from the sets contained in the row" [row el]
  (map #(equality-cond % el) row))

(defn remove-row-matrix "Remove element in row number removei"  [curr removei el matrix]
  (cond (empty? matrix) matrix
        (= curr removei) (cons (remove-from-row (first matrix) el) (rest matrix))
        :else (cons (first matrix) (remove-row-matrix (+ 1 curr) removei el (rest matrix)))))

(defn transform-row-3x3 "Used to remove el from the part of the row between sy and ey" [curry el row sy ey]
  (cond (empty? row) row
  (and (<= sy curry) (> ey curry))
        (cons (if (not= (first row) #{el}) (into #{} (remove-number (first row) el)) (first row))
              (transform-row-3x3 (+ 1 curry) el (rest row) sy ey))
  :else (cons (first row) (transform-row-3x3 (+ 1 curry) el (rest row) sy ey))))

(defn remove-from-3x3-wrap "Wrapper function to remove element from 3x3 (starting from sx,sy to ex,ey)" [el matrix currx sx ex sy ey]
  (cond
    (empty? matrix) matrix
    (and (<= sx currx) (> ex currx)) (cons (transform-row-3x3 0 el (first matrix) sy ey)
                                           (remove-from-3x3-wrap el (rest matrix) (+ currx 1) sx ex sy ey))
    :else (cons (first matrix) (remove-from-3x3-wrap el (rest matrix) (+ currx 1) sx ex sy ey))))

(defn remove-from-3x3 "Remove element from 3x3 (starting from sx,sy to ex,ey)" [el matrix sx ex sy ey]
  (remove-from-3x3-wrap el matrix 0 sx ex sy ey))


(defn check-rows-one-el "Helper function to check in a row if all the elements have only one element" [row]
  (cond (empty? row) true
        (empty? (rest (first row))) (check-rows-one-el (rest row))
        :else false))

(defn check-matrix-rows "Check if all elements of the matrix have only one element" [matrix]
  (cond (empty? matrix) true
      (check-rows-one-el (first matrix)) (check-matrix-rows (rest matrix))
        :else false))



(defn switch_row "Helper function for switch_set_one_el to change the element at y in the 'row' with one-element
    set of el" [y cy el row]
  (cond (= y cy) (cons #{el} (rest row))
        :else (cons (first row) (switch_row y (+ 1 cy) el (rest row)))))

(defn switch_set_one_el "Set a set of el on position x y" [el matrix x y cx]
  (cond
    (empty? matrix) matrix
    (= x cx) (cons (switch_row y 0 el (first matrix)) (rest matrix))
    :else (cons (first matrix) (switch_set_one_el el (rest matrix) x y (+ 1 cx)))))

(defn get-cleared-matrix "Clear the matrix - switch position x y with set containing only el in matrix and remove it from the corresponding row, col and 3x3"
  [el x y matrix]
  (map-to-vect (switch_set_one_el el
                                  (map-to-vect
                                    (remove-from-3x3 el
                                                     (map-to-vect
                                                       (transpose-matrix
                                                         (remove-row-matrix 0 y el
                                                                            (map-to-vect
                                                                              (transpose-matrix
                                                                                (map-to-vect
                                                                                  (remove-row-matrix 0 x el matrix)))))))
                                                     (* (quot x 3) 3) (* (+ (quot x 3) 1) 3) (* (quot y 3) 3) (* (+ (quot y 3) 1) 3))) x y 0)))

(defn get-cleared-row "Remove el from the row containing element with coordinates x y" [el x y matrix]
  (switch_set_one_el el
                     (map-to-vect (remove-row-matrix 0 x el matrix))
                     x y 0))

(defn get-cleared-column "Remove el from column containing element with coordinates x y" [el x y matrix]
 (switch_set_one_el el
                    (map-to-vect
                      (transpose-matrix
                        (remove-row-matrix 0 y el
                                           (map-to-vect (transpose-matrix (map-to-vect matrix))))))
                    x y 0))

(defn get-cleared-3x3 "Remove el from the 3x3 containing element with coordinates x y" [el x y matrix]
  (switch_set_one_el el
                     (map-to-vect
                       (remove-from-3x3
                         el matrix
                         (* (quot x 3) 3)
                         (* (+ (quot x 3) 1) 3)
                         (* (quot y 3) 3)
                         (* (+ (quot y 3) 1) 3)))
                     x y 0))

(defn valid-row-ones? "Checks if the row x contains the el once" [matrix x el]
  (let [row-ones (filter #(contains? % el) (get-in matrix [x]))]
    (= (count row-ones) 1)))

(defn valid-column-ones? "Checks if the column y contains the el once" [matrix y el]
  (let [row-ones (filter #(contains? % el) (get-in (map-to-vect (transpose-matrix matrix)) [y]))]
    (= (count row-ones) 1)))

(defn valid-3x3-ones? "Checks if the 3x3 square containing the element with coordinates x y contains the el once" [matrix x y el]
  (let [row-ones (filter #(contains? % el) (for [i (range (* (quot x 3) 3) (* (+ (quot x 3) 1) 3))
                                  j (range (* (quot y 3) 3) (* (+ (quot y 3) 1) 3))]
                            (let [element (get-in matrix [i j])]
                            element)))]
    (= (count row-ones) 1)))

(defn analyze-el "Used by the analyze-elements function - it sets element of a matrix to a single element set
    and checks if the resulting matrix is valid (containing that element only once in a row, column or 3x3 square)" [el matrix x y]
  (let [mapped (map-to-vect matrix)]
    (let [cleared-matrix-row (get-cleared-row el x y matrix)
          cleared-matrix-column (get-cleared-column el x y matrix)
          cleared-matrix-3x3 (get-cleared-3x3 el x y matrix)
          mapped-switched (map-to-vect (switch_set_one_el el mapped x y 0))]
      (cond (or (valid-column-ones? mapped-switched y el)
                (valid-row-ones? mapped-switched x el)
                (valid-3x3-ones? mapped-switched x y el)) mapped-switched
           :else matrix))))

(defn analyze-elements "Analyzing row by row" [matrix x y elements]
  (if (empty? elements) matrix
    (let [analyzed-matrix (analyze-el (first elements) matrix x y)]
    (cond (not= matrix analyzed-matrix) analyzed-matrix
          :else (analyze-elements matrix x y (rest elements))))))

(defn iterate-existing-one-elements "Iterating over existing sets containing one element and removing it from the row, column and 3x3 square" [x y matrix]
  (cond
    (nil? (get-in matrix [x 0])) matrix
    (nil? (get-in matrix [x y])) (iterate-existing-one-elements (+ 1 x) 0 matrix)
    (empty? (rest (get-in matrix [x y]))) (iterate-existing-one-elements x (+ 1 y) (map-to-vect
                                    (remove-from-3x3 (first (get-in matrix [x y]))
                                                     (map-to-vect
                                                       (transpose-matrix
                                                         (remove-row-matrix 0 y (first (get-in matrix [x y]))
                                                                            (map-to-vect
                                                                              (transpose-matrix
                                                                                (map-to-vect
                                                                                  (remove-row-matrix 0 x (first (get-in matrix [x y])) matrix)))))))
                                                     (* (quot x 3) 3) (* (+ (quot x 3) 1) 3) (* (quot y 3) 3) (* (+ (quot y 3) 1) 3))))

    :else (iterate-existing-one-elements x (+ 1 y) (analyze-elements matrix x y
                                                                     (into '() (get-in (map-to-vect matrix) [x y]))))))


(defn solve-wrap "Wrapping the entire solving logic" [matrix]
  (let [cmatrix (iterate-existing-one-elements 0 0 (map-to-vect matrix))] ;; check if one element in each
        (cond (= matrix cmatrix) matrix
              :else (solve-wrap cmatrix))))


(defn print-sudoku "Print a sudoku matrix" [matrix]
  (doseq [i (range (count matrix))]
     (cond (= 0 (mod i 3))(println "------------------------------------------------------------------------------") :else ())
    (doseq [j (range (count (matrix i)))]
      (cond (= 0 j)(print "|") :else ())
      (cond (= 1 (count (get-in matrix [i,j]))) (print " " (get-in matrix [i,j]) " ") :else (print "   _    "))
      (cond (= 0 (mod (inc j) 3))(print "|") :else ())
      (cond (= 0 (mod (inc j) 9))(print "\n") :else ())
  )) (println "------------------------------------------------------------------------------") "Visual representation")


;; Baranata solve funkcija

(defn solve "Main solve function" [matrix]
  (let[solved (solve-wrap (transform matrix))] (do (print-sudoku (transform matrix)) (print-sudoku solved) solved)))



