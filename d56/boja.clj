
(defn check_matching "Compare two rows colors" [row1 row2]
  (cond (or (empty? row1) (empty? row2)) false
        (= (first row1) (first row2)) true
        :else (check_matching (rest row1) (rest row2))))

(defn rotate "Rotate a row" [lst]
  (if (empty? lst)
    lst
    (concat [(last lst)] (butlast lst))))

(defn check_matching_all "Checking if some of the colors are repeating" [cmp lst]
  (cond (empty? lst) false
        (check_matching cmp (first lst)) true
        :else (check_matching_all cmp (rest lst))))

(defn try-solve "Recursively iterating over the rows until a solution is fount or returning nil if not" [inp prev-rows current-time total-times]
    (cond (= current-time total-times) nil
            (empty? inp) prev-rows
            (not (check_matching_all (first inp) prev-rows))
          (try-solve (rest inp) (cons (first inp) prev-rows) 0 total-times)
            :else
          (try-solve (cons (rotate (first inp)) (rest inp)) prev-rows (+ 1 current-time) total-times)))

(defn show "Visualization function" [inp] (let [mapped-inp (into [] (map #(into [] %) inp))]
                   (do
                    (println "-------------------")
                     (doseq [i (range (count mapped-inp))]
                   (doseq [j (range (count (mapped-inp i)))]
                     (print "| " (get-in mapped-inp [i,j]) " "))
                   (print "|\n"))
                   (println "-------------------"))))

(defn solve "Main solving function" [inp] (let [solution (reverse (try-solve inp '() 0 (count (first inp))))]
                    (cond (not (empty? solution)) (do (show solution) solution)
                          :else (do (println "No solution") nil))))
