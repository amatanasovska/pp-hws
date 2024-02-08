(defn suit "Return the suit of the card" [card] (let [csuit (str (second (seq card)))]
                                                  (cond (or (= "C" csuit) (= "D" csuit)
                                                            (= "H" csuit) (= "S" csuit)) csuit
                                                        :else nil)))

(defn rank "Return the rank of the card" [card] (if (Character/isDigit (first (seq card))) (Integer/valueOf (str (first (seq card))))
                    ({"T" 10 "J" 11 "Q" 12 "K" 13 "A" 14} (str (first (seq card))))))

(defn count_value "Count how many times a value appears in a hand" [hand value]
  (cond
    (empty? hand) 0
    (= value (rank (first hand))) (+ 1 (count_value (rest hand) value))
        :else (count_value (rest hand) value)))

(defn get_counts "Map the values of the hand to their counts" [hand to_count]
  (if (empty? to_count) '()
        (cons (count_value hand (rank (first to_count))) (get_counts hand (rest to_count)))))

(defn count_appearences "Count how many times a value appears in lst" [value lst]
  (cond
    (empty? lst) 0
    (= value (first lst)) (+ 1 (count_appearences value (rest lst)))
        :else (count_appearences value (rest lst))))

(defn sequence? "Check if the values form a sequence" [values]
  (or (= (sort (map #(cond (= 14 %) 1 :else %) values))
         (range (apply min (map #(cond (= 14 %) 1 :else %) values)) (+ (apply max (map #(cond (= 14 %) 1 :else %) values)) 1)))
      (= (sort values) (range (apply min values) (+ (apply max values) 1)))))

(defn same_colors "Check if all the colors are the same" [colors] (or (empty? colors) (empty? (rest (set colors)))))

;; Ovaa funkcija proveruva dali ima 2+1+1+1 vrednosti (listata od counts da lichi 2 2 1 1 1),
;; ako mozhe da ima i 2+2+1 (listata od counts da lichi 2 2 2 2 1 mozhe da se stavi
;; brojot na dvojki >=2 (da ima barem eden par) i brojot na 3ki da e 0
(defn pair? "Check if a combination has 2 cards (not more) with the same value" [hand]
  (and (= 2 (count_appearences 2 (get_counts hand hand)))
       (= 3 (count_appearences 1 (get_counts hand hand)))))

(defn three-of-a-kind? "Check if a hand has 3 cards (not more) with the same value)" [hand]
  (and (= 3 (count_appearences 3 (get_counts hand hand)))
       (= 2 (count_appearences 1 (get_counts hand hand)))))

(defn four-of-a-kind? "Check if a hand has 4 cards with the same value" [hand]
  (= 4 (count_appearences 4 (get_counts hand hand))))

(defn flush? "Check if all the cards in a combination are in the same color but not sequential" [hand]
  (and (same_colors (map suit hand)) (not (sequence? (map rank hand)))))

(defn full-house? "Check if the cards are in combination 3+2" [hand]
  (and (= 2 (count_appearences 2 (get_counts hand hand)))
       (= 3 (count_appearences 3 (get_counts hand hand)))))

(defn two-pairs? "Check if a hand has 2 pairs" [hand] (= 4 (count_appearences 2 (get_counts hand hand))))

(defn straight? "Check if the combination is sequential" [hand]
  (or (sequence? (replace {1 14} (map rank hand))) (sequence? (replace {14 1} (map rank hand)))))

(defn straight-flush? "Check if a hand has sequential cards in the same suit" [hand] (and (straight? hand) (same_colors (map suit hand))))

(defn value "Return a value based on the conditions applied on the hand" [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))

(defn kickers "Sorts the hand unique values firstly based on their counts and then on the value" [hand]
  (let [values (map rank hand)
        freq (frequencies values)
        sorted-frequencies (keys (into (sorted-map-by (fn [key1 key2] (cond (= (freq key2) (freq key1)) (< key2 key1) :else (compare (freq key2) (freq key1))))) freq))]
    sorted-frequencies))

(defn higher-kicker? "Compares 2 hands' kickers" [kicker1 kicker2]
  (cond
    (or (empty? kicker1) (empty? kicker2)) false
    (> (first kicker1) (first kicker2)) true
        (< (first kicker1) (first kicker2)) false
        (= (first kicker1) (first kicker2)) (higher-kicker? (rest kicker1) (rest kicker2))))

(defn beats? "Compares values of hands then their sorted lists using higher-kicker?" [hand1 hand2]
  (cond (> (value hand1) (value hand2)) true
        (= (value hand1) (value hand2)) (cond (higher-kicker? (kickers hand1) (kickers hand2)) true
                                              :else nil)
        :else nil))

(defn reduce-hands "Helper reduce function for determining the winning hand" [h1 h2]
  (cond (beats? h1 h2) h1 :else h2))

(defn winning-hand-wrap "Get the winning hand" [hands]
  (reduce reduce-hands hands))

(defn winning-hand "Get the winning hands" [& hands]
  (cond (empty? hands) nil :else (let [wh (winning-hand-wrap hands) filt (filter #(= (map rank (sort wh)) (map rank (sort %))) hands)] (cond (empty? (rest filt)) (first filt) :else filt) )))
