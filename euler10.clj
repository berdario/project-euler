

(defn prime? [n]
  (every? #(not= % 0) (map #(rem n %) (range 2 (inc (int (java.lang.Math/sqrt n)))))))

(println (apply + (filter prime? (range 2 2000000))))
