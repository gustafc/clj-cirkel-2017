; Talking about precedence in 1.2.2 - is Clj ltr or rtl?
[(/ 100 2 4)
 (/ (/ 100 2) 4)
 (/ 100 (/ 2 4))]

(defn print-and-return [n]
  (println "*" n)
  n)

; Seems like ltr!
(+ (print-and-return 1) (print-and-return 2))