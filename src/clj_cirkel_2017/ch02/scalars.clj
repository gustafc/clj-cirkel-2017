;; Works correctly
(->> (range 100)
     (map (fn [n] 1000N))
     (reduce *))

;; Overflow! Not promoted to larger number type
(->> (range 100)
     (map (fn [n] 1000))
     (reduce *))


;; How to string concat?
(str "One " 2 " three")

