; Can, disappointingly, be written as:
; (defn pairs [coll] (map vector coll (rest coll)))

(defn pairs [coll]
  (if-not (seq coll)
    []
    (loop [pairs [] [head & tail] coll]
      (if-not (seq tail)
        pairs
        (let [[next-head & next-tail] tail]
          (recur (conj pairs [head next-head]) tail))))))

[(pairs [])
 (pairs [1])
 (pairs [1 2])
 (pairs [1 2 3])
 (pairs [1 2 3 4])
 (pairs [1 2 3 4 5])
 ]
