;--- Day 3: Squares With Three Sides ---
;
; Now that you can think clearly, you move deeper into the labyrinth of hallways and office furniture that makes up this
; part of Easter Bunny HQ. This must be a graphic design department; the walls are covered in specifications for
; triangles.
;
; Or are they?
;
; The design document gives the side lengths of each triangle it describes, but... 5 10 25? Some of these aren't
; triangles. You can't help but mark the impossible ones.
;
; In a valid triangle, the sum of any two sides must be larger than the remaining side. For example, the "triangle"
; given above is impossible, because 5 + 10 is not larger than 25.
;
; In your puzzle input, how many of the listed triangles are possible?
;
; --- Part Two ---
;
; Now that you've helpfully marked up their design documents, it occurs to you that triangles are specified in groups of
; three vertically. Each set of three numbers in a column specifies a triangle. Rows are unrelated.
;
; For example, given the following specification, numbers with the same hundreds digit would be part of the same
; triangle:
;
; 101 301 501
; 102 302 502
; 103 303 503
; 201 401 601
; 202 402 602
; 203 403 603
;
; In your puzzle input, and instead reading by columns, how many of the listed triangles are possible?



(ns advent-03
  (:use clojure.test))

(defn possible-triangle?
  {:test #(do
            (is (possible-triangle? [5 6 10]))
            (is (not (possible-triangle? [5 10 25])))
            (is (not (possible-triangle? [10 5 25])))
            )}
  [[a b c :as sides]]
  (let [sum-of-sides (reduce + sides)]
    (every? #(< %1 (- sum-of-sides %1)) sides)
    ))

(defn column-triples
  {:test #(do (is (= [[101 102 103]
                      [201 202 203]
                      [301 302 303]
                      [401 402 403]
                      [501 502 503]
                      [601 602 603]]
                     (column-triples [[101 301 501]
                                      [102 302 502]
                                      [103 303 503]
                                      [201 401 601]
                                      [202 402 602]
                                      [203 403 603]]))))}
  [lines]
  (->> [0 1 2]
       (mapcat (fn [col-n] (map #(nth % col-n) lines)))
       (reduce (fn [triples n]
                 (let [last-triple (peek triples)]
                   (if (or (not last-triple) (= 3 (count last-triple)))
                     (conj triples [n])
                     (conj (pop triples) (conj last-triple n)))
                   ))
               []
               ))
  )

(def input (->> (clojure.java.io/file "./resources/advent/day03_triangles.txt")
                (slurp)
                (re-seq #"(?m)^\s*(\d+)\s+(\d+)\s+(\d+)\s*$")
                (map (fn [[_ a b c]] (map read-string [a b c])))))

(defn solve-part-1
  {:test #(do (is (= 869 (solve-part-1))))}
  []
  (->> input
       (filter possible-triangle?)
       (count)))

(defn solve-part-2
  {:test #(do (is (= 1544 (solve-part-2))))}
  []
  (->> (column-triples input)
       (filter possible-triangle?)
       (count)))


(prn "Part 1: " (solve-part-1))
(prn "Part 2: " (solve-part-2))