(ns fibstream
  (:use clojure.test))

(defn fibs
  {:test #(do
            (is (= [] (take 0 (fibs))))
            (is (= [1] (take 1 (fibs))))
            (is (= [1 1] (take 2 (fibs))))
            (is (= [1 1 2] (take 3 (fibs))))
            (is (= [1 1 2 3] (take 4 (fibs))))
            (is (= [1 1 2 3 5] (take 5 (fibs))))
            (is (= [1 1 2 3 5 8] (take 6 (fibs))))
            (is (= [1 1 2 3 5 8 13] (take 7 (fibs))))
            (is (= [21 34 55] (take 3 (drop 7 (fibs)))))
            )}
  []
  (->> (iterate (fn [[pp p]] [p (+ pp p)]) [0 1])
       (map peek)
       ))