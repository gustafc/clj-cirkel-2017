(ns clj-cirkel-2017.hacking.misc.two-combinations
  (:use clojure.test))

(defn two-combinations
  {:test #(do
            (is (= [] (two-combinations [])))
            (is (= [] (two-combinations [:a])))
            (is (= [[:a :b]] (two-combinations [:a :b])))
            (is (= [[:a :b] [:a :c] [:a :d] [:b :c] [:b :d] [:c :d]] (two-combinations [:a :b :c :d])))
            (is (= [[0 1] [0 2] [0 3] [0 4] [0 5] [0 6]] (take 6 (two-combinations (range)))))
            )}
  [coll]
  (->> (iterate rest coll)
       (take-while seq)
       (mapcat (fn [coll] (map (partial vector (first coll)) (rest coll))))
       ))

(time (println (take 3 (drop 2e6 (two-combinations (range 1e6))))))
