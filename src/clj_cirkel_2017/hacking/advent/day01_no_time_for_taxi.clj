; --- Day 1: No Time for a Taxicab ---
;
; Santa's sleigh uses a very high-precision clock to guide its movements, and the clock's oscillator is regulated by
; stars. Unfortunately, the stars have been stolen... by the Easter Bunny. To save Christmas, Santa needs you to
; retrieve all fifty stars by December 25th.
;
; Collect stars by solving puzzles. Two puzzles will be made available on each day in the advent calendar; the second
; puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
;
; You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near", unfortunately, is as close as you can
; get - the instructions on the Easter Bunny Recruiting Document the Elves intercepted start here, and nobody had time
; to work them out further.
;
; The Document indicates that you should start at the given coordinates (where you just landed) and face North. Then,
; follow the provided sequence: either turn left (L) or right (R) 90 degrees, then walk forward the given number of
; blocks, ending at a new intersection.
;
; There's no time to follow such ridiculous instructions on foot, though, so you take a moment and work out the
; destination. Given that you can only walk on the street grid of the city, how far is the shortest path to the
; destination?
;
; For example:
;
; Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
; R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
; R5, L5, R5, R3 leaves you 12 blocks away.
;
; --- Part Two ---
;
; Then, you notice the instructions continue on the back of the Recruiting Document. Easter Bunny HQ is actually at the
; first location you visit twice.
;
; For example, if your instructions are R8, R4, R4, R8, the first location you visit twice is 4 blocks away, due East.
;
; How many blocks away is the first location you visit twice?


(ns clj_cirkel_2017.hacking.advent.day01-no-time-for-taxi
  (:use clojure.test))

(def N :north)
(def E :east)
(def S :south)
(def W :west)

(def R :right)
(def L :left)


(defn abs
  {:test #(do
            (is (= 1 (abs 1)))
            (is (= 1 (abs -1)))
            )}
  [n]
  (if (<= 0 n) n (- n)))

(defn turn
  {:test #(doseq [[facing after-left after-right] [[N W E] [E N S] [S E W] [W S N]]]
            (is (= after-left (turn facing L)))
            (is (= after-right (turn facing R)))
            )}
  [now-facing turn-to]
  (->>
    (mod (+ (now-facing {N 0 E 1 S 2 W 3})
            (turn-to {R 1 L -1})
            4)
         4)
    (nth [N E S W])))

;; BEGIN OLD
; Solution built to solve first part. Gives correct result by invoking:
;     (apply + (map val (simplify (navigate input))))

(defn navigate
  {:test #(do
            (is (= {N 0 E 0 S 0 W 0} (navigate [])))
            (is (= {N 0 E 1 S 0 W 0} (navigate [[R 1]])))
            (is (= {N 3 E 2 S 0 W 0} (navigate [[R 2] [L 3]])))
            (is (= {N 0 E 2 S 2 W 2} (navigate [[R 2] [R 2] [R 2]])))
            (is (= {N 1 E 2 S 1 W 1} (navigate (take 5 (repeat [R 1])))))
            )}
  [directions]
  (:moves
    (reduce
      (fn [{:keys [facing moves]} [turn-to count]]
        (let [now-facing (turn facing turn-to)]
          {:facing now-facing :moves (update moves now-facing #(+ % count))}))
      {:facing N :moves {N 0 E 0 S 0 W 0}}
      directions)))

(defn simplify
  {:test #(do
            (is (= {} (simplify {N 0 E 0 S 0 W 0})))
            (is (= {N 5} (simplify {N 5 E 0 S 0 W 0})))
            (is (= {W 3} (simplify {N 0 E 0 S 0 W 3})))
            (is (= {N 1 W 2} (simplify {N 5 E 4 S 4 W 6})))
            )}
  [moves]
  (reduce
    (fn [simplified dir]
      (let [rev (turn (turn dir R) R)
            diff (- (dir moves) (rev moves))]
        (cond
          (= 0 diff) simplified
          (< 0 diff) (assoc simplified dir diff)
          (> 0 diff) (assoc simplified rev (- diff)))))
    {}
    [N E]))

;; END OLD

(defn to-coordinates
  {:test #(do
            (is (= [[0 0]] (to-coordinates [])))
            (is (= [[0 0] [1 0]] (to-coordinates [[R 1]])))
            (is (= [[0 0] [1 0] [2 0] [2 1] [2 2]] (to-coordinates [[R 2] [R 2]])))
            )}
  [directions]
  (last
    (reduce
      (fn [[facing-before coords] [turn-direction steps]]
        (let [[prev-x prev-y] (last coords)
              facing-after (turn facing-before turn-direction)
              movement-x (case facing-after :east 1 :west -1 0)
              movement-y (case facing-after :north -1 :south 1 0)]
          [facing-after
           (concat coords
                   (map #(vector (+ prev-x (* movement-x %1))
                                 (+ prev-y (* movement-y %1)))
                        (range 1 (+ 1 steps))))]))
      [N [[0 0]]]
      directions)))

(defn first-duplicate
  {:test #(do
            (is (= nil (first-duplicate [])))
            (is (= nil (first-duplicate [1])))
            (is (= nil (first-duplicate [1 2])))
            (is (= 1 (first-duplicate [1 1])))
            (is (= 3 (first-duplicate [1 2 3 4 3 2 1])))
            )}
  [coll]
  (loop [visited #{} [curr & rest :as remaining] coll]
    (if-not (seq remaining)
      nil
      (let [new-visited (conj visited curr)]
        (if (= visited new-visited) curr
                                    (recur new-visited rest))))))

(defn distance-to
  {:test #(do
            (is (= 0 (distance-to [0 0])))
            (is (= 2 (distance-to [1 1])))
            (is (= 2 (distance-to [-1 -1])))
            )}
  [[x y :as coord]]
  (reduce + (map abs coord)))

(def input-str "R5, L2, L1, R1, R3, R3, L3, R3, R4, L2, R4, L4, R4, R3, L2, L1, L1, R2, R4, R4, L4, R3, L2, R1, L4, R1,
                R3, L5, L4, L5, R3, L3, L1, L1, R4, R2, R2, L1, L4, R191, R5, L2, R46, R3, L1, R74, L2, R2, R187, R3,
                R4, R1, L4, L4, L2, R4, L5, R4, R3, L2, L1, R3, R3, R3, R1, R1, L4, R4, R1, R5, R2, R1, R3, L4, L2, L2,
                R1, L3, R1, R3, L5, L3, R5, R3, R4, L1, R3, R2, R1, R2, L4, L1, L1, R3, L3, R4, L2, L4, L5, L5, L4, R2,
                R5, L4, R4, L2, R3, L4, L3, L5, R5, L4, L2, R3, R5, R5, L1, L4, R3, L1, R2, L5, L1, R4, L1, R5, R1, L4,
                L4, L4, R4, R3, L5, R1, L3, R4, R3, L2, L1, R1, R2, R2, R2, L1, L1, L2, L5, L3, L1")
(def input (->> (re-seq #"([RL])(\d+)" input-str)
                (map (fn [[_ dir n]] [(if (= "R" dir) R L) (read-string n)]))))

(defn solve-part-1
  {:test #(do (is (= 287 (solve-part-1 input))))}           ; Already solved, ensure we don't break
  [input]
  (->> (to-coordinates input)
       (last)
       (distance-to)))

(defn solve-part-2
  {:test #(do (is (= 133 (solve-part-2 input))))}           ; Already solved, ensure we don't break
  [input]
  (->> (to-coordinates input)
       (first-duplicate)
       (distance-to)))

(prn "Part 1: " (solve-part-1 input))
(prn "Part 2: " (solve-part-2 input))
