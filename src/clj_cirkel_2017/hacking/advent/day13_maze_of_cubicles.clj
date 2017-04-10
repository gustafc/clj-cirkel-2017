; --- Day 13: A Maze of Twisty Little Cubicles ---
;
; You arrive at the first floor of this new building to discover a much less welcoming environment than the shiny atrium
; of the last one. Instead, you are in a maze of twisty little cubicles, all alike.
;
; Every location in this area is addressed by a pair of non-negative integers (x,y). Each such coordinate is either a
; wall or an open space. You can't move diagonally. The cube maze starts at 0,0 and seems to extend infinitely toward
; positive x and y; negative values are invalid, as they represent a location outside the building. You are in a small
; waiting area at 1,1.
;
; While it seems chaotic, a nearby morale-boosting poster explains, the layout is actually quite logical. You can
; determine whether a given x,y coordinate will be a wall or an open space using a simple system:
;
;     Find x*x + 3*x + 2*x*y + y + y*y.
;     Add the office designer's favorite number (your puzzle input).
;     Find the binary representation of that sum; count the number of bits that are 1.
;         If the number of bits that are 1 is even, it's an open space.
;         If the number of bits that are 1 is odd, it's a wall.
;
; For example, if the office designer's favorite number were 10, drawing walls as # and open spaces as ., the corner of
; the building containing 0,0 would look like this:
;
;   0123456789
; 0 .#.####.##
; 1 ..#..#...#
; 2 #....##...
; 3 ###.#.###.
; 4 .##..#..#.
; 5 ..##....#.
; 6 #...##.###
;
; Now, suppose you wanted to reach 7,4. The shortest route you could take is marked as O:
;
;   0123456789
; 0 .#.####.##
; 1 .O#..#...#
; 2 #OOO.##...
; 3 ###O#.###.
; 4 .##OO#OO#.
; 5 ..##OOO.#.
; 6 #...##.###
;
; Thus, reaching 7,4 would take a minimum of 11 steps (starting from your current location, 1,1).
;
; What is the fewest number of steps required for you to reach 31,39?
;
; Your puzzle input is 1364.



(ns clj_cirkel_2017.hacking.advent.day13-maze-of-cubicles
  (:use clojure.test)
  (:require [clojure.string :as strings])
  (:require [clojure.set :as sets]))

(defn count-set-bits
  {:test #(do
            (is (= 0 (count-set-bits 2r0)))
            (is (= 1 (count-set-bits 2r1)))
            (is (= 1 (count-set-bits 2r10)))
            (is (= 2 (count-set-bits 2r11)))
            (is (= 3 (count-set-bits 2r10000101)))
            )}
  [n]
  (loop [n n
         set-bits 0]
    (if (= 0 n)
      set-bits
      (recur (unsigned-bit-shift-right n 1) (+ set-bits (bit-and n 1))))
    ))

(def example-magic 10)

(defn open-space?
  {:test #(is (= [".#.####.##"
                  "..#..#...#"
                  "#....##..."
                  "###.#.###."
                  ".##..#..#."
                  "..##....#."
                  "#...##.###"]
                 (->> (range 7)
                      (map (fn [y] (->> (range 10)
                                        (map (fn [x]
                                               (if (open-space? example-magic x y)
                                                 \.
                                                 \#)))
                                        (apply str))))
                      (vec)))
              )}
  [magic-n x y]
  (let [; Find x*x + 3*x + 2*x*y + y + y*y.
        multiplications (+ (* x x) (* 3 x) (* 2 x y) y (* y y))
        ; Add the office designer's favorite number (your puzzle input).
        with-magic (+ magic-n multiplications)
        ; Find the binary representation of that sum; count the number of bits that are 1.
        set-bits (count-set-bits with-magic)
        ]
    ; If the number of bits that are 1 is even, it's an open space.
    (= 0 (mod set-bits 2))
    ))


(defn traversible-neighbours-for-magic
  {:test #(let [example-neigbhours (comp set (traversible-neighbours-for-magic example-magic))]
            (is (= #{[0 1]} (example-neigbhours [0 0])))
            (is (= #{[0 0] [1 1]} (example-neigbhours [0 1])))
            (is (= #{[6 4]
                     [5 5] [7 5]
                     [6 6]
                     } (example-neigbhours [6 5])))
            )}
  [magic-n]
  (let [traversible-coordinate? (memoize (fn [[x y]] (and (not (neg? x))
                                                          (not (neg? y))
                                                          (open-space? magic-n x y))))]
    (fn [[x y]]
      (->> [[x (dec y)]
            [(dec x) y] [(inc x) y]
            [x (inc y)]]
           (filter traversible-coordinate?)
           ))))

(defn find-paths-from
  {:test #(do (is (= #{[0 0] [0 1] [1 1]}
                     (->> (find-paths-from example-magic [0 1])
                          (take 3)
                          (map peek)
                          (set)))))}
  [magic-n start-coord]
  (->> (comp (traversible-neighbours-for-magic magic-n) peek)
       (clj_cirkel_2017.hacking.advent.day11-rtg/traverse-breadth-first start-coord)))

(defn find-path
  {:test #(do
            (is (= [[0 0] [0 1] [1 1]] (find-path example-magic [0 0] [1 1])))
            (is (= 12 (count (find-path example-magic [1 1] [7 4]))))
            )}
  [magic-n start-coord end-coord]
  (->> (find-paths-from magic-n start-coord)
       (filter #(= (peek %) end-coord))
       (first)))

(defn coords-within-steps
  {:test #(do
            (is (= #{[0 0] [0 1] [1 1]}
                   (coords-within-steps example-magic [0 1] 1))))}
  [magic-n start-coord steps]
  (->> (find-paths-from magic-n start-coord)
       (take-while #(<= (count %) (inc steps)))
       (reduce into #{})
       ))

(def puzzle-magic 1364)

(defn solve-part-1
  {:test #(is (= 86 (dec (count (solve-part-1)))))}
  []
  (find-path puzzle-magic [1 1] [31 39]))

(defn solve-part-2
  {:test #(is (= 127 (count (solve-part-2))))}
  []
  (coords-within-steps puzzle-magic [1 1] 50))