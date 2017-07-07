; --- Day 18: Like a Rogue ---
;
; As you enter this room, you hear a loud click! Some of the tiles in the floor here seem to be pressure plates for
; traps, and the trap you just triggered has run out of... whatever it tried to do to you. You doubt you'll be so lucky
; next time.
;
; Upon closer examination, the traps and safe tiles in this room seem to follow a pattern. The tiles are arranged into
; rows that are all the same width; you take note of the safe tiles (.) and traps (^) in the first row (your puzzle
; input).
;
; The type of tile (trapped or safe) in each row is based on the types of the tiles in the same position, and to either
; side of that position, in the previous row. (If either side is off either end of the row, it counts as "safe" because
; there isn't a trap embedded in the wall.)
;
; For example, suppose you know the first row (with tiles marked by letters) and want to determine the next row (with
; tiles marked by numbers):
;
; ABCDE
; 12345
;
; The type of tile 2 is based on the types of tiles A, B, and C; the type of tile 5 is based on tiles D, E, and an
; imaginary "safe" tile. Let's call these three tiles from the previous row the left, center, and right tiles,
; respectively. Then, a new tile is a trap only in one of the following situations:
;
;     Its left and center tiles are traps, but its right tile is not.
;     Its center and right tiles are traps, but its left tile is not.
;     Only its left tile is a trap.
;     Only its right tile is a trap.
;
; In any other situation, the new tile is safe.
;
; Then, starting with the row ..^^., you can determine the next row by applying those rules to each new tile:
;
;     The leftmost character on the next row considers the left (nonexistent, so we assume "safe"), center (the first .,
;     which means "safe"), and right (the second ., also "safe") tiles on the previous row. Because all of the trap
;     rules require a trap in at least one of the previous three tiles, the first tile on this new row is also safe, ..
;
;     The second character on the next row considers its left (.), center (.), and right (^) tiles from the previous
;     row. This matches the fourth rule: only the right tile is a trap. Therefore, the next tile in this new row is a
;     trap, ^.
;
;     The third character considers .^^, which matches the second trap rule: its center and right tiles are traps, but
;     its left tile is not. Therefore, this tile is also a trap, ^.
;
;     The last two characters in this new row match the first and third rules, respectively, and so they are both also
;     traps, ^.
;
; After these steps, we now know the next row of tiles in the room: .^^^^. Then, we continue on to the next row, using
; the same rules, and get ^^..^. After determining two new rows, our map looks like this:
;
; ..^^.
; .^^^^
; ^^..^
;
; Here's a larger example with ten tiles per row and ten rows:
;
; .^^.^.^^^^
; ^^^...^..^
; ^.^^.^.^^.
; ..^^...^^^
; .^^^^.^^.^
; ^^..^.^^..
; ^^^^..^^^.
; ^..^^^^.^^
; .^^^..^.^^
; ^^.^^^..^^
;
; In ten rows, this larger example has 38 safe tiles.
;
; Starting with the map in your puzzle input, in a total of 40 rows (including the starting row), how many safe tiles
; are there?
;
; --- Part Two ---
;
; How many safe tiles are there in a total of 400000 rows?
;

(ns clj-cirkel-2017.hacking.advent.day18-like-a-rogue
  (:use [clojure.test :refer [is]])
  )

(defn parse-row
  {:test #(is (= [:safe :safe :trap :trap :safe] (parse-row "..^^.")))}
  [s]
  (map #(if (= \^ %) :trap :safe) s)
  )

(defn format-row
  {:test #(is (= "..^^." (format-row (parse-row "..^^."))))}
  [row]
  (clojure.string/join (map #(if (= :trap %) \^ \.) row))
  )

(defn generate-rows
  {:test #(do
            (is (= '(
                      "..^^."
                      ".^^^^"
                      "^^..^")
                   (map format-row (take 3 (generate-rows (parse-row "..^^."))))
                   ))
            (is (= '(
                      ".^^.^.^^^^"
                      "^^^...^..^"
                      "^.^^.^.^^."
                      "..^^...^^^"
                      ".^^^^.^^.^"
                      "^^..^.^^.."
                      "^^^^..^^^."
                      "^..^^^^.^^"
                      ".^^^..^.^^"
                      "^^.^^^..^^")
                   (map format-row (take 10 (generate-rows (parse-row ".^^.^.^^^^"))))
                   ))
            )}
  [previous]
  (let [cols (count previous)]
    (iterate (fn [previous]
               (letfn [(safe? [index]
                         (let [tiles [(nth previous (dec index) :safe)
                                      (nth previous index)
                                      (nth previous (inc index) :safe)]]
                           (case tiles
                             ;     Its left and center tiles are traps, but its right tile is not.
                             [:trap :trap :safe] false
                             ;     Its center and right tiles are traps, but its left tile is not.
                             [:safe :trap :trap] false
                             ;     Only its left tile is a trap.
                             [:trap :safe :safe] false
                             ;     Only its right tile is a trap.
                             [:safe :safe :trap] false
                             ;     In any other situation, the new tile is safe.
                             true
                             )))]
                 (->> (range cols)
                      (map #(if (safe? %) :safe :trap))
                      (vec)
                      )))
             previous))
  )

(defn count-safe-tiles-in-rows
  {:test #(is (= 6 (count-safe-tiles-in-rows (take 3 (generate-rows (parse-row "..^^."))))))}
  [rows]
  (->> (mapcat identity rows)
       (filter #(= % :safe))
       (count))
  )

(def puzzle-input ".^^^^^.^^^..^^^^^...^.^..^^^.^^....^.^...^^^...^^^^..^...^...^^.^.^.......^..^^...^.^.^^..^^^^^...^.")

(defn solve-part-1
  {:test #(is (= 1956 (solve-part-1)))}
  []
  (count-safe-tiles-in-rows (take 40 (generate-rows (parse-row puzzle-input))))
  )

(defn solve-part-2
  {:test #(is (= 19995121 (solve-part-2)))}
  []
  (count-safe-tiles-in-rows (take 400000 (generate-rows (parse-row puzzle-input))))
  )