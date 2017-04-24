(ns clj-cirkel-2017.hacking.advent.day15-timing
  (:use [clojure.test :only [is]]))

; --- Day 15: Timing is Everything ---
;
; The halls open into an interior plaza containing a large kinetic sculpture. The sculpture is in a sealed enclosure and
; seems to involve a set of identical spherical capsules that are carried to the top and allowed to bounce through the
; maze of spinning pieces.
;
; Part of the sculpture is even interactive! When a button is pressed, a capsule is dropped and tries to fall through
; slots in a set of rotating discs to finally go through a little hole at the bottom and come out of the sculpture. If
; any of the slots aren't aligned with the capsule as it passes, the capsule bounces off the disc and soars away. You
; feel compelled to get one of those capsules.
;
; The discs pause their motion each second and come in different sizes; they seem to each have a fixed number of
; positions at which they stop. You decide to call the position with the slot 0, and count up for each position it
; reaches next.
;
; Furthermore, the discs are spaced out so that after you push the button, one second elapses before the first disc is
; reached, and one second elapses as the capsule passes from one disc to the one below it. So, if you push the button at
; time=100, then the capsule reaches the top disc at time=101, the second disc at time=102, the third disc at time=103,
; and so on.
;
; The button will only drop a capsule at an integer time - no fractional seconds allowed.
;
; For example, at time=0, suppose you see the following arrangement:
;
; Disc #1 has 5 positions; at time=0, it is at position 4.
; Disc #2 has 2 positions; at time=0, it is at position 1.
;
; If you press the button exactly at time=0, the capsule would start to fall; it would reach the first disc at time=1.
; Since the first disc was at position 4 at time=0, by time=1 it has ticked one position forward. As a five-position
; disc, the next position is 0, and the capsule falls through the slot.
;
; Then, at time=2, the capsule reaches the second disc. The second disc has ticked forward two positions at this point:
; it started at position 1, then continued to position 0, and finally ended up at position 1 again. Because there's only
; a slot at position 0, the capsule bounces away.
;
; If, however, you wait until time=5 to push the button, then when the capsule reaches each disc, the first disc will
; have ticked forward 5+1 = 6 times (to position 0), and the second disc will have ticked forward 5+2 = 7 times (also to
; position 0). In this case, the capsule would fall through the discs and come out of the machine.
;
; However, your situation has more than two discs; you've noted their positions in your puzzle input. What is the first
; time you can press the button to get a capsule?
;
; --- Part Two ---
;
; After getting the first capsule (it contained a star! what great fortune!), the machine detects your success and
; begins to rearrange itself.
;
; When it's done, the discs are back in their original configuration as if it were time=0 again, but a new disc with 11
; positions and starting at position 0 has appeared exactly one second below the previously-bottom disc.
;
; With this new disc, and counting again starting from time=0 with the configuration in your puzzle input, what is the
; first time you can press the button to get another capsule?
;

(def example-input "Disc #1 has 5 positions; at time=0, it is at position 4.
Disc #2 has 2 positions; at time=0, it is at position 1.")

(def puzzle-input "Disc #1 has 5 positions; at time=0, it is at position 2.
Disc #2 has 13 positions; at time=0, it is at position 7.
Disc #3 has 17 positions; at time=0, it is at position 10.
Disc #4 has 3 positions; at time=0, it is at position 2.
Disc #5 has 19 positions; at time=0, it is at position 9.
Disc #6 has 7 positions; at time=0, it is at position 0.")

(defn mk-disc
  [& {:keys [offset positions initial-position]}]
  {:offset offset :positions positions :initial-position initial-position})

(defn parse-input
  {:test #(do
            (is (= [(mk-disc :offset 1 :positions 5 :initial-position 4)
                    (mk-disc :offset 2 :positions 2 :initial-position 1)]
                   (parse-input example-input)))
            )}
  [s]
  (->> (re-seq #"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)." s)
       (map #(mk-disc :offset (read-string (%1 1))
                      :positions (read-string (%1 2))
                      :initial-position (read-string (%1 3))))
       ))

(def example-discs (parse-input example-input))
(def puzzle-discs (parse-input puzzle-input))

(defn pass-through-at?
  {:test #(let [disc (mk-disc :offset 1 :positions 5 :initial-position 4)]
            (is (= {true [0 5 10] false [1 2 3 4 6 7 8 9 11]}
                   (group-by (partial pass-through-at? disc) (range 12))))
            )}
  [disc drop-time]
  (= 0 (mod
         (+ drop-time
            (disc :initial-position)
            (disc :offset))
         (disc :positions)
         )))

(defn all-pass-through?
  {:test #(is (= {true [5] false [0 1 2 3 4 6 7 8 9]}
                 (group-by (partial all-pass-through? example-discs) (range 10)))
              )}
  [discs drop-time]
  (every? #(pass-through-at? % drop-time) discs))

(defn first-pass-through-drop-time
  {:test #(is (= 5 (first-pass-through-drop-time example-discs)))}
  [discs]
  (->> (range)
       (filter (partial all-pass-through? discs))
       (first)))

(defn solve-part-1
  {:test #(is (= 148737 (solve-part-1)))}
  []
  (first-pass-through-drop-time puzzle-discs)
  )

(defn solve-part-2
  {:test #(is (= 2353212 (solve-part-2)))}
  []
  (first-pass-through-drop-time (conj puzzle-discs (mk-disc :offset (inc (count puzzle-discs))
                                                            :positions 11
                                                            :initial-position 0)))
  )