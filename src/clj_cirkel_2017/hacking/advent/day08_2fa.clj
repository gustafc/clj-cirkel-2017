; --- Day 8: Two-Factor Authentication ---
;
; You come across a door implementing what you can only assume is an implementation of two-factor authentication after a
; long game of requirements telephone.
;
; To get past the door, you first swipe a keycard (no problem; there was one on a nearby desk). Then, it displays a code
; on a little screen, and you type that code on a keypad. Then, presumably, the door unlocks.
;
; Unfortunately, the screen has been smashed. After a few minutes, you've taken everything apart and figured out how it
; works. Now you just have to work out what the screen would have displayed.
;
; The magnetic strip on the card you swiped encodes a series of instructions for the screen; these instructions are your
; puzzle input. The screen is 50 pixels wide and 6 pixels tall, all of which start off, and is capable of three somewhat
; peculiar operations:
;
;     rect AxB turns on all of the pixels in a rectangle at the top-left of the screen which is A wide and B tall.
;
;     rotate row y=A by B shifts all of the pixels in row A (0 is the top row) right by B pixels. Pixels that would fall
;     off the right end appear at the left end of the row.
;
;     rotate column x=A by B shifts all of the pixels in column A (0 is the left column) down by B pixels. Pixels that
;     would fall off the bottom appear at the top of the column.
;
; For example, here is a simple sequence on a smaller screen:
;
;     rect 3x2 creates a small rectangle in the top-left corner:
;
;     ###....
;     ###....
;     .......
;
;     rotate column x=1 by 1 rotates the second column down by one pixel:
;
;     #.#....
;     ###....
;     .#.....
;
;     rotate row y=0 by 4 rotates the top row right by four pixels:
;
;     ....#.#
;     ###....
;     .#.....
;
;     rotate column x=1 by 1 again rotates the second column down by one pixel, causing the bottom pixel to wrap back to
;     the top:
;
;     .#..#.#
;     #.#....
;     .#.....
;
; As you can see, this display technology is extremely powerful, and will soon dominate the tiny-code-displaying-screen
; market. That's what the advertisement on the back of the display tries to convince you, anyway.
;
; There seems to be an intermediate check of the voltage used by the display: after you swipe your card, if the screen
; did work, how many pixels should be lit?
;

(ns clj_cirkel_2017.hacking.advent.day08-2fa
  (:use clojure.test)
  (:require [clojure.string :as strings]))

(defn make-display
  {:test #(is (= {:width 3 :height 2 :pixels [false false false
                                              false false false]}
                 (make-display 3 2)))}
  [width height]
  {:width width :height height :pixels (vec (repeat (* width height) false))})

(defn- lines [& lns]
  (strings/join "\n" lns))

(def display-4-by-3 (make-display 4 3))

(defn render-display
  {:test #(do (is (= (lines "...."
                            "...."
                            "....")
                     (render-display display-4-by-3)))
              )}
  [{:keys [width pixels]}]
  (->> (partition width pixels)
       (map #(strings/join (map (fn [v] (if v "#" ".")) %)))
       (apply lines)
       ))

(defn set-pixels
  {:test #(do (is (= (lines ".#.#"
                            "#.#."
                            "....")
                     (render-display (set-pixels display-4-by-3 [1 3 4 6]))))
              (is (vector? (:pixels (set-pixels display-4-by-3 [1 2 3]))))
              )}
  [display pixels-to-set]
  (if (empty? pixels-to-set)
    display
    (update display :pixels #(->> (repeat true)
                                  (mapcat vector pixels-to-set)
                                  (apply assoc %)))
    ))

(defn set-pixel-at
  {:test #(do (is (= (lines "...."
                            "..#."
                            "....")
                     (render-display (set-pixel-at display-4-by-3 2 1))))
              )}
  [display x y]
  (set-pixels display [(+ x (* y (display :width)))])
  )

(defn- d43-with-pixel [x y] (set-pixel-at display-4-by-3 x y))

(defn op-rect
  {:test #(do (is (= display-4-by-3 (op-rect display-4-by-3 0 0)))
              (is (= (lines "#..."
                            "...."
                            "....")
                     (render-display (op-rect display-4-by-3 1 1))))
              (is (= (lines "###."
                            "###."
                            "....")
                     (render-display (op-rect display-4-by-3 3 2))))
              )}
  [{:keys [width pixels] :as display} rect-w rect-h]
  (->> (range rect-h)
       (map #(* width %))
       (mapcat #(range % (+ rect-w %)))
       (set-pixels display)
       ))

(defn rotate-indices
  {:test #(let [arr [:a :b :c :d :e]]
            (is (= arr (rotate-indices [1 3 4] 0 arr)))
            (is (= arr (rotate-indices [1 3 4] 3 arr)))
            (is (= [:a :e :c :b :d] (rotate-indices [1 3 4] 1 arr)))
            (is (= [:a :e :c :b :d] (rotate-indices [1 3 4] 4 arr)))
            )}
  [indices rotations arr]
  (let [rotations (mod rotations (count indices))
        target-indices (concat (drop rotations indices) (take rotations indices))]
    (->> (map vector indices target-indices)
         (reduce (fn [curr-arr [src dst]]
                   (assoc curr-arr dst (arr src)))
                 arr)
         )))

(defn op-rotate-row
  {:test #(let [d43-with-0-0-set (d43-with-pixel 0 0)
                d43-with-2-1-set (d43-with-pixel 2 1)
                d43-with-3-2-set (d43-with-pixel 3 2)]
            ; First row
            (is (= display-4-by-3 (op-rotate-row display-4-by-3 0 0)))
            (is (= d43-with-0-0-set (op-rotate-row d43-with-0-0-set 0 0)))
            (is (= d43-with-0-0-set (op-rotate-row d43-with-0-0-set 0 4)))
            (is (= (render-display (d43-with-pixel 2 0))
                   (render-display (op-rotate-row d43-with-0-0-set 0 2))))
            (is (= (render-display (d43-with-pixel 2 0))
                   (render-display (op-rotate-row d43-with-0-0-set 0 6))))
            (is (= (render-display (d43-with-pixel 1 0))
                   (render-display (op-rotate-row d43-with-0-0-set 0 1))))
            (is (= (render-display (d43-with-pixel 1 0))
                   (render-display (op-rotate-row d43-with-0-0-set 0 5))))
            ; Middle row
            (is (= (render-display d43-with-2-1-set)
                   (render-display (op-rotate-row d43-with-2-1-set 1 4))))
            (is (= (render-display (d43-with-pixel 3 1))
                   (render-display (op-rotate-row d43-with-2-1-set 1 1))))
            (is (= (render-display (d43-with-pixel 1 1))
                   (render-display (op-rotate-row d43-with-2-1-set 1 3))))
            ; Last row
            (is (= (render-display d43-with-3-2-set)
                   (render-display (op-rotate-row d43-with-3-2-set 2 4))))
            (is (= (render-display (d43-with-pixel 0 2))
                   (render-display (op-rotate-row d43-with-3-2-set 2 1))))
            (is (= (render-display (d43-with-pixel 2 2))
                   (render-display (op-rotate-row d43-with-3-2-set 2 7))))
            )}
  [{:keys [width] :as display} row rotations]
  (update display :pixels (fn [pixels]
                            (let [start-index (* width row)
                                  end-index (+ width start-index)]
                              (rotate-indices
                                (range start-index end-index)
                                rotations
                                pixels))
                            ))
  )

(defn op-rotate-col
  {:test #(do
            ; Simple rotation
            (is (= (render-display (d43-with-pixel 0 1))
                   (render-display (op-rotate-col (d43-with-pixel 0 0) 0 1))))
            ; Overflow
            (is (= (render-display (d43-with-pixel 3 0))
                   (render-display (op-rotate-col (d43-with-pixel 3 1) 3 2))))
            )}
  [{:keys [width] :as display} col rotations]
  (update display :pixels (fn [pixels]
                            (let [first-index col
                                  last-index (- (count pixels) (- width col))
                                  indices (range first-index (inc last-index) width)]
                              (rotate-indices
                                indices
                                rotations
                                pixels))
                            )))

(defn compile-op
  {:test #(let [test-display (-> display-4-by-3             ; setup a randomly lit display
                                 (op-rect 1 4)
                                 (op-rotate-col 1 3)
                                 (op-rotate-row 2 2))
                compile-and-run (fn [op] ((compile-op op) test-display))]
            (is (= (op-rect test-display 1 3) (compile-and-run "rect 1x3")))
            (is (= (op-rotate-row test-display 1 3) (compile-and-run "rotate row y=1 by 3")))
            (is (= (op-rotate-col test-display 3 2) (compile-and-run "rotate column x=3 by 2")))
            )}
  [op]

  (let [compile-if-matches (fn [re compiler]
                             (if-let [matching (re-seq re op)]
                               (let [[_ operand1 operand2] (first matching)]
                                 (compiler (read-string operand1) (read-string operand2)))))]
    (or (compile-if-matches #"rect (\d+)x(\d+)" (fn [width height] #(op-rect % width height)))
        (compile-if-matches #"rotate row y=(\d+) by (\d+)" (fn [row rotations] #(op-rotate-row % row rotations)))
        (compile-if-matches #"rotate column x=(\d+) by (\d+)" (fn [col rotations] #(op-rotate-col % col rotations)))
        (assert false (str "Unrecognized op: " op)))))

(defn execute-ops
  {:test #(is (=
                (lines ".#..#.#"
                       "#.#...."
                       ".#.....")
                (render-display (execute-ops (make-display 7 3) ["rect 3x2"
                                                                 "rotate column x=1 by 1"
                                                                 "rotate row y=0 by 4"
                                                                 "rotate column x=1 by 1"]))))}
  [display ops]
  (reduce (fn [display op] ((compile-op op) display))
          display
          ops))

(defn count-lit-display-pixels
  {:test #(is (= 4 (count-lit-display-pixels (op-rect display-4-by-3 2 2))))}
  [display]
  (->> (:pixels display)
       (filter identity)
       (count)))

(def input (->> (clojure.java.io/file "./resources/advent/day08_display.txt")
                (slurp)
                (strings/split-lines)
                ))

(def finished-display (execute-ops (make-display 50 6) input))

(defn solve-part-1
  {:test #(is (= 115 (solve-part-1)))}
  []
  (count-lit-display-pixels finished-display))

(defn solve-part-2
  {:test #(is (not= "EFEYKFRFIJ" (solve-part-2)))}          ; Easier just to read the damn thing!
  []
  (render-display finished-display))
