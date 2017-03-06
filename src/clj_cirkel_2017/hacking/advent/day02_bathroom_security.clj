; --- Day 2: Bathroom Security ---
;
; You arrive at Easter Bunny Headquarters under cover of darkness. However, you left in such a rush that you forgot to
; use the bathroom! Fancy office buildings like this one usually have keypad locks on their bathrooms, so you search the
; front desk for the code.
;
; "In order to improve security," the document you find says, "bathroom codes will no longer be written down. Instead,
; please memorize and follow the procedure below to access the bathrooms."
;
; The document goes on to explain that each button to be pressed can be found by starting on the previous button and
; moving to adjacent buttons on the keypad: U moves up, D moves down, L moves left, and R moves right. Each line of
; instructions corresponds to one button, starting at the previous button (or, for the first line, the "5" button);
; press whatever button you're on at the end of each line. If a move doesn't lead to a button, ignore it.
;
; You can't hold it much longer, so you decide to figure out the code as you walk to the bathroom. You picture a keypad
; like this:
;
; 1 2 3
; 4 5 6
; 7 8 9
;
; Suppose your instructions are:
;
; ULL
; RRDDD
; LURDL
; UUUUD
;
; You start at "5" and move up (to "2"), left (to "1"), and left (you can't, and stay on "1"), so the first button is 1.
;
; Starting from the previous button ("1"), you move right twice (to "3") and then down three times (stopping at "9"
; after two moves and ignoring the third), ending up with 9.
;
; Continuing from "9", you move left, up, right, down, and left, ending with 8.
;
; Finally, you move up four times (stopping at "2"), then down once, ending with 5.
;
; So, in this example, the bathroom code is 1985.
;
; Your puzzle input is the instructions from the document you found at the front desk. What is the bathroom code?

(ns advent-02
  (:use clojure.test))

(defn try-advance
  {:test #(do
            ; Up
            (is (= 2 (try-advance 5 \U)))
            (is (= 1 (try-advance 4 \U)))
            (is (= 6 (try-advance 9 \U)))
            ; Down
            (is (= 6 (try-advance 3 \D)))
            (is (= 8 (try-advance 5 \D)))
            (is (= 7 (try-advance 7 \D)))
            ; Left
            (is (= 1 (try-advance 2 \L)))
            (is (= 5 (try-advance 6 \L)))
            (is (= 7 (try-advance 7 \L)))
            ; Right
            (is (= 3 (try-advance 2 \R)))
            (is (= 5 (try-advance 4 \R)))
            (is (= 9 (try-advance 9 \R)))
            )}
  [pos ch]
  (cond (and (= \U ch) (< 3 pos)) (- pos 3)
        (and (= \D ch) (< pos 7)) (+ pos 3)
        (and (= \L ch) (not= 1 (mod pos 3))) (dec pos)
        (and (= \R ch) (not= 0 (mod pos 3))) (inc pos)
        :else pos))

(defn translate-line
  {:test #(do
            (is (= 7 (translate-line 7 "")))
            (is (= 4 (translate-line 5 "LL")))
            (is (= 6 (translate-line 2 "DR")))
            (is (= 1 (translate-line 1 "UL")))
            )}
  [start-pos line]
  (reduce try-advance start-pos line))

(defn to-code
  {:test #(do
            ; Basic tests
            (is (= [5 1 5 8] (to-code ["" "LU" "DR" "D"])))
            ; Given example
            (is (= [1 9 8 5] (to-code ["ULL" "RRDDD" "LURDL" "UUUUD"])))
            )}
  [lines]
  (reduce
    (fn [digits line] (conj (vec digits)
                            (translate-line (if digits (peek digits) 5) line)))
    nil
    lines))

(def input-str
  "LDUDDRUDRRURRRRDRUUDULDLULRRLLLUDDULRDLDDLRULLDDLRUURRLDUDDDDLUULUUDDDDLLLLLULLRURDRLRLRLLURDLLDDUULUUUUDLULLRLUUDDLRDRRURRLURRLLLRRDLRUDURRLRRRLULRDLUDRDRLUDDUUULDDDDDURLDULLRDDRRUDDDDRRURRULUDDLLRRDRURDLLLLLUUUDLULURLULLDRLRRDDLUDURUDRLRURURLRRDDLDUULURULRRLLLDRURDULRDUURRRLDLDUDDRLURRDRDRRLDLRRRLRURDRLDRUDLURRUURDLDRULULURRLDLLLUURRULUDDDRLDDUDDDRRLRDUDRUUDDULRDDULDDURULUDLUDRUDDDLRRRRRDLULDRLRRRRUULDUUDRRLURDLLUUDUDDDLUUURDRUULRURULRLLDDLLUDLURRLDRLDDDLULULLURLULRDLDRDDDLRDUDUURUUULDLLRDRUDRDURUUDDLRRRRLLLUULURRURLLDDLDDD
   DRURURLLUURRRULURRLRULLLURDULRLRRRLRUURRLRRURRRRUURRRLUDRDUDLUUDULURRLDLULURRLDURLUUDLDUDRUURDDRDLLLDDRDDLUUDRDUDDRRDLDUDRLDDDRLLDDLUDRULRLLURLDLURRDRUDUDLDLULLLRDLLRRDULLDRURRDLDRURDURDULUUURURDLUDRRURLRRLDULRRDURRDRDDULLDRRRLDRRURRRRUURDRLLLRRULLUDUDRRDDRURLULLUUDDRLDRRDUDLULUUDRDDDDLRLRULRLRLLDLLRRDDLDRDURRULLRLRRLULRULDDDRDRULDRUUDURDLLRDRURDRLRDDUDLLRUDLURURRULLUDRDRDURLLLDDDRDRURRDDRLRRRDLLDDLDURUULURULRLULRLLURLUDULDRRDDLRDLRRLRLLULLDDDRDRU
   URUUDUDRDDRDRRRDLLUDRUDRUUUURDRRDUDUULDUDLLUDRRUDLLRDLLULULDRRDDULDRLDLDDULLDDRDDDLRLLDLLRDUUDUURLUDURDRRRRLRRLDRRUULLDLDLRDURULRURULRRDRRDDUUURDURLLDDUUDLRLDURULURRRDRRUUUDRDDLRLRRLLULUDDRRLRRRRLRDRUDDUULULRRURUURURRLRUDLRRUUURUULLULULRRDDULDRRLLLDLUDRRRLLRDLLRLDUDDRRULULUDLURLDRDRRLULLRRDRDLUURLDDURRLDRLURULDLDRDLURRDRLUUDRUULLDRDURLLDLRUDDULLLLDLDDDLURDDUDUDDRLRDDUDDURURLULLRLUDRDDUDDLDRUURLDLUUURDUULRULLDDDURULDDLLD
   LRRLLRURUURRDLURRULDDDLURDUURLLDLRRRRULUUDDLULLDLLRDLUDUULLUDRLLDRULDDURURDUUULRUDRLLRDDDURLRDRRURDDRUDDRRULULLLDLRLULLDLLDRLLLUDLRURLDULRDDRDLDRRDLUUDDLURDLURLUDLRDLDUURLRRUULDLURULUURULLURLDDURRURDRLUULLRRLLLDDDURLURUURLLLLDLLLUDLDLRDULUULRRLUUUUDLURRURRULULULRURDDRRRRDRUDRURDUDDDDUDLURURRDRRDRUDRLDLDDDLURRRURRUDLDURDRLDLDLDDUDURLUDUUDRULLRLLUUDDUURRRUDURDRRUURLUDRRUDLUDDRUUDLULDLLDLRUUDUULLDULRRLDRUDRRDRLUUDDRUDDLLULRLULLDLDUULLDRUUDDUDLLLLDLDDLDLURLDLRUUDDUULLUDUUDRUDLRDDRDLDRUUDUDLLDUURRRLLLLRLLRLLRLUUDULLRLURDLLRUUDRULLULRDRDRRULRDLUDDURRRRURLLRDRLLDRUUULDUDDLRDRD
   DDLRRULRDURDURULLLLRLDDRDDRLLURLRDLULUDURRLUDLDUDRDULDDULURDRURLLDRRLDURRLUULLRUUDUUDLDDLRUUDRRDDRLURDRUDRRRDRUUDDRLLUURLURUDLLRRDRDLUUDLUDURUUDDUULUURLUDLLDDULLUURDDRDLLDRLLDDDRRDLDULLURRLDLRRRLRRURUUDRLURURUULDURUDRRLUDUDLRUDDUDDRLLLULUDULRURDRLUURRRRDLLRDRURRRUURULRUDULDULULUULULLURDUDUDRLDULDRDDULRULDLURLRLDDDDDDULDRURRRRDLLRUDDRDDLUUDUDDRLLRLDLUDRUDULDDDRLLLLURURLDLUUULRRRUDLLULUUULLDLRLDLLRLRDLDULLRLUDDDRDRDDLULUUR")

(def input (re-seq #"[LRUD]+" input-str))

(defn solve-part-1
  {:test #(do (is (= [7 6 7 9 2] (solve-part-1))))}
  []
  (to-code input))

(prn "Part 1: " (solve-part-1))