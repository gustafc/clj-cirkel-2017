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
;
; --- Part Two ---
;
; You finally arrive at the bathroom (it's a several minute walk from the lobby so visitors can behold the many fancy
; conference rooms and water coolers on this floor) and go to punch in the code. Much to your bladder's dismay, the
; keypad is not at all like you imagined it. Instead, you are confronted with the result of hundreds of man-hours of
; bathroom-keypad-design meetings:
;
;     1
;   2 3 4
; 5 6 7 8 9
;   A B C
;     D
;
; You still start at "5" and stop when you're at an edge, but given the same instructions as above, the outcome is very
; different:
;
; You start at "5" and don't move at all (up and left are both edges), ending at 5.
; Continuing from "5", you move right twice and down three times (through "6", "7", "B", "D", "D"), ending at D.
; Then, from "D", you move five more times (through "D", "B", "C", "C", "B"), ending at B.
; Finally, after five more moves, you end at 3.
;
; So, given the actual keypad layout, the code would be 5DB3.
;
; Using the same instructions in your puzzle input, what is the correct bathroom code?

(ns advent-02
  (:use clojure.test))

(defn square-try-advance
  {:test #(do
            ; Up
            (is (= 2 (square-try-advance 5 \U)))
            (is (= 1 (square-try-advance 4 \U)))
            (is (= 6 (square-try-advance 9 \U)))
            ; Down
            (is (= 6 (square-try-advance 3 \D)))
            (is (= 8 (square-try-advance 5 \D)))
            (is (= 7 (square-try-advance 7 \D)))
            ; Left
            (is (= 1 (square-try-advance 2 \L)))
            (is (= 5 (square-try-advance 6 \L)))
            (is (= 7 (square-try-advance 7 \L)))
            ; Right
            (is (= 3 (square-try-advance 2 \R)))
            (is (= 5 (square-try-advance 4 \R)))
            (is (= 9 (square-try-advance 9 \R)))
            )}
  [pos ch]
  (cond (and (= \U ch) (< 3 pos)) (- pos 3)
        (and (= \D ch) (< pos 7)) (+ pos 3)
        (and (= \L ch) (not= 1 (mod pos 3))) (dec pos)
        (and (= \R ch) (not= 0 (mod pos 3))) (inc pos)
        :else pos))

(defn translate-line
  {:test #(do
            (is (= 7 (translate-line square-try-advance 7 "")))
            (is (= 4 (translate-line square-try-advance 5 "LL")))
            (is (= 6 (translate-line square-try-advance 2 "DR")))
            (is (= 1 (translate-line square-try-advance 1 "UL")))
            )}
  [advancer start-pos line]
  (reduce advancer start-pos line))

(defn to-code
  {:test #(do
            ; Basic tests
            (is (= [5 1 5 8] (to-code square-try-advance 5 ["" "LU" "DR" "D"])))
            (is (= [4 5] (to-code square-try-advance 4 ["" "R"])))
            ; Given example
            (is (= [1 9 8 5] (to-code square-try-advance 5 ["ULL" "RRDDD" "LURDL" "UUUUD"])))
            )}
  [advancer initial-key lines]
  (reduce
    (fn [digits line] (conj (vec digits)
                            (translate-line advancer (or (peek digits) initial-key) line)))
    nil
    lines))

;;; PART 2

(def diamond-keypad [nil nil 0x1 nil nil
                     nil 0x2 0x3 0x4 nil
                     0x5 0x6 0x7 0x8 0x9
                     nil 0xA 0xB 0xC nil
                     nil nil 0xD nil nil])

(defn diamond-try-advance
  {:test #(do
            ; Up
            (is (= 0x1 (diamond-try-advance 0x1 \U)))
            (is (= 0x2 (diamond-try-advance 0x2 \U)))
            (is (= 0x3 (diamond-try-advance 0x7 \U)))
            ; Down
            (is (= 0xB (diamond-try-advance 0x7 \D)))
            (is (= 0xC (diamond-try-advance 0xC \D)))
            (is (= 0xD (diamond-try-advance 0xD \D)))
            ; Left
            (is (= 0x2 (diamond-try-advance 0x3 \L)))
            (is (= 0x2 (diamond-try-advance 0x2 \L)))
            (is (= 0x5 (diamond-try-advance 0x5 \L)))
            ; Right
            (is (= 0x1 (diamond-try-advance 0x1 \R)))
            (is (= 0x7 (diamond-try-advance 0x6 \R)))
            (is (= 0x9 (diamond-try-advance 0x9 \R)))
            (is (= 0xC (diamond-try-advance 0xB \R)))
            )}
  [initial-key ch]
  (let [from-key-index (.indexOf diamond-keypad initial-key)]
    (or (diamond-keypad
          (case ch
            \U (max 0 (- from-key-index 5))
            \D (min 24 (+ from-key-index 5))
            \L (- from-key-index 1)
            \R (+ from-key-index 1)))
        initial-key)))

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
  (to-code square-try-advance 5 input))


(defn solve-part-2
  {:test #(do (is (= [0xA 0x7 0xA 0xC 0x3] (solve-part-2))))}
  []
  (to-code diamond-try-advance 0x7 input))

;; TODO -- could solve part 1 with part 2 by making keypad list arg instead of hardwired

(prn "Part 1: " (solve-part-1))
(prn "Part 2: " (->> (solve-part-2)
                     (map #(format "%X" %))
                     (apply str)))
