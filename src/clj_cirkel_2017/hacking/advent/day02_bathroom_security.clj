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

;; NOTE -- keypad layout's first element should be nil. Otherwise bad things can happen.

(def mini-keypad {:layout      [nil 1 2 nil
                                nil 3 4 nil]
                  :line-length 4})

(def square-keypad {:initial-key 5
                    :line-length 5
                    :layout      [nil 1 2 3 nil
                                  nil 4 5 6 nil
                                  nil 7 8 9 nil]})

(def diamond-keypad {:initial-key 0x7
                     :line-length 5
                     :layout      [nil nil 0x1 nil nil
                                   nil 0x2 0x3 0x4 nil
                                   0x5 0x6 0x7 0x8 0x9
                                   nil 0xA 0xB 0xC nil
                                   nil nil 0xD nil nil]})

(defn keypad-try-advance
  {:test #(do
            ; Up
            (is (= 1 (keypad-try-advance mini-keypad 1 \U)))
            (is (= 1 (keypad-try-advance mini-keypad 3 \U)))
            ; Down
            (is (= 4 (keypad-try-advance mini-keypad 2 \D)))
            (is (= 4 (keypad-try-advance mini-keypad 4 \D)))
            ; Left
            (is (= 1 (keypad-try-advance mini-keypad 2 \L)))
            (is (= 1 (keypad-try-advance mini-keypad 1 \L)))
            ; Right
            (is (= 4 (keypad-try-advance mini-keypad 3 \R)))
            (is (= 4 (keypad-try-advance mini-keypad 4 \R)))
            )}
  [{:keys [layout line-length]} from-key ch]
  (let [from-key-index (.indexOf layout from-key)
        max-index (dec (count layout))]
    (or (layout
          (case ch
            \U (max 0 (- from-key-index line-length))
            \D (min max-index (+ from-key-index line-length))
            \L (max 0 (dec from-key-index))
            \R (min max-index (inc from-key-index))))
        from-key)))

(defn keypad-translate-line
  {:test #(do (let [kp {:layout      [nil 1 2 nil
                                      nil 3 4 nil]
                        :line-length 4}]
                (is (= 4 (keypad-translate-line mini-keypad 4 "")))
                (is (= 1 (keypad-translate-line mini-keypad 2 "LL")))
                (is (= 4 (keypad-translate-line mini-keypad 1 "DR")))
                (is (= 3 (keypad-translate-line mini-keypad 2 "DLL")))
                (is (= 1 (keypad-translate-line mini-keypad 4 "UL")))
                (is (= 5 (keypad-translate-line square-keypad 2 "UD")))
                ))}
  [keypad start-pos line]
  (reduce #(keypad-try-advance keypad %1 %2) start-pos line))

(defn keypad-resolve-code
  {:test #(do
            ; Basic tests
            (is (= [5 1 5 8] (keypad-resolve-code square-keypad ["" "LU" "DR" "D"])))
            (is (= [5 6] (keypad-resolve-code square-keypad ["" "R"])))
            ; Given example
            (is (= [1 9 8 5] (keypad-resolve-code square-keypad ["ULL" "RRDDD" "LURDL" "UUUUD"])))
            )}
  [{:keys [initial-key] :as keypad} lines]
  (reduce
    (fn [digits line] (conj (vec digits)
                            (keypad-translate-line keypad (or (peek digits) initial-key) line)))
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
  (keypad-resolve-code
    square-keypad
    input))


(defn solve-part-2
  {:test #(do (is (= [0xA 0x7 0xA 0xC 0x3] (solve-part-2))))}
  []
  (keypad-resolve-code
    diamond-keypad
    input))

(prn "Part 1: " (apply str (solve-part-1)))
(prn "Part 2: " (->> (solve-part-2)
                     (map #(format "%X" %))
                     (apply str)))
