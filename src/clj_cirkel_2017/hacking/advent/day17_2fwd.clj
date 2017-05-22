; --- Day 17: Two Steps Forward ---
;
; You're trying to access a secure vault protected by a 4x4 grid of small rooms connected by doors. You start in the
; top-left room (marked S), and you can access the vault (marked V) once you reach the bottom-right room:
;
; #########
; #S| | | #
; #-#-#-#-#
; # | | | #
; #-#-#-#-#
; # | | | #
; #-#-#-#-#
; # | | |
; ####### V
;
; Fixed walls are marked with #, and doors are marked with - or |.
;
; The doors in your current room are either open or closed (and locked) based on the hexadecimal MD5 hash of a passcode
; (your puzzle input) followed by a sequence of uppercase characters representing the path you have taken so far (U for
; up, D for down, L for left, and R for right).
;
; Only the first four characters of the hash are used; they represent, respectively, the doors up, down, left, and right
; from your current position. Any b, c, d, e, or f means that the corresponding door is open; any other character (any
; number or a) means that the corresponding door is closed and locked.
;
; To access the vault, all you need to do is reach the bottom-right room; reaching this room opens the vault and all
; doors in the maze.
;
; For example, suppose the passcode is hijkl. Initially, you have taken no steps, and so your path is empty: you simply
; find the MD5 hash of hijkl alone. The first four characters of this hash are ced9, which indicate that up is open (c),
; down is open (e), left is open (d), and right is closed and locked (9). Because you start in the top-left corner,
; there are no "up" or "left" doors to be open, so your only choice is down.
;
; Next, having gone only one step (down, or D), you find the hash of hijklD. This produces f2bc, which indicates that
; you can go back up, left (but that's a wall), or right. Going right means hashing hijklDR to get 5745 - all doors
; closed and locked. However, going up instead is worthwhile: even though it returns you to the room you started in,
; your path would then be DU, opening a different set of doors.
;
; After going DU (and then hashing hijklDU to get 528e), only the right door is open; after going DUR, all doors lock.
; (Fortunately, your actual passcode is not hijkl).
;
; Passcodes actually used by Easter Bunny Vault Security do allow access to the vault if you know the right path. For
; example:
;
;     If your passcode were ihgpwlah, the shortest path would be DDRRRD.
;     With kglvqrro, the shortest path would be DDUDRLRRUDRD.
;     With ulqzkmiv, the shortest would be DRURDRUDDLLDLUURRDULRLDUUDDDRR.
;
; Given your vault's passcode, what is the shortest path (the actual path, not just the length) to reach the vault?
;
; Your puzzle input is pxxbnzuo.
(ns clj-cirkel-2017.hacking.advent.day17-2fwd
  (:use [clojure.test :refer [is]])
  (:use [clj_cirkel_2017.hacking.advent.day05-how-about-a-nice-game-of-chess :refer [md5 format-md5]])
  (:use [clj_cirkel_2017.hacking.advent.day11-rtg :refer [traverse-breadth-first]])
  (:use [clj_cirkel_2017.ch08.debugging :refer [printvars]]))

(defn path->coord
  {:test #(do
            (is (= [0 0] (path->coord "")))
            (is (= [1 0] (path->coord "R")))
            (is (= [0 1] (path->coord "D")))
            (is (= [0 0] (path->coord "DRUL")))
            (is (= [3 3] (path->coord "DDRRRD")))
            (is (= [3 3] (path->coord "DDUDRLRRUDRD")))
            (is (= [3 3] (path->coord "DRURDRUDDLLDLUURRDULRLDUUDDDRR")))
            )}
  [path]
  [(->> path (map {\U 0 \D 0 \L -1 \R 1}) (reduce + 0))
   (->> path (map {\U -1 \D 1 \L 0 \R 0}) (reduce + 0))])

(def example-board {:magic "hijkl" :width 4 :height 4})

(defn unlocked-doors
  {:test #(do
            (is (= [\U \D \L] (unlocked-doors example-board "")))
            (is (= [\U \L \R] (unlocked-doors example-board "D")))
            (is (= [] (unlocked-doors example-board "DR")))
            (is (= [\R] (unlocked-doors example-board "DU")))
            (is (= [] (unlocked-doors example-board "DUR")))
            )}
  [board path]
  (let [hash (format-md5 (md5 (str (:magic board) path)))
        [up down left right] hash
        conj-if-unlocked (fn [ch to-conj coll] (if (<= (int \b) (int ch) (int \f))
                                                 (conj coll to-conj)
                                                 coll))]
    (->> []
         (conj-if-unlocked up \U)
         (conj-if-unlocked down \D)
         (conj-if-unlocked left \L)
         (conj-if-unlocked right \R)
         )))

(defn on-board?
  {:test #(is (= '("" "D" "DD" "DDD" "LRDU" "UD" "R" "RR" "RRR")
                 (filter (partial on-board? example-board)
                         [""
                          "D" "DD" "DDD" "DDDD"
                          "U" "UU"
                          "LRDU"
                          "UD"
                          "R" "RR" "RRR" "RRRR"
                          ])
                 )
              )}
  [{width :width height :height} path]
  (let [[x y] (path->coord path)]
    (and (< -1 x width)
         (< -1 y height))
    ))

(defn paths-after-path
  {:test #(do
            (is (= ["D"] (paths-after-path example-board "")))
            (is (= ["DU" "DR"] (paths-after-path example-board "D")))
            (is (= [] (paths-after-path example-board "DR")))
            (is (= ["DUR"] (paths-after-path example-board "DU")))
            (is (= [] (paths-after-path example-board "DUR")))
            )}
  [board path]
  (->> (unlocked-doors board path)
       (map (partial str path))
       (filter (partial on-board? board))
       ))

(defn walk-board
  {:test #(is (= '(
                    ""
                    "D"
                    "DU" "DR"
                    "DUR") (walk-board example-board)))}
  [board]
  (traverse-breadth-first "" #(paths-after-path board (peek %))))

(defn is-at-end?
  {:test #(do
            (is (= false (is-at-end? example-board "")))
            (is (= true (is-at-end? example-board "DDDRRR")))
            (is (= true (is-at-end? example-board "DDRRRD")))
            (is (= true (is-at-end? example-board "DDUDRLRRUDRD")))
            (is (= true (is-at-end? example-board "DRURDRUDDLLDLUURRDULRLDUUDDDRR")))
            )}
  [{width :width height :height} path]
  (= [(dec width) (dec height)] (path->coord path))
  )

(defn shortest-path-to-end
  {:test #(do
            ;     If your passcode were ihgpwlah, the shortest path would be DDRRRD.
            (is (= "DDRRRD" (shortest-path-to-end (assoc example-board :magic "ihgpwlah"))))
            ;     With kglvqrro, the shortest path would be DDUDRLRRUDRD.
            (is (= "DDUDRLRRUDRD" (shortest-path-to-end (assoc example-board :magic "kglvqrro"))))
            ;     With ulqzkmiv, the shortest would be DRURDRUDDLLDLUURRDULRLDUUDDDRR.
            (is (= "DRURDRUDDLLDLUURRDULRLDUUDDDRR" (shortest-path-to-end (assoc example-board :magic "ulqzkmiv"))))
            )}
  [board]
  (->> (walk-board board)
       (map last)
       (filter (partial is-at-end? board))
       (first)
       ))

(def puzzle-board {:magic "pxxbnzuo" :width 4 :height 4})

(defn solve-part-1
  {:test #(is (= "RDULRDDRRD" (solve-part-1)))}
  []
  (shortest-path-to-end puzzle-board))