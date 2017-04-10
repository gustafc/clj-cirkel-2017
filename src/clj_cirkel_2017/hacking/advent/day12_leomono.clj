; --- Day 12: Leonardo's Monorail ---
;
; You finally reach the top floor of this building: a garden with a slanted glass ceiling. Looks like there are no more
; stars to be had.
;
; While sitting on a nearby bench amidst some tiger lilies, you manage to decrypt some of the files you extracted from
; the servers downstairs.
;
; According to these documents, Easter Bunny HQ isn't just this building - it's a collection of buildings in the nearby
; area. They're all connected by a local monorail, and there's another building not far from here! Unfortunately, being
; night, the monorail is currently not operating.
;
; You remotely connect to the monorail control systems and discover that the boot sequence expects a password. The
; password-checking logic (your puzzle input) is easy to extract, but the code it uses is strange: it's assembunny code
; designed for the new computer you just assembled. You'll have to execute the code and get the password.
;
; The assembunny code you've extracted operates on four registers (a, b, c, and d) that start at 0 and can hold any
; integer. However, it seems to make use of only a few instructions:
;
;     cpy x y copies x (either an integer or the value of a register) into register y.
;
;     inc x increases the value of register x by one.
;
;     dec x decreases the value of register x by one.
;
;     jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not
;     zero.
;
; The jnz instruction moves relative to itself: an offset of -1 would continue at the previous instruction, while an
; offset of 2 would skip over the next instruction.
;
; For example:
;
; cpy 41 a
; inc a
; inc a
; dec a
; jnz a 2
; dec a
;
; The above code would set register a to 41, increase its value by 2, decrease its value by 1, and then skip the last
; dec a (because a is not zero, so the jnz a 2 skips it), leaving register a at 42. When you move past the last
; instruction, the program halts.
;
; After executing the assembunny code in your puzzle input, what value is left in register a?
;


(ns clj_cirkel_2017.hacking.advent.day12-leomono
  (:use clojure.test)
  (:require [clojure.string :as strings])
  (:require [clojure.set :as sets]))

(def zero-state {:program-counter 0 :a 0 :b 0 :c 0 :d 0})

(defn step-program-counter
  {:test #(do
            (is (= 1 (:program-counter (step-program-counter zero-state))))
            (is (= 13 (:program-counter (step-program-counter 13 zero-state))))
            )}
  ([state] (step-program-counter 1 state))
  ([jump state] (update state :program-counter (partial + jump)))
  )

(defn op-inc
  {:test #(do
            (is (= {:program-counter 1 :a 1 :b 0 :c 0 :d 0}
                   (op-inc :a zero-state)))
            (is (= {:program-counter 2 :a 0 :b 0 :c 0 :d 2}
                   (op-inc :d (op-inc :d zero-state))))
            )}
  [reg state]
  (->> (update state reg inc)
       (step-program-counter))
  )

(defn op-dec
  {:test #(do
            (is (= {:program-counter 1 :a 0 :b -1 :c 0 :d 0}
                   (op-dec :b zero-state)))
            (is (= {:program-counter 2 :a 0 :b 0 :c -2 :d 0}
                   (op-dec :c (op-dec :c zero-state))))
            )}
  [reg state]
  (->> (update state reg dec)
       (step-program-counter)))

(defn read-register-or-constant
  [operand state]
  (if (keyword? operand)
    (operand state)
    operand))

(defn op-cpy
  {:test #(do
            (is (= {:program-counter 1 :a 0 :b 42 :c 0 :d 0}
                   (op-cpy 42 :b zero-state)))
            (is (= {:program-counter 2 :a 0 :b 13 :c 13 :d 0}
                   (op-cpy :b :c (op-cpy 13 :b zero-state))))
            )}
  [src dst state]
  (->> (read-register-or-constant src state)
       (assoc state dst)
       (step-program-counter)))

(defn op-jnz
  {:test #(do
            ; op1 is register
            (is (= {:program-counter 1 :a 0 :b 0 :c 0 :d 0}
                   (op-jnz :b 42 zero-state)))
            (is (= {:program-counter 43 :a 0 :b 0 :c 1 :d 0}
                   (op-jnz :c 42 (op-inc :c zero-state))))
            ; op1 is number
            (is (= {:program-counter 1 :a 0 :b 0 :c 0 :d 0}
                   (op-jnz 0 42 zero-state)))
            (is (= {:program-counter 42 :a 0 :b 0 :c 0 :d 0}
                   (op-jnz 1 42 zero-state)))
            )}
  [operand jump state]
  (if (= 0 (read-register-or-constant operand state))
    (step-program-counter state)
    (step-program-counter jump state)
    ))

(defn take-until
  {:test #(do
            (is (= [] (take-until neg? [])))
            (is (= [1 2 3] (take-until neg? [1 2 3])))
            (is (= [1 2 3 -7] (take-until neg? [1 2 3 -7])))
            (is (= [1 2 3 -7] (take-until neg? [1 2 3 -7 0])))
            )}
  [pred coll]
  (if-not (seq coll)
    coll
    (reductions (fn [_ item] (if (pred item) (reduced item) item)) coll))
  )

(defn execute-assembunny
  {:test #(do
            (is (= [zero-state
                    {:program-counter 1 :a 41 :b 0 :c 0 :d 0} ; cpy
                    {:program-counter 2 :a 42 :b 0 :c 0 :d 0} ; inc
                    {:program-counter 3 :a 43 :b 0 :c 0 :d 0} ; inc
                    {:program-counter 4 :a 42 :b 0 :c 0 :d 0} ; dec
                    {:program-counter 6 :a 42 :b 0 :c 0 :d 0} ; jnz
                    ]
                   (execute-assembunny
                     [(partial op-cpy 41 :a)
                      (partial op-inc :a)
                      (partial op-inc :a)
                      (partial op-dec :a)
                      (partial op-jnz :a 2)
                      (partial op-dec :a)]
                     zero-state)))
            )}
  [ops initial-state]
  (->> initial-state
       (iterate (fn [{:keys [program-counter] :as state}]
                  ((ops program-counter) state)))
       (take-until #(<= (count ops) (:program-counter %))))
  )

(def example-input "cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a")

(defn compile-assembunny
  {:test #(do
            (is (= {:program-counter 6 :a 42 :b 0 :c 0 :d 0}
                   (last (execute-assembunny (compile-assembunny example-input) zero-state))))
            )}
  [code]
  (->> (strings/split-lines code)
       (map strings/trim)
       (remove empty?)
       (filter #(not= \# (nth % 0)))
       (map #(let [[op-name o1 o2] (strings/split % #"\s+")
                   parse-register (fn [s] (let [reg ({"a" :a "b" :b "c" :c "d" :d} s)]
                                            (assert reg (str "Invalid register: " s))
                                            reg))
                   parse-register-or-number (fn [s] (if (re-matches #"-?[0-9]+" o1)
                                                      (read-string o1)
                                                      (parse-register o1)))]
               (case op-name
                 "inc" (partial op-inc (parse-register o1))
                 "dec" (partial op-dec (parse-register o1))
                 "cpy" (partial op-cpy
                                (parse-register-or-number o1)
                                (parse-register o2))
                 "jnz" (partial op-jnz (parse-register-or-number o1) (read-string o2))
                 )))
       (vec)))

(def puzzle-input "cpy 1 a
cpy 1 b
cpy 26 d
jnz c 2
jnz 1 5
cpy 7 c
inc d
dec c
jnz c -2
cpy a c
inc a
dec b
jnz b -2
cpy c b
dec d
jnz d -6
cpy 19 c
cpy 14 d
inc a
dec d
jnz d -2
dec c
jnz c -5
")

(def compiled-input (compile-assembunny puzzle-input))


(defn solve-part-1
  {:test #(is (= {:a               318077
                  :b               196418
                  :c               0
                  :d               0
                  :program-counter 23}
                 (solve-part-1)))}
  []
  (->> (execute-assembunny compiled-input zero-state)
       (last)))


(defn solve-part-2
  {:test #(is (= {:a               9227731
                  :b               5702887
                  :c               0
                  :d               0
                  :program-counter 23}
                 (time (solve-part-2))))}
  []
  (->> (execute-assembunny compiled-input (assoc zero-state :c 1))
       (last)))
