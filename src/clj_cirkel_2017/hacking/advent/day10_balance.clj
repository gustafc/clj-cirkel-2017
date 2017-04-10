; --- Day 10: Balance Bots ---
;
; You come upon a factory in which many robots are zooming around handing small microchips to each other.
;
; Upon closer examination, you notice that each bot only proceeds when it has two microchips, and once it does, it gives
; each one to a different bot or puts it in a marked "output" bin. Sometimes, bots take microchips from "input" bins,
; too.
;
; Inspecting one of the microchips, it seems like they each contain a single number; the bots must use some logic to
; decide what to do with each chip. You access the local control computer and download the bots' instructions (your
; puzzle input).
;
; Some of the instructions specify that a specific-valued microchip should be given to a specific bot; the rest of the
; instructions indicate what a given bot should do with its lower-value or higher-value chip.
;
; For example, consider the following instructions:
;
; value 5 goes to bot 2
; bot 2 gives low to bot 1 and high to bot 0
; value 3 goes to bot 1
; bot 1 gives low to output 1 and high to bot 0
; bot 0 gives low to output 2 and high to output 0
; value 2 goes to bot 2
;
; - Initially, bot 1 starts with a value-3 chip, and bot 2 starts with a value-2 chip and a value-5 chip.
;
; - Because bot 2 has two microchips, it gives its lower one (2) to bot 1 and its higher one (5) to bot 0.
;
; - Then, bot 1 has two microchips; it puts the value-2 chip in output 1 and gives the value-3 chip to bot 0.
;
; - Finally, bot 0 has two microchips; it puts the 3 in output 2 and the 5 in output 0.
;
; In the end, output bin 0 contains a value-5 microchip, output bin 1 contains a value-2 microchip, and output bin 2
; contains a value-3 microchip. In this configuration, bot number 2 is responsible for comparing value-5 microchips
; with value-2 microchips.
;
; Based on your instructions, what is the number of the bot that is responsible for comparing value-61 microchips with
; value-17 microchips?
;
; --- Part Two ---
;
; What do you get if you multiply together the values of one chip in each of outputs 0, 1, and 2?



(ns clj_cirkel_2017.hacking.advent.day10-balance
  (:use clojure.test)
  (:require [clojure.set :as sets])
  (:require [clojure.string :as strings]))

(def example-input ["value 5 goes to bot 2"
                    "bot 2 gives low to bot 1 and high to bot 0"
                    "value 3 goes to bot 1"
                    "bot 1 gives low to output 1 and high to bot 0"
                    "bot 0 gives low to output 2 and high to output 0"
                    "value 2 goes to bot 2"])

(def parsed-example-input {:bot    {0 {:values #{} :low-to [:output 2] :high-to [:output 0]}
                                    1 {:values #{3} :low-to [:output 1] :high-to [:bot 0]}
                                    2 {:values #{5 2} :low-to [:bot 1] :high-to [:bot 0]}}
                           :output {}})

(defn parse-bot-instructions
  {:test #(is (= parsed-example-input
                 (parse-bot-instructions example-input)))}
  [input]
  (reduce
    (fn [setup instruction]
      (if-let [[_ value bot-id] (re-find #"value (\d+) goes to bot (\d+)" instruction)]
        (update-in setup [:bot (read-string bot-id) :values] #(conj (or % #{}) (read-string value)))
        (if-let [[_ bot-id lo-type lo-id hi-type hi-id]
                 (re-find #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)" instruction)]
          (update-in setup [:bot (read-string bot-id)]
                     (fn [bot]
                       (-> (or bot {:values #{}})
                           (assoc :low-to [(if (= "bot" lo-type) :bot :output) (read-string lo-id)])
                           (assoc :high-to [(if (= "bot" hi-type) :bot :output) (read-string hi-id)])))
                     )
          (assert false (str "Unrecognized instruction: " instruction)))
        ))
    {:bot {} :output {}}
    input)
  )

(defn update-one-bot
  {:test #(let [updated-once (-> parsed-example-input
                                 (assoc-in [:bot 0 :values] #{5})
                                 (assoc-in [:bot 1 :values] #{2 3})
                                 (assoc-in [:bot 2 :values] #{})
                                 )]
            (is (= updated-once (update-one-bot parsed-example-input)))
            (is (= (-> updated-once
                       (assoc-in [:bot 0 :values] #{3 5})
                       (assoc-in [:bot 1 :values] #{})
                       (assoc-in [:output 1 :values] #{2})
                       )
                   (update-one-bot updated-once)
                   )))}
  [board]
  (let [[id bot :as hit] (first
                           (filter
                             #(<= 2 (count (get-in % [1 :values] [])))
                             (board :bot)))]
    (assert hit (str "No bot to update on board " board))
    (let [hi (apply max (bot :values))
          lo (apply min (bot :values))]
      (-> board
          (update-in [:bot id :values] #(sets/difference % #{lo hi}))
          (update-in (concat (bot :low-to) [:values]) #(conj (or % #{}) lo))
          (update-in (concat (bot :high-to) [:values]) #(conj (or % #{}) hi))
          ))
    )
  )

(defn run-bot-board [board] (iterate update-one-bot board))

(defn bot-responsible-for-comparing
  {:test #(is (= 2
                 (bot-responsible-for-comparing parsed-example-input 2 5)))}
  [board n1 n2]
  (->> (run-bot-board board)
       (mapcat (fn [board] (->> (board :bot)
                                (filter (fn [[_ bot]] (sets/superset? (bot :values) [n1 n2])))
                                (map first)
                                )))
       (first))
  )

(defn values-in-outputs
  {:test #(do
            (is (= #{2 3 5}
                   (values-in-outputs parsed-example-input #{0 1 2})))
            )}
  [board output-ids]
  (->> (run-bot-board board)
       (drop-while (fn [board] (not (sets/superset? (set (keys (board :output))) output-ids))))
       (first)
       (:output)
       (mapcat (fn [[id {:keys [values]}]]
                 (if-not (output-ids id) []
                                         values)))
       (set))
  )

(def input (->> (clojure.java.io/file "./resources/advent/day10_bots.txt")
                (slurp)
                (strings/split-lines)
                (parse-bot-instructions)))

(defn solve-part-1
  {:test #(is (= 147 (solve-part-1)))}
  []
  (bot-responsible-for-comparing input 61 17))

(defn solve-part-2
  {:test #(is (= 55637 (solve-part-2)))}
  []
  (apply * (values-in-outputs input #{0 1 2})))

(println "Part 1:" (solve-part-1))
(println "Part 2:" (solve-part-2))
