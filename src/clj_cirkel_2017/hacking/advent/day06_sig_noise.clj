; --- Day 6: Signals and Noise ---
;
; Something is jamming your communications with Santa. Fortunately, your signal is only partially jammed, and protocol
; in situations like this is to switch to a simple repetition code to get the message through.
;
; In this model, the same message is sent repeatedly. You've recorded the repeating message signal (your puzzle input),
; but the data seems quite corrupted - almost too badly to recover. Almost.
;
; All you need to do is figure out which character is most frequent for each position. For example, suppose you had
; recorded the following messages:
;
; eedadn
; drvtee
; eandsr
; raavrd
; atevrs
; tsrnev
; sdttsa
; rasrtv
; nssdts
; ntnada
; svetve
; tesnvt
; vntsnd
; vrdear
; dvrsen
; enarar
;
; The most common character in the first column is e; in the second, a; in the third, s, and so on. Combining these
; characters returns the error-corrected message, easter.
;
; Given the recording in your puzzle input, what is the error-corrected version of the message being sent?
;
; --- Part Two ---
;
; Of course, that would be the message - if you hadn't agreed to use a modified repetition code instead.
;
; In this modified code, the sender instead transmits what looks like random data, but for each character, the character
; they actually want to send is slightly less likely than the others. Even after signal-jamming noise, you can look at
; the letter distributions in each column and choose the least common letter to reconstruct the original message.
;
; In the above example, the least common character in the first column is a; in the second, d, and so on. Repeating this
; process for the remaining characters produces the original message, advent.
;
; Given the recording in your puzzle input and this new decoding methodology, what is the original message that Santa is
; trying to send?
;

(ns advent-06
  (:use clojure.test)
  (:require [clojure.string :as strings]))


(defn columns
  {:test #(is (= [[:a1 :a2 :a3]
                  [:b1 :b2 :b3]
                  [:c1 :c2 :c3]]
                 (columns [[:a1 :b1 :c1]
                           [:a2 :b2 :c2]
                           [:a3 :b3 :c3]]))
              )}
  [lines]
  (->> lines
       (mapcat (partial map vector (range)))
       (group-by #(% 0))
       (into (sorted-map))
       (vals)
       (map #(map (fn [[col value]] value) %))
       ))

(defn count-elements
  {:test #(is (= {:a 3 :b 2 :c 1} (count-elements [:b :a :b :a :c :a])))}
  [seq]
  (reduce #(update %1 %2 (fn [v] (inc (or v 0)))) {} seq)
  )

(def example-input ["eedadn" "drvtee" "eandsr" "raavrd" "atevrs" "tsrnev" "sdttsa" "rasrtv"
                    "nssdts" "ntnada" "svetve" "tesnvt" "vntsnd" "vrdear" "dvrsen" "enarar"])


(defn correct-errors
  {:test #(is (= "easter" (correct-errors example-input)))}
  [msgs]
  (->> (columns msgs)
       (map count-elements)
       (map clojure.set/map-invert)
       (map (partial into (sorted-map)))
       (map #(last (last %)))
       (apply str)
       ))

(defn correct-errors-with-modified-repetition
  {:test #(is (= "advent" (correct-errors-with-modified-repetition example-input)))}
  [msgs]
  (->> (columns msgs)
       (map count-elements)
       (map clojure.set/map-invert)
       (map (partial into (sorted-map)))
       (map #(last (first %)))
       (apply str)
       )
  )

(def input (->> (clojure.java.io/file "./resources/advent/day06_msg.txt")
                (slurp)
                (strings/split-lines)
                ))

(defn solve-part-1
  {:test #(is (= "cyxeoccr" (solve-part-1)))}
  []
  (correct-errors input))

(defn solve-part-2
  {:test #(is (= "batwpask" (solve-part-2)))}
  []
  (correct-errors-with-modified-repetition input))

(prn "Part 1: " (solve-part-1))
(prn "Part 2: " (solve-part-2))
