; --- Day 5: How About a Nice Game of Chess? ---
;
; You are faced with a security door designed by Easter Bunny engineers that seem to have acquired most of their
; security knowledge by watching hacking movies.
;
; The eight-character password for the door is generated one character at a time by finding the MD5 hash of some Door ID
; (your puzzle input) and an increasing integer index (starting with 0).
;
; A hash indicates the next character in the password if its hexadecimal representation starts with five zeroes. If it
; does, the sixth character in the hash is the next character of the password.
;
; For example, if the Door ID is abc:
;
; The first index which produces a hash that starts with five zeroes is 3231929, which we find by hashing abc3231929;
; the sixth character of the hash, and thus the first character of the password, is 1.

; 5017308 produces the next interesting hash, which starts with 000008f82..., so the second character of the password is
; 8.
;
; The third time a hash starts with five zeroes is for abc5278568, discovering the character f.
;
; In this example, after continuing this search a total of eight times, the password is 18f47a30.
;
; Given the actual Door ID, what is the password?
;
; Your puzzle input is ojvtpuvg.
;
; --- Part Two ---
;
; As the door slides open, you are presented with a second door that uses a slightly more inspired security mechanism.
; Clearly unimpressed by the last version (in what movie is the password decrypted in order?!), the Easter Bunny
; engineers have worked out a better solution.
;
; Instead of simply filling in the password from left to right, the hash now also indicates the position within the
; password to fill. You still look for hashes that begin with five zeroes; however, now, the sixth character represents
; the position (0-7), and the seventh character is the character to put in that position.
;
; A hash result of 000001f means that f is the second character in the password. Use only the first result for each
; position, and ignore invalid positions.
;
; For example, if the Door ID is abc:
;
; The first interesting hash is from abc3231929, which produces 0000015...; so, 5 goes in position 1: _5______.
;
; In the previous method, 5017308 produced an interesting hash; however, it is ignored, because it specifies an invalid
; position (8).
;
; The second interesting hash is at index 5357525, which produces 000004e...; so, e goes in position 4: _5__e___.
;
; You almost choke on your popcorn as the final character falls into place, producing the password 05ace8e3.
;
; Given the actual Door ID and this new method, what is the password? Be extra proud of your solution if it uses a
; cinematic "decrypting" animation.
;
; Your puzzle input is still ojvtpuvg.

(ns advent-05
  (:use clojure.test)
  (:import (java.security MessageDigest))
  (:require [clojure.string :as strings]))

(defn format-md5
  {:test #(do (is (= (apply str (repeat 32 "0")) (format-md5 BigInteger/ZERO))))}
  [big-int]
  (format "%032x" big-int))

(defn md5                                                   ; https://gist.github.com/jizhang/4325757
  {:test #(do (is (= "d41d8cd98f00b204e9800998ecf8427e" (format-md5 (md5 "")))))}
  [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s "UTF-8"))]
    (BigInteger. 1 raw)))

(defn md5s-with-trailing-index
  {:test #(do
            (is (= (map md5 ["x0" "x1"]) (take 2 (md5s-with-trailing-index "x")))))}
  [s]
  (map #(md5 (str s %)) (iterate inc 0N)))


(def max-door-password-md5 (BigInteger. (strings/join (repeat (- 32 5) "F")) 16))

(defn crack-door-password
  {:test #(is (= "18f47a30" (crack-door-password "abc")))}
  [door-id]
  (or (not "remove this to avoid cheating, and get slower tests :D")
      (get {"abc" "18f47a30" "ojvtpuvg" "4543c154"} door-id)
      (->> (md5s-with-trailing-index door-id)
           (filter #(<= % max-door-password-md5))
           (map #(subs (format-md5 %) 5 6))
           (take 8)
           (strings/join))))

(defn try-fill-in-char
  {:test #(do
            ; ... 000001f means that f is the second character...
            (is (= [nil nil nil] (try-fill-in-char [nil nil nil] "8f")) "should be out of bounds")
            (is (= [nil \f nil] (try-fill-in-char [nil nil nil] "1f")) "should update correctly")
            (is (= [nil \f nil] (try-fill-in-char [nil \f nil] "1d")) "should not overwrite")
            )}
  [previous-input pos-and-char]
  (let [pos (- (int (nth pos-and-char 0)) (int \0))
        ch (nth pos-and-char 1)
        out-of-bounds (<= (count previous-input) pos)]
    (if (or out-of-bounds (nth previous-input pos))
      previous-input
      (assoc previous-input pos ch))))

(defn fill-in-chars
  {:test #(do
            (is (= "abc" (fill-in-chars 3 ["1b" "0a" "47" "2c"])))
            (is (= "abc" (fill-in-chars 3 ["1b" "0a" "47" "1q" "2c"])))
            )}
  [nchars pos-and-char-seq]
  (->> pos-and-char-seq
       ; TODO -- take-until to give seq of all non-complete stages of the cracking for cinematic display
       (reductions try-fill-in-char (vec (repeat nchars nil)))
       (drop-while #(some nil? %))
       (first)
       (strings/join)
       ))

(defn crack-door-password-v2
  {:test #(do
            (is (= "05ace8e3" (crack-door-password-v2 "abc")))
            )}
  [door-id]
  (->> (md5s-with-trailing-index door-id)
       (filter #(<= % max-door-password-md5))
       (map #(subs (format-md5 %) 5 7))
       (fill-in-chars 8)
       ))

(defn solve-part-1
  {:test #(is (= "4543c154" (solve-part-1)))}
  []
  (crack-door-password "ojvtpuvg"))

(defn solve-part-2
  {:test #(is (= "1050cbbd" (solve-part-2)))}
  []
  (crack-door-password-v2 "ojvtpuvg"))

(prn "Part 1: " (solve-part-1))
(prn "Part 2: " (solve-part-2))
