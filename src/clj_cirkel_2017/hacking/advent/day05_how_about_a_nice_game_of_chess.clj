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
  (->> (md5s-with-trailing-index door-id)
       (filter #(<= % max-door-password-md5))
       (map #(subs (format-md5 %) 5 6))
       (take 8)
       (strings/join)))

(defn solve-part-1
  {:test #(is (= "4543c154" (solve-part-1)))}
  []
  (crack-door-password "ojvtpuvg"))

(prn "Part 1: " (solve-part-1))
