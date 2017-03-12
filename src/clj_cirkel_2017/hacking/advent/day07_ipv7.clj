; --- Day 7: Internet Protocol Version 7 ---
;
; While snooping around the local network of EBHQ, you compile a list of IP addresses (they're IPv7, of course; IPv6 is
; much too limited). You'd like to figure out which IPs support TLS (transport-layer snooping).
;
; An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA. An ABBA is any four-character sequence
; which consists of a pair of two different characters followed by the reverse of that pair, such as xyyx or abba.
; However, the IP also must not have an ABBA within any hypernet sequences, which are contained by square brackets.
;
; For example:
;
;     abba[mnop]qrst supports TLS (abba outside square brackets).
;     abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even though xyyx is outside square brackets).
;     aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior characters must be different).
;     ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even though it's within a larger string).
;
; How many IPs in your puzzle input support TLS?


(ns advent-07
  (:use clojure.test)
  (:require [clojure.string :as strings]))

(defn is-abba?
  {:test #(do
            (is (not (is-abba? "abcdef")))
            (is (not (is-abba? "abxa")))
            (is (not (is-abba? "abxa")))
            (is (not (is-abba? "lol")))
            (is (not (is-abba? "looool")))
            (is (is-abba? "abba"))
            (is (is-abba? "slabbar"))
            (is (is-abba? "pyxxylol"))
            (is (is-abba? "trollollol"))
            )}
  [s]
  (->> (re-seq #"(.)(.)\2\1" s)
       (some (fn [[_ one two]] (not= one two)))
       ))

(defn supports-tls?
  {:test #(do
            (is (supports-tls? "abba[mnop]qrst"))
            (is (not (supports-tls? "abcd[bddb]xyyx")))
            (is (not (supports-tls? "sqqs[bddb]xzyx")))
            (is (not (supports-tls? "aaaa[qwer]tyui")))
            (is (supports-tls? "ioxxoj[asdfgh]zxcvbn"))
            (is (supports-tls? "ioxzoj[asdfgh]zxcvbn[data]baab"))
            (is (not (supports-tls? "ioxzoj[asdfgh]zxcvbn[datta]baab")))
            )}
  [addr]
  (let [evidence (->> addr
                      (re-seq #"(^|\[|\])([a-z]+)")
                      (map (fn [[_ hnet part]] (cond (= "[" hnet) (if (is-abba? part) false :maybe)
                                                     (is-abba? part) true
                                                     :else :maybe)))
                      (filter #(not= :maybe %))
                      )]
    (and (not-empty evidence) (every? identity evidence))
    ))

(def input (->> (clojure.java.io/file "./resources/advent/day07_ip_addrs.txt")
                (slurp)
                (strings/split-lines)
                ))

(defn solve-part-1
  {:test #(is (= 115 (solve-part-1)))}
  []
  (->> input
       (filter supports-tls?)
       (count)
       ))


(prn "Part 1: " (solve-part-1))
;(prn "Part 2: " (solve-part-2))
