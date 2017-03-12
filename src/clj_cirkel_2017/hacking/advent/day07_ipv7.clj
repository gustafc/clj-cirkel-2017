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

(defn parse-ipv7
  {:test #(do
            (is (= {:supernet ["abc" "def"] :hypernet ["ghi" "jkl"]}
                   (parse-ipv7 "abc[ghi]def[jkl]")))
            (is (= {:supernet ["abc" "def" "xyz"] :hypernet ["ghi" "jkl"]}
                   (parse-ipv7 "abc[ghi]def[jkl]xyz")))
            )}
  [addr]
  (->> (re-seq #"(^|\[|\])([a-z]+)" addr)
       (reduce (fn [addr-now [_ hyper-or-super content]]
                 (update addr-now (if (= "[" hyper-or-super) :hypernet :supernet) #(conj % content))
                 )
               {:supernet [] :hypernet []})
       ))

(defn supports-tls?
  {:test #(do
            (is (supports-tls? (parse-ipv7 "abba[mnop]qrst")))
            (is (not (supports-tls? (parse-ipv7 "abcd[bddb]xyyx"))))
            (is (not (supports-tls? (parse-ipv7 "sqqs[bddb]xzyx"))))
            (is (not (supports-tls? (parse-ipv7 "aaaa[qwer]tyui"))))
            (is (supports-tls? (parse-ipv7 "ioxxoj[asdfgh]zxcvbn")))
            (is (supports-tls? (parse-ipv7 "ioxzoj[asdfgh]zxcvbn[data]baab")))
            (is (not (supports-tls? (parse-ipv7 "ioxzoj[asdfgh]zxcvbn[datta]baab"))))
            )}
  [{:keys [supernet hypernet]}]
  (and
    (not-any? is-abba? hypernet)
    (some is-abba? supernet)
    ))

(def input (->> (clojure.java.io/file "./resources/advent/day07_ip_addrs.txt")
                (slurp)
                (strings/split-lines)
                (map parse-ipv7)
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
