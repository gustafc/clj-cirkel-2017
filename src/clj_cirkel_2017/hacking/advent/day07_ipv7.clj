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
;
; --- Part Two ---
;
; You would also like to know which IPs support SSL (super-secret listening).
;
; An IP supports SSL if it has an Area-Broadcast Accessor, or ABA, anywhere in the supernet sequences (outside any
; square bracketed sections), and a corresponding Byte Allocation Block, or BAB, anywhere in the hypernet sequences. An
; ABA is any three-character sequence which consists of the same character twice with a different character between
; them, such as xyx or aba. A corresponding BAB is the same characters but in reversed positions: yxy and bab,
; respectively.
;
; For example:
;
;     aba[bab]xyz supports SSL (aba outside square brackets with corresponding bab within square brackets).
;
;     xyx[xyx]xyx does not support SSL (xyx, but no corresponding yxy).
;
;     aaa[kek]eke supports SSL (eke in supernet with corresponding kek in hypernet; the aaa sequence is not related,
;     because the interior character must be different).
;
;     zazbz[bzb]cdb supports SSL (zaz has no corresponding aza, but zbz has a corresponding bzb, even though zaz and zbz
;     overlap).
;
; How many IPs in your puzzle input support SSL?
;

(ns clj_cirkel_2017.hacking.advent.day07-ipv7
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

(defn extract-aba
  {:test #(do
            (is (= [] (extract-aba "ba")))
            (is (= [] (extract-aba "asdfghjk")))
            (is (= ["aba"] (extract-aba "aba")))
            (is (= ["aba" "kek"] (extract-aba "njkabakkkek")))
            )}
  [s]
  (if (< (count s) 3)
    []
    (->> (map vector s (drop 1 s) (drop 2 s))
         (filter (fn [[a b c]] (and (not= a b) (= a c))))
         (map strings/join)
         )))

(defn aba-to-bab
  {:test #(is (= "bab" (aba-to-bab "aba")))}
  [s]
  (str (subs s 1 3) (subs s 1 2))
  )

(defn supports-ssl?
  {:test #(do
            (is (supports-ssl? (parse-ipv7 "aba[bab]xyz")))
            (is (not (supports-ssl? (parse-ipv7 "xyx[xyx]xyx"))))
            (is (supports-ssl? (parse-ipv7 "aaa[kek]eke")))
            (is (supports-ssl? (parse-ipv7 "zazbz[bzb]cdb")))
            )}
  [{:keys [supernet hypernet] :as inp}]
  (let [super-aba (into #{} (mapcat extract-aba supernet))
        hyper-bab (mapcat #(map aba-to-bab (extract-aba %)) hypernet)]
    (some #(contains? super-aba %) hyper-bab)
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

(defn solve-part-2
  {:test #(is (= 231 (solve-part-2)))}
  []
  (->> input
       (filter supports-ssl?)
       (count)
       ))


(prn "Part 1: " (solve-part-1))
(prn "Part 2: " (solve-part-2))
