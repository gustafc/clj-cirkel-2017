; --- Day 4: Security Through Obscurity ---
;
; Finally, you come across an information kiosk with a list of rooms. Of course, the list is encrypted and full of decoy
; data, but the instructions to decode the list are barely hidden nearby. Better remove the decoy data first.
;
; Each room consists of an encrypted name (lowercase letters separated by dashes) followed by a dash, a sector ID, and a
; checksum in square brackets.
;
; A room is real (not a decoy) if the checksum is the five most common letters in the encrypted name, in order, with
; ties broken by alphabetization. For example:
;
; aaaaa-bbb-z-y-x-123[abxyz] is a real room because the most common letters are a (5), b (3), and then a tie between x,
; y, and z, which are listed alphabetically.
;
; a-b-c-d-e-f-g-h-987[abcde] is a real room because although the letters are all tied (1 of each), the first five are
; listed alphabetically.
;
; not-a-real-room-404[oarel] is a real room.
;
; totally-real-room-200[decoy] is not.
;
; Of the real rooms from the list above, the sum of their sector IDs is 1514.
;
; What is the sum of the sector IDs of the real rooms?
;
; --- Part Two ---
;
; With all the decoy data out of the way, it's time to decrypt this list and get moving.
;
; The room names are encrypted by a state-of-the-art shift cipher, which is nearly unbreakable without the right
; software. However, the information kiosk designers at Easter Bunny HQ were not expecting to deal with a master
; cryptographer like yourself.
;
; To decrypt a room name, rotate each letter forward through the alphabet a number of times equal to the room's sector
; ID. A becomes B, B becomes C, Z becomes A, and so on. Dashes become spaces.
;
; For example, the real name for qzmt-zixmtkozy-ivhz-343 is very encrypted name.
;
; What is the sector ID of the room where North Pole objects are stored?
;

(ns clj_cirkel_2017.hacking.advent.day04-security-through-obscurity
  (:use clojure.test)
  (:require [clojure.string :as strings]))

(defn easter-bunny-checksum
  {:test #(do
            (is (= "abxyz" (easter-bunny-checksum "aaaaa-bbb-z-y-x")))
            (is (= "abcde" (easter-bunny-checksum "a-b-c-d-e-f-g-h")))
            (is (= "oarel" (easter-bunny-checksum "not-a-real-room")))
            (is (not= "decoy" (easter-bunny-checksum "totally-real-room")))
            )}
  [str]
  (->> (re-seq #"[a-z]" str)
       (reduce (fn [m ch] (update m ch #(inc (or % 0)))) {})
       (sort (fn [[k1 v1] [k2 v2]] (let [count-cmp (compare v2 v1)]
                                     (if (not= 0 count-cmp)
                                       count-cmp
                                       (compare k1 k2)))))
       (take 5)
       (map first)
       (strings/join)
       ))

(defn parse-room
  {:test #(do
            (is (= {:encrypted-name "ibghopzs-dzoghwq-ufogg-rsdzcmasbh" :sector-id 948 :checksum "ghosz"}
                   (parse-room "ibghopzs-dzoghwq-ufogg-rsdzcmasbh-948[ghosz]"))))}
  [room-str]
  (let [[[_ name sector chk]] (re-seq #"^(.*)-(\d+?)\[(.*)\]$" room-str)]
    {:encrypted-name name :sector-id (read-string sector) :checksum chk}))

(defn decoy?
  {:test #(do
            (is (not (decoy? (parse-room "aaaaa-bbb-z-y-x-123[abxyz]"))))
            (is (decoy? (parse-room "totally-real-room-200[decoy]")))
            )}
  [{:keys [encrypted-name checksum]}]
  (not= checksum (easter-bunny-checksum encrypted-name))
  )

(defn sum-of-non-decoy-room-sectors
  {:test #(do (is (= 1514 (sum-of-non-decoy-room-sectors (map parse-room ["aaaaa-bbb-z-y-x-123[abxyz]"
                                                                          "a-b-c-d-e-f-g-h-987[abcde]"
                                                                          "not-a-real-room-404[oarel]"
                                                                          "totally-real-room-200[decoy]"])))))}
  [rooms]
  (->> (filter #(not (decoy? %)) rooms)
       (map :sector-id)
       (reduce + 0))
  )

(defn rot-char
  {:test #(do
            (is (= \a (rot-char \a 0)))
            (is (= \b (rot-char \a 1)))
            (is (= \a (rot-char \a 26)))
            (is (= \a (rot-char \a (* 2 26))))
            (is (= \z (rot-char \z 0)))
            (is (= \a (rot-char \z 1)))
            (is (= \a (rot-char \z 1)))
            (is (= \z (rot-char \z 26)))
            (is (= \z (rot-char \z (* 2 26))))
            )}
  [ch rotations]
  (char
    (+ (int \a)
       (mod (- (+ (int ch) rotations) (int \a))
            (- (int \z) (int \a) -1)))))

(defn decrypt-room-name
  {:test #(do
            (is (= "very encrypted name" (decrypt-room-name (parse-room "qzmt-zixmtkozy-ivhz-343[zimth]"))))
            )}
  [{:keys [encrypted-name sector-id]}]
  (strings/join (map (fn [ch] (if (= \- ch) \space
                              (rot-char ch sector-id)))
       encrypted-name)))

(def input (->> (clojure.java.io/file "./resources/advent/day04_rooms.txt")
                (slurp)
                (strings/split-lines)
                (map parse-room)
                ))

(defn solve-part-1
  {:test #(do (is (= 409147 (solve-part-1))))}
  []
  (sum-of-non-decoy-room-sectors input))

(defn solve-part-2
  {:test #(do
            (is (= {:encrypted-name "kloqemlib-lygbzq-pqloxdb", :sector-id 991, :checksum "lbqod"}
                   (solve-part-2))))}
  []
  (->> (filter #(not (decoy? %)) input)
       (filter #(= "northpole object storage" (decrypt-room-name %)))
       (first)))

(prn "Part 1: " (solve-part-1))
(prn "Part 2: " (solve-part-2))
