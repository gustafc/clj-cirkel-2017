; --- Day 19: An Elephant Named Joseph ---
;
; The Elves contact you over a highly secure emergency channel. Back at the North Pole, the Elves are busy
; misunderstanding White Elephant parties.
;
; Each Elf brings a present. They all sit in a circle, numbered starting with position 1. Then, starting with the first
; Elf, they take turns stealing all the presents from the Elf to their left. An Elf with no presents is removed from the
; circle and does not take turns.
;
; For example, with five Elves (numbered 1 to 5):
;
;   1
; 5   2
;  4 3
;
;     Elf 1 takes Elf 2's present.
;     Elf 2 has no presents and is skipped.
;     Elf 3 takes Elf 4's present.
;     Elf 4 has no presents and is also skipped.
;     Elf 5 takes Elf 1's two presents.
;     Neither Elf 1 nor Elf 2 have any presents, so both are skipped.
;     Elf 3 takes Elf 5's three presents.
;
; So, with five Elves, the Elf that sits starting in position 3 gets all the presents.
;
; With the number of Elves given in your puzzle input, which Elf gets all the presents?
;
; Your puzzle input is 3001330.

(ns clj-cirkel-2017.hacking.advent.day19-white-elephant
  (:use [clojure.test :refer [is]])
  )

(defn pqueue
  [& args]
  (into clojure.lang.PersistentQueue/EMPTY args))

(defn create-state
  {:test #(is (= (create-state 5)
                 (pqueue {:name 1 :presents 1}
                         {:name 2 :presents 1}
                         {:name 3 :presents 1}
                         {:name 4 :presents 1}
                         {:name 5 :presents 1})))}
  [n-participants]
  (->> (range n-participants)
       (map (fn [n] {:name (inc n) :presents 1}))
       (into (pqueue))
       ))

(def circle-of-5 (create-state 5))

(defn steal-and-advance
  {:test #(do
            (is (= (pqueue {:name 3 :presents 1}
                           {:name 4 :presents 1}
                           {:name 5 :presents 1}
                           {:name 1 :presents 2})
                   (steal-and-advance circle-of-5)))
            (is (= (pqueue {:name 5 :presents 1}
                           {:name 1 :presents 2}
                           {:name 3 :presents 2}
                           )
                   (steal-and-advance circle-of-5 2)))
            (is (= (pqueue {:name 3 :presents 2}
                           {:name 5 :presents 3})
                   (steal-and-advance circle-of-5 3)))
            (is (= (pqueue {:name 3 :presents 5})
                   (steal-and-advance circle-of-5 4)))
            )}
  ([state times] (->> (iterate steal-and-advance state)
                      (take (inc times))
                      (last)))
  ([participants]
   (let [thief (peek participants)
         without-thief (pop participants)
         victim (peek without-thief)
         without-thief-and-victim (pop without-thief)
         ]
     (conj without-thief-and-victim (update thief :presents #(+ % (victim :presents))))
     )))

(defn find-winner
  {:test #(is (= {:name 3 :presents 5}
                 (find-winner circle-of-5)))}
  [state]
  (->> (iterate steal-and-advance state)
       (filter #(= 1 (count %)))
       (first)
       (peek)
       ))

(def puzzle-input 3001330)

(defn solve-part-1
  {:test #(is (= 1808357 (solve-part-1)))}
  []
  (:name (find-winner (create-state puzzle-input))))