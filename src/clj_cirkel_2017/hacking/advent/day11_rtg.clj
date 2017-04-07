; --- Day 11: Radioisotope Thermoelectric Generators ---
;
; You come upon a column of four floors that have been entirely sealed off from the rest of the building except for a
; small dedicated lobby. There are some radiation warnings and a big sign which reads "Radioisotope Testing Facility".
;
; According to the project status board, this facility is currently being used to experiment with Radioisotope
; Thermoelectric Generators (RTGs, or simply "generators") that are designed to be paired with specially-constructed
; microchips. Basically, an RTG is a highly radioactive rock that generates electricity through heat.
;
; The experimental RTGs have poor radiation containment, so they're dangerously radioactive. The chips are prototypes
; and don't have normal radiation shielding, but they do have the ability to generate an electromagnetic radiation
; shield when powered. Unfortunately, they can only be powered by their corresponding RTG. An RTG powering a microchip
; is still dangerous to other microchips.
;
; In other words, if a chip is ever left in the same area as another RTG, and it's not connected to its own RTG, the
; chip will be fried. Therefore, it is assumed that you will follow procedure and keep chips connected to their
; corresponding RTG when they're in the same room, and away from other RTGs otherwise.
;
; These microchips sound very interesting and useful to your current activities, and you'd like to try to retrieve them.
; The fourth floor of the facility has an assembling machine which can make a self-contained, shielded computer for you
; to take with you - that is, if you can bring it all of the RTGs and microchips.
;
; Within the radiation-shielded part of the facility (in which it's safe to have these pre-assembly RTGs), there is an
; elevator that can move between the four floors. Its capacity rating means it can carry at most yourself and two RTGs
; or microchips in any combination. (They're rigged to some heavy diagnostic equipment - the assembling machine will
; detach it for you.) As a security measure, the elevator will only function if it contains at least one RTG or
; microchip. The elevator always stops on each floor to recharge, and this takes long enough that the items within it
; and the items on that floor can irradiate each other. (You can prevent this if a Microchip and its Generator end up on
; the same floor in this way, as they can be connected while the elevator is recharging.)
;
; You make some notes of the locations of each component of interest (your puzzle input). Before you don a hazmat suit
; and start moving things around, you'd like to have an idea of what you need to do.
;
; When you enter the containment area, you and the elevator will start on the first floor.
;
; For example, suppose the isolated area has the following arrangement:
;
; The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
; The second floor contains a hydrogen generator.
; The third floor contains a lithium generator.
; The fourth floor contains nothing relevant.
;
; As a diagram (F# for a Floor number, E for Elevator, H for Hydrogen, L for Lithium, M for Microchip, and G for
; Generator), the initial state looks like this:
;
; F4 .  .  .  .  .
; F3 .  .  .  LG .
; F2 .  HG .  .  .
; F1 E  .  HM .  LM
;
; Then, to get everything up to the assembling machine on the fourth floor, the following steps could be taken:
;
;     Bring the Hydrogen-compatible Microchip to the second floor, which is safe because it can get power from the
;     Hydrogen Generator:
;
;     F4 .  .  .  .  .
;     F3 .  .  .  LG .
;     F2 E  HG HM .  .
;     F1 .  .  .  .  LM
;
;     Bring both Hydrogen-related items to the third floor, which is safe because the Hydrogen-compatible microchip is
;     getting power from its generator:
;
;     F4 .  .  .  .  .
;     F3 E  HG HM LG .
;     F2 .  .  .  .  .
;     F1 .  .  .  .  LM
;
;     Leave the Hydrogen Generator on floor three, but bring the Hydrogen-compatible Microchip back down with you so you
;     can still use the elevator:
;
;     F4 .  .  .  .  .
;     F3 .  HG .  LG .
;     F2 E  .  HM .  .
;     F1 .  .  .  .  LM
;
;     At the first floor, grab the Lithium-compatible Microchip, which is safe because Microchips don't affect each
;     other:
;
;     F4 .  .  .  .  .
;     F3 .  HG .  LG .
;     F2 .  .  .  .  .
;     F1 E  .  HM .  LM
;
;     Bring both Microchips up one floor, where there is nothing to fry them:
;
;     F4 .  .  .  .  .
;     F3 .  HG .  LG .
;     F2 E  .  HM .  LM
;     F1 .  .  .  .  .
;
;     Bring both Microchips up again to floor three, where they can be temporarily connected to their corresponding
;     generators while the elevator recharges, preventing either of them from being fried:
;
;     F4 .  .  .  .  .
;     F3 E  HG HM LG LM
;     F2 .  .  .  .  .
;     F1 .  .  .  .  .
;
;     Bring both Microchips to the fourth floor:
;
;     F4 E  .  HM .  LM
;     F3 .  HG .  LG .
;     F2 .  .  .  .  .
;     F1 .  .  .  .  .
;
;     Leave the Lithium-compatible microchip on the fourth floor, but bring the Hydrogen-compatible one so you can still
;     use the elevator; this is safe because although the Lithium Generator is on the destination floor, you can connect
;     Hydrogen-compatible microchip to the Hydrogen Generator there:
;
;     F4 .  .  .  .  LM
;     F3 E  HG HM LG .
;     F2 .  .  .  .  .
;     F1 .  .  .  .  .
;
;     Bring both Generators up to the fourth floor, which is safe because you can connect the Lithium-compatible
;     Microchip to the Lithium Generator upon arrival:
;
;     F4 E  HG .  LG LM
;     F3 .  .  HM .  .
;     F2 .  .  .  .  .
;     F1 .  .  .  .  .
;
;     Bring the Lithium Microchip with you to the third floor so you can use the elevator:
;
;     F4 .  HG .  LG .
;     F3 E  .  HM .  LM
;     F2 .  .  .  .  .
;     F1 .  .  .  .  .
;
;     Bring both Microchips to the fourth floor:
;
;     F4 E  HG HM LG LM
;     F3 .  .  .  .  .
;     F2 .  .  .  .  .
;     F1 .  .  .  .  .
;
; In this arrangement, it takes 11 steps to collect all of the objects at the fourth floor for assembly. (Each elevator
; stop counts as one step, even if nothing is added to or removed from it.)
;
; In your situation, what is the minimum number of steps required to bring all of the objects to the fourth floor?




(ns advent-11
  (:use clojure.test)
  (:require [clojure.string :as strings])
  (:require [clojure.set :as sets]))


(defn mk-floor [& items]
  {:pre [(= 0 (mod (count items) 2))]}
  (set (map vec (partition 2 items))))

(def empty-floor #{})

(def example-layout-inputs ["F4 .  .  .  .  .
                             F3 .  .  .  LG .
                             F2 .  HG .  .  .
                             F1 E  .  HM .  LM"
                            "F4 .  .  .  .  .  \nF3 .  .  .  LG .  \nF2 E  HG HM .  .  \nF1 .  .  .  .  LM "
                            "F4 .  .  .  .  .  \nF3 E  HG HM LG .  \nF2 .  .  .  .  .  \nF1 .  .  .  .  LM "
                            "F4 .  .  .  .  .  \nF3 .  HG .  LG .  \nF2 E  .  HM .  .  \nF1 .  .  .  .  LM "
                            "F4 .  .  .  .  .  \nF3 .  HG .  LG .  \nF2 .  .  .  .  .  \nF1 E  .  HM .  LM "
                            "F4 .  .  .  .  .  \nF3 .  HG .  LG .  \nF2 E  .  HM .  LM \nF1 .  .  .  .  .  "
                            "F4 .  .  .  .  .  \nF3 E  HG HM LG LM \nF2 .  .  .  .  .  \nF1 .  .  .  .  .  "
                            "F4 E  .  HM .  LM \nF3 .  HG .  LG .  \nF2 .  .  .  .  .  \nF1 .  .  .  .  .  "
                            "F4 .  .  .  .  LM \nF3 E  HG HM LG .  \nF2 .  .  .  .  .  \nF1 .  .  .  .  .  "
                            "F4 E  HG .  LG LM \nF3 .  .  HM .  .  \nF2 .  .  .  .  .  \nF1 .  .  .  .  .  "
                            "F4 .  HG .  LG .  \nF3 E  .  HM .  LM \nF2 .  .  .  .  .  \nF1 .  .  .  .  .  "
                            "F4 E  HG HM LG LM \nF3 .  .  .  .  .  \nF2 .  .  .  .  .  \nF1 .  .  .  .  .  "
                            ])

(defn make-layout
  ([on-floor floors] (make-layout on-floor floors (->> (map vector (range) floors)
                                                       (filter (fn [[n f]] (not (empty? f))))
                                                       (first)
                                                       (first))))
  ([on-floor floors first-occupied-floor]
   {:on-floor             on-floor
    :floors               floors
    :first-occupied-floor first-occupied-floor})
  )

(defn parse-example-layout
  {:test #(do (is (= {:on-floor             0
                      :first-occupied-floor 0
                      :floors               [; The first floor contains a hydrogen-compatible microchip and a lithium-compatible
                                             ; microchip.
                                             #{[:hydrogen :chip] [:lithium :chip]}
                                             ; The second floor contains a hydrogen generator.
                                             #{[:hydrogen :generator]}
                                             ; The third floor contains a lithium generator.
                                             #{[:lithium :generator]}
                                             ; The fourth floor contains nothing relevant.
                                             #{}
                                             ]}
                     (parse-example-layout (example-layout-inputs 0)))))}
  [s]
  (let [parsed-floors (->> (strings/split-lines s)
                           (map strings/trim)
                           (reverse)
                           (map (fn [floor-number floor-string]
                                  {:floor-number floor-number
                                   :has-elevator (= \E (nth floor-string 3))
                                   :items        (->> (re-seq #"(H|L)(M|G)" floor-string)
                                                      (map #(map {"H" :hydrogen "L" :lithium "M" :chip "G" :generator}
                                                                 (drop 1 %)))
                                                      (map vec)
                                                      (set))
                                   })
                                (range))
                           )]
    (make-layout (->> (filter :has-elevator parsed-floors)
                      (map :floor-number)
                      (first))
                 (vec (map :items parsed-floors)))
    ))

(def example-layouts (map parse-example-layout example-layout-inputs))


(defn nothing-gets-fried?
  {:test #(do
            (is (true? (nothing-gets-fried? (mk-floor :x :generator))))
            (is (true? (nothing-gets-fried? (mk-floor :x :chip))))
            (is (true? (nothing-gets-fried? (mk-floor :x :chip :x :generator))))
            (is (true? (nothing-gets-fried? (mk-floor :x :generator :x :chip))))
            (is (true? (nothing-gets-fried? (mk-floor :x :generator :x :chip))))
            (is (false? (nothing-gets-fried? (mk-floor :x :generator :x :chip :y :chip))))
            (is (false? (nothing-gets-fried? (mk-floor :x :generator :y :chip :z :chip))))
            (is (false? (nothing-gets-fried? (mk-floor :x :chip :y :generator :z :generator))))
            )}
  [items]
  (let [items-by-device (group-by #(% 1) items)
        generator-substances (set (map first (items-by-device :generator)))
        chip-substances (set (map first (items-by-device :chip)))]
    (or (empty? generator-substances)
        (sets/subset? chip-substances generator-substances))
    ))

(defn successive-pairs
  {:test #(do
            (is (= [] (successive-pairs [])))
            (is (= [] (successive-pairs [:x])))
            (is (= [[:x :y]] (successive-pairs [:x :y])))
            (is (= [[:x :y] [:x :z] [:y :z]]
                   (successive-pairs [:x :y :z])
                   ))
            )}
  [coll]
  (if (< (count coll) 2)
    []
    (let [[x & xs] (seq coll)]
      (lazy-cat
        (map #(vector x %) xs)
        (successive-pairs xs)
        ))
    ))

(defn items-for-elevator
  {:test #(do
            (is (= [[[:x :generator]]
                    [[:y :generator]]
                    [[:x :chip]]
                    [[:x :generator] [:y :generator]]
                    [[:x :generator] [:x :chip]]
                    ; This last pair is a little unclear if it's legal, but since the elevator's "capacity rating means
                    ; it can carry at most yourself and two RTGs or microchips in any combination" seems to imply that
                    ; things don't fry each other in the elevator.
                    [[:y :generator] [:x :chip]]
                    ]
                   (items-for-elevator [[:x :generator] [:y :generator] [:x :chip]])))
            )}
  [items-on-floor]
  (lazy-cat (map vector items-on-floor)
            ; Or should it be: (filter nothing-gets-fried? (successive-pairs items-on-floor))
            (successive-pairs items-on-floor))
  )

(defn possible-moves
  {:test #(do
            (is (= #{[empty-floor (mk-floor :a :chip)]}
                   (set (possible-moves (mk-floor :a :chip) empty-floor))))
            (is (= #{[empty-floor (mk-floor :x :generator :a :generator :a :chip)]
                     [(mk-floor :a :chip) (mk-floor :x :generator :a :generator)]}
                   (set (possible-moves (mk-floor :a :generator :a :chip) (mk-floor :x :generator)))))
            (is (= #{[(mk-floor :a :generator :x :generator) (mk-floor :a :chip)]
                     [(mk-floor :a :generator :a :chip) (mk-floor :x :generator)]
                     [(mk-floor :x :generator) (mk-floor :a :generator :a :chip)]
                     [(mk-floor :a :chip) (mk-floor :a :generator :x :generator)]}
                   (set (possible-moves (mk-floor :a :generator :a :chip :x :generator) empty-floor))))
            )}
  [src-floor dst-floor]
  (->> (items-for-elevator src-floor)
       (map (fn [elevator-items]
              [(sets/difference src-floor (set elevator-items))
               (sets/union dst-floor (set elevator-items))]
              ))
       (filter #(every? nothing-gets-fried? %))
       ))

(defn successive-layouts
  {:test #(do
            (is (contains? (set (successive-layouts (nth example-layouts 0))) (nth example-layouts 1)))
            (is (contains? (set (successive-layouts (nth example-layouts 1))) (nth example-layouts 2)))
            (is (contains? (set (successive-layouts (nth example-layouts 2))) (nth example-layouts 3)))
            (is (contains? (set (successive-layouts (nth example-layouts 3))) (nth example-layouts 4)))
            (is (contains? (set (successive-layouts (nth example-layouts 4))) (nth example-layouts 5)))
            (is (contains? (set (successive-layouts (nth example-layouts 5))) (nth example-layouts 6)))
            (is (contains? (set (successive-layouts (nth example-layouts 6))) (nth example-layouts 7)))
            (is (contains? (set (successive-layouts (nth example-layouts 7))) (nth example-layouts 8)))
            (is (contains? (set (successive-layouts (nth example-layouts 8))) (nth example-layouts 9)))
            (is (contains? (set (successive-layouts (nth example-layouts 9))) (nth example-layouts 10)))
            (is (contains? (set (successive-layouts (nth example-layouts 10))) (nth example-layouts 11)))
            (is (= []                                       ; Never move down below first occupied floor
                   (->> (successive-layouts (nth example-layouts 5))
                        (filter (fn [successor] (not (empty? (get-in successor [:floors 0])))))
                        )))
            )}
  [{:keys [on-floor first-occupied-floor floors] :as layout}]
  (letfn [(make-moves [to-floor]
            (if-not (< (dec first-occupied-floor) to-floor (count floors))
              []
              (->> (possible-moves (floors on-floor) (floors to-floor))
                   (map (fn [[src dst]] (make-layout
                                          to-floor
                                          (assoc floors on-floor src to-floor dst)
                                          (if (and (= first-occupied-floor on-floor)
                                                   (empty? src))
                                            to-floor        ; src is emptied, dst becomes first occupied floor
                                            first-occupied-floor
                                            )))))
              ))]
    (concat
      (make-moves (inc on-floor))
      (make-moves (dec on-floor))
      ))
  )

(defn traverse-breadth-first
  {:test #(letfn [(successors [s] [(str s "a") (str s "b")])]
            (is (= [""] (take 1 (traverse-breadth-first "" successors))))
            (is (= ["" "a" "b"]
                   (take 3 (traverse-breadth-first "" successors))))
            (is (= ["" "a" "b" "aa" "ab" "ba" "bb"]
                   (take 7 (traverse-breadth-first "" successors))))
            )}
  [initial-state get-successors]
  (letfn [(generate [expanded]
            (lazy-cat expanded
                      (generate (mapcat get-successors expanded))))]
    (generate [initial-state])
    ))

(defn everything-on-top-floor?
  {:test #(do
            (is (= false (everything-on-top-floor? (first example-layouts))))
            (is (= true (everything-on-top-floor? (last example-layouts))))
            )}
  [layout]
  (every? empty? (butlast (:floors layout)))
  )

(defn count-steps-to-get-everything-to-top-floor
  {:test #(do
            (is (= 11 (time (count-steps-to-get-everything-to-top-floor (first example-layouts)))))
            )}
  [first-layout]
  (->> (traverse-breadth-first (list first-layout)
                               (fn [path]
                                 (->> (successive-layouts (peek path))
                                      (filter #(not-any? (partial = %) path))
                                      (map #(conj path %)))))
       (filter (comp everything-on-top-floor? peek))
       (first)
       (count)
       (dec))
  )


(def puzzle-input
  (make-layout 0
               [; The first floor contains a strontium generator, a strontium-compatible microchip, a
                ; plutonium generator, and a plutonium-compatible microchip.
                (mk-floor :strontium :generator
                          :strontium :chip
                          :plutonium :generator
                          :plutonium :chip)
                ; The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible
                ; microchip, a curium generator, and a curium-compatible microchip.
                (mk-floor :thulium :generator
                          :ruthenium :generator
                          :ruthenium :chip
                          :curium :generator
                          :curium :chip)
                ; The third floor contains a thulium-compatible microchip.
                (mk-floor :thulium :chip)
                ; The fourth floor contains nothing relevant.
                empty-floor
                ]
               ))

(defn solve-part-1
  {:test #(is (= "FIXME" (solve-part-1)))}
  []
  "FIXME"
  ;(count-steps-to-get-everything-to-top-floor puzzle-input)
  )

(defn solve-part-2
  {:test #(is (= "FIXME" (solve-part-2)))}
  []
  "FIXME")

;(println "Part 1:" (solve-part-1))
;(println "Part 2:" (solve-part-2))
