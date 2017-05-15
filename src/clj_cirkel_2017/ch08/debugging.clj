(ns clj_cirkel_2017.ch08.debugging
  (:use clojure.test))

(defmacro form-and-value [f] `['~f ~f])

(defmacro printvars
  "Prints a caption, a list of forms with their computed values, and returns the first value without recomputing it."
  [caption value & others]
  (let [vars (concat `['~value ~value]
                      (mapcat (fn [v] `['~v ~v]) others)
                      )]
    `(let  [~'to-print [~caption ~@vars]]
       (apply prn ~'to-print)
       (nth ~'to-print 2))))

