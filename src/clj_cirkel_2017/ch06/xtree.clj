(ns xtree
  (:use clojure.test))

(defn make-xt [& args] (reduce (fn [xt v] (xt-conj v xt)) nil args))

(defn xt-conj
  {:test #(do
            (is (= [nil 1 nil] (make-xt 1)))
            (is (= [[nil 0 nil] 1 nil] (make-xt 1 0)))
            (is (= [[nil 0 nil] 1 [nil 2 nil]] (make-xt 1 0 2)))
            )}
  [new-val xt]
  (if-not xt [nil new-val nil]
             (update xt
                     (if (< new-val (xt 1)) 0 2)
                     (fn [sub-xt] (xt-conj new-val sub-xt)))
             ))

(defn xt-seq
  {:test #(do
            (is (= [] (xt-seq nil)))
            (is (= [1 2 3] (xt-seq (make-xt 3 1 2))))
            )}
  [[left val right :as xt]]
  (if-not xt []
             (lazy-cat (xt-seq left) [val] (xt-seq right)))
  )