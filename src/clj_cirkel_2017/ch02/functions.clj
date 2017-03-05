(defn test-fn
  ([] "Nothing")
  ([a] ["one" a]))

(test-fn)
(test-fn 1)
(map #(str % "!")
     [1 2 3])
