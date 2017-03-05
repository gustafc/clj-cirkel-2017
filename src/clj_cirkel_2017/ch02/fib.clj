(defn fib [fib-n]
  (if (< fib-n 2)
    1
    (loop [prevprev 1 prev 1 n 2]
      (let [curr (+ prevprev prev)]
        (if (= fib-n n)
          curr
          (recur prev curr (inc n)))))))

(map fib (range 10))
