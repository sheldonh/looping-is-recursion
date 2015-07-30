(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc e]
                 (if (zero? e)
                   acc
                   (recur (* acc base) (dec e))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [l s]
                 (if (empty? s)
                   l
                   (recur (first s) (rest s))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [a b]
                 (cond
                   (every? empty? (list a b)) true
                   (some empty? (list a b)) false
                   (not= (first a) (first b)) false
                   :else
                     (recur (rest a) (rest b))))]
    (helper seq1 seq2)))

(defn find-first-index
  "Returns the first index in seq for which predicate returns true, or nil if no
  such index exists.

  ```
  (find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
  (find-first-index zero? [1 1 3 7 2])                          ;=> nil
  (find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
  (find-first-index nil? [])                                    ;=> nil
  ```"
  {:doc/format :markdown}
  [pred a-seq]
  (loop [i 0
         r a-seq]
    (cond
      (empty? r) nil
      (pred (first r)) i
      :else (recur (inc i) (rest r)))))

(defn avg
  "Implement the function (avg a-seq) that computes the average of a sequence.
  Throws `ArithmeticException` for an empty sequence.

  ```
  (avg [1 2 3])   ;=> 2
  (avg [0 0 0 4]) ;=> 1
  (avg [1 0 0 1]) ;=> 1/2 ;; or 0.5
  ```"
  {:doc/format :markdown}
  [a-seq]
  (loop [sum 0
         n   0
         r   a-seq]
    (if (empty? r)
      (/ sum n)
      (recur (+ sum (first r)) (inc n) (rest r)))))

(defn- toggle
  "If the set contains x, returns the set without x.
  If the set is missing x, returns the set including x."
  [a-set x]
  (if (contains? a-set x)
    (disj a-set x)
    (conj a-set x)))

(defn parity
  "Returns a set of those elements that occur an odd number of times in the
  sequence.

  ```
  (parity [:a :b :c])           ;=> #{:a :b :c}
  (parity [:a :b :c :a])        ;=> #{:b :c}
  (parity [1 1 2 1 2 3 1 2 3 4] ;=> #{2 4}
  ```"
  {:doc/format :markdown}
  [a-seq]
  (loop [p  #{}
         xs a-seq]
    (if (empty? xs)
      p
      (recur (toggle p (first xs)) (rest xs)))))

(defn fast-fibo
  "Returns the `n`th fibonacci number. Tail call optimized.

  ```
  (fast-fibo 0) ;=> 0
  (fast-fibo 1) ;=> 1
  (fast-fibo 2) ;=> 1
  (fast-fibo 3) ;=> 2
  (fast-fibo 4) ;=> 3
  (fast-fibo 5) ;=> 5
  (fast-fibo 6) ;=> 8
  ```"
  {:doc/format :markdown}
  [n]
  (loop [fib      0
         fib-next 1
         i        n]
    (if (zero? i)
      fib
      (recur fib-next (+ fib fib-next) (dec i)))))

(defn cut-at-repetition
  "Returns elements from the sequence up to the first repetition.

  ```
  (cut-at-repetition [1 1 1 1 1])
  ;=> [1] doesn't have to be a vector, a sequence is fine too
  (cut-at-repetition [:cat :dog :house :milk 1 :cat :dog])
  ;=> [:cat :dog :house :milk 1]
  (cut-at-repetition [0 1 2 3 4 5])
  ;=> [0 1 2 3 4 5]
  ```"
  {:doc/format :markdown}
  [a-seq]
  (loop [n    0
         seen #{}
         xs   a-seq]
    (cond
      (empty? xs)                 a-seq
      (contains? seen (first xs)) (take n a-seq)
      :else (recur (inc n)        (conj seen (first xs)) (rest xs)))))

