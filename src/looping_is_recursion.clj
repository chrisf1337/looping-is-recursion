(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [last rest-seq]
                 (if (empty? rest-seq)
                   last
                   (recur (first rest-seq) (rest rest-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2]
                 (cond
                   (and (empty? s1) (empty? s2)) true
                   (or (empty? s1) (empty? s2)) false
                   (not= (first s1) (first s2)) false
                   :else (recur (rest s1) (rest s2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         rest-seq a-seq]
    (cond
      (empty? rest-seq) nil
      (pred (first rest-seq)) index
      :else (recur (inc index) (rest rest-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         rest-seq a-seq]
    (if (empty? rest-seq)
      (/ sum n)
      (recur (+ sum (first rest-seq)) (inc n) (rest rest-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [parity-set #{}
         rest-seq a-seq]
    (if (empty? rest-seq)
      parity-set
      (recur (toggle parity-set (first rest-seq)) (rest rest-seq)))))

(defn fast-fibo [n]
  (loop [n0 0
         n1 1
         nn n]
    (if (zero? nn)
      n0
      (recur n1 (+ n0 n1) (dec nn)))))

(defn cut-at-repetition [a-seq]
  (loop [unique-elems #{}
         unique-vec []
         rest-seq a-seq]
    (if (or (empty? rest-seq) (contains? unique-elems (first rest-seq)))
      unique-vec
      (recur (conj unique-elems (first rest-seq)) (conj unique-vec (first rest-seq)) (rest rest-seq)))))
