(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [result base exp]
                 (if (zero? exp)
                   result
                   (recur (* result base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [seq1]
                 (if (>= 1 (count seq1))
                   (first seq1)
                   (recur (rest seq1))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seqa seqb]
                 (if (and (empty? seqa) (empty? seqb))
                   true
                   (if (or (not= (first seqa) (first seqb)) (or (empty? seqa) (empty? seqb)))
                     false
                     (recur (rest seqa) (rest seqb)))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         seq1 a-seq]
    (if (empty? seq1)
      nil
      (if (pred (first seq1))
        acc
        (recur (inc acc) (rest seq1))))))

(defn avg [a-seq]
  (loop [num 0
         acc 0
         seq1 a-seq]
    (if (empty? seq1)
      (/ acc num)
      (recur (inc num) (+ acc (first seq1)) (rest seq1)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq1 a-seq
         seq2 (set nil)]
    (if (empty? seq1)
      seq2
      (recur (rest seq1) (toggle seq2 (first seq1))))))

(defn fast-fibo [l]
  (if (zero? l)
    0
    (loop [fl 1
           fl-1 0
           acc (- l 1)]
      (if (zero? acc)
        fl
        (recur (+ fl fl-1) fl (dec acc))))))

(defn cut-at-repetition [a-seq]
  (loop [seq1 #{}
         seq2 []
         seq3 a-seq]
    (if (or (contains? seq1 (first seq3)) (empty? seq3))
      seq2
      (recur (conj seq1 (first seq3)) (conj seq2 (first seq3)) (rest seq3)))))
