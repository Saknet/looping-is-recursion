(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [n exp]
                 (if (zero? exp)
                   n
                   (recur (* base n) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (not (= (first seq1) (first seq2))) false
   (or (empty? seq1) (empty? seq2)) false
   :else (seq= (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         a-seq a-seq]
    (cond
     (empty? a-seq) nil
     (pred (first a-seq)) i
     :else (recur (inc i) (rest a-seq)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [average 0
           i 0
           a-seq a-seq]
      (if (empty? a-seq)
        (/ average i)
        (recur (+ average (first a-seq)) (inc i) (rest a-seq))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (if (empty? a-seq)
    nil
    (loop [a-set #{}
           a-seq a-seq]
      (if (empty? a-seq)
        a-set
        (recur (toggle a-set (first a-seq)) (rest a-seq))))))

(defn fast-fibo [n]
  (loop [f1 1
         f0 0
         i n]
    (cond
     (= i 0) f0
     :else (recur (+ f1 f0) f1 (dec i)))))

(defn cut-at-repetition [a-seq]
  (loop [e #{}
         a []
         a-seq a-seq]
    (if (or (contains? e (first a-seq)) (empty? a-seq))
      a
      (recur (conj e (first a-seq)) (conj a (first a-seq)) (rest a-seq)))))


