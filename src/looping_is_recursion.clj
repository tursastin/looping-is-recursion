(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc k]
                 (if (zero? k)
                   acc
                   (recur (* acc base) (dec k))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [a b]
                 (if (empty? b)
                   a
                   (recur (first b) (rest b))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (loop [a seq1
         b seq2]
    (cond (and (empty? a) (empty? b)) true
          (and (empty? a) (not (empty? b))) false
          (and (not (empty? a)) (empty? b)) false
          (not (= (first a) (first b))) false
          :else (recur (rest a) (rest b)))))

(defn find-first-index [pred a-seq]
  (loop [a a-seq
         n 0]
    (cond
     (nil? a) nil
     (empty? a) nil
     (pred (first a)) n
     :else (recur (rest a) (inc n)))))

(defn avg [a-seq]
  (loop [a a-seq
         sum 0
         n 0]
    (if (empty? a)
      (/ sum n)
      (recur (rest a)
             (+ sum (first a))
             (inc n)))))

(defn parity [a-seq]
  (let [pred (fn [coll value]
               (odd? (count (filter (fn [x] (= x value)) coll))))]
    (loop [a (set a-seq)
           result #{}]
      (cond
       (empty? a)
         result
       (pred a-seq (first a))
         (recur (rest a) (conj result (first a)))
       :else
         (recur (rest a) result)))))

(defn fast-fibo [n]
  (loop [a 0
         b 1
         k n]
    (if (= k 0)
      a
      (recur b (+ a b) (dec k)))))

(defn cut-at-repetition [a-seq]
  (loop [a a-seq
         seen #{}
         result []]
    (if (or (empty? a)
            (contains? seen (first a)))
      result
      (recur (rest a)
             (conj seen (first a))
             (conj result (first a))))))

