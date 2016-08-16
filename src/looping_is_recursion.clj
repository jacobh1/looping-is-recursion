(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (= exp 0)
                   acc
                   (recur (* acc base) base (dec exp))
                   ))]
   (helper 1 base exp)
   )) 

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [helper (fn [sq]
                   (if (empty? (rest sq))
                     (first sq)
                     (recur (rest sq))
                     ))]
      (helper a-seq)
      )
    ))

(defn seq= [seq1 seq2]
  (cond 
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false
    )
  )

(defn find-first-index [pred a-seq] 
  (let [helper (fn [idx sq]
                 (cond 
                   (empty? sq) nil
                   (pred (first sq)) idx
                   :else (recur (inc idx) (rest sq))
                   ))]
    (helper 0 a-seq)
    ))

(defn avg [a-seq]
  (let [helper (fn [sum cnt a-seq]
                 (if (empty? a-seq)
                   (/ sum cnt)
                   (recur (+ sum (first a-seq)) (inc cnt) (rest a-seq))
                   ))]
    (helper 0 0 a-seq)
    ))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    )
  )

(defn parity [a-seq]
  (let [helper (fn [odd-set sq]
                 (if (empty? sq)
                   odd-set
                   (recur (toggle odd-set (first sq)) (rest sq))
                   ))]
    (helper #{} a-seq)
    ))

(defn fast-fibo [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (let [helper (fn [n f-n-1 f-n-2]
                   (if (= n 0)
                     (+ f-n-1 f-n-2)
                     (recur (dec n) f-n-2 (+ f-n-1 f-n-2))
                     ))]
      (helper (- n 2) 0 1)
      )
    ))

(defn cut-at-repetition [a-seq]
  (let [helper (fn [seen-set prefix sq]
                 (if (or (empty? sq)
                         (contains? seen-set (first sq)))
                   prefix
                   (recur (conj seen-set (first sq)) (conj prefix (first sq)) (rest sq))
                   ))]
    (helper #{} [] a-seq)
    ))


