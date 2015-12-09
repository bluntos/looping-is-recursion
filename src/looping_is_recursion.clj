(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc ba ex]
                  (if (= 0 ex)
                     acc
                     (recur (* acc ba)  ba (dec ex))))]
     (helper 1 base exp)))
  
(defn last-element [a-seq]
  (cond (empty? a-seq) nil
     (= 1 (count a-seq)) (first a-seq)
     :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (not (== (count seq1) (count seq2))) false
      (and (empty? seq1) (empty? seq2)) true
      (== (first seq1) (first seq2)) 
          (recur (rest seq1) (rest seq2)) 
      :else false ))

(defn find-first-index [pred a-seq]
  (loop [index 0 
         seq a-seq]
     (cond (empty? seq) nil 
           (pred (first seq)) index
           :else (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [nums 0 
         average 0
         seq a-seq]
     (if (empty? seq) (/ average nums)
        (recur (inc nums) (+ average (first seq)) (rest seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [res #{}
         seq a-seq]
     (if (empty? seq) res
	(recur (toggle res (first seq)) (rest seq)))))

(defn fast-fibo [n]
  (loop [iter 2
         fib-2 0
         fib-1 1]
     (cond (= 0 n) 0
	   (= 1 n) 1
           (= n iter) (+ fib-1 fib-2)
           :else (recur (inc iter)  fib-1 (+ fib-1 fib-2) ))))

(defn cut-at-repetition [a-seq]
  (loop [res []
         seq a-seq]
   (if (or (empty? seq) (contains? (set res) (first seq))) 
       res
       (recur (conj res (first seq))  (rest seq)))))

