(ns adventofcode-2019.exercises.day-7.part-1
  (:require [clojure.string :as str]))

(defn intcode-value [intcode pos param]
  (if (= param "0")
    (nth intcode (nth intcode pos))
    (nth intcode pos)))

(defn intcode-pos [intcode pos param]
  (if (= param "0")
    (nth intcode pos)
    pos))

(defn format-op [op]
  (format "%05d" op))

(defn split-op [op]
  (str/split op #""))

(defn read-intcode [intcode pos inputs outputs]
  (let [op-str (format-op (nth intcode pos))
        op (subs op-str (- (count op-str) 2))]
    (cond
      (= op "99")
      outputs
      
      (or (= op "01") (= op "02"))
      (let [int-a (inc pos)
            int-b (inc int-a)
            res-pos (inc int-b)
            next-pos (inc res-pos)
            [res-mode b-mode a-mode] (split-op op-str)]
        (recur
          (assoc 
            intcode
            (intcode-pos intcode res-pos res-mode)
            ((if (= op "01") + *) 
              (intcode-value intcode int-a a-mode)
              (intcode-value intcode int-b b-mode)))
          next-pos
          inputs
          outputs))
          
      (= op "03")
      (let [res-pos (inc pos)
            next-pos (inc res-pos)
            [_ _ res-mode] (split-op op-str)]
        (recur
          (assoc 
            intcode 
            (intcode-pos intcode res-pos res-mode)
            (first inputs))
          next-pos
          (drop 1 inputs)
          outputs))
          
      (= op "04")
      (let [res-pos (inc pos)
            next-pos (inc res-pos)
            [_ _ res-mode] (split-op op-str)]
        (recur
          intcode
          next-pos
          inputs
          (conj outputs (intcode-value intcode res-pos res-mode))))
          
      (or (= op "05") (= op "06"))
      (let [int-pos (inc pos)
            res-pos (inc int-pos)
            [_ res-mode int-mode] (split-op op-str)
            int-val (intcode-value intcode int-pos int-mode)
            res-val (intcode-value intcode res-pos res-mode) 
            next-pos (if ((if (= op "05") not= =) int-val 0) 
                        res-val
                        (inc res-pos))]
        (recur
          intcode
          next-pos
          inputs
          outputs))
          
      (or (= op "07") (= op "08"))
      (let [int-a (inc pos)
            int-b (inc int-a)
            res-pos (inc int-b)
            next-pos (inc res-pos)
            [res-mode b-mode a-mode] (split-op op-str)]
        (recur
          (assoc 
            intcode
            (intcode-pos intcode res-pos res-mode)
            (if ((if (= op "07") < =) 
                  (intcode-value intcode int-a a-mode)
                  (intcode-value intcode int-b b-mode))
              1 0))
          next-pos
          inputs
          outputs))
          
      :else
      outputs)))

(defn parse-input [inputs] (vec (map #(Integer/parseInt %) (str/split (first inputs) #","))))

(defn calculate-outcome [intcode signals]
  (reduce
    (fn [acc signal]
      (first (read-intcode intcode 0 [signal acc] [])))
    0
    signals))

(defn run
  [inputs]
  (let [intcode (parse-input inputs)
        signals-permutation (for [a (range 5) 
                                  b (range 5)
                                  c (range 5)
                                  d (range 5)
                                  e (range 5)
                                  :when (= (count (set [a b c d e])) 5)] 
                              [a b c d e])]
   (apply max (map #(calculate-outcome intcode %) signals-permutation))))

