(ns adventofcode-2019.exercises.day-7.part-2
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

(defn read-intcode [intcode pos inputs]
  (let [op-str (format-op (nth intcode pos))
        op (subs op-str (- (count op-str) 2))]
    (cond
      (= op "99")
      [nil nil inputs (first inputs)]
      
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
          inputs))
          
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
          (drop 1 inputs)))
          
      (= op "04")
      (let [res-pos (inc pos)
            next-pos (inc res-pos)
            [_ _ res-mode] (split-op op-str)]
        [intcode next-pos inputs (intcode-value intcode res-pos res-mode)])
          
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
          inputs))
          
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
          inputs)))))

(defn parse-input [inputs] (vec (map #(Integer/parseInt %) (str/split (first inputs) #","))))

(defn calculate-outcome [signals amp input]
  (let [next-amp (rem (inc amp) 5)
        [intcode pos inputs] (nth signals amp)
        [new-intcode new-pos new-inputs output] (read-intcode intcode pos (conj inputs input))]
    (if (and (= amp 4) (nil? new-intcode))
      output
      (recur
        (assoc
          signals
          amp
          [new-intcode new-pos new-inputs])
        next-amp
        output))))

; I am not proud of the code bellow, but lets move on =D
(defn run
  [inputs]
  (let [intcode (parse-input inputs)
        signals-permutation (for [a (range 5 10) 
                                  b (range 5 10)
                                  c (range 5 10)
                                  d (range 5 10)
                                  e (range 5 10)
                                  :when (= (count (set [a b c d e])) 5)] 
                              [[(vec intcode) 0 [a]]
                               [(vec intcode) 0 [b]]
                               [(vec intcode) 0 [c]]
                               [(vec intcode) 0 [d]]
                               [(vec intcode) 0 [e]]])]
   (apply max (map #(calculate-outcome % 0 0) signals-permutation))))

