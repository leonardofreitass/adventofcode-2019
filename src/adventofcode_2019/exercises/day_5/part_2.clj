(ns adventofcode-2019.exercises.day-5.part-2
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
      intcode
      
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
        (println (intcode-value intcode res-pos res-mode))
        (recur
          intcode
          next-pos
          inputs))
          
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

(defn run
  [inputs]
  (let [intcode (parse-input inputs)]
    (read-intcode intcode 0 [5])))

