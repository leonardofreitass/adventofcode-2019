(ns adventofcode-2019.exercises.day-2.part-1
  (:require [clojure.string :as str]))

(defn intcode-pos [intcode pos]
  (nth intcode (nth intcode pos)))

(defn read-intcode [intcode pos]
  (let [op (nth intcode pos)]
    (if (= op 99)
      intcode
      (let [int-a (inc pos)
            int-b (inc int-a)
            res-pos (inc int-b)
            next-pos (inc res-pos)]
        (recur
          (assoc 
            intcode 
            (nth intcode res-pos)
            ((cond 
                (= op 1) +
                (= op 2) *) 
              (intcode-pos intcode int-a)
              (intcode-pos intcode int-b)))
          next-pos)))))

(defn prepare [intcode]
  (assoc
    intcode 
    1 12 
    2 2))

(defn parse-input [inputs] (vec (map #(Integer/parseInt %) (str/split (first inputs) #","))))

(defn run
  [inputs]
  (let [intcode (parse-input inputs)]
    (str/join "," (read-intcode (prepare intcode) 0))))

