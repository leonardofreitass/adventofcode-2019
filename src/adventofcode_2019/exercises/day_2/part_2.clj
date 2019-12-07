(ns adventofcode-2019.exercises.day-2.part-2
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

(defn prepare [intcode noun verb]
  (assoc 
    intcode 
    1 noun 
    2 verb))

(defn find-target [intcode target]
  (some
    (fn [[noun verb]]
      (let [result (read-intcode (prepare intcode noun verb) 0)]
        (when
          (= target (first result))
          [noun verb])))
    (for [x (range 100) y (range 100)] [x y])))

(defn parse-input [inputs] (vec (map #(Integer/parseInt %) (str/split (first inputs) #","))))

(defn run
  [inputs]
  (let [intcode (parse-input inputs)
        [noun verb] (find-target intcode 19690720)]
    (+ (* 100 noun) verb)))

