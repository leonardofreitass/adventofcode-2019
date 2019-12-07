(ns adventofcode-2019.exercises.day-3.part-1
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn walk [trace distance]
  (let [[x y] (last trace)
        dir (subs distance 0 1)
        steps (Integer/parseInt (subs distance 1))]
    (if (zero? steps)
      trace
      (recur
        (conj
          trace
          (cond
            (= dir "R")
            [(inc x) y]
            
            (= dir "L")
            [(dec x) y]

            (= dir "U")
            [x (inc y)]

            (=   dir "D")
            [x (dec y)]))
        (str dir (dec steps))))))

(defn setup-cables [cables]
  (for [cable cables]
    (loop [trace [[0 0]]
           distances (str/split cable #",")]
      (println (count trace))
      (if (empty? distances)
        (set (drop 1 trace))
        (recur
          (walk trace (first distances))
          (drop 1 distances))))))

(defn point-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn run
  [inputs]
  (apply min (map point-distance (apply set/intersection (setup-cables inputs)))))

