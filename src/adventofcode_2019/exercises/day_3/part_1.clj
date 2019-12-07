(ns adventofcode-2019.exercises.day-3.part-1
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn walk [trace distance]
  (let [[_ [x y]] (last trace)
        dir (subs distance 0 1)
        steps (Integer/parseInt (subs distance 1))]
    (conj
      trace
      (cond
        (= dir "R")
        [[x y] [(+ x steps) y]]
        
        (= dir "L")
        [[x y] [(- x steps) y]]

        (= dir "U")
        [[x y] [x (+ y steps)]]

        (= dir "D")
        [[x y] [x (- y steps)]]))))

(defn seg-axis [[[xa ya] [xb _]]]
  (if (= xa xb)
    {:axis "Y" :val xa}
    {:axis "X" :val ya}))

(defn intersects? [[[xa ya] [xb yb]] axis]
  (if (= (:axis axis) "Y")
    (or (<= xa (:val axis) xb) (>= xa (:val axis) xb))
    (or (<= ya (:val axis) yb) (>= ya (:val axis) yb))))

(defn wire-intersection [seg-a seg-b]
  (let [axis-a (seg-axis seg-a)
        axis-b (seg-axis seg-b)]
    (cond
      (= (:axis axis-a) (:axis axis-b))
      false
      
      (and (intersects? seg-a axis-b) (intersects? seg-b axis-a))
      (if (= (:axis axis-a) "Y")
        [(:val axis-a) (:val axis-b)]
        [(:val axis-b) (:val axis-a)])
      
      :else
      false)))

(defn wire-intersections [traces]
  (set (for [seg-a (first traces)
            seg-b (last traces)
            :let [intersection (wire-intersection seg-a seg-b)]
            :when (and (not= intersection false) (not= intersection [0 0]))]
          intersection)))

(defn setup-wires [wires]
  (for [wire wires]
    (loop [trace [[[0 0] [0 0]]]
           distances (str/split wire #",")]
      (if (empty? distances)
        (drop 1 trace)
        (recur
          (walk trace (first distances))
          (drop 1 distances))))))

(defn point-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn min-central-dist [intersections]
  (apply min (map point-distance intersections)))


(defn run
  [inputs]
  (min-central-dist (wire-intersections (setup-wires inputs))))

