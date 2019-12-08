(ns adventofcode-2019.exercises.day-3.part-2
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn walk [trace distance]
  (let [[_ [x y] _ length] (last trace)
        last-pos [x y]
        dir (subs distance 0 1)
        steps (Integer/parseInt (subs distance 1))
        new-length (+ length steps)]
    (conj
      trace
      (cond
        (= dir "R")
        [last-pos [(+ x steps) y] length new-length]
        
        (= dir "L")
        [last-pos [(- x steps) y] length new-length]

        (= dir "U")
        [last-pos [x (+ y steps)] length new-length]

        (= dir "D")
        [last-pos [x (- y steps)] length new-length]))))

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
        axis-b (seg-axis seg-b)
        [[xa ya]] seg-a
        [[xb yb]] seg-b
        length (+ (nth seg-a 2) (nth seg-b 2))]
    (cond
      (= (:axis axis-a) (:axis axis-b))
      false
      
      (and (intersects? seg-a axis-b) (intersects? seg-b axis-a))
      (if (= (:axis axis-a) "Y")
        [(:val axis-a) (:val axis-b) (+ 
                                        length 
                                        (Math/abs (- (:val axis-b) ya))
                                        (Math/abs (- (:val axis-a) xb)))]
        [(:val axis-b) (:val axis-a) (+ 
                                        length 
                                        (Math/abs (- (:val axis-b) xa))
                                        (Math/abs (- (:val axis-a) yb)))])
      
      :else
      false)))

(defn wire-intersections [traces]
  (set (for [seg-a (first traces)
            seg-b (last traces)
            :let [intersection (wire-intersection seg-a seg-b)]
            :when (and (not= intersection false) (not= intersection [0 0 0]))]
          intersection)))

(defn setup-wires [wires]
  (for [wire wires]
    (loop [trace [[[0 0] [0 0] 0 0]]
           distances (str/split wire #",")]
      (if (empty? distances)
        (drop 1 trace)
        (recur
          (walk trace (first distances))
          (drop 1 distances))))))

(defn min-central-dist [intersections]
  (apply min (map #(nth % 2) intersections)))


(defn run
  [inputs]
  (min-central-dist (wire-intersections (setup-wires inputs))))

