(ns adventofcode-2019.exercises.day-1)

(defn calculate-fuel [mass]
  (- (Math/ceil (/ mass 3))
     2))

(defn run
  "Day one exercise"
  [inputs]
  (reduce (fn [a b] (+ a (calculate-fuel b))) 0))
