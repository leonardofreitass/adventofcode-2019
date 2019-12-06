(ns adventofcode-2019.exercises.day-1)

(defn calculate-fuel [mass]
  (- 
    (Math/floor (/
                  mass
                  3))
    2))

(defn run
  "Day one exercise"
  [inputs]
  (reduce 
    (fn [a b] 
      (+
        a 
        (calculate-fuel (Integer/parseInt b))))
    0
    inputs))