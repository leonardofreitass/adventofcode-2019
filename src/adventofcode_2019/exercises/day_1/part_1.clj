(ns adventofcode-2019.exercises.day-1.part-1)

(defn calculate-fuel [mass]
  (- 
    (Math/floor (/
                  mass
                  3))
    2))

(defn run
  [inputs]
  (reduce 
    (fn [a b] 
      (+
        a 
        (calculate-fuel (Integer/parseInt b))))
    0
    inputs))
