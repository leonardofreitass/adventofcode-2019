(ns adventofcode-2019.exercises.day-1.part-2)

(defn calculate-fuel [mass]
  (max 
    (+
      (- 
        (Math/floor (/
                      mass
                      3))
        2))
    0))

(defn calculate-massed-fuel [mass]
  (let [fuel (calculate-fuel mass)]
    (if (= fuel 0)
      fuel
      (+ fuel (calculate-massed-fuel fuel)))))

(defn run
  [inputs]
  (reduce 
    (fn [a b]
      (+
        a 
        (calculate-massed-fuel (Integer/parseInt b))))
    0
    inputs))
