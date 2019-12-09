(ns adventofcode-2019.exercises.day-6.part-1
  (:require [clojure.string :as str]))

(defn setup-orbits [orbits map]
  (if (> (count map) 0)
    (let [relation (first map)
        [target obj] (str/split relation #"\)")]
      (recur
        (assoc
          orbits
          (keyword obj)
          {:orbits (keyword target)})
        (drop 1 map)))
    orbits))

(defn count-orbits [orbits]
  (reduce
    (fn [acc [_ orbit]]
      (if (:orbits orbit)
        (let [next-orbit ((:orbits orbit) orbits)]
          (recur (inc acc) [(:orbits orbit) next-orbit]))
        acc))
    0
    orbits))

(defn run
  [inputs]
  (let [orbits (setup-orbits {:COM {:orbits nil}} inputs)]
    (count-orbits orbits)))
