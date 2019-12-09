(ns adventofcode-2019.exercises.day-6.part-2
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn setup-orbits [orbits map]
  (if (empty? map)
    orbits
    (let [relation (first map)
        [target obj] (str/split relation #"\)")]
      (recur
        (assoc
          orbits
          (keyword obj)
          {:orbits (keyword target) :orbited-by []})
        (drop 1 map)))))


(defn setup-orbited-by [orbits map]
  (if (empty? map)
    orbits
    (let [relation (first map)
          [target obj] (str/split relation #"\)")
          target-key (keyword target)
          obj-key (keyword obj)
          target-hm (target-key orbits)]
      (recur
        (assoc
          orbits
          target-key
          (assoc
            target-hm 
            :orbited-by 
            (conj (:orbited-by target-hm) obj-key)))
        (drop 1 map)))))

(defn find-target [target orbits trace tree]
  (let [[[key node] distance] (first tree)
        next (set/difference 
                (conj 
                    (set 
                      (:orbited-by node)) 
                    (:orbits node)) 
                trace)]
    (if (= key target)
      (dec distance)
      (recur 
        target
        orbits
        (conj trace key)
        (if (empty? next)
          (drop 1 tree)
          (apply conj 
            (drop 1 tree)
            (map #(vector [% (% orbits)] (inc distance)) next)))))))

(defn run
  [inputs]
  (let [orbits (setup-orbited-by 
                  (setup-orbits 
                    {:COM {:orbits nil :orbited-by []}} 
                    inputs) 
                  inputs)
        orbited-by-you (:orbits (:YOU orbits))]
    (find-target :SAN orbits #{:YOU nil} [[[orbited-by-you (orbited-by-you orbits)] 0]])))
