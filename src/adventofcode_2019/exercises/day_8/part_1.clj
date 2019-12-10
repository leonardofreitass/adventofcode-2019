(ns adventofcode-2019.exercises.day-8.part-1
  (:require [clojure.string :as str]))

(defn split-layers [decoded tall]
  (loop [layers []
         left decoded
         i 0]
    (if (empty? left)
      layers
      (recur
        (assoc
          layers
          (quot i tall)
          (conj 
            (nth 
              layers 
              (quot i tall)
              []) 
            (first left)))
        (drop 1 left)
        (inc i)))))

(defn decode [encoded width tall]
  (split-layers
    (re-seq (re-pattern (str "\\d{" width "}")) encoded)
    tall))

(defn run
  [inputs]
  (decode (first inputs) 3 2))

