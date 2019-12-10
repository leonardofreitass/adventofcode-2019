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

(defn count-in-layer [layer digit]
  (reduce
    #(+ %1 (count (re-seq (re-pattern (str digit)) %2)))
    0
    layer))

(defn find-by-min [layers digit]
  (loop [[max-count max-index max-layer] [##Inf nil nil]
         index 0
         left layers]
    (if (empty? left)
      [max-index max-layer]
      (let [layer (first left)
            layer-count (count-in-layer layer digit)]
        (recur
          (if (< layer-count max-count)
            [layer-count index layer]
            [max-count max-index max-layer])
          (inc index)
          (drop 1 left))))))

(defn run
  [inputs]
  (let [layers (decode (first inputs) 25 6)
        [_ layer] (find-by-min layers 0)]
    (* (count-in-layer layer 1) (count-in-layer layer 2))))

