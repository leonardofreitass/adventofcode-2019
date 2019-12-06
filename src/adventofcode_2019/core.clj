(ns adventofcode-2019.core
  (:require [adventofcode-2019.exercises :as exercises])
  (:gen-class))

(defn -main
  "Runs an advent of code exercise"
  [& [exercise]]
  (exercises/execute exercise))
