(ns adventofcode-2019.exercises)

(defn exercise-to-ns
  "Gets namespace from exercise"
  [exercise]
  (symbol (str "adventofcode-2019.exercises.day-" exercise)))

(defn exercise-to-input-file
  "Gets input file path from exercise"
  [exercise]
  (str "./resources/day_" exercise "_inputs"))

(defn execute 
  "Handler for executing exercises"
  [exercise]
  (println "Executing exercise from day" exercise)
  (require (exercise-to-ns exercise))
  (let [exercise-ns (find-ns (exercise-to-ns exercise))]
    (with-open [input-file (clojure.java.io/reader (exercise-to-input-file exercise))]
      (println "Solution:")
      (println ((ns-resolve exercise-ns 'run) (line-seq input-file))))))