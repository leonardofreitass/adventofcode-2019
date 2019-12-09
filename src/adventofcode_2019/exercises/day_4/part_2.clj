(ns adventofcode-2019.exercises.day-4.part-2
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn check-password [password]
  (reduce 
    (fn [{:keys [last increases count]} digit]
      (let [n (Integer/parseInt digit)
            c (nth count n)]
        {:last n 
         :increases (and increases (>= n last))
         :count (assoc count n (inc c))}))
    {:last -1 :increases true :count (vec (replicate 10 0))}
    (str/split (str password) #"")))

(defn valid? [password]
  (let [{:keys [increases count]} (check-password password)]
    (and increases (some #(= 2 %) count))))

(defn project-passwords [m M]
  (for [password (range m M)
        :let [valid-password (valid? password)]
        :when valid-password]
    password))

(defn run
  [inputs]
  (count (apply project-passwords (map #(Integer/parseInt %) (str/split (first inputs) #"-")))))

