(ns adventofcode-2019.exercises.day-4.part-1
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn check-password [password]
  (reduce 
    (fn [{:keys [last increases pair]} digit]
      (let [n (Integer/parseInt digit)]
        {:last n 
         :increases (and increases (>= n last))
         :pair (or pair (= n last))}))
    {:last -1 :increases true :pair false}
    (str/split (str password) #"")))

(defn valid? [password]
  (let [{:keys [increases pair]} (check-password password)]
    (and increases pair)))

(defn project-passwords [m M]
  (for [password (range m M)
        :let [valid-password (valid? password)]
        :when valid-password]
    password))

(defn run
  [inputs]
  (count (apply project-passwords (map #(Integer/parseInt %) (str/split (first inputs) #"-")))))

