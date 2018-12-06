(ns aoc18.day1
  (:require [clojure.string :refer [split-lines]]))

(def day1-input (split-lines (slurp "day1.txt")))
(def day1-parsed-input (map #(Integer/parseInt %1) day1-input))
(def day1-output (reduce + day1-parsed-input))

(defn day1-find-first-repeating-frequency [input]
  (loop [seen-frequencies #{0}
         current-frequency 0
         input-seq (cycle input)]
    (let [new-frequency (+ current-frequency (first input-seq))]
      (if (contains? seen-frequencies new-frequency)
        new-frequency
        (recur (conj seen-frequencies new-frequency) new-frequency (rest input-seq))))))

