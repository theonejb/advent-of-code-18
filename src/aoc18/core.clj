(ns aoc18.core
  (:gen-class)
  (:require aoc18.day5))

(defn -main [& args]
  (println (aoc18.day5/complete-reaction-3 (clojure.string/trim (slurp "day5.txt")))))