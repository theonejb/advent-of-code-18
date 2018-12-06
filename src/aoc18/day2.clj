(ns aoc18.day2
  (:require [clojure.string :refer [split-lines join]]
            [clojure.math.combinatorics :refer [combinations]]))

(def input (split-lines (slurp "day2.txt")))

(defn get-counter [input]
  (reduce (fn [counter next-val]
            (assoc counter next-val (inc (counter next-val 0))))
          {} input))
(defn is-interesting-id? [box-id]
  (let [counter (get-counter box-id)]
    (set (filter #(or (= 2 %1) (= 3 %1)) (vals counter)))))

(def interesting-bits (map is-interesting-id? input))
(def interesting-bits-counter (get-counter (flatten (map seq interesting-bits))))
(def output (* (interesting-bits-counter 2 0) (interesting-bits-counter 3 0)))

(defn are-matching? [id1 id2]
  (= 1 (count (filter #(not (= (first %1) (second %1))) (map vector id1 id2)))))
(defn matching-ids [input]
  (let [pairs (combinations input 2)]
    (first (filter (fn [[id1 id2]] (are-matching? id1 id2)) pairs))))
(defn get-common-parts-in-ids [[id1 id2]]
  (join (map first (filter #(= (first %1) (second %1)) (map vector id1 id2)))))