(ns aoc18.day20
  (:require [clojure.string :as str]))

(def test-input "ENWWW(NEEE|SSE(EE|N))NE")

(def is-direction? #{\N \E \S \W})
(defn parse-regex
  ([regex] (parse-regex regex []))
  ([regex parsed]
   (cond
     (empty? regex) [regex parsed]
     (= \| (first regex)) (recur (rest regex) (conj parsed :or))
     (= \( (first regex)) (let [[remaining-regex parsed-child] (parse-regex (rest regex))]
                            (recur remaining-regex (conj parsed parsed-child)))
     (= \) (first regex)) [(rest regex) parsed]
     (is-direction? (first regex)) (let [next-atom (str/join (take-while is-direction? regex))
                                         remaining-regex (drop-while is-direction? regex)
                                         parsed (conj parsed (list next-atom))]
                                     (recur remaining-regex parsed)))))

(def direction->coord-change
  {\N (fn [{:keys [y] :as coord}]
        (assoc coord :y (inc y)))

   \E (fn [{:keys [x] :as coord}]
        (assoc coord :x (inc x)))

   \S (fn [{:keys [y] :as coord}]
        (assoc coord :y (dec y)))

   \W (fn [{:keys [x] :as coord}]
        (assoc coord :x (dec x)))})

(defn get-coords-for-moves [movements starting-pos]
  (:moves (reduce (fn [{:keys [moves current-pos]} moving-to]
                    (let [next-pos ((direction->coord-change moving-to) current-pos)]
                      {:moves (conj moves next-pos) :current-pos next-pos})) {:moves [] :current-pos starting-pos} movements)))