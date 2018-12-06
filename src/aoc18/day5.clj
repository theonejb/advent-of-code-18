(ns aoc18.day5
  (:require [clojure.string :refer [lower-case upper-case]]))

(defn will-pair-react? [a b]
  (and
    (and a b)
    (= (lower-case (str a)) (lower-case (str b)))
    (not (= a b))))

(defn react [polymer unit]
  (if (will-pair-react? (last polymer) unit)
    (vec (drop-last polymer))
    (conj polymer unit)))

(defn trigger-reaction [polymer]
  (reduce react [] polymer))

(defn trigger-reactions-with-skips [polymer]
  (into {} (map (fn [[k v]]
                  [k (apply str v)])
                (reduce (fn [polymers-map unit]
                          (println (count (:default polymers-map)))
                          (let [default-polymer (get polymers-map :default [])
                                new-default-polymer (react default-polymer unit)

                                this-unit-key (lower-case (str unit))
                                unit-polymer (get polymers-map this-unit-key default-polymer)
                                new-polymers-map (assoc polymers-map :default new-default-polymer this-unit-key unit-polymer)]
                            (reduce (fn [polymers-map unit-key]
                                      (let [this-unit-key (lower-case (str unit-key))
                                            unit-polymer (get polymers-map this-unit-key)]
                                        (assoc polymers-map (lower-case (str unit-key)) (react unit-polymer unit))))
                                    new-polymers-map
                                    (remove #{:default this-unit-key} (keys new-polymers-map)))))
                        {} polymer))))

(defn complete-reaction [polymer]
  (println (count (trigger-reaction polymer))))

(defn get-upper-and-lowercase-unit [unit]
  (let [lcase (first (lower-case unit))                     ; first to convert string into a char
        ucase (first (upper-case lcase))]
    [lcase ucase]))

(defn complete-reaction-3 [polymer]
  (let [unit-types (set (map lower-case (set polymer)))]
    (println "Unit types: " unit-types)
    (map (fn [unit-type-to-remove]
           (let [unit-removal-pred (set (get-upper-and-lowercase-unit unit-type-to-remove))
                 new-polymer (remove unit-removal-pred polymer)
                 reacted-polymer (trigger-reaction new-polymer)
                 reacted-polymer-length (count reacted-polymer)]
             (println "Removal set: " unit-removal-pred " New length: " reacted-polymer-length)
             [unit-type-to-remove reacted-polymer-length])) unit-types)))