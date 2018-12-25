(ns aoc18.day24
  (:require [clojure.string :as str]))

(defrecord Group [team id units hit-points attack-damage attack-type initiative
                  weaknesses immunities])

(defn get-effective-power [{:keys [units hit-points]}]
  (* units hit-points))

(defn sort-groups-for-targeting [groups]
  (sort-by get-effective-power (sort-by :initiative groups)))

(defn sort-groups-for-attack [groups]
  (sort-by :initiative groups))

(defn damage-multiplier [attacker defender]
  (cond
    (contains? (:immunities defender) (:attack-type attacker)) 0
    (contains? (:weaknesses defender) (:attack-type attacker)) 2
    :default 1))

(defn damage-given [attacker defender]
  (let [base-damange (get-effective-power attacker)
        multipler (damage-multiplier attacker defender)]
    (* multipler base-damange)))

(defn take-damage [{:keys [units hit-points] :as defender} total-damage]
  (let [units-killed (int (/ total-damage hit-points))]
    (assoc defender :units (max 0 (- units units-killed)))))

(defn add-damage-to-groups [attacker groups]
  (map #(assoc %1 :damage (damage-given attacker %1)) groups))

(defn select-target [attacker groups]
  (let [possible-targets (remove #(= (:team attacker) (:team %1)) groups)
        _ (prn possible-targets)
        targets-with-damage-info (add-damage-to-groups attacker possible-targets)
        sorted-targets (sort-by get-effective-power (sort-by :initiative targets-with-damage-info))]
    (first sorted-targets)))

(def game [(->Group :immune 1 17 5390 4507 :fire 2  [:radiation :bludgeoning] [])
           (->Group :immune 2 989 1274 25 :slashing 3 [:bludgeoning :slashing] [:fire])

           (->Group :infection 1 801 4706 116 :bludgeoning 1 [:radiation] [])
           (->Group :infection 2 4485 2961 12 :slashing 4  [:fire :cold] [:radiation])])