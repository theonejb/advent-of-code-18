(ns aoc18.day24
  (:require [clojure.string :as str]))

(defrecord Group [team id units hit-points attack-damage attack-type initiative
                  weaknesses immunities])

(defn group->name [{:keys [team id]}]
  (str (if (= team :infection) "Infection" "Immune") " " id))

(defn get-effective-power [{:keys [units attack-damage]}]
  (* units attack-damage))

(defn sort-groups-for-targeting [groups]
  (sort-by get-effective-power > (sort-by :initiative > groups)))

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
        targets-with-damage-info (add-damage-to-groups attacker possible-targets)
        sorted-targets (sort-by :damage (sort-by get-effective-power > (sort-by :initiative > targets-with-damage-info)))]
    (first sorted-targets)))

(defn remove-already-selected-targets [attacker-target-pairs groups]
  (let [selected-group-ids (set (map :id (map :target attacker-target-pairs)))]
    (reduce (fn [filtered-groups group]
              (if (contains? selected-group-ids (:id group))
                filtered-groups
                (conj filtered-groups group))) [] groups)))

(defn map-targets [groups]
  (let [groups (sort-groups-for-targeting groups)]
    (loop [attacker (first groups)
           attacker-target-pairs []]
      (let [groups-without-attacker (remove (partial = attacker) groups)
            non-attacked-groups (remove-already-selected-targets attacker-target-pairs groups-without-attacker)
            target (select-target attacker non-attacked-groups)
            attacker-target-pairs (conj attacker-target-pairs {:attacker attacker :target target})]
        (if (= (count attacker-target-pairs) (count groups))
          attacker-target-pairs
          (recur (nth groups (count attacker-target-pairs)) attacker-target-pairs))))))

(def game [(->Group :immune :is1 17 5390 4507 :fire 2 #{:radiation :bludgeoning} #{})
           (->Group :immune :is2 989 1274 25 :slashing 3 #{:bludgeoning :slashing} #{:fire})

           (->Group :infection :i1 801 4706 116 :bludgeoning 1 #{:radiation} #{})
           (->Group :infection :i2 4485 2961 12 :slashing 4 #{:fire :cold} #{:radiation})])

(defn print-attack-plan [plan]
  (dorun (for [{:keys [attacker target]} plan]
           (println (group->name attacker) "will attack"
                    (group->name target)))))