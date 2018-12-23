(ns aoc18.day22
  (:require [loom.graph :as g]
            [loom.alg :as alg]))

(defrecord RegionInfo [coord geological-index erosion-level type risk-level])

(defn build-region-info [coord gi cave-depth]
  (let [el (mod (+ gi cave-depth) 20183)
        rl (mod el 3)
        rt (condp = rl
             0 :rocky
             1 :wet
             2 :narrow)]
    (->RegionInfo coord gi el rt rl)))

(defn get-coords-enclosing-target [{:keys [x y]}]
  (for [y (range (inc y))
        x (range (inc x))]
    {:x x :y y}))

(defn get-geological-index [region-map {:keys [x y]}]
  (let [neighbours [{:x (dec x) :y y} {:x x :y (dec y)}]
        regions (map region-map neighbours)
        erosion-levels (map :erosion-level regions)
        gi (apply * erosion-levels)]
    gi))

(defn new-region-map [target cave-depth]
  (let [coords (get-coords-enclosing-target target)]
    (reduce (fn [region-map {:keys [x y] :as coord}]
              (assoc region-map coord (cond
                                        (and (= 0 x) (= 0 y))
                                        (build-region-info coord 0 cave-depth)

                                        (= 0 y)
                                        (build-region-info coord (* x 16807) cave-depth)

                                        (= 0 x)
                                        (build-region-info coord (* y 48271) cave-depth)

                                        (= coord target)
                                        (build-region-info coord 0 cave-depth)

                                        :default
                                        (build-region-info coord (get-geological-index region-map coord) cave-depth))))
            {} coords)))

(defn get-map-bounds [regions-map]
  (let [coords (map :coord (vals regions-map))
        x-max (:x (apply max-key :x coords))
        y-max (:y (apply max-key :y coords))]
    {:x-max x-max :y-max y-max}))

(defn char-for-region [region]
  (condp = (:type region)
    :rocky \.
    :wet \=
    :narrow \|))

(defn print-map [region-map]
  (let [{:keys [x-max y-max]} (get-map-bounds region-map)]
    (dorun (for [y (range (inc y-max))
                 x (range (inc x-max))]
             (let [region-char (char-for-region (region-map {:x x :y y}))]
               (if (and (not (= 0 y)) (= 0 x)) (println))
               (print region-char))))
    (println)))

(defn get-neighbours [{:keys [x y]}]
  (filter
    #(and (>= (:x %1) 0) (>= (:y %1) 0))
    [{:x (dec x) :y y}
     {:x x :y (dec y)}
     {:x (inc x) :y y}
     {:x x :y (inc y)}]
    ))

(defn rm->graph [rm]
  (reduce (fn [g [coord region]]) (g/weighted-digraph) rm)