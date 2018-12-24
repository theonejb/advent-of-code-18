(ns aoc18.day22
  (:require [loom.graph :as g]
            [loom.alg :as alg]
            [clojure.string :as str])
  (:import (java.util.concurrent TimeUnit)))

(defn region-info->string [ri]
  (format "(%d, %d) %s" (get-in ri [:coord :x]) (get-in ri [:coord :y]) (:type ri)))

(defrecord RegionInfo [coord geological-index erosion-level type risk-level])

(defn build-region-info [coord gi cave-depth]
  (let [el (mod (+ gi cave-depth) 20183)
        rl (mod el 3)
        rt (condp = rl
             0 :rocky
             1 :wet
             2 :narrow)]
    (->RegionInfo coord gi el rt rl)))

(defn get-coords-for-width-and-height [width height]
  (for [y (range width)
        x (range height)]
    {:x x :y y}))

(defn get-geological-index [region-map {:keys [x y]}]
  (let [neighbours [{:x (dec x) :y y} {:x x :y (dec y)}]
        regions (map region-map neighbours)
        erosion-levels (map :erosion-level regions)
        gi (apply * erosion-levels)]
    gi))

(defn new-region-map [target cave-depth width height]
  (let [coords (get-coords-for-width-and-height width height)]
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

(defn print-map [region-map interest-point interest-point-char]
  (let [{:keys [x-max y-max]} (get-map-bounds region-map)]
    (dorun (for [y (range (inc y-max))
                 x (range (inc x-max))]
             (let [coord {:x x :y y}
                   region-char (if (= coord interest-point) interest-point-char (char-for-region (region-map coord)))]
               (if (and (not (= 0 y)) (= 0 x)) (println))
               (print region-char))))
    (println)))

(defn print-map-raw [rm]
  (map region-info->string (sort-by #(get-in %1 [:coord :y]) (sort-by #(get-in %1 [:coord :x]) (vals rm)))))

(defn get-neighbours [{:keys [x y]}]
  (filter
    #(and (>= (:x %1) 0) (>= (:y %1) 0))
    [{:x (dec x) :y y}
     {:x x :y (dec y)}
     {:x (inc x) :y y}
     {:x x :y (inc y)}]
    ))

(def valid-tools
  {:rocky  [:climbing :torch]
   :wet    [:climbing :neither]
   :narrow [:torch :neither]})

(defn region->node [{:keys [coord]}]
  {:x (:x coord) :y (:y coord)})

(defn region->node [{:keys [coord]}]
  (str (:x coord) "," (:y coord)))

(defn region+tool->node [region tool]
  (str (region->node region) " " tool))

(defn get-tool-transitions [region]
  (let [[t1 t2] (valid-tools (:type region))]
    [[(region+tool->node region t1) (region+tool->node region t2) 7]
     [(region+tool->node region t2) (region+tool->node region t1) 7]]))

(defn get-common-tools [{r1-type :type} {r2-type :type}]
  (let [r1-tools (valid-tools r1-type)
        r2-tools (valid-tools r2-type)

        common-tools (clojure.set/intersection (set r1-tools) (set r2-tools))]
    common-tools))

(defn get-region-edges [r1 r2]
  (let [common-tools (get-common-tools r1 r2)]
    (for [tool common-tools]
      [(region+tool->node r1 tool)
       (region+tool->node r2 tool)
       1])))

(defn rm->edges [rm]
  (apply concat (for [[coord region] rm]
                  (let [tool-transitions (get-tool-transitions region)
                        neighbour-coords (get-neighbours coord)
                        neighbour-regions (map rm neighbour-coords)
                        neighbour-edges (reduce (fn [edges neighbour]
                                                  (into edges (get-region-edges region neighbour)))
                                                [] neighbour-regions)

                        all-edges (concat tool-transitions neighbour-edges)]
                    all-edges))))

(defn rm->graph [rm]
  (apply g/weighted-digraph (rm->edges rm)))Ò