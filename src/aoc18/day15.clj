(ns aoc18.day15
  (:require [clojure.string :as str]
            [clojure.set :refer [map-invert]]))

(def infinity 9999999)

(defn fname->rows [fname]
  (str/split-lines (slurp fname)))

(def input-point->grid-point
  {\# :wall
   \. :empty
   \E :elf
   \G :goblin})
(def grid-point->print-repr
  (map-invert input-point->grid-point))

(defn row->points [row]
  (vec (map input-point->grid-point row)))

(defn fname->grid [fname]
  (->> fname
       fname->rows
       (map row->points)
       vec))

(defn get-grid-coords [grid]
  (let [x-max (count (first grid))
        y-max (count grid)]
    (for [y (range y-max)
          x (range x-max)]
      {:x x :y y})))

(defn print-grid [grid]
  (let [coords (get-grid-coords grid)]
    (dorun (for [{:keys [x y]} coords]
             (do
               (if (and (= 0 x) (not (= 0 y)))
                 (println))
               (print (grid-point->print-repr (get-in grid [y x])))))))
  (println))

(defn grid->players [grid]
  (let [coords (get-grid-coords grid)]
    (reduce (fn [players {:keys [x y]}]
              (let [point (get-in grid [y x])]
                (condp = point
                  :elf (conj players {:elf true :x x :y y})
                  :goblin (conj players {:goblin true :x x :y y})
                  players)))
            []
            coords)))

(defn grid->game-state [grid]
  (let [players (grid->players grid)]
    {:players players
     :grid    (reduce (fn [grid {:keys [x y]}]
                        (assoc-in grid [y x] :empty)) grid players)}))

(defn is-coord-of-player? [players]
  (fn [{:keys [x y]}]
    (not-empty (first (filter #(and (= x (:x %1)) (= y (:y %1))) players)))))

(defn dj--grid->initial-distance-map [grid {source-x :x source-y :y}]
  (reduce (fn [grid {:keys [x y]}]
            (if (and (= x source-x) (= y source-y))
              (assoc-in grid [y x] 0)
              (assoc-in grid [y x] infinity)))
          grid (get-grid-coords grid)))

(defn dj--filter-valid-coords [{:keys [grid players]} coords]
  "Given a list of coordinates, returns only those which are possible spaces for movement"
  (filter (fn [{:keys [x y]}] (= :empty (get-in grid [y x]))) coords))

(defn dj--grid->queue [{:keys [grid players] :as game}]
  (remove (is-coord-of-player? players) (dj--filter-valid-coords game (get-grid-coords grid))))

(defn dj--queue->min-distance-vertex [distance-map queue]
  (apply min-key (fn [{:keys [x y]}]
                   (get-in distance-map [y x])) queue))

(defn dj--coord->neighbours [{:keys [x y]}]
  [{:x x :y (dec y)}
   {:x (inc x) :y y}
   {:x x :y (inc y)}
   {:x (dec x) :y y}])

(defn fname->game [fname]
  (grid->game-state (fname->grid fname)))

(defn dj--build-distance-map [{:keys [grid] :as game} source-coord]
  (let [distance-map (dj--grid->initial-distance-map grid source-coord)
        queue (conj (dj--grid->queue game) source-coord)]
    (loop [q queue
           dm distance-map]
      (if (empty? q)
        dm
        (let [v (dj--queue->min-distance-vertex dm q)
              dist-v (get-in dm [(:y v) (:x v)])
              rest-q (remove #(= v %1) q)
              neighbours (dj--coord->neighbours v)
              valid-neighbours (dj--filter-valid-coords game neighbours)]
          (recur rest-q (reduce (fn [dm {:keys [x y]}]
                                  (let [neighbour-dist (get-in dm [y x])
                                        new-dist (inc dist-v)]
                                    (if (< new-dist neighbour-dist)
                                      (assoc-in dm [y x] new-dist)
                                      dm))) dm valid-neighbours)))))))

(defn add-distance-to-coords [dm coords]
  (remove #(= infinity (:distance %1))) (map (fn [{:keys [x y]}]
                                               {:x x :y y :distance (get-in dm [y x])}) coords))

(defn sort-coords-by-distance [coords]
  (sort-by :distance (sort-by :y (sort-by :x coords))))

(defn dj--shorted-path-from-point [distance-map from]
  (loop [where-at from
         path [from]]
    (let [neighbours (dj--coord->neighbours where-at)
          next-neighbour (first (sort-coords-by-distance (add-distance-to-coords distance-map neighbours)))]
      (println )
      (if (= (:distance next-neighbour) 0)
        (conj path next-neighbour)
        (recur next-neighbour (conj path next-neighbour))))))

(def game (fname->game "day15.small.txt"))
(def dm (dj--build-distance-map game {:x 1 :y 1}))