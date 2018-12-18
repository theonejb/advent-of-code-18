(ns aoc18.day18
  (:require [clojure.string :as str])
  (:import (java.util.concurrent TimeUnit)))

(defn get-input [fname]
  (str/split-lines (slurp fname)))

(defn input->grid [input]
  (vec (map vec input)))

(defn get-bounds [grid]
  {:width  (count (first grid))
   :height (count grid)})

(defn get-coords [grid]
  (let [{:keys [width height]} (get-bounds grid)]
    (for [y (range height)
          x (range width)]
      {:x x :y y})))

(defn is-valid-coord? [grid {:keys [x y]}]
  (let [{:keys [width height]} (get-bounds grid)]
    (and (>= x 0) (< x width) (>= y 0) (< y height))))

(defn get-neighbour-coords [grid {:keys [x y]}]
  (filter (partial is-valid-coord? grid)
          [{:x x :y (dec y)}
           {:x (inc x) :y (dec y)}
           {:x (inc x) :y y}
           {:x (inc x) :y (inc y)}
           {:x x :y (inc y)}
           {:x (dec x) :y (inc y)}
           {:x (dec x) :y y}
           {:x (dec x) :y (dec y)}]))

(defn get-coord-from-grid [grid {:keys [x y]}]
  (get-in grid [y x]))

(defn set-coord-in-grid [grid {:keys [x y]} value]
  (assoc-in grid [y x] value))

(defn count-neighbours [grid coord]
  (let [neighbour-coords (get-neighbour-coords grid coord)
        neighbours (map (partial get-coord-from-grid grid) neighbour-coords)
        counter (reduce (fn [counter neighbour]
                          (assoc counter neighbour (inc (counter neighbour 0)))) {} neighbours)]
    counter))

(defn print-grid [grid]
  (let [coords (get-coords grid)]
    (println (str/join (map (fn [{:keys [x y] :as coord}]
                              (str
                                (if (and (= 0 x) (not (= 0 y))) \newline)
                                (get-coord-from-grid grid coord)
                                \space))
                            coords)))))

(defn fname->grid [fname]
  (-> fname
      get-input
      input->grid))

(def open \.)
(def tree \|)
(def lumberyard \#)

(defn get-new-value-of-cell [cell neighbours]
  (condp = cell
    open (if (>= (neighbours tree 0) 3) tree open)
    tree (if (>= (neighbours lumberyard 0) 3) lumberyard tree)
    lumberyard (if (and (>= (neighbours lumberyard 0) 1) (>= (neighbours tree 0) 1))
                 lumberyard
                 open)))

(defn tick [grid]
  (reduce (fn [new-grid coord]
            (let [this-cell (get-coord-from-grid grid coord)
                  neighbours-count (count-neighbours grid coord)]
              (set-coord-in-grid new-grid coord (get-new-value-of-cell this-cell neighbours-count))))
          grid (get-coords grid)))

(defn count-types [grid]
  (reduce (fn [counter area-type]
            (assoc counter area-type (inc (counter area-type 0)))) {} (flatten grid)))

(defn do-ticks [grid resource-values n end-value]
  (let [counter (count-types grid)
        v (* (counter tree) (counter lumberyard))
        grid (tick grid)
        resource-values (conj resource-values v)]
    (println n)
    (if (= n end-value)
      {:grid grid :resource-values resource-values}
      (recur grid resource-values (inc n) end-value))))