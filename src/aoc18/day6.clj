(ns aoc18.day6
  (:require [clojure.string :as str]))

(defn get-input [fname] (str/split-lines (slurp fname)))
(defn parse-input-line [line]
  (let [[_ xS yS] (re-find #"(\d+), (\d+)" line)]
    {:x (Integer/parseInt xS)
     :y (Integer/parseInt yS)}))
(defn get-coords-from-input [input]
  (map-indexed (fn [index line]
                 (assoc (parse-input-line line)
                   :id (char (+ index (int \A)))
                   :ident (char (+ index (int \a)))))
               input))

(defn get-bounds-for-data [coords]
  {:x-max (inc (last (sort (map :x coords))))
   :y-max (int (last (sort (map :y coords))))})

(defn get-grid [w h]
  (vec (repeat h (vec (repeat w nil)))))

(defn print-grid [grid]
  (doseq [row grid]
    (doseq [col row]
      (if (not (nil? col))
        (print col)
        (print \.)))
    (println)))

(defn add-coords-to-grid [grid coords]
  (reduce (fn [grid coord]
            (assoc-in grid [(:y coord) (:x coord)] (:id coord))) grid coords))

(defn get-distance-from-coord [[orig-x orig-y] {:keys [x y]}]
  (+ (Math/abs (- orig-x x)) (Math/abs (- orig-y y))))
(defn add-distance-info-to-coord [origin coord]
  (assoc coord :distance (get-distance-from-coord origin coord)))

(defn get-closest-coords [from coords]
  (let [coords-with-distance (map #(add-distance-info-to-coord from %1) coords)
        min-distance (:distance (apply min-key :distance coords-with-distance))
        coords-with-min-distance (filter #(= min-distance (:distance %1)) coords-with-distance)]
    coords-with-min-distance))

(defn fill-grid [grid coords]
  (vec (map-indexed (fn [y row]
                      (vec (map-indexed (fn [x col]
                                          (if (nil? col)
                                            (let [closest-coords (get-closest-coords [x y] coords)]
                                              (if (= 1 (count closest-coords))
                                                (:ident (first closest-coords))
                                                nil))
                                            col))
                                        row)))
                    grid)))

(defn get-edge-coordinates [grid]
  (let [height (count grid)
        width (count (first grid))]
    (filter (fn [[x y]]
              (or
                (= 0 x)
                (= (dec width) x)
                (= 0 y)
                (= (dec height) y)))
            (for [x (range width)
                  y (range height)]
              [x y]))))

(defn get-idents-at-coords [grid coord-pairs]
  (map #(get-in grid %1) (map reverse coord-pairs)))

(defn main [fname]
  (let [coords (get-coords-from-input (get-input fname))
        bounds (get-bounds-for-data coords)
        grid (get-grid (inc (:x-max bounds)) (inc (:y-max bounds)))
        grid-with-coords (add-coords-to-grid grid coords)
        filled-grid (fill-grid grid-with-coords coords)

        edge-coordinates (get-edge-coordinates filled-grid)
        edge-idents (set (filter identity (get-idents-at-coords filled-grid edge-coordinates)))
        grid-without-edge-idents (remove edge-idents (flatten filled-grid))
        area-counts (reduce (fn [counter ident]
                              (assoc counter ident (inc (get counter ident 0)))) {} grid-without-edge-idents)
        area-counts-ident-only
        (let [idents (map :ident coords)
              idents-pred (set idents)]
          (filter #(idents-pred (first %1)) area-counts))

        largest-area (apply max-key second area-counts-ident-only)]

    largest-area))

;(def largest-area (main))
;(println "Largest area is: " (inc (second largest-area)) " of coord " (first largest-area))

(defn get-all-points [{:keys [x-max y-max]}]
  (for [x (range x-max)
        y (range y-max)]
    [x y]))

(defn calculate-distance-to-all-coords [point coords]
  (println "Processing point " point)
  (reduce (fn [sum next-distance]
            (if (> sum 10000)
              (reduced 10000)
              (+ sum next-distance)))
          0 (map #(get-distance-from-coord point %1) coords)))

(def coords (get-coords-from-input (get-input "day6.txt")))
(def bounds (get-bounds-for-data coords))
(def points (get-all-points {:x-max 354 :y-max 351}))
(defn get-points-closer-than [upper-bound] (filter #(< (calculate-distance-to-all-coords %1 coords) upper-bound) points))
(println (count (get-points-closer-than 10000)))